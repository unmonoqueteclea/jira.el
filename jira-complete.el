;;; jira-complete.el --- Help for filling information  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Created: 2025-02-16

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides functions to help filling user information in Jira issues
;; or other kinds of elements

;;; Code:

(require 'cl-lib)
(require 'org)

(require 'jira-api)


(defconst jira-complete--not-allowed-schemas
  '("attachment", "timetracking")
  "Schemas that are not allowed in the fields metadata.")

(defun jira-complete--validate-key (key)
  "Validate a Jira issue KEY format."
  (and key (string-match-p "^[A-Z0-9]+-[0-9]+$" key)))

(defun jira-complete--extract-key-from-url (url)
  "Extract Jira issue key from a URL."
  (let* ((path (url-filename (url-generic-parse-url url))))
    (car (last (split-string path "/")))))

(defun jira-complete--find-key-at-point ()
  "Find a Jira issue key at or around the current point."
  (let ((key-regex  "\\b[A-Z0-9]+-[0-9]+\\b"))
    (when (thing-at-point-looking-at key-regex)
      (match-string-no-properties 0))))

(defun jira-complete--get-default-issue-key-input ()
  "Get the default input for issue key completion, from URL or key at point."
  (let* ((url-at-point (thing-at-point 'url))
         (key-from-url (and url-at-point (jira-complete--extract-key-from-url url-at-point)))
         (key-at-point (jira-complete--find-key-at-point)))
    (cond (key-from-url key-from-url) (key-at-point key-at-point) (t ""))))

(defun jira-complete-ask-issue ()
  "Read an issue key or URL from the user and return its key."
  (let* ((default-input (jira-complete--get-default-issue-key-input))
         (input (read-string (format "Find issue by key or URL%s: "
                                     (if (string= default-input "")
                                         ""
                                       (concat " (default " default-input ")")))
                             nil
                             nil
                             default-input))
         (key (or (jira-complete--extract-key-from-url input) input)))
    (if (jira-complete--validate-key key)
	key (message "Invalid Jira issue key or URL: %s" input))))

(defun jira-complete--is-allowed (field)
  ;; check if the field is allowed to be used in the issue creation
  (let ((schema (alist-get 'schema field)))
    (not (member (alist-get 'system schema) jira-complete--not-allowed-schemas))))

(defun jira-complete--custom-field-id (field-id)
  ;; remove the "customfield_" prefix from the field-id if it exists
  (if (string-prefix-p "customfield_" field-id)
      (substring field-id (length "customfield_"))
    field-id))

(defun jira-complete--allowed-value-choices (val)
  ;; create a cons cell with the name and the identifier of an allowed field value
  (let* ((v-id (or (alist-get 'id val)
		   (alist-get 'accountId val)
		   (alist-get 'self val)))
	 (v-name (or (alist-get 'name val)
		     (alist-get 'displayName val)
		     (alist-get 'value val))))
    (cons (format "%s [%s]" v-name v-id) v-id)))

(defun jira-complete--extract-field (issue field-id)
  ;; for autocomplete-from-jql, extract unique field values from results
  (let ((value (alist-get (intern field-id) (alist-get 'fields issue))))
    (when value
      (let ((list-value (seq-into value 'list)))
        (if (stringp (car list-value))
            (mapcar (lambda (label-str) (cons label-str label-str)) list-value)
          (seq-map (lambda (entry) (jira-complete--allowed-value-choices entry))
                   list-value))))))

(defun jira-complete--autocomplete-from-jql (field-id field-name is-array)
  ;; autocomplete possibles values for a field with q JQL query
  (let* ((jql (if (string-prefix-p "customfield_" field-id)
                  (format "cf[%s] is not EMPTY" (jira-complete--custom-field-id field-id))
                (format "%s is not EMPTY" field-id)))
	 (params `(("jql" . ,jql) ("maxResults" . ,100) ("fields" . ,field-id)))
	 (req (jira-api-search :sync t :params params))
	 (issues (alist-get 'issues (request-response-data req)))
	 (extract (lambda (issue) (jira-complete--extract-field issue field-id)))
	 (choices
	  (cl-remove-duplicates
	   (apply #'append (mapcar extract issues))
	   :test #'equal)))
    (if is-array
        (let ((selected
	       (completing-read-multiple
                (format "Select values for %s: " field-name) choices nil t)))
          (if (string-equal field-name "Labels")
              selected
	    ;; labels just expects a list of strings, not objects
            (mapcar (lambda (s) `((id . ,(cdr (assoc s choices))))) selected)))
      (let ((selected
	     (completing-read
	      (format "Select a value for %s: " field-name) choices nil t)))
	`((id . ,(cdr (assoc selected choices))))))))

(defun jira-complete--autocomplete-options (url query)
  ;; call synchronously endpoint to retrieve autocomplete options for a field
  (request-response-data (jira-api-call "GET" (concat url query) :sync t)))

(defun jira-complete--ask-autocomplete (fname url is-array)
  ;; ask a user for an initial values to call autocomplete and show options
  ;; reporter autocomplete url has no ?query= at the end, so user cannot pre-filter it
  ;; see https://jira.atlassian.com/browse/JRACLOUD-84826
  (let* ((ask-query (not (string-search "user/recommend?context=Reporter" url)))
	 (msg (format "Find options for field [%s]. Write something and press RET: " fname))
         (query (if ask-query (read-string msg) ""))
         (values (jira-complete--autocomplete-options url query))
         (choices (mapcar 'jira-complete--allowed-value-choices values)))
    (if (seq-empty-p choices)
        (message "No values found for %s" fname)
      (if is-array
          (let ((selected (completing-read-multiple
			   (format "Select values for %s: " fname) choices nil t)))
            (mapcar (lambda (s) `((id . ,(cdr (assoc s choices))))) selected))
        (let ((selected (completing-read (format "Select a value for %s: " fname) choices nil t)))
          `((id . ,(cdr (assoc selected choices)))))))))

(defun jira-complete-ask-field (fd)
  ;; ask the user for a value for a field, providing some help
  ;; fd comes from the result of the metadata call
  (let* ((values (alist-get 'allowedValues fd))
         (fid (alist-get 'fieldId fd))
         (fname (alist-get 'name fd))
         (schema (alist-get 'schema fd))
         (is-array (string= (alist-get 'type schema) "array"))
         (schema-type
          (if is-array
              (alist-get 'items schema)
            (alist-get 'type schema)))
         (autocomplete (alist-get 'autoCompleteUrl fd)))
    (cond
     ((or (string-equal fname "Sprint") (string-equal fname "Labels"))
      (jira-complete--autocomplete-from-jql fid fname is-array))
     ((and autocomplete (not (string-empty-p autocomplete)))
      (jira-complete--ask-autocomplete fname autocomplete is-array))
     ((or (string= schema-type "date") (string= schema-type "datetime"))
      (let* ((with-time-p (string= schema-type "datetime"))
             (time (org-read-date with-time-p t)))
        (when time
          (if with-time-p
              (format-time-string "%Y-%m-%dT%H:%M:%S.000+0000" time)
            (format-time-string "%Y-%m-%d" time)))))
     ((and (string= schema-type "issuelink") (or (not values) (seq-empty-p values)))
      (let ((issue-key (read-string (format "Enter issue key for %s: " fname))))
        (when (and issue-key (not (string-empty-p issue-key)))
          `((key . ,issue-key)))))
     ((and values (not (seq-empty-p values)))
      ;; extract the id and name of the field
      (let* ((choices (mapcar 'jira-complete--allowed-value-choices values)))
        (if is-array
            (let ((selected
                   (completing-read-multiple
                    (format "Select values for %s: " fname) choices nil t)))
              (mapcar (lambda (s) `((id . ,(cdr (assoc s choices))))) selected))
          (let ((selected (completing-read (format "Select a %s: " fname) choices nil t)))
            `((id . ,(cdr (assoc selected choices))))))))
     (t
      (read-string (format "Enter value for field %s: " fname))))))


(cl-defun jira-complete--issue-from-metadata
    (metadata &key issue-type project-key parent-key callback)
  "Create a Jira issue asking the user values for each on of the files
from the METADATA provided by the API.

If project-key or parent-key are provided, they will be used to automatically
fill the 'project' and 'parent' fields."
  (let* ((fields (alist-get 'fields metadata))
	 (required-fields
	  (seq-filter (lambda (field)
			(and (eq (alist-get 'required field) t)
			     (not (eq (alist-get 'hasDefaultValue field) t))))
		      fields))
	 (pending-ields
	  (seq-filter (lambda (field) (jira-complete--is-allowed field)) fields))
	 (required-fields-keys
	  (mapcar (lambda (field) (alist-get 'key field)) required-fields))
	 (field-values
	  (mapcar
	   (lambda (field)
	     (cond
	      ((and parent-key (string= (alist-get 'key field) "parent"))
	       `((key . ,parent-key)))
	      ((and project-key (string= (alist-get 'key field) "project"))
	       `((key . ,project-key)))
	      ((and issue-type (string= (alist-get 'key field) "issuetype"))
	       `((id . ,issue-type)))
	      (t
	       (jira-complete-ask-field field))))
	   required-fields))
	 (response (cl-mapcar #'cons required-fields-keys field-values)))
    (if callback (funcall callback response) (message "%s" response))))


(cl-defun jira-complete-ask-issue-fields (project-key issue-type-id &key parent-key callback)
  "Ask for the fields required to create a Jira issue."
  (jira-api-get-create-metadata
   project-key issue-type-id
   :callback
   (lambda (data response)
     (jira-complete--issue-from-metadata
      data
      :issue-type issue-type-id
      :project-key project-key
      :parent-key parent-key
      :callback callback))))

(provide 'jira-complete)

;;; jira-complete.el ends here

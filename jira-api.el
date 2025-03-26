;;; jira-api.el --- Jira REST API  -*- lexical-binding: t -*-

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

;; Handle communication with Jira REST API.

;;; Code:

(require 'auth-source)
(require 'request)

(defgroup jira nil "Emacs interface to Jira."
  :group 'convenience  :group 'extensions)

(defcustom jira-username ""
  "Jira username (usually, an email)."
  :group 'jira :type 'string)

(defcustom jira-base-url ""
  "Jira instance URL, like: https://acme.atlassian.net."
  :group 'jira :type 'string)

(defcustom jira-api-version 3
  "Jira API version (Versions 2 or 3 are allowed)."
  :group 'jira
  :type '(choice (const 2) (const  3)))

(defcustom jira-token ""
  "Jira API token."
  :group 'jira :type 'string)

(defcustom jira-tempo-token ""
  "Jira tempo.io API token."
  :group 'jira :type 'string)

(defcustom jira-debug nil
  "Whether to log jira.el internal processes data, including API responses."
  :group 'jira :type 'boolean)

(defcustom jira-token-is-personal-access-token nil
  "Whether the provided token is a Personal Access Token (not an JIRA API Token)"
  :group 'jira :type'boolean)

(defvar jira-tempo-url "https://api.tempo.io/4/" "Jira Tempo API URL.")
(defvar jira-account-id nil "Jira account ID of the current user.")
(defvar jira-fields nil "Jira custom fields available for the current user.")
(defvar jira-active-issue-transitions nil "Allowed transitions for active issue.")
(defvar jira-statuses nil "Jira allowed statuses.")
(defvar jira-resolutions nil "Jira allowed resolutions.")
(defvar jira-projects nil "Jira projects (5 most recent).")
(defvar jira-projects-versions nil "Jira project versions (releases).")

(defun jira-api--username (host)
  "Retrieve the username for HOST from config or auth-source."
  (if (and jira-username (not (string= "" jira-username)))
      jira-username
      (let* ((auth-host (replace-regexp-in-string "https://" "" host))
	     (auth-info (car (auth-source-search :host auth-host :require '(:user)))))
	(when auth-info (plist-get auth-info :user)))))

(defun jira-api--token (host)
  "Retrieve the token for HOST from config or auth-source."
  (if (and jira-token (not (string= "" jira-token)))
      jira-token
      (let* ((auth-host (replace-regexp-in-string "https://" "" host))
	     (auth-info (car (auth-source-search :host auth-host :require '(:secret)))))
	(when auth-info (funcall (plist-get auth-info :secret))))))

(defun jira-api--tempo-token ()
  "Retrieve the token for HOST from config or auth-source."
  (if (and jira-tempo-token (not (string= "" jira-tempo-token)))
      jira-tempo-token
      (let* ((auth-info (car (auth-source-search :host "tempo.io" :require '(:secret)))))
	(when auth-info (funcall (plist-get auth-info :secret))))))

(defun jira-api--auth-header (username token)
  "Generate the Authorization header for Jira requests with USERNAME and TOKEN."
  (if jira-token-is-personal-access-token
      (format "Bearer %s" token)
    (format "Basic %s" (base64-encode-string (concat username ":" token) t))))

(defun jira-api--tempo-auth-header (token)
  "Generate the Authorization header for Jira Tempo API requests with TOKEN."
  (format "Bearer %s" token))

(defun jira-api--url (base-url)
  "Generate the full URL from BASE-URL for Jira API requests."
  (let ((version (if (numberp jira-api-version)
		     (number-to-string jira-api-version)
		   jira-api-version)))
    (concat base-url "/rest/api/" version "/")))

(defun jira-api--callback-success-log (data _response)
  "Log the RESPONSE DATA of a successful Jira API request."
  (message "[Jira API Response]: %s" (json-encode data)))


(defun jira-api--callback-error-log (response error-thrown)
  "Log the RESPONSE data and ERROR-THROWN of a failed Jira API request."
  (let ((status-code (request-response-status-code response))
        (response-data (request-response-data response))
        (response-headers (request-response-headers response))
        (error-message (if (symbolp error-thrown)
                           (symbol-name error-thrown)
                         (format "%s" error-thrown))))
    (message "[Jira API Error]: %s" error-message)
    (message "[Jira API Status Code]: %s" (or status-code "Unknown"))
    (message "[Jira API Response Headers]: %s" (or response-headers "No headers"))
    (message "[Jira API Response Body]: %s" (or response-data "No response body"))))


(cl-defun jira-api-call (verb endpoint &key params data callback parser)
  "Perform a VERB request to the Jira API ENDPOINT.

PARAMS is a list of cons cells, DATA is the request body, and CALLBACK
is the function to call if successful. PARSER is a function to call
in a buffer with the result data, defaulting to `json-read'."
  (message "[Jira API Call]: %s %s" verb endpoint)
  (when (and jira-debug data)
    (message "[Jira API Call Data]: %s" (json-encode data)))
  (let* ((username (jira-api--username jira-base-url))
	 (token (jira-api--token jira-base-url))
	 (auth (jira-api--auth-header username token)))
    (request
      (concat (jira-api--url jira-base-url) endpoint)
      :type verb
      :headers `(("Authorization" . ,auth)
                 ("Content-Type" . "application/json"))
      :params (or params '())
      :data (if data (json-encode data) nil)
      :parser (or parser 'json-read)
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
		  (when jira-debug
                    (jira-api--callback-success-log data response))
		  (when callback
		    (funcall callback data response))))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (jira-api--callback-error-log response error-thrown))))))

(cl-defun jira-api-tempo-call (verb endpoint &key params callback)
  "Perform a VERB request to the Jira Tempo API ENDPOINT.
Calling CALLBACK if successful and passing PARAMS."
  (message "[Jira Tempo Call]: %s %s" verb endpoint)
  (let ((auth (jira-api--tempo-auth-header (jira-api--tempo-token))))
    (request
      (concat jira-tempo-url endpoint)
      :type verb
      :headers `(("Authorization" . ,auth)
                 ("Content-Type" . "application/json"))
      :params (or params '())
      :parser 'json-read
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
		  (when jira-debug
                    (jira-api--callback-success-log data response))
		  (when callback
		    (funcall callback data response))))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (jira-api--callback-error-log response error-thrown))))))

(cl-defun jira-api-get-account-id (&key force callback)
  "Retrive the account ID of the current user.

FORCE will force the request even if the account ID is already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-account-id))
      (let ((c (lambda (data _response)
                 (setq jira-account-id (cdr (assoc 'accountId data)))
                 (when callback (funcall callback)))))
        (jira-api-call "GET" "myself" :callback c))
    (when callback (funcall callback))))

(cl-defun jira-api-get-fields (&key force callback)
  "Retrive the available custom fields for the current user.

FORCE will force the request even if the fields are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-fields))
      (let ((c (lambda (data _response)
                 (setq jira-fields
                       (mapcar (lambda (field)
                                 (cons (cdr (assoc 'name field))
                                       (cdr (assoc 'key field))))
                         data))
                 (when callback (funcall callback)))))
        (jira-api-call "GET" "field" :callback c))
    (when callback (funcall callback))))

(defun jira-api-get-transitions (issue-key)
  "Get the transitions available for an ISSUE-KEY."
  (let* ((format-transition
          (lambda (tr) (cons (alist-get 'name (alist-get 'to tr))
                             (alist-get 'id tr))))
         (extract
          (lambda (data _response)
            (let* ((data-alist (json-read-from-string (json-encode data)))
                   (transitions (alist-get 'transitions data-alist)))
              (mapcar format-transition transitions)))))
    (jira-api-call "GET" (format "issue/%s/transitions" issue-key)
                   :callback (lambda (data response)
                               (setq jira-active-issue-transitions
                                     (funcall extract data response))))))

(cl-defun jira-api-get-statuses (&key force callback)
  "Get the list of allowed issues statuses.

FORCE will force the request even if the statuses are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-statuses))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'id s)))))
        (jira-api-call
         "GET" "status"
         :callback
         (lambda (data _response)
           (let* ((statuses (json-read-from-string (json-encode data))))
             (setq jira-statuses (mapcar fmt statuses)))
           (when callback (funcall callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-resolutions (&key force callback)
  "Get the list of allowed resolutions.

FORCE will force the request even if the resolutions are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-resolutions))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'id s)))))
        (jira-api-call
         "GET" "resolution"
         :callback
         (lambda (data _response)
           (let* ((resolutions (json-read-from-string (json-encode data))))
             (setq jira-resolutions (mapcar fmt resolutions)))
           (when callback (funcall callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-projects (&key force callback)
  "Get the 10 most recent projects.

FORCE will force the request even if the projects are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-projects))
      (let* ((fmt (lambda (s) (cons (alist-get 'key s) (alist-get 'name s)))))
        (jira-api-call
         "GET" "project"
         :params `(("recent" . ,10))
         :callback
         (lambda (data _response)
           (let* ((projects (json-read-from-string (json-encode data))))
             (setq jira-projects (mapcar fmt projects))
             (jira-api-get-versions :force t :callback callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-versions (&key force callback)
  "Get the list of versions for the stored projects.

FORCE will force the request even if the versions are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-projects-versions))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'id s)))))
        (setq jira-projects-versions nil)
        (dolist (project jira-projects)
          (jira-api-call
           "GET" (concat "project/" (car project) "/version")
           :params `(("orderBy" . "-sequence")
                     ("maxResults" . 50))
           :callback
           (lambda (data _response)
             (let* ((data-alist (json-read-from-string (json-encode data)))
                    (versions (mapcar fmt (alist-get  'values data-alist))))
               (setq jira-projects-versions
                     (cons (cons (car project) versions) jira-projects-versions)))
             (when callback (funcall callback))))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-basic-data (&key force)
  "Get some basic data (custom fields, projects, statuses, etc) from JIRA API.

FORCE will force the request even if the data is already stored."
  ;; one call after the other, to avoid request burst
  (let* ((fds (lambda () (jira-api-get-fields :force force)))
         (st (lambda () (jira-api-get-statuses :force force :callback fds)))
         (res (lambda () (jira-api-get-resolutions :force force :callback st)))
         (prj (lambda () (jira-api-get-projects :force force :callback res)))
         (account (lambda () (jira-api-get-account-id :force force :callback prj))))
    (funcall account)))


(provide 'jira-api)

;;; jira-api.el ends here

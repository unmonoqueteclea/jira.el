;;; jira-export.el --- Export functionality for jira.el  -*- lexical-binding: t -*-

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

;; This module provides export functionality for JIRA issues list.
;; Supports exporting to Markdown, Org-mode, and CSV formats.

;;; Code:

(require 'jira-fmt)
(require 'jira-utils)
(require 'jira-table)
(require 'tablist)
(require 'transient)


(defvar jira-export-supported-formats '(markdown org csv)
  "List of supported export formats.")

(defun jira-export--escape-csv-field (value)
  "Escape VALUE for CSV format.
Handles quotes and commas properly."
  (let ((v (or value "")))
    (if (or (string-match-p "\"" v) (string-match-p "," v) (string-match-p "\n" v))
        (concat "\"" (replace-regexp-in-string "\"" "\"\"" v) "\"")
      v)))

(defun jira-export--extract-link-info (formatted-value)
  "Extract link information from FORMATTED-VALUE.
Return (link-text . url) if it's a link, otherwise return nil.
Handles button properties and detects generic URLs."
  (when formatted-value
    (cond
     ;; Handle button list from jira-fmt-issue-key (format: (text 'href url 'face ...))
     ((listp formatted-value)
      (let ((text (car formatted-value))
	    (url nil)
	    (found-href nil))
        (dotimes (i (1- (length formatted-value)))
          (when (eq (nth i formatted-value) 'href)
            (setq found-href t)
            (setq url (nth (1+ i) formatted-value))))
        (when (and text found-href url (stringp url))
          (cons text url))))
     ;; Handle button properties (for other potential formatters)
     ((and (listp formatted-value) (plist-get formatted-value :href))
      (cons (or (plist-get formatted-value :display)
                (if (stringp (car formatted-value)) (car formatted-value) "")
                "")
            (plist-get formatted-value :href)))
     ;; Handle generic URLs in text
     ((stringp formatted-value)
      (let ((url-match (string-match "\\(https?://[^[:space:]\n]+\\)" formatted-value)))
        (when url-match
          (let ((url (match-string 1 formatted-value))
                (text (if (> url-match 0)
                          (substring formatted-value 0 url-match)
                        formatted-value)))
            (cons (string-trim text) url)))))
     ;; Handle buttonized strings
     ((and (stringp formatted-value)
           (get-text-property 0 'href formatted-value))
      (cons formatted-value (get-text-property 0 'href formatted-value)))
     ;; No link found
     (t nil))))

(defun jira-export--format-link (link-text url format-type)
  "Format LINK-TEXT and URL according to FORMAT-TYPE."
  (if (and link-text url (not (string-empty-p link-text)) (not (string-empty-p url)))
      (pcase format-type
        ('markdown (format "[%s](%s)" link-text url))
        ('org (format "[[%s][%s]]" url link-text))
        ('csv url)
        (_ (or link-text url "")))
    (or link-text url "")))

(defun jira-export--extract-text (formatted-value)
  "Extract text content from FORMATTED-VALUE."
  (cond
   ((stringp formatted-value) formatted-value)
   ((and (listp formatted-value) (stringp (car formatted-value))) (car formatted-value))
   (t (format "%s" formatted-value))))

(defun jira-export--get-field-value (issue field-path &optional format-type)
  "Get FIELD-PATH value from ISSUE using existing field extraction and formatters.
If FORMAT-TYPE is provided, format links appropriately for that format."
  (let* ((raw-value (jira-table-extract-field jira-issues-fields field-path issue))
         (value (or raw-value ""))
         (formatter (jira-table-field-formatter jira-issues-fields field-path))
         (formatted-value (if formatter (funcall formatter value) (format "%s" value)))
         (link-info (jira-export--extract-link-info formatted-value)))
    (if link-info
        (jira-export--format-link (car link-info) (cdr link-info) format-type)
      (jira-export--extract-text formatted-value))))

(defun jira-export--get-issue-fields (issue &optional format-type)
  "Get non-empty field values for ISSUE as a list of (name . value) pairs.
If FORMAT-TYPE is provided, format links appropriately for that format."
  (let ((fields '()))
    (dolist (field jira-issues-table-fields)
      (let* ((field-name (jira-table-field-name jira-issues-fields field))
             (field-value (jira-export--get-field-value issue field format-type)))
        (when (and field-value (not (string= field-value "")))
          (push (cons field-name field-value) fields))))
    (reverse fields)))

(defun jira-export--generate-issue-content (issue format-type)
  "Generate content for a single ISSUE in FORMAT-TYPE."
  (let* ((key (jira-export--extract-text (jira-table-extract-field jira-issues-fields :key issue)))
         (summary (jira-export--get-field-value issue :summary))
         (issue-fields (jira-export--get-issue-fields issue format-type)))
    (pcase format-type
      ('markdown
       (concat (format "# %s - %s\n" key (or summary "No Summary"))
               (mapconcat (lambda (field-pair)
                            (format "- **%s:** %s\n" (car field-pair) (cdr field-pair)))
                          issue-fields "")
               "\n"))
      ('org
       (concat (format "* %s - %s\n" key (or summary "No Summary"))
               ":PROPERTIES:\n"
               (mapconcat (lambda (field-pair)
                            (format ":%s: %s\n"
                                    (replace-regexp-in-string " " "_" (car field-pair))
                                    (cdr field-pair)))
                          issue-fields "")
               ":END:\n\n"))
      (_ ""))))

(defun jira-export--generate-markdown (issues)
  "Generate markdown format from ISSUES list."
  (mapconcat (lambda (issue) (jira-export--generate-issue-content issue 'markdown))
             issues ""))

(defun jira-export--generate-org-mode (issues)
  "Generate org-mode format from ISSUES list."
  (mapconcat (lambda (issue) (jira-export--generate-issue-content issue 'org))
             issues ""))

(defun jira-export--get-csv-row (issue)
  "Generate a CSV row for ISSUE using all fields."
  (let ((row '()))
    (dolist (field jira-issues-table-fields)
      (let* ((field-value (jira-export--get-field-value issue field 'csv))
             (escaped-value (jira-export--escape-csv-field field-value)))
        (push escaped-value row)))
    (reverse row)))

(defun jira-export--generate-csv (issues)
  "Generate CSV format from ISSUES list."
  (let* ((headers (mapcar (lambda (field)
                            (jira-table-field-name jira-issues-fields field))
                          jira-issues-table-fields))
         (csv-rows (list (string-join headers ","))))
    (dolist (issue (append issues nil))
      (push (string-join (jira-export--get-csv-row issue) ",") csv-rows))
    (string-join (reverse csv-rows) "\n")))


(defun jira-export--generate-content (format-type issues)
  "Generate content in FORMAT-TYPE from ISSUES list."
  (pcase format-type
    ('markdown (jira-export--generate-markdown issues))
    ('org (jira-export--generate-org-mode issues))
    ('csv (jira-export--generate-csv issues))
    (_ (error "Unsupported export format: %s" format-type))))

(defun jira-export--create-output-buffer (format-type issues)
  "Create output buffer with FORMAT-TYPE content from ISSUES."
  (let* ((format-name (capitalize (symbol-name format-type)))
         (buffer-name (format "*Jira Export: %s*" format-name))
         (content (jira-export--generate-content format-type issues)))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert content)
      (goto-char (point-min))
      (cond
       ((eq format-type 'org) (org-mode))
       ((eq format-type 'markdown) (fundamental-mode)))
      (setq buffer-read-only t))
    (pop-to-buffer buffer-name)
    (message "Exported %d issues to %s format" (length issues) format-name)))

(defun jira-export-issues (format-type)
  "Export marked issues or all visible issues in FORMAT-TYPE.
FORMAT-TYPE should be one of 'markdown, 'org, or 'csv'."
  (interactive
   (list (intern (completing-read "Export format: "
                                  (mapcar (lambda (f) (symbol-name f))
                                          jira-export-supported-formats)))))
  (unless (memq format-type jira-export-supported-formats)
    (error "Unsupported format: %s. Supported formats: %s"
           format-type jira-export-supported-formats))

  (let ((issues jira-issues--raw-issues))
    (if (null issues)
        (message "No issues to export")
      (jira-export--create-output-buffer format-type issues))))

(defmacro jira-export--define-format-function (format-symbol format-name)
  "Define a format-specific export function.
FORMAT-SYMBOL is the symbol to pass to `jira-export-issues'.
FORMAT-NAME is a string used in the docstring."
  (let ((func-name (intern (format "jira-export-%s" (symbol-name format-symbol)))))
    `(defun ,func-name ()
       ,(format "Export issues to %s format." format-name)
       (if (derived-mode-p 'jira-issues-mode)
           (jira-export-issues ',format-symbol)
         (message "Export is only available in jira-issues-mode")))))

(jira-export--define-format-function markdown "Markdown")
(jira-export--define-format-function org "Org-mode")
(jira-export--define-format-function csv "CSV")

(defun jira-export-interactive ()
  "Interactive export function that shows the export menu."
  (if (derived-mode-p 'jira-issues-mode)
      (jira-export-menu)
    (message "Export is only available in jira-issues-mode")))

;;;###autoload (autoload 'jira-export-menu "jira-export" "Show menu for exporting Jira Issues." t)
(transient-define-prefix jira-export-menu ()
  "Show menu for exporting Jira Issues."
  ["Export Format"
   ("m" "Markdown" (lambda () (interactive) (jira-export-markdown)))
   ("o" "Org-mode" (lambda () (interactive) (jira-export-org)))
   ("c" "CSV" (lambda () (interactive) (jira-export-csv)))])


(provide 'jira-export)

;;; jira-export.el ends here

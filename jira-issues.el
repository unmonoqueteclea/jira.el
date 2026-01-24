;;; jira-issues.el --- Jira Issues  -*- lexical-binding: t -*-

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

;; List and manage Jira issues

;;; Code:

(require 'tablist)
(require 'transient)

(require 'jira-api)
(require 'jira-table)
(require 'jira-fmt)
(require 'jira-utils)
(require 'jira-tempo)
(require 'jira-actions)
(require 'jira-detail)
(require 'jira-export)

(defcustom jira-issues-table-fields
  '(:key :issue-type-name :status-name :assignee-name
	 :progress-percent :work-ratio :remaining-time :summary)
  "Fields to show in the Jira issues table.

Allowed values in variable `jira-issues-fields'."
  :group 'jira :type '(repeat (symbol :tag "Field")))

(defcustom jira-issues-max-results 30
  "Maximum number of Jira issues to retrieve."
  :group 'jira :type 'integer)

(defvar jira-issues--loading-p nil
  "A flag to indicate if an API call is in progress.")
(defvar jira-issues--current-jql nil
  "The JQL query for the current issue list.")
(defvar jira-issues--pagination-current nil
  "The pagination token for the currently displayed page of issues.")
(defvar jira-issues--pagination-next nil
  "The pagination token for the next page of issues.")
(defvar jira-issues--pagination-previous nil
  "A list of pagination tokens for previously viewed pages.")
(defvar jira-issues--raw-issues nil
  "The raw issues data for the current page.")


;; files that we always want to retrieve because they are used
;; in some operations
(defvar jira-issues-required-fields '("key" "summary"))

(defvar jira-issues-key-summary-map nil
  "Hash map to store issue keys and the associated summaries.
This information is added to worklogs to make it easier to identify")

(defcustom jira-issues-sort-key (cons "Status" nil)
  "Default tabulated-list sort key for jira-issues-mode.
Value is a cons (COLUMN . DESCENDING). When DESCENDING is non-nil, sort
order is descending. COLUMN must match one of the visible tabulated-list
column headers."
  :group 'jira
  :type '(cons (string :tag "Column")
          (boolean :tag "Descending")))

(defun jira-issues--api-get-issues (jql callback &optional page-token)
  "Retrieve issues from the given JQL filter and call CALLBACK function.

PAGE-TOKEN is optional and used for pagination."
  (let* ((parent (lambda (fd) (jira-table-field-parent jira-issues-fields fd)))
         (fields (append (mapcar parent jira-issues-table-fields)
			 jira-issues-required-fields))
         (params `(("jql" . ,jql)
                   ("maxResults" . ,jira-issues-max-results)
                   ("fields" . ,(mapconcat (lambda (x) (format "%s" x))
                                           (cl-remove nil fields) ",")))))
    (when page-token
      (setq params (append params `(("nextPageToken" . ,page-token)))))
    (when jira-debug (message (concat "Get issues with jql " jql)))
    (jira-api-search :params params :callback callback :errback #'jira-issues--fetch-error)))

(defun jira-issues--api-filters-and (filters)
  "Concat all FILTERS with AND."
  (let ((valid-filters (remove nil filters)))
    (when valid-filters
      (mapconcat (lambda (filter) filter) valid-filters " AND "))))

(defun jira-issues--data-format-cell (issue field)
  "Format the given FIELD from the given ISSUE."
  (let* ((extracted (jira-table-extract-field jira-issues-fields field issue))
         (value (or extracted ""))
         (formatter (jira-table-field-formatter jira-issues-fields field)))
    (if formatter (funcall formatter value) (format "%s" value))))

(defun jira-issues--data-format-issue (issue)
  "Format the given ISSUE."
  (let ((key (jira-table-extract-field jira-issues-fields :key issue))
        (formatter (lambda (fd) (jira-issues--data-format-cell issue fd))))
    (list key (vconcat (mapcar formatter jira-issues-table-fields)))))

(defun jira-issues--store-key-summary (issue)
  "Store the KEY and SUMMARY of the given ISSUE."
  (let ((key (jira-table-extract-field jira-issues-fields :key issue))
	(summary (jira-table-extract-field jira-issues-fields :summary issue)))
    (puthash key summary jira-issues-key-summary-map)))

(defun jira-issues-store-issues-info (issues)
  "Store the KEY and SUMMARY of the given ISSUES."
  (let ((map (make-hash-table :test 'equal)))
    (cl-loop
     for issue across issues
     do (let ((key (jira-table-extract-field jira-issues-fields :key issue))
              (summary (jira-table-extract-field jira-issues-fields :summary issue)))
          (puthash key summary map)))
    map))

(defun jira-issues--fetch-error (&rest _)
  "Handle fetch errors."
  (setq jira-issues--loading-p nil)
  (message "Jira API request failed"))

(defun jira-issues--fetch-and-display (page-token)
  "Fetch and display issues for the given PAGE-TOKEN."
  (when (not jira-issues--loading-p)
    (setq jira-issues--loading-p t)
    (setq jira-issues--pagination-current page-token)
    (jira-issues--api-get-issues
     jira-issues--current-jql #'jira-issues--refresh-table page-token)))

(defun jira-issues-next-page ()
  "Go to the next page of issues."
  (interactive)
  (if jira-issues--pagination-next
      (progn
        (push jira-issues--pagination-current jira-issues--pagination-previous)
        (jira-issues--fetch-and-display jira-issues--pagination-next))
    (message "No more pages.")))

(defun jira-issues-previous-page ()
  "Go to the previous page of issues."
  (interactive)
  (if jira-issues--pagination-previous
      (let ((token (pop jira-issues--pagination-previous)))
        (jira-issues--fetch-and-display token))
    (message "Already on the first page.")))

(defun jira-issues--refresh-table (data _response)
  "Refresh the table with the given RESPONSE DATA."
  (let* ((issues (or (cdr (assoc 'issues data)) (vector)))
         (next-token (cdr (assoc 'nextPageToken data))))
    (setq jira-issues--pagination-next next-token)
    (when jira-debug
      (message "Jira Issues: Page %d" (1+ (length jira-issues--pagination-previous))))
    (setq jira-issues--raw-issues issues)
    (setq tabulated-list-entries (mapcar #'jira-issues--data-format-issue issues))
    (setq jira-issues-key-summary-map (jira-issues-store-issues-info issues))
    (tabulated-list-print t)
    (setq jira-issues--loading-p nil)))

(defun jira-issues--reset-pagination ()
  "Reset pagination state to start from the beginning."
  (setq jira-issues--pagination-current nil)
  (setq jira-issues--pagination-next nil)
  (setq jira-issues--pagination-previous nil)
  (setq jira-issues--raw-issues nil))

(defun jira-issues--refresh ()
  "Refresh the table."
  (let* ((args (transient-args 'jira-issues-menu))
         (myself (transient-arg-value "--myself" args))
         (current-sprint (transient-arg-value "--current-sprint" args))
         (jql (transient-arg-value "--jql=" args))
         (status (transient-arg-value "--status=" args))
         (project (transient-arg-value "--project=" args))
         (resolution (transient-arg-value "--resolution=" args))
         (version (transient-arg-value "--version=" args))
         (assignee (transient-arg-value "--assignee=" args))
         (reporter (transient-arg-value "--reporter=" args))
         (issue-type (transient-arg-value "--type=" args)))
    (let ((additional-filters
           (list (when myself "assignee = currentUser()")
                 (when current-sprint "sprint in openSprints()")
                 (when status (concat "status = \"" status "\""))
                 (when project (concat "project = \"" project "\""))
                 (when resolution (concat "resolution = \"" resolution "\""))
                 (when version (concat "fixversion = \"" version "\""))
                 (when issue-type (concat "issuetype = \"" issue-type "\""))
                 (when assignee
                   (if (string-match-p "EMPTY" assignee)
                       "assignee = EMPTY"
                     (let ((account-id (gethash assignee jira-users)))
                       (when account-id (concat "assignee = " account-id)))))
                 (when reporter
                   (let ((account-id (gethash reporter jira-users)))
                     (when account-id (concat "reporter = " account-id)))))))
      (setq jira-issues--current-jql
            (if jql
                ;; If we have base JQL, combine it with additional filters
                (let ((additional-jql (jira-issues--api-filters-and additional-filters)))
                  (if (and additional-jql (not (string= additional-jql "")))
                      (concat "(" jql ") AND " additional-jql)
                    jql))
              ;; No base JQL, just use additional filters
              (jira-issues--api-filters-and additional-filters)))))
  (jira-issues--reset-pagination)
  (jira-issues--fetch-and-display nil))


(defun jira-issues--myself-inapt-p ()
  "Return t if assignee is set."
  (if transient-current-command
      (let ((args (transient-args transient-current-command)))
	(transient-arg-value "--assignee=" args))))

(defun jira-issues--assignee-inapt-p ()
  "Return t if myself is set."
  (if transient-current-command
      (let ((args (transient-args transient-current-command)))
	(transient-arg-value "--myself" args))))



(transient-define-prefix jira-issues-menu ()
  "Show menu for listing Jira Issues."
  :refresh-suffixes t
  :value '("--myself")
  [["Arguments"
    ("a" "Assignee" "--assignee="
     :transient transient--do-call
     :inapt-if jira-issues--assignee-inapt-p
     :choices
     (lambda () (when jira-users (cons "[EMPTY]" (hash-table-keys jira-users)))))
    ("m" "Just from myself" "--myself"
     :transient transient--do-call
     :inapt-if jira-issues--myself-inapt-p)
    ("c" "Just from current sprint" "--current-sprint"
     :transient t)
    ("s" "Status" "--status="
     :transient t
     :choices
     (lambda () (mapcar (lambda (st) (car st)) jira-statuses)))
    ("p" "Project" "--project="
     :transient t
     :choices
     (lambda () (mapcar (lambda (prj) (car prj)) jira-projects)))
    ("R" "Reporter" "--reporter="
     :transient t
     :choices
     (lambda () (when jira-users (hash-table-keys jira-users))))
    ("r" "Resolution" "--resolution="
     :transient t
     :choices
     (lambda () (mapcar (lambda (res) (car res)) jira-resolutions)))
    ("j""JQL filter" "--jql="
     :transient transient--do-call)
    ("v" "Fix Versions" "--version="
     :transient t
     :choices
     (lambda () (apply #'append (mapcar #'cdr jira-projects-versions))))
    ("t" "Issue Type" "--type="
     :transient t
     :choices
     (lambda () (mapcar #'car jira-issue-types)))]
   ["Arguments Help"
    ("C-x" :info (concat (jira-fmt-set-face "Check " 'italic)
                         "additional options"))
    ("C-x s" :info (concat (jira-fmt-set-face "Set" 'italic)
                           " current arguments for Emacs session"))
    ("C-x C-s" :info (concat (jira-fmt-set-face "Persist" 'italic)
                             " current arguments for all Emacs sessions"))
    ("C-x C-k" :info (concat (jira-fmt-set-face "Reset" 'italic)
                             " arguments to default ones"))]]

  ["Actions"
   ("l" "List Jira Issues" tablist-revert)
   ("F" "Apply named filter" (lambda (&optional _) (interactive) (jira-issues--apply-filter)))])

(defun jira-issues--jump-to-tempo ()
  "Jump to Tempo worklogs, closing current buffer."
  (kill-buffer (buffer-name))
  (jira-tempo))

(defun jira-issues--switch-host-and-refresh ()
  "Switch JIRA host and refresh the issue list."
  (interactive)
  (let ((selected-host (completing-read "JIRA host: " (jira-api--available-urls))))
    (jira-api--set-current-url selected-host)
    (jira-api-get-basic-data)
    (tablist-revert)))

(defun jira-issues--apply-filter ()
  "Apply a named filter and warn user that other arguments won't be applied."
  (if (not jira-filters)
      (message "No filters found")
    (let ((selected-filter (completing-read "Filter: " (mapcar #'car jira-filters))))
      (when selected-filter
        (message "Filter applied: %s (other arguments will be ignored)" selected-filter)
        (setq jira-issues--current-jql (cdr (assoc selected-filter jira-filters)))
        (jira-issues--reset-pagination)
        (jira-issues--fetch-and-display nil)))))

(transient-define-prefix jira-issues-actions-menu ()
  "Show menu for actions on Jira Issues."
  [[:description "Jira Issues List"
                 ("?" "Show this menu" jira-issues-actions-menu)
                 ("g" "Refresh list" tablist-revert)
                 ("l" "List Jira Issues menu" jira-issues-menu)
		 ("f" "Find issue by key/url"
		  (lambda () (interactive) (jira-detail-find-issue-by-key)))
		 ("T" "Jump to Tempo worklogs"
		  (lambda () (interactive) (jira-issues--jump-to-tempo)))
		 ("H" "Switch JIRA host" jira-issues--switch-host-and-refresh)
		 ("e" "Export issues" jira-export-menu)
                 ("M-n" "Next page" jira-issues-next-page)
                 ("M-p" "Previous page" jira-issues-previous-page)]]
  [[:description
    (lambda () (jira-utils-transient-description "Actions on issue"))
    :inapt-if-not jira-utils-marked-item
    ("c" "Copy selected issue id to clipboard"
     (lambda () (interactive)
       (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item)))
     :inapt-if (lambda () (interactive) (jira-utils-multiple-marked-items-p)))
    ("C" "Change issue" jira-actions-change-issue-menu)
    ("I" "Show issue information"
     (lambda () (interactive) (jira-detail-show-issue (jira-utils-marked-item)))
     :inapt-if (lambda () (interactive) (jira-utils-multiple-marked-items-p)))
    ("O" "Open issue in browser"
     (lambda () (interactive) (jira-actions-open-issue (jira-utils-marked-item)))
     :inapt-if (lambda () (interactive) (jira-utils-multiple-marked-items-p)))
    ("W" "Add worklog to issue"
     jira-actions-add-worklog-menu
     :inapt-if (lambda () (interactive) (jira-utils-multiple-marked-items-p)))]])

(defvar jira-issues-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "?" 'jira-issues-actions-menu)
    (define-key map "l" 'jira-issues-menu)
    (define-key map "h" 'jira-issues-actions-menu)
    (define-key map "H" 'jira-issues--switch-host-and-refresh)
    (define-key map "T" (lambda () (interactive) (jira-issues--jump-to-tempo)))
    (define-key map "f" (lambda () (interactive) (jira-detail-find-issue-by-key)))
    (define-key map (kbd "c")
		(lambda () (interactive)
		  (jira-actions-copy-issues-id-to-clipboard (jira-utils-marked-item))))
    (define-key map "C" 'jira-actions-change-issue-menu)
    (define-key map "I" (lambda () (interactive)
                          (jira-detail-show-issue (jira-utils-marked-item))))
    (define-key map (kbd "RET") (lambda () (interactive)
                                  (jira-detail-show-issue (jira-utils-marked-item))))
    (define-key map "O" (lambda () (interactive)
                          (jira-actions-open-issue (jira-utils-marked-item))))
    (define-key map "W" 'jira-actions-add-worklog-menu)
    (define-key map "e" 'jira-export-menu)
    (define-key map (kbd "M-n") 'jira-issues-next-page)
    (define-key map (kbd "M-p") 'jira-issues-previous-page)
    map)
  "Keymap for `jira-issues-mode'.")

;;;###autoload (autoload 'jira-issues "jira-issues" nil t)
(defun jira-issues ()
  "List Jira issues."
  (interactive)
  (switch-to-buffer "*Jira Issues*")
  (jira-issues-mode)
  (jira-api--initialize-current-url)
  (jira-api-get-basic-data)
  (tablist-revert))

(define-derived-mode jira-issues-mode tabulated-list-mode "Jira Issues"
  "Major mode for listing Jira issues."
  :interactive nil
  (let* ((name (lambda (fd) (jira-table-field-name jira-issues-fields fd)))
         (columns (lambda (fd) (jira-table-field-columns jira-issues-fields fd)))
         (col-info (lambda (fd) (list (funcall name fd) (funcall columns fd) t))))
    (setq tabulated-list-format
          (vconcat (mapcar col-info jira-issues-table-fields))))

  (setq tabulated-list-sort-key jira-issues-sort-key)
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'jira-issues--refresh nil t)
  (add-hook 'jira-issues-changed-hook #'tablist-revert)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(provide 'jira-issues)

;;; jira-issues.el ends here

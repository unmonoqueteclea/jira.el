;;; jira-detail.el --- Jira Detail  -*- lexical-binding: t -*-

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

;; Show information about a specific Jira elements.

;;; Code:

(require 'magit-section)
(require 'seq)
(require 'transient)

(require 'jira-actions)
(require 'jira-api)
(require 'jira-doc)
(require 'jira-fmt)
(require 'jira-table)
(require 'jira-utils)
(require 'jira-complete)
(require 'jira-edit)


(defvar-local jira-detail--current nil
  "All data from current issue being displayed in the detail view.")

(defvar-local jira-detail--current-key nil
  "The key of the current issue being displayed in the detail view.")

(defvar-local jira-detail--current-update-metadata nil
  "Metadata needed to updates fields from the current issue.")

(defvar-local jira-detail--current-watchers nil
  "Watchers of the current issue being displayed.")

(defconst jira-detail--updatable-fields
  (list (cons "Parent Issue" "parent")
        (cons "Description" "description")
	(cons "Issue Type" "issuetype")
	(cons "Resolution" "resolution")
	(cons "Project" "project")
	(cons "Fix Versions" "fixVersions")
	(cons "Summary" "summary")
	(cons "Components" "components")
	(cons "Priority" "priority")
	(cons "Labels" "labels")
	(cons "Assignee" "assignee")
	(cons "Reporter" "reporter")
	(cons "Due Date" "duedate")
	(cons "Original Estimate" "originalEstimate")
	(cons "Remaining Estimate" "remainingEstimate")
	(cons "Sprint" (cons :custom "Sprint"))
	(cons "Business Line" (cons :custom "Business line"))
	(cons "Cost Center" (cons :custom "Cost center")))
  "List of fields that can be updated in the Jira detail view.")

(defcustom jira-comments-display-recent-first
  t
  "The order to display Jira comments."
  :type '(choice (const :tag "Newest first" t)
          (const :tag "Oldest first" nil))
  :group 'jira)

(defcustom jira-detail-reuse-buffer
  nil
  "If non-nil, reuse a single Jira detail buffer instead of creating separate
buffers for each issue.
When enabled, all issue details will be displayed in one buffer named
 \"*Jira Issue Detail*\" instead of creating separate buffers
like \"*Jira Issue Detail: [PROJ-123]*\"."
  :type 'boolean
  :group 'jira)

;; they override the default formatters specified
;; in jira-issues-fields
(defvar jira-detail-formatters
  '((:key . jira-fmt-issue-key-not-tabulated)
    (:parent-key . jira-fmt-issue-key-not-tabulated)))

(defun jira-detail--project-key ()
  "Get the project key of the current issue."
  (jira-table-extract-field jira-issues-fields :project-key jira-detail--current))

(defun jira-detail--is-subtask ()
  "Check if the current issue is a subtask."
  (eq (jira-table-extract-field jira-issues-fields :issue-type-subtask jira-detail--current) t))

(defun jira-detail--header (header)
  "Format HEADER to be used as a header in the detail view."
  (concat
   (jira-fmt-set-face header 'italic)
   (when (> (- 16 (string-width header)) 0)
     (make-string (- 16 (string-width header)) ?\s))))

(defun jira-detail--issue-fmt (issue field)
  "Extract FIELD from ISSUE and format it."
  (let* ((extracted (jira-table-extract-field jira-issues-fields field issue))
         (value (or extracted ""))
         (formatter
          (or (cdr (assoc field jira-detail-formatters))
              (jira-table-field-formatter jira-issues-fields field))))
    (if formatter (funcall formatter value) (format "%s" value))))

(defun jira-detail--issue-summary (issue)
  "Show the summary of the ISSUE."
  (let ((key (jira-detail--issue-fmt issue :key))
        (type (jira-detail--issue-fmt issue :issue-type-name))
        (status (jira-detail--issue-fmt issue :status-name))
        (resolution (jira-detail--issue-fmt issue :resolution))
        (project (jira-detail--issue-fmt issue :project-name))
        (estimate (jira-detail--issue-fmt issue :original-estimate))
        (remaining (jira-detail--issue-fmt issue :remaining-time))
        (ratio (jira-detail--issue-fmt issue :work-ratio))
        (fix-versions (jira-detail--issue-fmt issue :fix-versions))
        (summary (jira-detail--issue-fmt issue :summary)))
    (insert (jira-detail--header "Issue Key") key " (" type")\n")
    (insert (jira-detail--header "Status") status)
    (if (and resolution (not (string= resolution "")))
        (insert " (" resolution ")\n")
      (insert "\n"))
    (insert (jira-detail--header "Project") project "\n")
    (insert (jira-detail--header "Time")
            estimate
            " (remaining: " remaining ")\n")
    (insert (jira-detail--header "Work Ratio") ratio "\n")
    (insert (jira-detail--header "Fix Versions") fix-versions "\n")
    (insert (jira-detail--header "Summary") summary "\n")))


(defun jira-detail--issue-team (issue)
  "Show the team information of the ISSUE."
  (let ((line (jira-detail--issue-fmt issue :line))
        (parent-key (jira-detail--issue-fmt issue :parent-key))
        (parent-type (jira-detail--issue-fmt issue :parent-type-name))
        (parent-status (jira-detail--issue-fmt issue :parent-status))
        (sprint (jira-detail--issue-fmt issue :sprints))
        (components (jira-detail--issue-fmt issue :components)))
    (insert (jira-detail--header "Business Line") line "\n")
    (insert (jira-detail--header "Parent"))
    (when (and (not (string= parent-status "")) (not (string= parent-type "")))
      (insert parent-key " (" parent-type ") (" parent-status ")"))
    (insert "\n")
    (insert (jira-detail--header "Sprint") sprint "\n")
    (insert (jira-detail--header "Components") components "\n")))

(defun jira-detail--issue-manager-data (issue)
  "Show the manager data of the ISSUE."
  (let ((cost-center (jira-detail--issue-fmt issue :cost-center))
        (priority (jira-detail--issue-fmt issue :priority-name))
        (labels (jira-detail--issue-fmt issue :labels))
        (assignee (jira-detail--issue-fmt issue :assignee-name))
        (reporter (jira-detail--issue-fmt issue :reporter-name))
        (due-date (jira-detail--issue-fmt issue :due-date)))
    (insert (jira-detail--header "Cost Center") cost-center "\n")
    (insert (jira-detail--header "Priority") priority "\n")
    (insert (jira-detail--header "Labels") labels "\n")
    (insert (jira-detail--header "Assignee")  assignee "\n")
    (insert (jira-detail--header "Reporter") reporter "\n")
    (insert (jira-detail--header "Due Date") due-date "\n")))

(defun jira-detail--description (issue)
  "Show the description of the ISSUE."
  (let* ((doc (alist-get 'description (alist-get 'fields issue))))
    (insert  (jira-doc-format doc))))

(defcustom jira-detail-show-announcements t
  "Whether to show announcements in Jira detail view."
  :type 'boolean
  :group 'jira)

(cl-defun jira-detail--issue (key issue)
  "Show the detailed information of the ISSUE with KEY."
  (with-current-buffer (jira-detail--get-issue-buffer key)
    (jira-detail)
    (setq jira-detail--current-key key)
    (setq jira-detail--current issue)
    ;; prepare the information about how to update each field
    ;; so that it is ready if user asks for it
    (jira-detail--ensure-update-metadata)
    ;; avoid horizontal scroll
    (setq truncate-lines nil)
    (visual-line-mode 1)
    (let ((inhibit-read-only t))
      (erase-buffer)
      ;; section: Jira Issue Summary
      (magit-insert-section (jira-issue-summary nil nil)
        (magit-insert-heading "📝 Jira Issue Summary")
        (magit-insert-section-body (jira-detail--issue-summary issue)))
      (insert "\n")
      ;; section: Information
      (magit-insert-section (jira-issue-information nil nil)
        ;; section: Team Data
        (magit-insert-section (team-data nil nil)
          (magit-insert-heading "👥 Team Data")
          (magit-insert-section-body (jira-detail--issue-team issue))
          (insert "\n"))
        ;; section: Manager Data
        (magit-insert-section (manager-data nil nil)
          (magit-insert-heading "📊 Manager Data")
          (magit-insert-section-body
            (jira-detail--issue-manager-data issue)
            (insert "\n")
            (when jira-detail-show-announcements
              (insert "----------------------------------------------------------------\n")
              (insert (jira-fmt-set-face "jira.el v2.0" 'jira-face-h1) " allows you to ")
              (insert "update fields with smart suggestions!\n")
              (insert "Press " (jira-fmt-set-face "?"  'jira-face-date)
		      " to open transient menu, ")
              (insert "and " (jira-fmt-set-face "U" 'jira-face-date)
		      " to update a field.")
              (insert "\n----------------------------------------------------------------\n\n"))))
        (magit-insert-section (description nil nil)
          (magit-insert-heading "📄 Description")
          (magit-insert-section-body
            (jira-detail--description issue)
            (insert "\n\n")))))
    (jira-detail--show-attachments key issue)
    (jira-detail--show-subtasks key issue)
    (jira-detail--show-children-section key)
    (jira-detail--show-linked-issues key issue)
    (jira-detail--show-other key)
    (pop-to-buffer (current-buffer))))

(defun jira-detail--add-comment (key)
  "Add a comment to the issue with KEY."
  (jira-edit-create-editor-buffer
   (format "*Jira Comment: %s*" key)
   ""
   (lambda (content)
     (jira-actions-add-comment
      key content
      (lambda () (jira-detail-show-issue key))))))

(defun jira-detail--edit-description-and-update ()
  "Open a buffer to edit the description and update the issue."
  (let ((key jira-detail--current-key))
    (jira-edit-create-editor-buffer
     (concat "*Jira Description [" key "]*")
     ""
     (lambda (new-description)
       (jira-detail--update-field-action
	"description" (jira-doc-build new-description) key)))))

(defun jira-detail--comment-author (comment)
  "Show the author of the COMMENT."
  (concat
   (jira-fmt-bold (alist-get 'displayName (alist-get 'author comment)))
   " @ "
   (jira-fmt-datetime (alist-get 'updated comment))))

(defun jira-detail--comments (key comments)
  "Format and insert COMMENTS from issue KEY."
  (with-current-buffer (jira-detail--get-issue-buffer key)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (magit-insert-section (jira-issue-comments nil nil)
	(magit-insert-section (comments-list nil nil)
          (magit-insert-heading "💬 Comments (press + to add new)")
          (magit-insert-section-body
	    (mapcar (lambda (comment)
		      (magit-insert-section (comment (alist-get 'id comment) nil)
			(magit-insert-heading (jira-detail--comment-author comment))
			(magit-insert-section-body
			  (insert (jira-doc-format (alist-get 'body comment)))
			  (insert "\n\n"))))
		    comments)))))))

(defun jira-detail--show-comments (key)
  "Retrieve and display comments for issue KEY."
  (jira-api-call
   "GET" (format "issue/%s/comment?orderBy=%screated"
                 key
                 (if jira-comments-display-recent-first
                     "-"
                   ""))
   :callback
   (lambda (data _response)
     (jira-detail--comments key (alist-get 'comments data)))))

(defun jira-detail--show-subtasks (key issue)
  "Display subtasks for ISSUE with key KEY."
  (let* ((fields (alist-get 'fields issue))
         (subtasks (alist-get 'subtasks fields)))
    (when (and subtasks (sequencep subtasks) (> (length subtasks) 0))
      (with-current-buffer (jira-detail--get-issue-buffer key)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (magit-insert-section (jira-issue-subtasks nil nil)
	    (magit-insert-section (subtasks-list nil nil)
              (magit-insert-heading "🔗 Subtasks")
              (magit-insert-section-body
		(seq-doseq (subtask subtasks)
                  (let ((subtask-key (alist-get 'key subtask)))
                    (magit-insert-section (jira-subtask-section subtask-key nil)
                      (jira-detail--format-issue-entry subtask)))))
              (insert "\n"))))))))

(defvar-keymap jira-child-issue-section-map
  :doc "Keymap for Jira child issue sections."
  "<RET>" #'jira-detail--visit-child-issue
  "o" #'jira-detail--visit-child-issue-other-window)

(defclass jira-child-issue-section (magit-section)
  ((keymap :initform 'jira-child-issue-section-map)))

(defun jira-detail--visit-child-issue ()
  "Visit the child issue at point."
  (interactive)
  (pcase (magit-section-value-if [jira-child-issue-section])
    (`(,key . ,_summary)
     (jira-detail-show-issue key))
    (_ (message "Not a child issue"))))

(defun jira-detail--visit-child-issue-other-window ()
  "Visit the child issue at point in another window."
  (interactive)
  (pcase (magit-section-value-if [jira-child-issue-section])
    (`(,key . ,_summary)
     (let ((jira-detail-reuse-buffer nil))
       (jira-detail-show-issue key)
       (display-buffer (jira-detail--get-issue-buffer key)
                       '(display-buffer-use-some-window))))
    (_ (message "Not a child issue"))))

(defun jira-detail--show-children-section (key)
  "Fetch and display children for issue KEY asynchronously."
  (jira-detail--get-children
   key
   (lambda (children)
     (when (and children (> (length children) 0))
       (with-current-buffer (jira-detail--get-issue-buffer key)
         (let ((inhibit-read-only t))
           (goto-char (point-max))
           (magit-insert-section (jira-issue-children nil nil)
             (magit-insert-section (children-list nil nil)
               (magit-insert-heading
                 (format "👶 Children (%d)" (length children)))
               (magit-insert-section-body
                 (seq-doseq (child children)
                   (jira-detail--format-child-issue-entry child)))
               (insert "\n")))))))))

(defun jira-detail--get-issue-buffer (key)
  "Get or create the Jira issue detail buffer for KEY.
If `jira-detail-reuse-buffer' is enabled, reuse a single buffer for all issues."
  (if jira-detail-reuse-buffer
      (get-buffer-create "*Jira Issue Detail*")
    (get-buffer-create (concat "*Jira Issue Detail: [" key "]*"))))

(defun jira-detail--process-link (link)
  "Process a single LINK and return (issue direction link-type) tuple."
  (let* ((link-type (alist-get 'type link))
         (outward-issue (alist-get 'outwardIssue link))
         (inward-issue (alist-get 'inwardIssue link)))
    (cond
     (outward-issue (list outward-issue "outward" link-type))
     (inward-issue (list inward-issue "inward" link-type)))))

(defun jira-detail--show-linked-issues (key issue)
  "Display linked issues for ISSUE with key KEY."
  (let* ((fields (alist-get 'fields issue))
         (issuelinks (alist-get 'issuelinks fields)))
    (when (and issuelinks (sequencep issuelinks) (> (length issuelinks) 0))
      (with-current-buffer (jira-detail--get-issue-buffer key)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (magit-insert-section (jira-issue-linked-issues nil nil)
            (magit-insert-section (linked-issues-list nil nil)
              (magit-insert-heading "🔗 Linked Issues")
              (magit-insert-section-body
                (seq-doseq (link issuelinks)
                  (let* ((processed (jira-detail--process-link link))
                         (issue (car processed))
                         (direction (cadr processed))
                         (link-type (caddr processed)))
                    (when issue
                      (jira-detail--format-linked-issue issue link-type direction)))))
              (insert "\n"))))))))

(defun jira-detail--format-entry-content (key summary status &optional link-text type-name)
  "Format the content of an issue entry line.
KEY, SUMMARY and STATUS are required. LINK-TEXT and TYPE-NAME are optional."
  (format " - %s%s %s%s %s\n"
          (if link-text (concat "[" link-text "] ") "")
          (jira-fmt-issue-key-not-tabulated key)
          (if type-name (concat "[" type-name "] ") "")
          (jira-fmt-issue-status status)
          summary))

(defun jira-detail--format-issue-entry (issue &optional link-text)
  "Format a single ISSUE entry, optionally with LINK-TEXT for linked issues.
This is a shared function used by both subtasks and linked issues."
  (let* ((issue-key (alist-get 'key issue))
         (issue-fields (alist-get 'fields issue))
         (issue-summary (alist-get 'summary issue-fields))
         (issue-status (alist-get 'status issue-fields)))
    (if (and (stringp issue-key) (stringp issue-summary))
        (insert (jira-detail--format-entry-content
                 issue-key issue-summary issue-status link-text))
      (insert (format " - [Invalid issue data: %s]\n" issue-key)))))

(defun jira-detail--format-child-issue-entry (issue)
  "Format a single child ISSUE entry with interactive navigation support."
  (let* ((issue-key (alist-get 'key issue))
         (issue-fields (alist-get 'fields issue))
         (issue-summary (alist-get 'summary issue-fields))
         (issue-status (alist-get 'status issue-fields))
         (issue-type (alist-get 'issuetype issue-fields))
         (type-name (alist-get 'name issue-type)))
    (if (and (stringp issue-key) (stringp issue-summary))
        (magit-insert-section (jira-child-issue-section (cons issue-key issue-summary) nil)
          (insert (jira-detail--format-entry-content
                   issue-key issue-summary issue-status nil type-name)))
      (insert (format " - [Invalid issue data: %s]\n" issue-key)))))

(defun jira-detail--format-linked-issue (issue link-type direction)
  "Format a single linked ISSUE with LINK-TYPE and DIRECTION."
  (let* ((link-text (if (string= direction "outward")
                        (alist-get 'outward link-type)
                      (alist-get 'inward link-type))))
    (jira-detail--format-issue-entry issue (or link-text "[Unknown link type]"))))

(defvar-keymap jira-subtask-section-map
  :doc "Keymap for Jira subtask sections."
  "<RET>" #'jira-detail--open-subtask-at-point
  "o" #'jira-detail--open-subtask-in-browser)

(defclass jira-subtask-section (magit-section)
  ((keymap :initform 'jira-subtask-section-map)))

(defun jira-detail--open-subtask-at-point ()
  "Open the subtask at point in the detail view."
  (interactive)
  (when-let ((subtask-key (magit-section-value-if 'jira-subtask-section)))
    (jira-detail-show-issue subtask-key)))

(defun jira-detail--open-subtask-in-browser ()
  "Open the subtask at point in the browser."
  (interactive)
  (when-let ((subtask-key (magit-section-value-if 'jira-subtask-section)))
    (jira-actions-open-issue subtask-key)))

(defun jira-detail--subtask-at-point-p ()
  "Return non-nil if point is on a subtask section."
  (magit-section-value-if 'jira-subtask-section))

(defvar-keymap jira-attachment-section-map
  :doc "Keymap for Jira attachment sections."
  "<RET>" #'jira-detail--get-attachment)

(defclass jira-attachment-section (magit-section)
  ((keymap :initform 'jira-attachment-section-map)))

(defun jira-detail--show-attachments (key issue)
  "Display attachments for ISSUE with key KEY."
  (let* ((fields (alist-get 'fields issue))
         (attachments (alist-get 'attachment fields)))
    (when (> (length attachments) 0)
      (with-current-buffer (jira-detail--get-issue-buffer key)
	(let ((inhibit-read-only t))
          (goto-char (point-max))
          (magit-insert-section (jira-issue-attachments nil nil)
	    (magit-insert-section (attachements-list nil nil)
              (magit-insert-heading "📎 Attachments (press RET to visualize)")
              (magit-insert-section-body
		(mapc
		 (lambda (attachment)
		   (let* ((url (url-generic-parse-url (alist-get 'content attachment)))
                          ;; FIXME: verify that filename matches
                          ;; "attachment/content/[0-9]+"
                          (id (file-name-nondirectory (url-filename url)))
                          (val (list (alist-get 'filename attachment) id)))
                     (magit-insert-section (jira-attachment-section val nil)
                       (magit-insert-section-body
			 (insert (format " - %-30s %10s %5sB %s\n"
					 (alist-get 'filename attachment)
					 (alist-get 'mimeType attachment)
					 (file-size-human-readable
                                          (alist-get 'size attachment))
					 (jira-fmt-datetime
                                          (alist-get 'created attachment))))))))
		 attachments)
		(insert "\n")))))))))

(defun jira-detail--get-attachment ()
  "Get the attachment in the current section and visit it in a new buffer."
  (interactive)
  (pcase (magit-section-value-if [jira-attachment-section])
    (`(,name ,id)
     (message "Fetching %s..." name)
     (jira-api-call
      "GET" (format "attachment/content/%s" id)
      ;; don't want the default parser `json-read' here: the
      ;; attachment content is not wrapped in JSON.
      :parser #'buffer-string
      :callback
      (lambda (data _response)
        (jira-detail--show-attachment name data))))
    (_ (error "Not a Jira attachment"))))

(defun jira-detail--show-attachment (name data)
  "Show attachment NAME with DATA in a new buffer."
  (let ((x (generate-new-buffer-name name)))
    (pop-to-buffer x)
    (insert data)
    (goto-char (point-min))
    ;; Bind these so `normal-mode' will check the buffer contents
    ;; instead of just looking for a -*- line.
    (let ((buffer-file-name (file-name-concat temporary-file-directory
                                              name))
          (default-directory temporary-file-directory))
      (normal-mode))
    (set-buffer-modified-p t)))

(defun jira-detail--show-other (key)
  "Show other issue details for KEY."
  (jira-detail--watchers
   key
   (lambda (data _response)
     (jira-detail--show-comments key)
     (with-current-buffer (jira-detail--get-issue-buffer key)
       (let ((inhibit-read-only t)
             (names (mapcar (lambda (u)
                              (alist-get 'displayName u))
                            (alist-get 'watchers data))))
         (setq jira-detail--current-watchers names)
         (goto-char (point-max))
	 ;; section: Other
	 (magit-insert-section (jira-issue-other nil nil)
           (magit-insert-section (other nil nil)
             (magit-insert-heading "➕ Other")
             (magit-insert-section-body
               (insert (jira-detail--header "Watchers")
                       (string-join names ", ")
                       "\n\n")))))))))

(defun jira-detail--watchers (key callback)
  "Show the watchers list of issue with KEY.
CALLBACK is called with the watchers data."
  (jira-api-call "GET"
                 (format "issue/%s/watchers" key)
                 :callback callback))

(defun jira-detail-show-issue (key)
  "Retrieve and show the detail information of the issue with KEY."
  (jira-api-call
   "GET" (concat "issue/" key)
   :callback (lambda (data _) (jira-detail--issue key data))))

(defun jira-detail--remove-comment-at-point ()
  "Remove the comment at point."
  (let ((current-section (magit-current-section)))
    (if current-section
	(if (string-equal (caar (magit-section-ident current-section)) "comment")
	    (let ((comment-id (magit-section-ident-value current-section)))
	      (when (yes-or-no-p (format "Really delete comment %s?" comment-id))
                (jira-actions-delete-comment
		 jira-detail--current-key
                 comment-id
		 (lambda () (jira-detail-show-issue jira-detail--current-key)))))
	  (message "No comment at point"))
      (message "No comment at point"))))

(defun jira-detail--update-field-action (field-id value key)
  "Update FIELD-ID with VALUE for the current issue KEY."
  (jira-api-call
   "PUT" (concat "issue/" key)
   :data `(("fields" . ((,field-id . ,value))))
   :callback (lambda (_data _response) (jira-detail-show-issue key))))

(defun jira-detail--create-issue-from-fields (fields)
  "Create a new issue with FIELDS."
  (jira-api-call
   "POST" "issue/"
   :data `(("fields" . ,fields))
   :callback (lambda (data _response) (jira-detail-show-issue (alist-get 'key data)))))

(defun jira-detail--update-timetracking-action (field-id value key)
  "Update timetracking FIELD-ID with VALUE for the current issue KEY."
  (jira-api-call
   "PUT" (concat "issue/" key)
   :data `(("fields" . (("timetracking" . ((,field-id . ,value))))))
   :callback (lambda (_data _response) (jira-detail-show-issue key))))

(defun jira-detail--ensure-update-metadata ()
  "Ensure jira-detail--current-update-metadata is populated and return it."
  (unless jira-detail--current-update-metadata
    (let* ((issue-type-id (jira-table-extract-field
			   jira-issues-fields
			   :issue-type-id jira-detail--current)))
      (if (null issue-type-id)
          (progn
	    (message "Warning: No issue type ID found for current issue")
	    (setq jira-detail--current-update-metadata nil))
        (setq jira-detail--current-update-metadata
              (jira-api-get-create-metadata
               (jira-detail--project-key)
               issue-type-id
               :sync t)))))
  jira-detail--current-update-metadata)

(defun jira-detail--updatable-field-id (field-name)
  "Get the updatable field ID for FIELD-NAME."
  (let ((id (cdr (assoc field-name jira-detail--updatable-fields))))
    (if (and (consp id) (eq (car id) :custom))
        (cdr (assoc (cdr id) jira-fields))
      id)))


(defun jira-detail--update-field ()
  "Update a field for the current issue."
  (let* ((field-alist jira-detail--updatable-fields)
         (field-names (mapcar #'car field-alist))
         (chosen-field-name (completing-read "Field to update: " field-names nil t)))
    (when chosen-field-name
      (jira-detail--ensure-update-metadata)
      (cond ((string= chosen-field-name "Description")
             (jira-detail--edit-description-and-update))
            ((or (string= chosen-field-name "Original Estimate")
		 (string= chosen-field-name "Remaining Estimate"))
             (let* ((field-id (jira-detail--updatable-field-id chosen-field-name))
                    (value (read-string (format "Enter value for %s: " chosen-field-name))))
               (jira-detail--update-timetracking-action field-id value jira-detail--current-key)))
            (t (let* ((field-id (jira-detail--updatable-field-id chosen-field-name))
                      (fields (alist-get 'fields jira-detail--current-update-metadata))
                      (field-metadata
                       (car (seq-filter (lambda (f) (string= (alist-get 'fieldId f) field-id))
                                        fields))))
                 (if field-metadata
                     (let ((value (jira-complete-ask-field field-metadata)))
                       (jira-detail--update-field-action field-id value jira-detail--current-key))
                   (message "Could not find metadata for field %s" field-id))))))))

(defun jira-detail-find-issue-by-key ()
  "Find and show a Jira issue by key."
  (let ((key (jira-complete-ask-issue)))
    (when key (jira-detail-show-issue key))))

(transient-define-prefix jira-detail--watchers-menu ()
  "Transient menu for adding or removing watchers to an issue."
  ["Watchers"
   ("+" "Add watcher"
    (lambda () (interactive)
      (jira-actions-add-watcher jira-detail--current-key
                                (lambda ()
                                  (jira-detail-show-issue jira-detail--current-key)))))
   ("-" "Remove watcher"
    (lambda () (interactive)
      (jira-actions-remove-watcher jira-detail--current-key
                                   jira-detail--current-watchers
                                   (lambda ()
                                     (jira-detail-show-issue jira-detail--current-key)))))])

(transient-define-prefix jira-detail--actions-menu ()
  "Show menu for actions on Jira Detail."
  ["Comments"
   ("+" "Add comment to issue"
    (lambda () (interactive ) (jira-detail--add-comment jira-detail--current-key)))
   ("-" "Remove comment at point"
    (lambda () (interactive ) (jira-detail--remove-comment-at-point)))]
  ["Issue Navigation"
   ("P" "Show parent issue" (lambda () (interactive) (jira-detail--show-parent-issue)))
   ("K" "Show children (list)" (lambda () (interactive) (jira-detail--show-children)))]
  ["Issue Actions"
   ("C" "Change issue status"
    (lambda () (interactive) (call-interactively #'jira-actions-change-issue-menu)))
   ("O" "Open issue in browser"
    (lambda () (interactive) (jira-actions-open-issue jira-detail--current-key)))
   ("S" "Add subtask" (lambda () (interactive) (jira-detail--create-subtask)))
   ("U" "Update issue field"
    (lambda () (interactive) (jira-detail--update-field)))
   ("w" "Update watchers" jira-detail--watchers-menu)
   ("f" "Find issue by key/url"
    (lambda () (interactive) (jira-detail-find-issue-by-key)))
   ("c" "Copy selected issue id to clipboard"
    (lambda () (interactive)
      (jira-actions-copy-issues-id-to-clipboard jira-detail--current-key)))
   ("g" "Refresh issue detail"
    (lambda () (interactive) (jira-detail-show-issue jira-detail--current-key)))]
  [:if jira-detail--subtask-at-point-p
   ("RET" "Open subtask" jira-detail--open-subtask-at-point)
   ("o" "Open subtask in browser" jira-detail--open-subtask-in-browser)])

(defvar jira-detail-changed-hook nil
  "Hook run after a Jira issue has been changed in jira-detail-mode.")

(defvar jira-detail-mode-map
  (let ((map (copy-keymap magit-section-mode-map)))
    (define-key map (kbd "?") 'jira-detail--actions-menu)
    (define-key map (kbd "+")
		(lambda () (interactive ) (jira-detail--add-comment jira-detail--current-key)))
    (define-key map (kbd  "-")
		(lambda () "Remove comment at point"
		  (interactive) (jira-detail--remove-comment-at-point)))
    (define-key map (kbd "C")
		(lambda () "Change issue status"
		  (interactive) (call-interactively #'jira-actions-change-issue-menu)))
    (define-key map (kbd "O")
		(lambda () "Open issue in browser"
		  (interactive)  (jira-actions-open-issue jira-detail--current-key)))
    (define-key map (kbd "U")
		(lambda () "Update issue field"
		  (interactive) (jira-detail--update-field)))
    (define-key map (kbd "w") 'jira-detail--watchers-menu)
    (define-key map (kbd "f")
		(lambda () "Find issue by key"
		  (interactive) (jira-detail-find-issue-by-key)))
    (define-key map (kbd "c")
		(lambda () "Copy selected issue id to clipboard"
		  (interactive)
		  (jira-actions-copy-issues-id-to-clipboard jira-detail--current-key)))
    (define-key map (kbd "g")
		(lambda () "Refresh issue detail"
		  (interactive) (jira-detail-show-issue jira-detail--current-key)))
    (define-key map (kbd "h") 'jira-detail--actions-menu)
    (define-key map (kbd "P")
		(lambda () "Show parent issue"
		  (interactive) (jira-detail--show-parent-issue)))
    (define-key map (kbd "K")
		(lambda () "Show children (list)"
		  (interactive) (jira-detail--show-children)))
    (define-key map (kbd "S")
		(lambda () "Add subtask" (interactive) (jira-detail--create-subtask)))
    map)
  "Keymap for Jira Issue Detail buffers.")

(defun jira-detail--show-parent-issue ()
  "Show the detail view of the parent issue.
Shows the detail view of the parent issue for the current issue."
  (interactive)
  (let ((parent-key (jira-detail--issue-fmt jira-detail--current :parent-key)))
    (if (and parent-key (not (string= parent-key "")))
        (jira-detail-show-issue parent-key)
      (message "No parent issue for the current issue."))))

(defun jira-detail--get-children (key callback)
  "Fetch child issues for issue KEY and call CALLBACK with the results.
CALLBACK receives the list of child issues."
  (let ((jql (format "parent = %s ORDER BY created ASC" key)))
    (jira-api-search
     :params `(("jql" . ,jql)
               ("maxResults" . 50)
               ("fields" . "key,summary,status,issuetype"))
     :callback (lambda (data _response)
                 (funcall callback (alist-get 'issues data)))
     :errback (lambda (&rest _) (funcall callback nil)))))

(defun jira-detail--show-children ()
  "Show child issues of the current issue in a new issues list buffer.
Opens the issues list filtered to show only children of the current Epic/Story."
  (interactive)
  (let ((key jira-detail--current-key))
    (if key
        (let ((jql (format "parent = %s ORDER BY created ASC" key))
              (buffer-name (format "*Jira Children: %s*" key)))
          (switch-to-buffer (get-buffer-create buffer-name))
          (jira-issues-mode)
          (setq jira-issues--current-jql jql)
          (jira-issues--reset-pagination)
          (jira-issues--fetch-and-display nil))
      (message "No current issue."))))

(defun jira-detail--create-subtask-type ()
  "Find subtasks types available for the current project, asking the user to
 choose one if there are multiple options."
  (let* ((project (jira-detail--project-key))
         (response (jira-api-get-project-issue-types project :sync t))
         (types (alist-get 'issueTypes response))
         (subtasks-types
          (cl-remove-if-not (lambda (item) (eq (cdr (assoc 'subtask item)) t))
			    (append types nil))))
    (if (> (length subtasks-types) 1)
        (let* ((choices
                (mapcar
                 (lambda (item)
                   (cons (format "%s" (cdr (assoc 'name item))) (cdr (assoc 'id item))))
                 subtasks-types)))
          (completing-read "Select subtask type: " choices nil t))
      (alist-get 'id (car subtasks-types)))))

(defun jira-detail--create-subtask ()
  "Create a subtask for the current issue."
  (if (jira-detail--is-subtask)
      (message "Cannot create a subtask for a subtask.")
    (let* ((project (jira-detail--project-key))
           (type-id (jira-detail--create-subtask-type)))
      (jira-complete-ask-issue-fields
       project type-id
       :parent-key jira-detail--current-key
       :callback #'jira-detail--create-issue-from-fields))))


(define-derived-mode jira-detail-mode magit-section-mode "Jira Detail"
  :interactive nil
  (add-hook 'jira-detail-changed-hook
            (lambda () (jira-detail-show-issue jira-detail--current-key)) nil t))

(defun jira-detail ()
  "Activate `jira-detail-mode' in the current buffer."
  (interactive)
  (kill-all-local-variables)
  (magit-section-mode)
  (jira-detail-mode))

(provide 'jira-detail)

;;; jira-detail.el ends here

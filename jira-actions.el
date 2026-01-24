;;; jira-issues.el ---  Actions on items  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo
;; Created: 2025-02-16

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>

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

;; Manage actions on Jira objects

;;; Code:

(require 'transient)
(require 'tablist)

(require 'jira-api)
(require 'jira-doc)
(require 'jira-tempo)
(require 'jira-utils)
(require 'jira-users)

(defun jira-actions--marked-issue-description ()
  "Get the description of the marked issue from `jira-issues-key-summary-map'."
  (let ((issue-key (jira-utils-marked-item)))
    (gethash issue-key jira-issues-key-summary-map)))

(defun jira-actions--add-worklog ()
  "Add a worklog to marked issue."
  (let* ((args (transient-args 'jira-actions-add-worklog-menu))
         (notify (transient-arg-value "--notify-users" args))
         (comment (transient-arg-value "--comment=" args))
         (estimate (transient-arg-value "--new-estimate=" args))
         (date (transient-arg-value "--date=" args))
         (time (transient-arg-value "--time=" args))
         (adjust-estimate
          (if (and estimate (not (string-empty-p estimate))) "new" "auto")))
    (jira-api-call
     "POST" (concat "issue/"(jira-utils-marked-item) "/worklog")
     :params `(("notifyUsers" . ,(if notify "true" "false"))
               ("adjustEstimate" . ,adjust-estimate)
               ,@(when (string= adjust-estimate "new")
                   `(("newEstimate" . ,estimate))))
     :data `(("started" . ,date)
             ("comment" . (("type" . "doc")
                           ("version" . 1)
                           ("content" . ((("type" . "paragraph")
                                          ("content" . ((("type" . "text")
                                                         ("text" . ,comment)))))))))
             ("timeSpent" . ,time))
     :callback (lambda (_data _response)
                  (kill-buffer "*Jira Issues*")
                  (jira-tempo)))))

(transient-define-prefix jira-actions-add-worklog-menu ()
  "Show menu for adding a Worklog to a Jira Issue."
  :value (lambda () `
           (,(concat "--comment=" (jira-utils-marked-item)
                     ": " (jira-actions--marked-issue-description))
            ,(concat "--date="
                     (format-time-string "%FT%T.%3N%z" (current-time)))
            "--time=1h"))
  ["Arguments"
   ("n" "Notify users" "--notify-users")
   ("e" "New estimate" "--new-estimate=")
   ("c" "Comment" "--comment=")
   ("d" "Date" "--date="
    :reader (lambda (&rest _)
	      (completing-read
	       "Choose an option: "
	       (jira-utils-last-n-dates (current-time) 7)
	       nil nil nil 'transient-history)))
  ("t" "Time Spent" "--time=")]
  ["Actions"
   ("a" "Add worklog" (lambda () (interactive) (jira-actions--add-worklog)))]

  (interactive)
  (if (jira-utils-marked-item)
      (transient-setup 'jira-actions-add-worklog-menu)
    (message "Run jira-issues first")))

(defun jira-actions--change-issue ()
  "Apply user-selected options to marked issue."
  (let* ((args (transient-args 'jira-actions-change-issue-menu))
	 ;; Jira API version 2 does not support bulk transitions
	 (bulk (if (= jira-api-version 3) t nil))
         (status (transient-arg-value "--status=" args))
         (resolution (transient-arg-value "--resolution=" args))
         (status-id (cdr (assoc status jira-active-issue-transitions)))
         (time-estimate (transient-arg-value "--remaining-time-estimate=" args))
	 (hook-fn (lambda ()
		    (cond ((derived-mode-p 'jira-issues-mode)
			   (run-hooks 'jira-issues-changed-hook))
			  ((derived-mode-p 'jira-detail-mode)
			   (run-hooks 'jira-detail-changed-hook)))))
         (hook (if bulk
		   ;; bulk status change API is asynchronous, so we
		   ;; wait same time before reloading the list
		   (lambda (_data _response) (run-at-time "1.5 sec" nil hook-fn))
		 (lambda (_data _response) (funcall hook-fn))))
	 (items (or (jira-utils-marked-items) (list (jira-utils-marked-item)))))
    (when status-id
      (if bulk
	  (jira-api-call
	   "POST" "bulk/issues/transition"
	   :data `(("bulkTransitionInputs" . ((("selectedIssueIdsOrKeys" . ,items)
					       ("transitionId" . ,status-id)))))
	   :callback hook)
	(jira-api-call
	 "POST" (concat "issue/" (jira-utils-marked-item) "/transitions")
	 :data `(("transition" . (("id" . ,status-id))))
	 :callback hook)))
    (when (or resolution time-estimate)
      (jira-api-call
       "PUT" (concat "issue/" (jira-utils-marked-item))
       :data `(("fields" .
		,(let (fields)
                   (when resolution
		     (if (string-equal resolution "Unresolved")
			 (push `( "resolution" . nil) fields)
		       (push `( "resolution" . (("name" . ,resolution))) fields)))
                   (when time-estimate
                     (push `( "timetracking" . (("remainingEstimate" . ,time-estimate))) fields))
                   fields)))
       :callback hook))))

(transient-define-prefix jira-actions-change-issue-menu ()
  "Show menu for updating a Jira Issue."
  ["Arguments"
   ("s" "Status" "--status="
    :choices
    (lambda () (mapcar (lambda (tr) (car tr)) jira-active-issue-transitions)))
   ("r" "Resolution" "--resolution="
    :choices
    (lambda () (mapcar (lambda (res) (car res)) jira-resolutions))
    :inapt-if (lambda () (interactive) (jira-utils-multiple-marked-items-p)))
   ("e" "Time Estimate" "--remaining-time-estimate="
    :inapt-if (lambda () (interactive) (jira-utils-multiple-marked-items-p)))]
  ["Actions"
   ("c" "Change" (lambda () (interactive) (jira-actions--change-issue)))]

  (interactive)
  (let* ((items (or (jira-utils-marked-items) (list (jira-utils-marked-item)))))
    (if (and items (seq-some #'identity items))
	(progn  ;; retrieve possible statuses transtions
          (jira-api-get-transitions items)
          (transient-setup 'jira-actions-change-issue-menu))
      (message "Run jira-issues first"))))

(defun jira-actions-open-issue (issue-key)
  "Open ISSUE-KEY in browser."
  (browse-url (format "%s/browse/%s" jira-base-url issue-key)))

(defun jira-actions-add-comment (issue-key text callback)
  "Create a comment to the issue ISSUE-KEY.
It will include the given TEXT and run the CALLBACK at the end."
  (jira-api-call
   "POST" (concat "issue/" issue-key "/comment")
   :data `(("body" . ,(jira-doc-build text)))
   :callback (lambda (_data _response)
               (message "Comment added to %s" issue-key)
	       (funcall callback))))

(defun jira-actions-edit-comment (issue-key comment-id text callback)
  "Update a comment on the issue ISSUE-KEY with ID COMMENT-ID.
It will include the given TEXT and run the CALLBACK at the end."
  (jira-api-call
   "PUT" (concat "issue/" issue-key "/comment/" comment-id)
   :data `(("body" . ,(jira-doc-build text)))
   :callback (lambda (_data _response)
               (message "Comment added to %s" issue-key)
	       (funcall callback))))

(defun jira-actions-delete-comment (issue-key comment-id callback)
  "Delete the comment COMMENT-ID from ISSUE-KEY and run CALLBACK when done."
  (jira-api-call
   "DELETE" (concat "issue/" issue-key "/comment/" comment-id)
   :callback (lambda (_data _response)
               (message "Comment %s deleted" comment-id)
               (funcall callback))))

(defun jira-actions-copy-issues-id-to-clipboard (issue-key)
  "Copy ISSUE-KEY to the clipboard."
  (when issue-key
    (kill-new issue-key)
    (message "Copied issue ID: %s" issue-key)))

(defun jira-actions-add-watcher (issue-key callback)
  "Add a user as a watcher to ISSUE-KEY.
CALLBACK is called after adding the watcher."
  (pcase (jira-users-read-user "Add watcher: ")
    (`(,name ,id)
     (jira-api-call "POST"
                    (format "issue/%s/watchers" issue-key)
                    :data id
                    :callback
                    (lambda (_data _response)
                      (message "Added %s as a watcher." name)
                      (funcall callback))))))

(defun jira-actions-remove-watcher (issue-key watchers callback)
  "Remove a user as a watcher to ISSUE-KEY.
WATCHERS is the list of current watchers and CALLBACK is called after removal."
  (pcase (jira-users-read-user "Remove watcher: " watchers)
    (`(,name ,id)
     (jira-api-call "DELETE"
                    (format "issue/%s/watchers" issue-key)
                    :params `(("accountId" . ,id))
                    :callback
                    (lambda (_data _response)
                      (message "Removed %s as a watcher." name)
                      (funcall callback))))))

(provide 'jira-actions)

;;; jira-actions.el ends here

;;; jira-utils.el --- Utilities  -*- lexical-binding: t -*-

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

;; Mixed utility functions for jira.el

;;; Code:

(defvar jira-detail--current-key)

(require 'tablist)

(defvar jira-issues-fields
  '((:key . ((:path . (key))
             (:columns . 10)
             (:name . "Key")
             (:formatter . jira-fmt-issue-key)))
    (:priority-name . ((:path . (fields priority name))
                       (:columns . 10)
                       (:name . "Priority")))
    (:priority-icon.  ((:path . (fields priority iconUrl))
                       (:columns . 10)
                       (:name . "Priority")))
    (:labels . ((:path . (fields labels))
                (:columns . 10)
                (:name . "Labels")))
    (:original-estimate . ((:path . (fields aggregatetimeoriginalestimate))
                           (:columns . 10)
                           (:name . "Estimate")
                           (:formatter . jira-fmt-time-from-secs)))
    (:work-ratio . ((:path . (fields workratio))
                    (:columns . 6)
                    (:name . "WR")
                    (:formatter . jira-fmt-issue-progress)))
    (:remaining-time . ((:path . (fields timeestimate))
                        (:columns . 10)
                        (:name . "Remaining")
                        (:formatter . jira-fmt-time-from-secs)))
    (:assignee-name . ((:path . (fields assignee displayName))
                       (:columns . 14)
                       (:name . "Assignee")))
    (:reporter-name . ((:path . (fields reporter displayName))
                       (:columns . 14)
                       (:name . "Reporter")))
    (:components . ((:path . (fields components))
                    (:columns . 10)
                    (:name . "Components")
                    (:formatter . jira-fmt-issue-components)))
    (:fix-versions . ((:path . (fields fixVersions))
                      (:columns . 10)
                      (:name . "Fix Versions")
                      (:formatter . jira-fmt-issue-fix-versions)))
    (:status-name . ((:path . (fields status))
                     (:columns . 15)
                     (:name . "Status")
                     (:formatter . jira-fmt-issue-status)))
    (:status-category-name . ((:path . (fields status statusCategory name))
                              (:columns . 10)
                              (:name . "Status Category")))
    (:creator-name . ((:path (fields creator  displayName))
                      (:columns . 10)
                      (:name . "Creator")))
    (:progress-percent . ((:path . (fields progress  percent))
                          (:columns . 10)
                          (:name . "Progress")
                          (:formatter . jira-fmt-issue-progress)))
    (:issue-type-name . ((:path . (fields issuetype name))
                         (:columns . 15)
                         (:name . "Type")
                         (:formatter . jira-fmt-issue-type-name)))
    (:issue-type-id . ((:path . (fields issuetype id))
                         (:columns . 15)
                         (:name . "Type")))
    (:issue-type-icon . ((:path . (fields issuetype iconUrl))
                         (:columns .  10)
                         (:name . "Type")))
    (:project-key . ((:path . (fields project key))
                     (:columns . 10)
                     (:name . "Project")))
    (:project-name .  ((:path . (fields project name))
                       (:columns . 10)
                       (:name . "Project")))
    (:parent-type-name . ((:path . (fields parent fields issuetype name))
                          (:columns . 10)
                          (:name . "Parent Type")
                          (:formatter . jira-fmt-issue-type-name)))
    (:parent-status . ((:path . (fields parent fields status))
                          (:columns . 10)
                          (:name . "Parent Status")
                          (:formatter . jira-fmt-issue-status)))
    (:parent-key . ((:path . (fields parent key))
                    (:columns . 10)
                    (:name . "Parent Key")
                    (:formatter . jira-fmt-issue-key)))
    (:parent-summary . ((:path . (fields parent fields summary))
			(:columns . 40)
			(:name . "Parent Summary")))
    (:created . ((:path . (fields created))
                 (:columns . 10)
                 (:name . "Created")
		 (:formatter . jira-fmt-datetime)))
    (:updated . ((:path . (fields updated))
                 (:columns . 10)
                 (:name . "Updated")
		 (:formatter . jira-fmt-datetime)))
    (:description . ((:path . (fields description))
                     (:columns . 10)
                     (:name . "Description")))
    (:summary . ((:path . (fields summary))
                 (:columns . 10)
                 (:name . "Summary")))
    (:due-date . ((:path . (fields duedate))
                 (:columns . 10)
                 (:name . "Due Date")
                 (:formatter . jira-fmt-date)))
    (:sprints . ((:path . (fields (custom "Sprint")))
                 (:columns . 10)
                 (:name . "Sprints")
                 (:formatter . jira-fmt-issue-sprints)))
    (:line . ((:path . (fields (custom "Business line")))
              (:columns . 10)
              (:name . "Business Line")
              (:formatter . jira-fmt-business-line)))
    (:cost-center . ((:path . (fields (custom "Cost center")))
                     (:columns . 10)
                     (:name . "Const Center")
                     (:formatter . jira-fmt-cost-center)))
    (:resolution . ((:path . (fields resolution name))
                    (:columns . 10)
                    (:name . "Resolution")))
    (:issuelinks . ((:path . (fields issuelinks))
                   (:columns . 15)
                   (:name . "Linked Issues")
                   (:formatter . jira-fmt-issuelinks)))
    (:issue-type-subtask . ((:path . (fields issuetype subtask))
                            (:columns . 5)
                            (:name . "Subtask?")))))

(defun jira-utils-marked-items ()
  "Return the marked items in the current tablist."
  (mapcar (lambda (i) (car i)) (tablist-get-marked-items)))

(defun jira-utils-marked-item ()
  "Return the first marked item in the current tablist or the current issue key.
Returns the first marked item in the current tablist, or the current issue
key if we are in the Jira Detail buffer."
  (or (car (car (tablist-get-marked-items))) jira-detail--current-key))

(defun jira-utils-multiple-marked-items-p ()
  "Return t if there are multiple marked items in the current tablist."
  (let ((marked-items (tablist-get-marked-items)))
    (and marked-items (> (length marked-items) 1))))

(defun jira-utils-transient-description (prefix &optional item)
  "Return a transient description for the given ITEM (or the marked one)
with the given PREFIX."
  (let* ((item (or item (jira-utils-marked-items)))
	 (item-str (if (listp item) (string-join item ", ") item)))
    (cond ((stringp item-str) (concat prefix " [" (format "%s" item-str) "]"))
	  (t  prefix))))

(defun jira-utils-get-day-of-week (date-string)
  "Return the day of the week for a given DATE-STRING in the format YYYY-MM-DD."
  (let* ((parsed-time (parse-time-string (concat date-string "T00:00:00")))
         (time (encode-time (nth 0 parsed-time)  ; second
                            (nth 1 parsed-time)  ; minute
                            (nth 2 parsed-time)  ; hour
                            (nth 3 parsed-time)  ; day
                            (nth 4 parsed-time)  ; month
                            (nth 5 parsed-time)))) ; year
    (format-time-string "%a" time)))

(defun jira-utils-week-start-and-end (date)
  "Return the start and end dates of the given DATE week, in YYYY-MM-DD format."
  (let* ((decoded-time (decode-time date))
         (current-day (nth 6 decoded-time))
         (days-from-monday (mod (- current-day 1) 7))
         (start-of-week (time-subtract date (days-to-time days-from-monday)))
         (end-of-week (time-add start-of-week (days-to-time 6)))
         (format-time (lambda (time) (format-time-string "%F" time))))
    (list (funcall format-time start-of-week)
          (funcall format-time end-of-week))))

(defun jira-utils-set-time-to-midnight (time)
  "Set the given TIME to 00:00:00 while keeping the same date."
  (let* ((decoded (decode-time time))
         (year (nth 5 decoded))
         (month (nth 4 decoded))
         (day (nth 3 decoded)))
    (encode-time 0 0 0 day month year)))

(defun jira-utils-last-n-dates (date n)
  "Return a list of dates for the last N days from the given DATE."
  (let ((dates '())
	(init-date (jira-utils-set-time-to-midnight date)))
    (dotimes (i n)
      (push (format-time-string
	     "%FT%T.%3N%z"
	     (time-subtract init-date (days-to-time i)))
	    dates))
    (reverse dates)))

(provide 'jira-utils)

;;; jira-utils.el ends here

;;; jira-users.el --- Jira user database             -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Dan McCarthy

;; Author: Dan McCarthy <daniel.c.mccarthy@gmail.com>

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

;; `jira-users' is a hash table of usernames ("displayName" property)
;; and IDs ("accountID").

;;; Code:

(require 'jira-api)

(eval-when-compile
  (require 'cl-macs))

(defcustom jira-users-max-results
  1000
  "Maximum number of Jira usernames to retrieve."
  :group 'jira :type 'integer)

(defvar jira-users
  nil
  "Hash table of all Jira users.")

(cl-defun jira-users-get-users (&key force)
  "Fetch the list of all Jira user names and IDs and store it in `jira-users'.

If FORCE is non-nil, re-fetches the user list.
"
  (interactive)
  (when (or (not jira-users)
            force)
    (let ((table (make-hash-table :test #'equal)))
      ;; Theses params are undocumented but work:
      ;; https://stackoverflow.com/a/64786638
      (jira-api-call "GET"
                     "users/search"
                     :params
                     `((query . "+")
                       (maxResults . ,jira-users-max-results))
                     :callback
                     (lambda (data _response)
                       (mapc #'(lambda (u)
                                 (let ((id (alist-get 'accountId u))
                                       (name (alist-get 'displayName u)))
                                   (unless (eq :json-false (alist-get 'active u))
                                     (setf (gethash name table) id))))
                             data)))
      (setq jira-users table))))

(defun jira-users-read-user (prompt)
  "Complete a Jira username with PROMPT.

Returns a list: (NAME ACCOUNT-ID)."
  (let* ((name (completing-read prompt jira-users))
         (account-id (gethash name jira-users)))
    (list name account-id)))

(provide 'jira-users)

;;; jira-users.el ends here

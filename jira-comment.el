;;; jira-comment.el --- Writing comments  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Dan McCarthy, Pablo González Carrizo

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

;; Edit comments/descriptions with font-lock support.
;; Markup taken from https://jira.atlassian.com/secure/WikiRendererHelpAction.jspa?section=all

;;; Code:
(eval-when-compile
  (require 'rx))

(require 'jira-users)
(require 'jira-fmt)

(defvar-local jira-comment--callback nil
  "The callback function to call after adding a comment.")

(defvar jira-mark-keywords
  `((,(rx bow "*" (+? not-newline) "*" eow)
     0 'bold prepend)
    (,(rx bow "_" (+? not-newline) "_" eow)
     0 'italic prepend)
    (,(rx bow "-" (+? (not "-")) "-")
     0 'jira-face-deleted prepend)
    (,(rx "+" (+? not-newline) "+")
     0 'jira-face-inserted prepend)
    ;; can't display subscript or superscript: AFAICT font-lock
    ;; shouldn't manage the 'display text property.
    (,(rx "^" (+? not-newline) "^")
     0 'font-lock-builtin-face prepend)
    (,(rx "~" (+? not-newline) "~")
     0 'font-lock-builtin-face prepend)))

(defvar jira-regexp-link
  (rx "[" (submatch (*? (not "]"))) "]"))

(defvar jira-regexp-code
  (rx (or (seq "`" (submatch-n 1 (*? (not "`"))) "`")
          (seq "{{" (submatch-n 1 (*? (not "}"))) "}}"))))

(defvar jira-regexp-mention
  (rx "[~" (submatch (*? (not "]"))) "]"))

(defvar jira-regexp-emoji
  (rx ":" (submatch (+ (or lower digit "-"))) ":"))

(defvar jira-inline-block-keywords
  `((,jira-regexp-mention . 'jira-face-mention)
    (,jira-regexp-link . 'jira-face-link)
    (,jira-regexp-code . 'jira-face-code)
    (,jira-regexp-emoji 0 'jira-face-emoji-reference prepend)))

(defvar jira-regexp-blockquote
  (rx bol "bq. " (submatch (+ not-newline))))

(defvar jira-regexp-heading
  (rx bol "h" (submatch (any "1-6") ". " (*? not-newline)) eol))

(defvar jira-regexp-hr
  (rx bol "----"))

(defvar jira-regexp-list-item
  (rx bol
      (submatch (+ (or "*" "#" "-")))
      (+ space)
      (submatch (+? not-newline))
      eol))

(defvar jira-block-keywords
  `((,jira-regexp-blockquote
     0 'jira-face-blockquote prepend)
    (,jira-regexp-list-item
     . font-lock-builtin-face)
    ;; can't use `jira-regexp-heading' because font-lock can't select
    ;; a face based on the contents of the match.
    (,(rx bol "h1. " (*? not-newline) eol)
     . 'jira-face-h1)
    (,(rx bol "h2. " (*? not-newline) eol)
     . 'jira-face-h2)
    (,(rx bol "h3. " (*? not-newline) eol)
     . 'jira-face-h3)
    (,(rx bol "h4. " (*? not-newline) eol)
     . 'jira-face-h4)
    (,(rx bol "h5. " (*? not-newline) eol)
     . 'jira-face-h5)
    (,(rx bol "h6. " (*? not-newline) eol)
     . 'jira-face-h6)))

(defvar jira-regexp-comment-instruction
  (rx bol (+ ";") (+? not-newline) eol))

(defvar jira-font-lock-keywords
  (append `((,jira-regexp-comment-instruction
             0 font-lock-comment-face prepend))
          jira-block-keywords
          jira-inline-block-keywords
          jira-mark-keywords))

(defvar jira-comment-instructions
  ";; Write your message above. Lines beginning with ;; will be ignored.

;; C-c C-c: send
;; C-c C-k: cancel
;; C-c m: insert user mention

;; Markup:
;; *strong*
;; _emphasis_
;; -deleted-
;; +inserted+
;; ^superscript^
;; ~subscript~
;; `code`

;; [title|link]
;; :emoji:

;; bq. blockquote
;; h1-6. header
;; ---- horizontal rule
"
  "Instructions included in jira-comment-mode buffers.")

(defun jira-comment-insert-mention ()
  "Insert a mention at point, prompting for a username."
  (interactive)
  (pcase (jira-users-read-user "Mention user: ")
    (`(,name ,_id)
     (insert (concat "[~" name "]")))))

(defvar-keymap jira-comment-mode-map
  "C-c C-c" #'(lambda ()
                "Send the buffer contents to Jira."
                (interactive)
                (funcall jira-comment--callback))
  "C-c C-k" 'kill-buffer
  "C-c m"   'jira-comment-insert-mention)

(define-derived-mode jira-comment-mode text-mode
  "Jira Comment"
  "Major mode for writing Jira comments."
  (setq font-lock-defaults '(jira-font-lock-keywords))
  (set-syntax-table (let ((st (make-syntax-table)))
                      (modify-syntax-entry ?+ "w" st)
                      (modify-syntax-entry ?* "w" st)
                      (modify-syntax-entry ?_ "w" st)
                      (modify-syntax-entry ?- "w" st)
                      (modify-syntax-entry ?{ "w" st)
                      (modify-syntax-entry ?} "w" st)
                      st))
  (set-buffer-modified-p nil))

(defun jira-comment-create-editor-buffer
    (buffer-name initial-content save-callback)
  "Create and display an editor buffer with INITIAL-CONTENT and a SAVE-CALLBACK."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert initial-content)
      (insert "\n\n" jira-comment-instructions)
      (goto-char (point-min))
      (jira-comment-mode)
      (setq jira-comment--callback
            (lambda ()
              (let ((content
                     (progn
                       (goto-char (point-min))
                       (while (re-search-forward jira-regexp-comment-instruction nil t)
                         (replace-match ""))
                       (string-trim (buffer-string)))))
                (kill-buffer buf)
                (funcall save-callback content))))
      (display-buffer buf)
      (select-window (get-buffer-window buf)))))

(provide 'jira-comment)

;;; jira-comment.el ends here

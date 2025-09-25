;;; jira-edit.el --- Editing text with Jira markup  -*- lexical-binding: t; -*-

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

(defvar-local jira-edit--callback nil
  "The callback function to call after adding a comment.")

(defvar jira-mark-keywords
  `((,(rx symbol-start "*" (+? not-newline) "*" symbol-end)
     0 'bold prepend)
    (,(rx symbol-start "_" (+? not-newline) "_" symbol-end)
     0 'italic prepend)
    (,(rx symbol-start "-" (+? not-newline) "-" symbol-end)
     0 'jira-face-deleted prepend)
    (,(rx symbol-start "+" (+? not-newline) "+" symbol-end)
     0 'jira-face-inserted prepend)
    ;; can't display subscript or superscript: AFAICT font-lock
    ;; shouldn't manage the 'display text property.
    (,(rx symbol-start "^" (+? not-newline) "^" symbol-end)
     0 'font-lock-builtin-face prepend)
    (,(rx symbol-start "~" (+? not-newline) "~" symbol-end)
     0 'font-lock-builtin-face prepend)))

(defconst jira-regexp-link
  (rx "[" (submatch (*? (not "]"))) "]"))

(defconst jira-regexp-code
  (rx (or (seq "`" (submatch-n 1 (*? (not "`"))) "`")
          (seq "{{" (submatch-n 1 (*? (not "}"))) "}}"))))

(defconst jira-regexp-mention
  (rx "[~" (submatch (*? (not "]"))) "]"))

(defconst jira-regexp-emoji
  (rx ":" (submatch (+ (or lower digit "-"))) ":"))

(defvar jira-inline-block-keywords
  `((,jira-regexp-mention . 'jira-face-mention)
    (,jira-regexp-code . 'jira-face-code)
    (,jira-regexp-link . 'jira-face-link)
    (,jira-regexp-emoji 0 'jira-face-emoji-reference prepend)))

(defconst jira-regexp-blockquote
  (rx bol "bq. " (submatch (+ not-newline))))

(defconst jira-regexp-heading
  (rx bol "h" (submatch (any "1-6") ". " (*? not-newline)) eol))

(defconst jira-regexp-table-row
  (rx bol
      (submatch
       (submatch-n 2 "|" (? "|"))
       (+ (+? (intersection (not "|") not-newline))
          (backref 2))
      (* not-newline)
      eol)))

(defconst jira-regexp-hr
  (rx bol "----"))

(defconst jira-regexp-list-item
  (rx bol
      (submatch (+ (or "*" "#" "-")))
      (+ space)
      (submatch (+? not-newline))
      eol))

(defvar jira-block-keywords
  `((,jira-regexp-blockquote
     0 'jira-face-blockquote prepend)
    (,jira-regexp-list-item
     1 font-lock-builtin-face)
    (,jira-regexp-table-row
     . 'jira-face-code)
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

(defconst jira-regexp-comment-instruction
  (rx bol (+ ";") (+? not-newline) eol))

(defconst jira-regexp-code-block
  (rx bol "{code}" (submatch (*? anychar)) "{code}" (*? whitespace) eol))

;; Implementation taken from https://stackoverflow.com/questions/9452615/emacs-is-there-a-clear-example-of-multi-line-font-locking
(defun jira-edit-font-lock-extend-region ()
  "Extend the search region to include an entire paragraph."
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (or (re-search-backward "\n\n" nil t)
                     (point-min))))
      (goto-char font-lock-end)
      (when (re-search-forward "\n\n" nil t)
        (setq font-lock-end (line-beginning-position)))
      (setq font-lock-beg found))))

(defvar jira-font-lock-keywords
  (append `((,jira-regexp-comment-instruction
             0 font-lock-comment-face prepend))
          `((,jira-regexp-code-block . 'jira-face-code))
          jira-block-keywords
          jira-inline-block-keywords
          jira-mark-keywords))

(defvar jira-edit-instructions
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

;; Bullet lists:
;; * a
;; ** b
;; *** c

;; Ordered lists:
;; # 1
;; # 2
;; ## 2a
;; ## 2b
;; ### 2b1

;; Code blocks:
;; {code}
;; if (someFunction()) {
;;   x = 42;
;; }
;; {code}

;; Tables:
;; ||heading 1||heading 2||heading 3||
;; |col A1|col A2|col A3|
;; |col B1|col B2|col B3|
"
  "Instructions included in jira-edit-mode buffers.")

(defun jira-edit-insert-mention ()
  "Insert a mention at point, prompting for a username."
  (interactive)
  (pcase (jira-users-read-user "Mention user: ")
    (`(,name ,_id)
     (insert (concat "[~" name "]")))))

(defvar-keymap jira-edit-mode-map
  "C-c C-c" #'(lambda ()
                "Send the buffer contents to Jira."
                (interactive)
                (funcall jira-edit--callback))
  "C-c C-k" 'kill-buffer
  "C-c m"   'jira-edit-insert-mention)

(define-derived-mode jira-edit-mode text-mode
  "Jira Edit"
  "Major mode for writing Jira comments, descriptions etc."
  (setq font-lock-defaults '(jira-font-lock-keywords))
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            'jira-edit-font-lock-extend-region)
  (set-syntax-table (let ((st (make-syntax-table)))
                      (modify-syntax-entry ?+ "w" st)
                      (modify-syntax-entry ?* "w" st)
                      (modify-syntax-entry ?_ "w" st)
                      (modify-syntax-entry ?- "w" st)
                      (modify-syntax-entry ?{ "w" st)
                      (modify-syntax-entry ?} "w" st)
                      st))
  (set-buffer-modified-p nil))

(defun jira-edit-create-editor-buffer
    (buffer-name initial-content save-callback)
  "Create and display an editor buffer with INITIAL-CONTENT and a SAVE-CALLBACK."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert initial-content)
      (insert "\n\n" jira-edit-instructions)
      (goto-char (point-min))
      (jira-edit-mode)
      (setq jira-edit--callback
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

(provide 'jira-edit)

;;; jira-edit.el ends here

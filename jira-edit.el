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
;; Markup taken from:
;; https://confluence.atlassian.com/docm/latest/confluence-wiki-markup-1108674308.html

;;; Code:
(eval-when-compile
  (require 'rx))

(require 'org)
(require 'jira-users)
(require 'jira-fmt)

(defvar-local jira-edit--callback nil
  "The callback function to call after adding a comment.")

(defconst jira-regexp-color-tag
  (rx "{color"
      (? ":"
         (or (+ alpha)
             (seq "#" (+ hex))))
      "}"))

(defconst jira-regexp-color
  (rx "{color:"
      (submatch (or (+ alpha)
                    (seq "#" (+ hex))))
      "}"
      (submatch (*? any))
      "{color}"))

(defun jira-edit--markable-p (point)
  "Return t if POINT is at a place where text marks may apply."
  (let ((face (get-text-property point 'face)))
    (and (not (eq (char-before point) ?\\))
         (if (consp face)
             (and (not (memq 'jira-face-code face))
                  (not (memq 'jira-face-link face)))
           (and (not (eq 'jira-face-code face))
                (not (eq 'jira-face-link face)))))))

(defun jira-edit--mark-matcher (regexp)
  "Return a function which searches for markup matching REGEXP."
  #'(lambda (limit)
      (let (matched)
        (while (and (< (point) limit)
                    (not matched))
          (if (re-search-forward regexp limit t)
              (setq matched
                    (and (jira-edit--markable-p (match-beginning 0))
                         (jira-edit--markable-p (match-end 0))))
            (goto-char limit)))
        matched)))

(defvar jira-mark-keywords
  `((,(jira-edit--mark-matcher
       (rx "*" (not (or "*" space)) (*? (or "\\*" (not (or "\r" "\n")))) "*"))
     0 'bold append)
    ;; unlike other marks, deleted checks for word boundaries to avoid
    ;; false positives on hyphenated words: like-so and then like-this.
    (,(jira-edit--mark-matcher
       (rx bow "-" (not (or "-" space)) (+? (or "\\-" (not (or "\r" "\n")))) "-" eow))
     0 'jira-face-deleted append)
    (,(jira-edit--mark-matcher
       (rx "_" (+? (or "\\_" (not (or "\r" "\n")))) "_"))
     0 'italic append)
    (,(jira-edit--mark-matcher
       (rx "+" (+? (or "\\+" (not (or "\r" "\n")))) "+"))
     0 'jira-face-inserted append)
    ;; can't display subscript or superscript: AFAICT font-lock
    ;; shouldn't manage the 'display text property.
    (,(jira-edit--mark-matcher
       (rx "^" (+? (or "\\^" (not (or "\r" "\n")))) "^"))
     0 'font-lock-builtin-face append)
    (,(jira-edit--mark-matcher
       (rx "~" (+? (or "\\~" (not (or "\r" "\n")))) "~"))
     0 'font-lock-builtin-face append)
    (,jira-regexp-color-tag
     0 'font-lock-builtin-face append)))

(defconst jira-regexp-link
  (rx "["
      (submatch
       ;; title
       (? (+? any) "|")
       ;; scheme
       alpha (*? (or alpha digit "+" "-" "."))
       "://"
       (*? (not "]")))
      "]"))

(defconst jira-regexp-code
  (rx (or (seq "`" (submatch-n 1 (*? (or "\\`" (not "`")))) "`")
          (seq "{{" (submatch-n 1 (*? (or "\\}" (not "}")))) "}}"))))

(defconst jira-regexp-mention
  (rx "[~" (submatch (*? (not "]"))) "]"))

(defconst jira-regexp-emoji
  (rx symbol-start ":" (submatch (+ (or lower digit "-"))) ":" symbol-end))

(defconst jira-regexp-task-item
  (rx bol (* space) (submatch "[" (? any) "]") (submatch (*? not-newline)) eol))

(defconst jira-regexp-toplevel-adf
  (rx "{ADF:" (+ alpha) ":" (submatch (+? any)) "}"))
(defconst jira-regexp-inline-adf
  (rx "{ADF: " (+ alpha) ":" (submatch (+? any)) "}"))

(defconst jira-regexp-date
  (rx "{{"
      (submatch (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
      "}}"))

(defvar jira-inline-block-keywords
  `((,jira-regexp-mention . 'jira-face-mention)
    (,jira-regexp-code 0 'jira-face-code t)
    (,jira-regexp-date 1 'jira-face-date t)
    (,jira-regexp-link 0 'jira-face-link t)
    (,jira-regexp-task-item 1 font-lock-builtin-face)
    (,jira-regexp-emoji 0 'jira-face-emoji-reference prepend)
    (,jira-regexp-inline-adf
     (0 '(face jira-face-placeholder))
     (1 '(face jira-face-placeholder invisible jira-inline-adf)))))

(defconst jira-regexp-blockquote
  (rx bol
      "bq. "
      (submatch (+ not-newline)
                (* (or "\r" "\n" "\r\n")
                   (+ not-newline)))))

(defconst jira-regexp-heading
  (rx bol "h" (submatch (any "1-6") ". " (*? not-newline)) eol))

(defconst jira-regexp-table-row
  (rx bol
      (submatch
       (submatch-n 2 "|" (? "|"))
       ;; Emacs 29 rx.el doesn't accept:
       ;; (intersection (not "|") not-newline)
       ;; so we use the equivalent literal.
       (+ (+? (regexp "[^\r\n|]"))
          (backref 2))
       (* not-newline))
      eol))

(defconst jira-regexp-hr
  (rx bol "----"))

(defconst jira-regexp-list-item
  (rx bol
      (submatch (+ (or "*" "#" "-")))
      (+ space)
      (submatch (+? not-newline))
      eol))

(defconst jira-regexp-code-block
  (rx bol "{code}" (submatch (*? anychar)) "{code}" (*? whitespace) eol))

(defvar jira-block-keywords
  `((,jira-regexp-code-block
     . 'jira-face-code)
    (,jira-regexp-blockquote
     0 'jira-face-blockquote prepend)
    (,jira-regexp-list-item
     1 font-lock-builtin-face prepend)
    (,jira-regexp-table-row
     . 'jira-face-code)
    (,jira-regexp-toplevel-adf
     (0 '(face jira-face-placeholder))
     (1 '(face jira-face-placeholder invisible jira-inline-adf)))
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
     . 'jira-face-h6)
    (,jira-regexp-hr
     . font-lock-builtin-face)))

(defconst jira-regexp-comment-instruction
  (rx bol (+ ";") (+? not-newline) eol))

(defconst jira-regexp-char-escape
  (rx (or (seq bol "\\" (submatch-n 1 (or "h" "b")))
          (seq "\\" (submatch-n 1 punct)))))

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
  (append `((,jira-regexp-char-escape
             0 font-lock-constant-face)
            (,jira-regexp-comment-instruction
             0 font-lock-comment-face prepend))
          jira-block-keywords
          jira-inline-block-keywords
          jira-mark-keywords))

(defvar jira-edit-instructions
  ";; Write your message above. Lines beginning with ;; will be ignored.

;; C-c C-c: send
;; C-c C-k: cancel
;; C-c m: insert user mention
;; C-c d: insert date

;; Markup:
;; *strong*
;; _emphasis_
;; -deleted-
;; +inserted+
;; ^superscript^
;; ~subscript~
;; `code`

;; \ escapes the next markup character

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

;; Action items:
;; [] incomplete task
;; [x] completed task
"
  "Instructions included in `jira-edit-mode' buffers.")

(defun jira-edit-insert-mention ()
  "Insert a mention at point, prompting for a username."
  (interactive)
  (pcase (jira-users-read-user "Mention user: ")
    (`(,name ,_id)
     (insert "[~" name "]"))))

(defun jira-edit-make-color (text color)
  (concat (format "{color:%s}" color)
          text
          "{color}"))

(defun jira-edit-insert-color (color)
  "Prompt for a color value, then insert color tags at point.

If the region is active, the tags are inserted around it"
  (interactive
   (list (read-color)))
  (cond ((use-region-p)
         (let ((text (buffer-substring (region-beginning)
                                       (region-end))))
           (delete-region (region-beginning) (region-end))
           (insert (jira-edit-make-color text color))))
        (t
         (insert (jira-edit-make-color "" color))
         ;; leave point inside the color tags
         (backward-sexp 1))))

(defun jira-edit-insert-date ()
  "Prompt for a date, then insert it with markup at point."
  (interactive
   (let ((d (org-read-date nil t)))
     (insert (format-time-string "{{%F}}" d)))))

(defvar-keymap jira-edit-mode-map
  "C-c C-c" (lambda ()
                "Send the buffer contents to Jira."
                (interactive)
                (funcall jira-edit--callback))
  "C-c C-k" 'kill-buffer
  "C-c c"   'jira-edit-insert-color
  "C-c d"   'jira-edit-insert-date
  "C-c m"   'jira-edit-insert-mention)

(define-derived-mode jira-edit-mode text-mode
  "Jira Edit"
  "Major mode for writing Jira comments, descriptions etc."
  (setq font-lock-defaults '(jira-font-lock-keywords
                             nil
                             nil
                             nil
                             (font-lock-extra-managed-props
                              invisible)))
  (add-to-invisibility-spec '(jira-inline-adf . t))
  (setq-local font-lock-multiline t)
  (add-hook 'font-lock-extend-region-functions
            #'jira-edit-font-lock-extend-region)
  (set-syntax-table (let ((st (make-syntax-table)))
                      (modify-syntax-entry ?\" "w" st)
                      (modify-syntax-entry ?+ "w" st)
                      (modify-syntax-entry ?* "w" st)
                      (modify-syntax-entry ?: "w" st)
                      (modify-syntax-entry ?_ "w" st)
                      (modify-syntax-entry ?- "w" st)
                      st))
  (set-buffer-modified-p nil))

(defun jira-edit--adf-ify-links (text)
  "Convert raw links in TEXT to ADF format.
For example, http://example.com becomes [http://example.com|http://example.com].
It avoids converting links that are already inside square brackets."
  (with-temp-buffer
    (insert text)
    (modify-syntax-entry ?\[ "(")
    (modify-syntax-entry ?\] ")(")
    (let ((url-re "\\(https?://[^][ \t\n<>()]+\\)"))
      (goto-char (point-min))
      (while (re-search-forward url-re nil t)
        (let ((is-in-brackets
               (save-excursion
                 (goto-char (match-beginning 0))
                 (ignore-errors (backward-up-list) t))))
          (unless is-in-brackets
            (replace-match "[\\&|\\&]" t nil)))))
    (buffer-string)))

(defun jira-edit-create-editor-buffer
    (buffer-name initial-content save-callback)
  "Create and display an editor buffer with INITIAL-CONTENT and a SAVE-CALLBACK.
BUFFER-NAME is the name of the buffer to create."
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
                (funcall save-callback (jira-edit--adf-ify-links content)))))
      (display-buffer buf)
      (select-window (get-buffer-window buf)))))

(provide 'jira-edit)

;;; jira-edit.el ends here

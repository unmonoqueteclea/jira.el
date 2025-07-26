;;; jira-comment.el --- Writing comments  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rx))

(defvar-local jira-comment--callback nil
  "The callback function to call after adding a comment.")

(defface jira-face-deleted
  '((t (:strike-through t)))
  "Face for Jira deleted markup."
  :group 'jira)

(defface jira-face-inserted
  '((t (:underline t)))
  "Face for Jira inserted markup."
  :group 'jira)

(defvar jira-mark-keywords
  `((,(rx bow "*" (+? not-newline) "*" eow)
     0 'bold prepend)
    (,(rx bow "_" (+? not-newline) "_" eow)
     0 'italic prepend)
    (,(rx bow "-" (+? (not "-")) "-")
     0 'jira-face-deleted prepend)
    (,(rx "+" (+? not-newline) "+")
     0 'jira-face-inserted prepend)))

(defvar jira-link-regexp
  (rx "[" (submatch (*? (not "]"))) "]"))

(defvar jira-code-regexp
  (rx (or (seq "`" (submatch-n 1 (*? (not "`"))) "`")
          (seq "{{" (submatch-n 1 (*? (not "}"))) "}}"))))

(defvar jira-mention-regexp
  (rx "[~" (submatch (*? (not "]"))) "]"))

(defvar jira-emoji-regexp
  (rx ":" (submatch (+ (or lower digit "-"))) ":"))

(defvar jira-inline-block-keywords
  `((,jira-mention-regexp . 'jira-face-mention)
    (,jira-link-regexp . 'jira-face-link)
    (,jira-code-regexp . 'jira-face-code)
    (,jira-emoji-regexp 0 'jira-face-emoji-reference prepend)))

(defvar jira-block-keywords
  `((,(rx "bq. " (+ not-newline))
     0 'jira-face-blockquote prepend)
    (,(rx bol (submatch (+ (or "*" "#" "-"))) " ")
     . font-lock-builtin-face)
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

(defvar jira-font-lock-keywords
  (append jira-block-keywords
          jira-inline-block-keywords
          jira-mark-keywords))

(defvar jira-marks-delimiters
  `(("_"  "_"  em)
    ("-"  "-"  strike)
    ("*"  "*"  strong)
    ("+"  "+"  underline))
  "List matching the start of a group of text marks.")

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
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c")
                (lambda ()
                  (interactive)
                  (funcall jira-comment--callback)))
    (define-key map (kbd "C-c C-k")
                (lambda () (interactive) (kill-buffer buf)))
    (set-buffer-modified-p nil)
    (use-local-map map)))

(defun jira-comment-create-editor-buffer
    (buffer-name initial-content instructions save-callback)
  "Create and display an editor buffer with INITIAL-CONTENT and a SAVE-CALLBACK."
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (erase-buffer)
      (insert instructions "\n\n")
      (insert initial-content)
      (jira-comment-mode)
      (setq jira-comment--callback
            (lambda ()
              (let ((content (buffer-string)))
                (kill-buffer buf)
                (funcall save-callback
		         (string-trim (string-remove-prefix instructions content))))))
      (display-buffer buf)
      (select-window (get-buffer-window buf)))))

(provide 'jira-comment)

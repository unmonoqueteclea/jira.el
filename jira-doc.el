;;; jira-doc.el --- Jira Doc  -*- lexical-binding: t -*-

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

;; Manage Altassian Document Format (ADF)
;; See https://developer.atlassian.com/cloud/jira/platform/apis/document/
;; Not all the kinds of blocks are supported yet, only the most common ones.

;;; Code:

(require 'cl-lib)
(require 'jira-fmt)
(require 'jira-api)

;; these blocks contain the content property
(defconst jira-doc--top-level-blocks
  '("blockquote" "bulletList" "codeBlock" "expand" "heading" "mediaGroup"
    "mediaSingle" "orderedList" "panel" "paragraph" "rule" "table"
    "multiBodiedExtension"))

;; these blocks contain the content property
(defconst jira-doc--child-blocks
  '("listItem" "media" "nestedExpand" "tableCell" "tableHeader"
    "tableRow" "extensionFrame"))

(defconst jira-doc--inline-blocks
  '("date" "emoji" "hardBreak" "inlineCard" "mention" "status"
    "text" "mediaInline"))

(defconst jira-doc--marks-delimiters
  ;; link and code are not included here because their structure is
  ;; not like other marks. See comment in
  ;; `jira-doc-build-inline-blocks'.
  `(("_" em)
    ("-" strike)
    ("*" strong)
    ("+" underline)
    ("^" (("type" . "subsup")
          ("attrs" .
           ("type" . "sup"))))
    ("~" (("type" . "subsup")
          ("attrs" .
           ("type" . "sub")))))
  "List matching the start of a group of text marks.")

(defun jira-doc--list-to-str (items sep)
  "Concatenate ITEMS with SEP."
  (mapconcat #'identity items sep))

(defun jira-doc--format-mention (block)
  "Format BLOCK, a mention node, as a string."
  (let* ((attrs (alist-get 'attrs block))
         (text (alist-get 'text attrs)))
    ;; Instead of using text, we could look up the user's info based  on the 'id attr.
    (jira-fmt-mention text)))

(defun jira-doc--format-date (block)
  "Format BLOCK, a date node, as a string."
  (let* ((timestamp (alist-get 'timestamp (alist-get 'attrs block)))
         ;; 32-bit time_t only requires 10 digits but Jira sends 13?
	 (correct-ts (if (= 13 (length timestamp)) (cl-subseq timestamp 0 10) timestamp))
         (ts (string-to-number correct-ts))
         (s (format-time-string "%F" ts t)))
    (message "The timestamp is %s" timestamp)
    (jira-fmt-date s t)))

(defun jira-doc--marks (block)
  "Return a list of mark attributes from BLOCK."
  (let ((m* '()))
    (mapc (lambda (mark)
            (let ((type (alist-get 'type mark))
                  (attrs (alist-get 'attrs mark)))
              (pcase type
                ("link"
                 (let ((url (alist-get 'href attrs)))
                   (push `(link . ,url) m*)))
                ("subsup"
                 (let ((subsup (alist-get 'type attrs)))
                   (push (intern subsup) m*)))
                ("textColor"
                 (let ((c (alist-get 'color attrs)))
                   (push `(color . ,c) m*)))
                ((or "code" "em" "strike" "strong" "underline")
                 (push (intern type) m*))
                (_
                 (message "[Jira Doc Error]: Ignoring unrecognized text mark %s" mark)))))
          (alist-get 'marks block))
    m*))

(defun jira-doc--format-inline-block(block)
  "Format inline BLOCK to a string."
  (let ((type (alist-get 'type block))
        (text (alist-get 'text block)))
    (cond ((string= type "hardBreak") "\n")
          ((string= type "inlineCard")
           (let* ((url (alist-get 'url (alist-get 'attrs block))))
             (buttonize url `(lambda (data) (interactive) (browse-url ,url)))))
          ((string= type "mention")
           (jira-doc--format-mention block))
          ((string= type "emoji")
           (let ((text (alist-get 'text (alist-get 'attrs block))))
             (jira-fmt-emoji text)))
          ((string= type "date")
           (jira-doc--format-date block))
          (text (let ((marks (jira-doc--marks block)))
                  (jira-fmt-with-marks text marks))))))

(defvar jira-doc--indent
  0
  "Curent indentation level for list item nodes.")

(defvar jira-doc--list-prefix
  nil
  "A thunk returning a prefix for list item nodes.")

(defun jira-doc--format-boxed-text (text prefix)
  "Format TEXT in an ASCII box with line wrapping."
  (let* ((fill-column 80)
         (lines (with-temp-buffer
                  (insert text) (fill-region (point-min) (point-max))
                  (split-string (buffer-string) "\n" t)))
         (width (apply #'max (mapcar #'string-width lines)))
         (hborder (concat "┌" prefix (make-string width ?-) "┐"))
         (bborder (concat "└" (make-string (+ width 2) ?-) "┘"))
         (boxed-lines
          (mapcar (lambda (line)
                    (concat "| " line (make-string (- width (string-width line)) ? ) " |"))
                  lines)))
    (concat
     "\n"
     (string-join
      (append (list hborder) boxed-lines (list bborder)) "\n")
     "\n")))

(defun jira-doc--format-content-block(block)
  "Format content BLOCK to a string."
  (let* ((type (alist-get 'type block))
	 (attrs (alist-get 'attrs block))
         (sep (if (string= type "paragraph") "" "\n"))
         (prefix (cond ((string= type "listItem")
                        (concat (make-string jira-doc--indent ?\ )
                                (jira-fmt-bold
                                 (funcall jira-doc--list-prefix))
                                " "))
                       ((string= type "heading")
                        "\n")
                       (t
                        "")))
	 (content
	  (concat
	   prefix
	   (jira-doc--list-to-str
	    (mapcar (lambda (b) (jira-doc--format-block b)) (alist-get 'content block))
            sep))))
    (cond
     ((string= type "table")
      "\n<TABLES NOT SUPPORTED BY jira.el>\n")
     ((string= type "codeBlock")
      (concat "\n" (jira-fmt-code content) "\n"))
     ((string= type "blockquote")
      (jira-fmt-blockquote content))
     ((string= type "heading")
      (jira-fmt-heading
       content (alist-get 'level (alist-get 'attrs block))))
     ((string= type "panel")
      (let* ((ptype (alist-get 'panelType attrs))
	     (prefix (cond ((string= ptype "info") "ℹ️")
			   ((string= ptype "warning") "⚠️")
			   ((string= ptype "note") "✏️")
			   (t ""))))
	(jira-doc--format-boxed-text content prefix)))
     (t content))))

(defun jira-doc--format-list-block (block)
  "Format BLOCK, an orderedList or bulletList, to a string."
  (let* ((type (alist-get 'type block))
         (jira-doc--list-prefix
          (cond
           ((string= type "orderedList")
            (let ((start (alist-get 'order
                                    (alist-get 'attrs block)
                                    1)))
              (lambda ()
                (prog1 (format "%s." start)
                  (cl-incf start)))))
           ((string= type "bulletList")
            (lambda () "-"))
           (t
            jira-doc--list-prefix)))
         (jira-doc--indent (+ 4 jira-doc--indent)))
    (jira-doc--format-content-block block)))

(defun jira-doc--format-block(block)
  "Format BLOCK to a string."
  (let ((type (alist-get 'type block)))
    (cond ((or (string= type "orderedList")
               (string= type "bulletList"))
           (jira-doc--format-list-block block))
          ((or (member type jira-doc--top-level-blocks)
               (member type jira-doc--child-blocks))
           (jira-doc--format-content-block block))
          (t
           (jira-doc--format-inline-block block)))))

(defun jira-doc-format (doc)
  "Format DOC in Jira Document Format to a string."
  (if (stringp doc)
      (jira-fmt-line-endings doc)
    (let* ((content (alist-get 'content doc)))
      (jira-doc--list-to-str
       (mapcar (lambda (block) (jira-doc--format-block block)) content)
       "\n"))))


;; Building ADF from Jira-style markdown.

(defun jira-doc--collect-marks (text)
  "Remove Jira marks from TEXT.

Return un-marked text and a list of applicable marks."
  (let ((marks '())
        done)
    (while (not done)
      (let (any-matched)
        (dolist (m jira-doc--marks-delimiters)
          (pcase m
            (`(,delim ,type)
             (when (and (string-prefix-p delim text)
                        (string-suffix-p delim text))
               (setq text (substring text
                                     (length delim)
                                     (- (length text)
                                        (length delim)))
                     marks (cons type marks)
                     any-matched t)))))
        (unless any-matched
          (setq done t))))
    (cl-values text marks)))

(defun jira-doc--split (nodes regexp f)
  "Split NODES into substrings by matching REGEXP.

REGEXP should have one submatch. Substrings matching REGEXP are
transformed by calling F with the submatch contents. Substrings which do
not match are returned as-is."
  (mapcan (lambda (s)
            (if (stringp s)
                (let ((i 0)
                      (subs '()))
                  (while (and (< i (length s))
                              (string-match regexp s i))
                    (unless (= i (match-beginning 0))
                      (push (substring s i (match-beginning 0))
                            subs))
                    (push (funcall f (match-string 1 s))
                          subs)
                    (setq i (match-end 0)))
                  (unless (= i (length s))
                    (push (substring s i) subs))
                  (nreverse subs))
              (list s)))
          nodes))

(defun jira-doc--build-marked-text (text)
  "Split TEXT into a list of ADF text nodes with marks."
  (let* ((mark-regexp (concat "\\("
                              (string-join (mapcar #'(lambda (d)
                                                       (let ((delim (regexp-quote (car d))))
                                                         (concat delim ".+?" delim)))
                                                   jira-doc--marks-delimiters)
                                           "\\|")
                              "\\)"))
         (areas (jira-doc--split (list text)
                                 mark-regexp
                                 #'(lambda (s)
                                     (pcase (jira-doc--collect-marks s)
                                       (`(,body ,marks)
                                        (jira-doc--build-text body marks)))))))
    (mapcar #'(lambda (s)
                (if (stringp s)
                    (jira-doc--build-text s)
                  s))
            areas)))

(defun jira-doc--build-text (body &optional marks)
  "Make an ADF text node with BODY as contents.

If given, MARKS should be a list of names of marks or ADF mark nodes."
  `(("type" . "text")
    ("text" . ,body)
    ,@(when marks
        `(("marks" . ,(apply #'vector
                             (mapcar (lambda (mark)
                                       (if (consp mark)
                                           mark
                                         `(("type" . ,mark))))
                                     marks)))))))

(defun jira-doc--build-mention (name)
  "Make an ADF mention node.

NAME should be a username defined in `jira-users'."
  (let ((account-id (gethash name jira-users)))
    `(("type" . "mention")
      ("attrs" .
       (("id" . ,account-id))))))

(defun jira-doc--build-link (contents)
  "Make an ADF link node."
  (let* ((has-title (string-search "|" contents))
         (title (if has-title
                    (substring contents 0 has-title)
                  contents))
         (url (if has-title
                  (substring contents (1+ has-title))
                contents)))
    (jira-doc--build-text title
                          `((("type" . "link")
                             ("attrs" .
                              (("href" . ,url)
                               ("title" . ,title))))))))

(defun jira-doc--build-code (contents)
  "Make an ADF text node marked as code."
  (jira-doc--build-text contents
                        `((("type" . "code")))))

(defun jira-doc--build-emoji (name)
  "Make an ADF emoji node."
  `(("type" . "emoji")
    ("attrs" .
     (("shortName" . ,name)))))

(defun jira-doc--build-blockquote (quoted-text)
  `(("type" . "blockquote")
    ("content" .
     (("type" . "paragraph")
      ("content" .
       ,(jira-doc-build-inline-blocks quoted-text))))))

(defun jira-doc--build-heading (heading-text)
  "Make an ADF heading node."
  (let ((level (aref heading-text 0))
        (content (substring heading-text 3))) ; skip "[1-6]. "
    `(("type" . "heading")
      ("attrs" .
       (("level" . ,(- level ?0))))
      ("content" .
       ,(jira-doc-build-inline-blocks content)))))

(defun jira-doc-build-inline-blocks (text)
  "Parse inline block nodes out of TEXT and convert everything to ADF nodes."
  (let ((blocks (jira-doc--split (list text)
                                 jira-regexp-mention
                                 #'jira-doc--build-mention)))
    ;; links and code are actually kinds of marks, but their ADF
    ;; structure is not like other marks, so it's easier to pretend
    ;; they're blocks.
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-link
                                  #'jira-doc--build-link))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-code
                                  #'jira-doc--build-code))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-emoji
                                  #'jira-doc--build-emoji))
    (mapcan (lambda (s)
              (if (stringp s)
                  (jira-doc--build-marked-text s)
                (list s)))
            blocks)))

(defun jira-doc-build-toplevel-blocks (text)
  "Parse toplevel blocks out of TEXT and convert to ADF nodes."
  (let ((blocks (jira-doc--split (list text)
                                 jira-regexp-blockquote
                                 #'jira-doc--build-blockquote)))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-heading
                                  #'jira-doc--build-heading))
    (mapcan (lambda (s)
              (if (stringp s)
                  (jira-doc-build-inline-blocks s)
                (list s)))
            blocks)))

(defun jira-doc-build (text)
  "Build a simple Jira document (ADF) from TEXT."
  (let* ((lines (split-string (or text "") "\n" t)))
    `(("type" . "doc") ("version" . 1)
      ("content" .
       ,(mapcar (lambda (line)
                  `(("type" . "paragraph")
                    ("content" ,@(jira-doc-build-toplevel-blocks line))))
                lines)))))

(provide 'jira-doc)

;;; jira-doc.el ends here

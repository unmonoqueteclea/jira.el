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
(require 'vtable)
(require 'seq)
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

(defun jira-doc--format-media-block (block)
  "Format BLOCK, a mediaSingle or mediaGroup node, to a string."
  (let ((media (mapcar (lambda (b)
                         (let* ((attrs (alist-get 'attrs b))
                                (type (alist-get 'type attrs))
                                (id (alist-get 'id attrs))
                                (collection (alist-get 'collection attrs)))
                           (if (string= type "link")
                               (let ((name (alist-get 'alt attrs id))
                                     (marks (jira-doc--marks block)))
                                 (jira-fmt-with-marks (concat "<" name ">") marks))
                             (jira-fmt-placeholder (format "<file:%s%s>"
                                                           (if (string= "" collection)
                                                               ""
                                                             (concat collection ":"))
                                                           id)))))
                       (alist-get 'content block))))
  (concat
   "\n"
   (string-join media "\n"))))

(defun jira-doc--format-table-row (block)
  (mapcar #'jira-doc--format-content-block
          (alist-get 'content block)))

(defun jira-doc--format-table (block)
  "Format BLOCK, a table node, as a string."
  (let* ((rows (alist-get 'content block))
         (header (seq-find (lambda (r)
                             (seq-every-p (lambda (c)
                                            (string= "tableHeader"
                                                     (alist-get 'type c)))
                                          (alist-get 'content r)))
                           rows))
         (body (remove header rows))
         (header (jira-doc--format-table-row header))
         (body (mapcar #'jira-doc--format-table-row body))
         (widths (apply #'cl-mapcar
                        (lambda (&rest col)
                          (apply #'max (mapcar #'length col)))
                        (if header
                            (cons header body)
                          body)))
         (table (make-vtable :objects body
                             :columns
                             (cl-mapcar (lambda (name width)
                                          (list :name name
                                                :width width))
                                        header
                                        widths)
                             :use-header-line nil
                             ;; Prefer the user's default font over
                             ;; `vtable' in GUI frames. In TTY frames
                             ;; that causes weirdly wide spacing
                             ;; however, so use `vtable' there.
                             :face (if (display-graphic-p) nil 'vtable)
                             ;; columns aren't separated at all on TTY frames?
                             :divider-width (if (display-graphic-p) 0 1)
                             :insert nil)))
    (concat "\n"
            (with-temp-buffer
              (vtable-insert table)
              (buffer-string)))))

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
      (jira-doc--format-table block))
     ((or (string= type "mediaGroup")
          (string= type "mediaSingle"))
      (jira-doc--format-media-block block))
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

(defun jira-doc--submatches (s)
  "Return all submatches of most recent match against S."
  (let ((submatches '())
        (d (cddr (match-data))))
    (while (consp d)
      (let ((start (car d))
            (end (cadr d)))
        (push (substring s start end) submatches)
        (setq d (cddr d))))
    (reverse submatches)))

(defun jira-doc--split (nodes regexp f)
  "Split NODES into substrings by matching REGEXP.

REGEXP should have at least one submatch. Substrings matching REGEXP are
transformed by calling F with the submatches as arguments. Substrings
which do not match are returned as-is."
  (mapcan (lambda (s)
            (if (stringp s)
                (let ((i 0)
                      (subs '()))
                  (while (and (< i (length s))
                              (string-match regexp s i))
                    (unless (= i (match-beginning 0))
                      (push (substring s i (match-beginning 0))
                            subs))
                    (save-match-data
                      (push (apply f (jira-doc--submatches s))
                            subs))
                    (setq i (match-end 0)))
                  (unless (= i (length s))
                    (push (substring s i) subs))
                  (nreverse subs))
              (list s)))
          nodes))

(defun jira-doc--build-marked-text (text)
  "Split TEXT into a list of ADF text nodes with marks."
  (let* ((mark-regexp (concat "\\_<\\("
                              (string-join (mapcar #'(lambda (d)
                                                       (let ((delim (regexp-quote (car d))))
                                                         (concat delim ".+?" delim)))
                                                   jira-doc--marks-delimiters)
                                           "\\|")
                              "\\)\\_>"))
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

(defun jira-doc--build-code-block (body)
  "Make an ADF codeBlock node."
  `(("type" . "codeBlock")
    ("content"
     ,(jira-doc--build-text (string-trim body)))))

(defun jira-doc--build-blockquote (quoted-text)
  `(("type" . "blockquote")
    ("content"
     (("type" . "paragraph")
      ("content"
       ,@(jira-doc-build-inline-blocks quoted-text))))))

(defun jira-doc--build-heading (heading-text)
  "Make an ADF heading node."
  (let ((level (aref heading-text 0))
        (content (substring heading-text 3))) ; skip "[1-6]. "
    `(("type" . "heading")
      ("attrs" .
       (("level" . ,(- level ?0))))
      ("content"
       ,@(jira-doc-build-inline-blocks content)))))

(defun jira-doc--build-rule ()
  `(("type" . "rule")))

(defun jira-doc--build-list-item (&rest children)
  "Make an ADF listItem node."
  `(("type" . "listItem")
    ("content" . ,children)))

(defun jira-doc--build-list (items ordered-p)
  "Make an ADF bulletList or orderedList node."
  `(("type" . ,(if ordered-p
                   "orderedList"
                 "bulletList"))
    ("content" . ,items)))

(defun jira-doc--build-table-row (text separator)
  "Make an ADF tableRow node and child nodes out of TEXT."
  (let ((cell-type (if (string= separator "||")
                       "tableHeader"
                     "tableCell"))
        (cells (string-split text separator nil)))
    ;; Using `jira-doc-build-inline-blocks' here is a compromise. ADF
    ;; table nodes can contain any toplevel block except another
    ;; table, but our markup doesn't allow e.g. lists or blockquotes
    ;; inside table cells. So instead we just scan for inline blocks,
    ;; whick can all be written inside table markup.
    `(("type" . "tableRow")
      ("content" . ,(mapcar #'(lambda (text)
                                `(("type" . ,cell-type)
                                  ("content" .
                                   ((("type" . "paragraph")
                                     ("content" . ,(jira-doc-build-inline-blocks text)))))))
                            ;; want to exclude empty strings at bol
                            ;; and eol, but include empty strings in
                            ;; the middle.
                            (take (- (length cells) 2)
                                  (cdr cells)))))))

(defun jira-doc--build-table (rows)
  "Make an ADF table node."
  `(("type" . "table")
    ("content" . ,rows)))

(defun jira-doc-build-inline-blocks (text)
  "Parse inline block nodes out of TEXT and convert everything to ADF nodes."
  (let ((blocks (jira-doc--split (list text)
                                 jira-regexp-mention
                                 #'jira-doc--build-mention)))
    ;; links and code are actually kinds of marks, but their ADF
    ;; structure is not like other marks, so it's easier to pretend
    ;; they're blocks.
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-code
                                  #'jira-doc--build-code))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-link
                                  #'jira-doc--build-link))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-emoji
                                  #'jira-doc--build-emoji))
    (mapcan (lambda (s)
              (if (stringp s)
                  (jira-doc--build-marked-text s)
                (list s)))
            blocks)))

(defun jira-doc--build-lists (block)
  ;; mark list items: depth, ordered?, text
  (let* ((b* (jira-doc--split (list block)
                              jira-regexp-list-item
                              #'(lambda (prefix text)
                                  `(list-item ,(length prefix)
                                              ,(eq (aref prefix (1- (length prefix)))
                                                   ?#)
                                              ,text))))
         ;; collect list items into a tree of lists.
         (ret '())
         (stack '())
         (cur '())
         (ordered-p nil)
         (finish-list #'(lambda ()
                          "Create the list node for CUR and put it where it belongs."
                          (when cur
                            (let ((l (jira-doc--build-list (reverse cur)
                                                           ordered-p)))
                              (if (null stack)
                                  (push l ret)
                                ;; add L to the last list-item in the previous list
                                (pcase-let ((`(,_ordered-p ,parent-list) (car stack)))
                                  (let ((parent (car parent-list)))
                                    (setf (alist-get "content" parent nil nil #'equal)
                                          (append (alist-get "content" parent nil nil #'equal)
                                                  (list l))))))))))
         (pop-lists #'(lambda (n)
                        "Finish N lists on STACK."
                        (dotimes (_ n)
                          (funcall finish-list)
                          (pcase-setq `(,ordered-p ,cur) (pop stack))))))
    (dolist (b b*)
      (pcase b
        (`(list-item ,d ,li-ordered-p ,text)
         (let ((li (jira-doc--build-list-item
                    `(("type" . "paragraph")
                      ;; The documentation for listItem says it doesn't support marks,
                      ;; but empirically it does, so let's interpret them.
                      ("content" . ,(jira-doc-build-inline-blocks text)))))
               (depth (+ (length stack)
                         (if cur 1 0))))
           (cond ((> depth d)
                  ;; the nested lists from DEPTH to D, including
                  ;; CUR, are finished.
                  (funcall pop-lists (- depth d)))
                 ((> d depth)
                  ;; starting a new list... but if D > (DEPTH+1) we
                  ;; need to make up intermediate lists with blank
                  ;; list-items.
                  (when cur
                    (push `(,ordered-p ,cur) stack)
                    (setq cur nil))
                  (dotimes (_ (- d depth 1))
                    ;; made-up intermediate lists are ordered if the
                    ;; first "real" item is ordered. (The parargraph
                    ;; with no content is what API v3 accepts;
                    ;; anything else fails with INVALID_INPUT.)
                    (push `(,li-ordered-p (,(jira-doc--build-list-item
                                             '((type . "paragraph")))))
                          stack))))
           (if (eq ordered-p li-ordered-p)
               (push li cur)
             (funcall finish-list)
             (setq cur (list li)
                   ordered-p li-ordered-p))))
        ("\n"
         ;; EOL after a list item: ignore
         )
        (_
         ;; not a list item: all pending lists are now complete.
         (funcall pop-lists (length stack))
         (funcall finish-list)
         (setq cur nil)
         (push b ret))))

    ;; end of BLOCK: all pending lists are now complete.
    (funcall pop-lists (length stack))
    (funcall finish-list)
    (reverse ret)))

(defun jira-doc-build-lists (blocks)
  "Collect list items out of BLOCKS and create bulletList and orderedList nodes."
  (mapcan #'jira-doc--build-lists blocks))

(defun jira-doc-build-tables (blocks)
  "Collect table rows out of BLOCKS and create table nodes."
  (let ((res '())
        (cur-table nil))
    (dolist (b
             (jira-doc--split blocks
                              jira-regexp-table-row
                              #'jira-doc--build-table-row))
      (pcase b
        ((map ("type" "tableRow"))
         (push b cur-table))
        ("\n"
         ;; EOL after a table row: ignore
         )
        (_
         ;; not a table row
         (when cur-table
           (push (jira-doc--build-table (reverse cur-table))
                 res)
          (setq cur-table nil))
         (push b res))))
    (when cur-table
      (push (jira-doc--build-table (reverse cur-table))
            res))
    (reverse res)))

(defun jira-doc-build-toplevel-blocks (text)
  "Parse toplevel blocks out of TEXT and convert to ADF nodes."
  (let ((blocks (jira-doc--split text
                                 jira-regexp-code-block
                                 #'jira-doc--build-code-block)))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-blockquote
                                  #'jira-doc--build-blockquote))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-heading
                                  #'jira-doc--build-heading))
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-hr
                                  #'jira-doc--build-rule))
    (setq blocks (jira-doc-build-tables blocks))
    (setq blocks (jira-doc-build-lists blocks))
    (mapcar (lambda (s)
              (if (stringp s)
                  `(("type" . "paragraph")
                    ("content" ,@(jira-doc-build-inline-blocks s)))
                s))
            blocks)))

(defun jira-doc-build-adf (text)
  "Build a Jira document (ADF) from TEXT."
  (let* ((paras (mapcar #'string-trim
                        (split-string (or text "")
                                      "\n\\([ 	]*\n\\)+"
                                      t))))
    `(("type" . "doc") ("version" . 1)
      ("content" .
       ,(jira-doc-build-toplevel-blocks paras)))))

;;; Building v2 texts.
(defun jira-doc-build-v2 (text)
  "Format TEXT for Jira API v2."
  ;; Rewrite `...` into {{...}}.
  (let ((blocks (jira-doc--split (list text)
                                 jira-regexp-code
                                 (lambda (x)
                                   (concat "{{" x "}}")))))
    ;; Rewrite mentions into "[~accountid:123456790]". This form is
    ;; not documented but it's how mentions are rendered in text
    ;; returned by the v2 API.
    (setq blocks (jira-doc--split blocks
                                  jira-regexp-mention
                                  (lambda (username)
                                    (let ((account-id (gethash username jira-users)))
                                      (concat "[~accountid:" account-id "]")))))
    (string-join blocks)))

;;; Entry point.
(defun jira-doc-build (text)
  "Build a Jira document from TEXT."
  (if (= jira-api-version 3)
      (jira-doc-build-adf text)
    (jira-doc-build-v2 text)))


(provide 'jira-doc)

;;; jira-doc.el ends here

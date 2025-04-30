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

(require 'jira-fmt)

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

(defun jira-doc--list-to-str (items sep)
  "Concatenate ITEMS with SEP."
  (mapconcat #'identity items sep))

(defun jira-doc--format-mention (block)
  (let* ((attrs (alist-get 'attrs block))
         (text (alist-get 'text attrs)))
    ;; Instead of using text, we could look up the user's info based
    ;; on the 'id attr.
    (jira-fmt-mention text)))

(defun jira-doc--marks (block)
  "Return a list of mark attributes from BLOCK."
  (let ((m* '()))
    (mapc #'(lambda (mark)
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
          (text (let ((text-str (format "%s " text))
                      (marks (jira-doc--marks block)))
                  (jira-fmt-with-marks text-str marks))))))

(defvar jira-doc--indent
  0
  "Curent indentation level for list item nodes.")
(defvar jira-doc--list-prefix
  nil
  "A thunk returning a prefix for list item nodes.")

(defun jira-doc--format-content-block(block)
  "Format content BLOCK to a string."
  (let* ((type (alist-get 'type block))
         (sep (if (string= type "paragraph") "" "\n"))
         (prefix (if (string= type "listItem")
                     (concat (make-string jira-doc--indent ?\ )
                             (jira-fmt-bold
                              (funcall jira-doc--list-prefix))
                             " ")
                   ""))
	 (content
	  (concat prefix
		  (jira-doc--list-to-str
		   (mapcar (lambda (b) (jira-doc--format-block b))
			   (alist-get 'content block))
                   sep))))
    (cond
     ((string= type "table")
      "\n<TABLES NOT SUPPORTED BY jira.el>\n")
     ((string= type "codeBlock")
      (concat "\n" (jira-fmt-code content) "\n"))
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

(defun jira-doc-build (text)
  "Build a Jira document (ADF) with a single paragraph of TEXT."
  `(("type" . "doc")
    ("version" . 1)
    ("content" .
     ((("type" . "paragraph")
       ("content" . ((("type" . "text") ("text" . ,text)))))))))

(provide 'jira-doc)

;;; jira-doc.el ends here

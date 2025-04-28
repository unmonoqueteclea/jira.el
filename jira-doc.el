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

(defun jira-doc--is-bold(block)
  "Check if BLOCK should be shown with bold text."
  (let ((marks (alist-get 'marks block)))
    (cl-find-if (lambda (m) (equal m '((type . "strong")))) marks)))

(defun jira-doc--format-mention (block)
  (let* ((attrs (alist-get 'attrs block))
         (text (alist-get 'text attrs)))
    ;; Instead of using text, we could look up the user's info based
    ;; on the 'id attr.
    (jira-fmt-mention text)))

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
          (text (let ((text-str (format "%s " text)))
		  (if (jira-doc--is-bold block)
		      (jira-fmt-bold text-str)
		    text-str))))))

(defun jira-doc--format-content-block(block)
  "Format content BLOCK to a string."
  (let* ((type (alist-get 'type block))
         (sep (if (string= type "paragraph") "" "\n"))
         (prefix (cond ((string= type "listItem") " - ") (t "")))
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

(defun jira-doc--format-block(block)
  "Format BLOCK to a string."
  (let ((type (alist-get 'type block)))
    (if (or (member type jira-doc--top-level-blocks)
            (member type jira-doc--child-blocks))
        (jira-doc--format-content-block block)
      (jira-doc--format-inline-block block))))

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

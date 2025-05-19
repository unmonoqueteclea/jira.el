;;; jira-fmt.el --- Formatting data  -*- lexical-binding: t -*-

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

;; Formatting data to display

;;; Code:

(require 'shr)
(require 'jira-utils)

(defcustom jira-use-color-marks
  t
  "If true, display color marks in Jira text.

Note that this only applies to colors specified by the author of the
text with the \\='Text Color\\=' menu or the {color} markup. Other text
colors can be controlled by customizing faces in the `jira' group."
  :type '(choice (const :tag "Use color marks" t)
                 (const :tag "Do not use color marks" nil))
  :group 'jira)

(defface jira-face-link
  '((t :inherit link)) "Face used to show links." :group 'jira)

(defface jira-face-date
  '((t :inherit shadow))"Face used to show dates." :group 'jira)

(defface jira-face-date-today
  '((t :inherit success))"Face used to show today date." :group 'jira)

(defface jira-face-time
  '((t :inherit match)) "Face used to show times." :group 'jira)

(defface jira-face-mention
  '((t :inherit link)) "Face used to show mentions." :group 'jira)

(defface jira-face-blockquote
  '((t :inherit font-lock-doc-face))
  "Face used to show Jira blockquotes."
  :group 'jira)

(defface jira-face-emoji-reference
  '((t :inherit font-lock-builtin-face))
  "Face used to show non-standard Jira emoji references."
  :group 'jira)

(defface jira-face-h1
  '((t :inherit shr-h1))
  "Face used for Jira h1 headings."
  :group 'jira)

(defface jira-face-h2
  '((t :inherit shr-h2))
  "Face used for Jira h2 headings."
  :group 'jira)

(defface jira-face-h3
  '((t :inherit shr-h3))
  "Face used for Jira h3 headings."
  :group 'jira)

(defface jira-face-h4
  '((t :inherit shr-h4))
  "Face used for Jira h4 headings."
  :group 'jira)

(defface jira-face-h5
  '((t :inherit shr-h5))
  "Face used for Jira h5 headings."
  :group 'jira)

(defface jira-face-h6
  '((t :inherit shr-h6))
  "Face used for Jira h6 headings."
  :group 'jira)

(defface jira-face-success
  '((t (:foreground "#fff" :background "#77AA77")))
  "Face used to show success status." :group 'jira)

(defface jira-face-error
  '((t (:foreground "#fff" :background "#884444")))
  "Face used to show error status." :group 'jira)

(defface jira-face-info
  '((t (:foreground "#fff" :background "#555588")))
  "Face used to show info status." :group 'jira)

(defface jira-face-tag
  '((t (:foreground "#fff" :background "#999999")))
  "Face used to show tags." :group 'jira)

(defface jira-face-code
  '((t (:family "Monospace")))
  "Face used to show code blocks." :group 'jira)

(defcustom jira-statuses-done '("Done" "Closed" "Waiting for QA")
  "A list of statuses names representing done state."
  :type '(repeat string) :group 'jira)

(defcustom jira-statuses-progress '("In Progress" "Pull-request")
  "A list of statuses names representing progress state."
  :type '(repeat string) :group 'jira)

(defcustom jira-statuses-todo '("To do" "Backlog")
  "A list of statuses names representing TODO."
  :type '(repeat string) :group 'jira)

(defcustom jira-statuses-error '("Blocked" "Paused")
  "A list of statuses names representing problems in tasks."
  :type '(repeat string) :group 'jira)

(defun jira-fmt--link-action (button)
  "Action to open the link in BUTTON."
  (browse-url (button-get button 'href)))

(defun jira-fmt-issue-key (issue-id)
  "Format ISSUE-ID as a link to the issue in Jira (in a tabulated list)."
  (list  issue-id
         'face 'jira-face-link
         'help-echo (format "Click to open: %s" issue-id)
         'href (concat jira-base-url "/browse/" issue-id)
         'follow-link t
         'action #'jira-fmt--link-action))

(defun jira-fmt-issue-key-not-tabulated (issue-id)
  "Format ISSUE-ID as a link to the issue in Jira (outside a tabulated list)."
  (buttonize
   issue-id
   (lambda (_data) (interactive)
     (browse-url (concat jira-base-url "/browse/" issue-id)))))

(defun jira-fmt-date (date &optional color-today)
  "Format DATE as `YYYY-MM-DD, Day of week` and adding color for current day.

COLOR-TODAY is a boolean to color the date if it is today."
  (if (and (stringp date) (not (string-empty-p date)))
      (let ((value (concat date ", " (jira-utils-get-day-of-week date))))
        (if (and color-today
                 (string= date (format-time-string "%F" (current-time))))
            (propertize value  'face 'jira-face-date-today)
          (propertize value  'face 'jira-face-date)))
    date))

(defun jira-fmt-datetime (date)
  "Format an ISO 8601 date string into a human-readable format."
  (let ((parsed-time (date-to-time date)))
    (propertize
     (format-time-string "%c" parsed-time)
     'face 'jira-face-date)))

(defun jira-fmt-time-from-secs (secs)
  "Format SECS as a string in the form `XhYm`."
  (let* ((secs-num (string-to-number (or secs 0)))
         (hours (truncate (/ (float secs-num) 3600)))
         (minutes (truncate (/ (float (% secs-num 3600)) 60))))
    (propertize
     (concat
      (if (> hours 0) (format "%sh" hours) "")
      (when (or (> minutes 0) (= hours 0))
        (concat (if (> hours 0) " "  "") (format "%sm" minutes))))
     'face 'jira-face-time)))

(defun jira-fmt-issue-progress (value)
  "Format progress VALUE as a percentage and adding color."
  (let ((val (concat (if (or (string-empty-p value) (string= value "-1"))
                         "0" value)
                     "%")))
    (cond ((= (string-to-number val) 100) (propertize val  'face 'jira-face-success))
          ((> (string-to-number val) 100) (propertize val  'face 'jira-face-error))
          (t val))))

(defun jira-fmt-issue-status (value)
  "Format status VALUE adding color."
  (cond ((member value jira-statuses-done)
         (propertize (upcase value) 'face 'jira-face-success))
        ((member value jira-statuses-progress)
         (propertize (upcase value) 'face 'jira-face-info))
        ((member value jira-statuses-todo)
         (propertize (upcase value) 'face 'jira-face-error))
        ((member value jira-statuses-error)
         (propertize (upcase value) 'face 'jira-face-error))
        ((or (string= value "Requirements Backlog"))
         (propertize (upcase "Backlog") 'face 'jira-face-tag))
        (t (propertize (upcase value) 'face 'jira-face-tag))))

(defun jira-fmt-truncate (len str)
  "Truncate STR to LEN characters, removing any line breaks."
  (let ((cleaned-str (replace-regexp-in-string "[\n\r]" " " str)))
    (if (> (length cleaned-str) len)
        (concat (substring cleaned-str 0 (- len 3)) "...")
      cleaned-str)))

(defun jira-fmt--list-from-regex (regex value)
  "Extract a list of values from VALUE using REGEX."
  (let* ((pos 0) matches)
  (while (string-match regex value pos)
    (push (match-string 1 value) matches)
    (setq pos (match-end 0)))
  (string-join (reverse matches) ", ")))

;; TODO Ugly, but it works, there were problems parsing the JSON
(defun jira-fmt-issue-fix-versions (value)
  "Extract a list of fix versions from VALUE."
  (jira-fmt--list-from-regex "(name\\s-+\\.\\s-+\\([^ )]+\\))" value))

(defun jira-fmt-issue-components (value)
  "Extract a list of components from VALUE."
  (jira-fmt--list-from-regex "(name\\s-+\\.\\s-+\\([^()]+\\))" value))

(defun jira-fmt-issue-sprints (value)
  "Extract a list of sprints from VALUE."
  (jira-fmt--list-from-regex "(name\\s-+\\.\\s-+\\([^()]+\\))" value))

(defun jira-fmt-cost-center (value)
  "Extract a list of cost centers from VALUE."
  (jira-fmt--list-from-regex "(value\\s-+\\.\\s-+\\([^()]+\\))" value))

(defun jira-fmt-business-line (value)
  "Extract a list of business lines from VALUE."
  (jira-fmt--list-from-regex "(value\\s-+\\.\\s-+\\([^()]+\\))" value))

(defun jira-fmt-issue-type-name (value)
  "Format issue type VALUE."
  (propertize value 'face 'bold))

(defun jira-fmt-bold (value)
  "Format VALUE as bold."
  (propertize value 'face 'bold))

(defun jira-fmt-line-endings (text)
  "Convert Windows line endings to Unix ones in the given TEXT."
  (replace-regexp-in-string "\r\n" "\n" text))

(defun jira-fmt-code (text)
  "Format TEXT as code."
  (propertize text 'face 'jira-face-code))

(defun jira-fmt-mention (text)
  "Format TEXT as a mention."
  (propertize text 'face 'jira-face-mention))

(defun jira-fmt-emoji (text)
  "Format TEXT as an emoji."
  (if (string-match-p "^:[-a-z_]+:\\'" text)
      (propertize text 'face 'jira-face-emoji-reference)
    ;; otherwise, `text' is probably a normal Unicode emoji
    text))

(defun jira-fmt--color (color-hex)
  "Return a hex color string usable instead of COLOR-HEX on the current frame."
  ;; This is easier than importing the whole palette, but maybe we
  ;; should do that?
  (let* ((crgb (color-name-to-rgb color-hex))
         (color-to-use (if (and (color-gray-p color-hex)
                                (color-dark-p crgb))
                           (color-complement color-hex)
                         crgb))
         (hex (lambda (v)
                (round (* 255 v)))))
    (pcase color-to-use
      (`(,r ,g ,b)
       (format "#%02x%02x%02x"
               (funcall hex r)
               (funcall hex g)
               (funcall hex b))))))

(defun jira-fmt-with-marks (text marks)
  "Format TEXT using MARKS.

See `jira-doc--marks' for the expected format of MARKS."
  (if-let* ((url (alist-get 'link marks)))
      (buttonize text
                 #'(lambda (_button)
                     (browse-url url))
                 nil
                 url)
    (if (memq 'code marks)
        (jira-fmt-code text)
      (let* ((face-attrs (mapcan #'(lambda (x)
                                     (pcase x
                                       (`(color . ,c)
                                        (and jira-use-color-marks
                                             `(:foreground ,(jira-fmt--color c))))
                                       ('strong       '(:weight bold))
                                       ('em           '(:slant italic))
                                       ('underline    '(:underline t))
                                       ('strike       '(:strike-through t))))
                                 marks))
             (baseline-offset 0)
             (display-specs
              (progn
                (when (memq 'sub marks)
                  (cl-decf baseline-offset))
                (when (memq 'sup marks)
                  (cl-incf baseline-offset))
                (and (/= 0 baseline-offset)
                     `(raise ,baseline-offset)))))
        (when face-attrs
          (setq text (propertize text 'face face-attrs)))
        (when display-specs
          (setq text (propertize text 'display `(,display-specs))))
        text))))

(defun jira-fmt-blockquote (text)
  "Format TEXT as a block quote."
  (let ((lines (string-split text "\n")))
    (string-join (mapcar (lambda (line)
                           (propertize (concat ">  " line)
                                       'face 'jira-face-blockquote))
                         lines)
                 "\n")))

(defun jira-fmt-heading (text level)
  "Format TEXT as a heading of importance LEVEL"
  (let ((faces [jira-face-h1 jira-face-h2 jira-face-h3
                jira-face-h4 jira-face-h5 jira-face-h6]))
    (if (<= 1 level (length faces))
        (propertize text 'face (aref faces (1- level)))
      (message "[Jira Fmt Error]: Invalid heading level %s" level)
      text)))

(provide 'jira-fmt)

;;; jira-fmt.el ends here

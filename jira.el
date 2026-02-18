;;; jira.el --- Emacs Interface to Jira  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Pablo González Carrizo

;; Author: Pablo González Carrizo <unmonoqueteclea@gmail.com>
;; Version: 2.21.0
;; Created: 2025-02-16
;; URL: https://github.com/unmonoqueteclea/jira.el
;; Package-Requires: ((emacs "29.1") (request "0.3.0") (tablist "1.0") (transient "0.8.3") (magit-section "4.2.0"))

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

;; This package allows you to visualuze and manipulate Jira issues from Emacs.

;;; Code:

(require 'jira-issues)
(require 'jira-export)
(require 'jira-tempo)

(defconst jira-version "jira.el v2.21.0" "Jira.el package version.")

(provide 'jira)

;;; jira.el ends here

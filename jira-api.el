;;; jira-api.el --- Jira REST API  -*- lexical-binding: t -*-

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

;; Handle communication with Jira REST API.

;;; Code:

(require 'auth-source)
(require 'request)

(defgroup jira nil "Emacs interface to Jira."
  :group 'convenience  :group 'extensions)

(defcustom jira-username ""
  "Jira username (usually, an email)."
  :group 'jira :type 'string)

(defcustom jira-base-url ""
  "Jira instance URL, like: https://acme.atlassian.net."
  :group 'jira :type 'string)

(defcustom jira-secondary-urls nil
  "List of secondary Jira instance URLs for multi-instance support.
Each URL should be a complete Jira URL like: https://acme.atlassian.net"
  :group 'jira :type '(repeat string))

(defcustom jira-api-version 3
  "Jira API version (Versions 2 or 3 are allowed)."
  :group 'jira
  :type '(choice (const 2) (const  3)))

(defcustom jira-token ""
  "Jira API token."
  :group 'jira :type 'string)

(defcustom jira-tempo-token ""
  "Jira tempo.io API token."
  :group 'jira :type 'string)

(defcustom jira-debug nil
  "Whether to log jira.el internal processes data, including API responses."
  :group 'jira :type 'boolean)

(defcustom jira-token-is-personal-access-token nil
  "Whether the provided token is a Personal Access Token (not an JIRA API Token)."
  :group 'jira :type'boolean)

(defcustom jira-users-max-results
  1000
  "Maximum number of Jira usernames to retrieve."
  :group 'jira :type 'integer)

(defvar jira-users
  nil
  "Hash table of all Jira users, (displayName property) and ids (accountID).")

(defvar jira-tempo-url "https://api.tempo.io/4/" "Jira Tempo API URL.")
(defvar jira-account-id nil "Jira account ID of the current user.")
(defvar jira-fields nil "Jira custom fields available for the current user.")
(defvar jira-active-issue-transitions nil "Allowed transitions for active issue.")
(defvar jira-statuses nil "Jira allowed statuses.")
(defvar jira-resolutions nil "Jira allowed resolutions.")
(defvar jira-filters nil "Jira user filters.")
(defvar jira-projects nil "Jira projects (5 most recent).")
(defvar jira-projects-versions nil "Jira project versions (releases).")
(defvar jira-issue-types nil "Jira issue types available in the instance.")
(defvar jira-search-endpoint nil "Cached search endpoint: \\='search\\=' or \\='search/jql\\='.")

(defvar jira-current-url nil "Currently active Jira URL for multi-instance support.")

(defun jira-api--username ()
  "Retrieve the username for current JIRA URL from config or auth-source."
  (if (and jira-username (not (string= "" jira-username)))
      jira-username
      (let* ((current-url (jira-api--get-current-url))
             (auth-host (replace-regexp-in-string "https://" "" current-url))
	     (auth-info (car (auth-source-search :host auth-host :require '(:user)))))
	(when auth-info (plist-get auth-info :user)))))

(defun jira-api--token ()
  "Retrieve the token for current JIRA URL from config or auth-source."
  (if (and jira-token (not (string= "" jira-token)))
      jira-token
      (let* ((current-url (jira-api--get-current-url))
             (auth-host (replace-regexp-in-string "https://" "" current-url))
	     (auth-info (car (auth-source-search :host auth-host :require '(:secret)))))
	(when auth-info (funcall (plist-get auth-info :secret))))))

(defun jira-api--tempo-token ()
  "Retrieve the token for HOST from config or auth-source."
  (if (and jira-tempo-token (not (string= "" jira-tempo-token)))
      jira-tempo-token
      (let* ((auth-info (car (auth-source-search :host "tempo.io" :require '(:secret)))))
	(when auth-info (funcall (plist-get auth-info :secret))))))

(defun jira-api--auth-header (username token)
  "Generate the Authorization header for Jira requests with USERNAME and TOKEN."
  (if jira-token-is-personal-access-token
      (format "Bearer %s" token)
    (format "Basic %s" (base64-encode-string (concat username ":" token) t))))

(defun jira-api--tempo-auth-header (token)
  "Generate the Authorization header for Jira Tempo API requests with TOKEN."
  (format "Bearer %s" token))

(defun jira-api--available-urls ()
  "Return list of available JIRA hosts for completion."
  (if jira-secondary-urls
      (cons jira-base-url jira-secondary-urls)
    (list jira-base-url)))

(defun jira-api--set-current-url (url)
  "Set the currently active Jira host to URL and clear cached data."
  (interactive (list (completing-read "JIRA host: " (jira-api--available-urls))))
  (when (and url (not (string-empty-p url)))
    (setq jira-current-url url)
    (jira-api--clear-cache)
    (message "Switched to JIRA host: %s" url)))

(defun jira-api--get-current-url ()
  "Get the currently active JIRA URL, falling back to base URL if not set."
  (or jira-current-url jira-base-url))

(defun jira-api--clear-cache ()
  "Clear all cached JIRA data for URL switching."
  (interactive)
  (setq jira-users nil
        jira-account-id nil
        jira-fields nil
        jira-active-issue-transitions nil
        jira-statuses nil
        jira-resolutions nil
        jira-filters nil
        jira-projects nil
        jira-projects-versions nil
        jira-search-endpoint nil
        jira-issue-types nil))

(defun jira-api--initialize-current-url ()
  "Initialize URL system for backward compatibility.
Ensure secondary URLs list exists for completion."
  (unless jira-secondary-urls
    (setq jira-secondary-urls '())))

(defun jira-api--url (base-url endpoint)
  "Generate the full API url from BASE-URL and the given ENDPOINT."
  (let* ((version (if (numberp jira-api-version)
		      (number-to-string jira-api-version)
		    jira-api-version))
	 (api-url (concat base-url "/rest/api/" version "/")))
    (if (string-prefix-p base-url endpoint) endpoint (concat api-url endpoint))))

(defun jira-api--callback-success-log (data _response)
  "Log the RESPONSE DATA of a successful Jira API request."
  (message "[Jira API Response]: %s" data))

(defun jira-api--callback-error-log (response error-thrown)
  "Log the RESPONSE data and ERROR-THROWN of a failed Jira API request."
  (let ((status-code (request-response-status-code response))
        (response-data (request-response-data response))
        (response-headers (request-response-headers response))
        (error-message (if (symbolp error-thrown)
                           (symbol-name error-thrown)
                         (format "%s" error-thrown))))
    (message "[Jira API Error]: %s" error-message)
    (message "[Jira API Status Code]: %s" (or status-code "Unknown"))
    (message "[Jira API Response Headers]: %s" (or response-headers "No headers"))
    (message "[Jira API Response Body]: %s" (or response-data "No response body"))))

(cl-defun jira-api-call (verb endpoint &key params data callback parser sync error complete)
  "Perform a VERB request to the Jira API ENDPOINT.

PARAMS is a list of cons cells, DATA is the request body, and CALLBACK
is the function to call if successful. PARSER is a function to call
in a buffer with the result data, defaulting to `json-read'.

Sync is a boolean indicating whether the request should be
synchronous or not. If SYNC is non-nil, the request will block
until it completes, otherwise it will be asynchronous.

ERROR is a function to call if the request fails.
COMPLETE is a function to call when the request completes (regardless of success/failure)."
  (let ((current-url (jira-api--get-current-url)))
    (message "[Jira API Call]: %s %s (URL: %s)" verb endpoint current-url)
    (when (and jira-debug data)
      (message "[Jira API Call Data]: %s" (json-encode data)))
    (when (and jira-debug params)
      (message "[Jira API Call Params]: %s" params))
    (let* ((username (jira-api--username))
	   (token (jira-api--token))
	   (auth (jira-api--auth-header username token))
	   (callback (cl-function
		      (lambda (&key data response &allow-other-keys)
			(when jira-debug
			  (jira-api--callback-success-log data response))
			(when callback
			  (funcall callback data response))))))
      (if (not auth) (message "[Jira API Error]: Authorization data not found"))
      (if jira-debug (message "[Jira API Call]: Authorization %s: " auth))
      (request
	(jira-api--url current-url endpoint)
	:type verb
	:headers `(("Authorization" . ,auth) ("Content-Type" . "application/json"))
	:sync sync
	:params (or params '())
	:data (if data (json-encode data) nil)
	:parser (or parser 'json-read)
	:success callback
	:error (or error
		   (cl-function
		    (lambda (&key response error-thrown &allow-other-keys)
		      (jira-api--callback-error-log response error-thrown))))
	:complete complete))))

(cl-defun jira-api-search (&key params callback sync errback)
  "Perform a JQL search, auto-detecting the correct endpoint.
PARAMS are the search parameters. If we already know the endpoint, we use it directly."
  (if jira-search-endpoint
      (jira-api-call
       "GET" jira-search-endpoint :params params :callback callback :sync sync :error errback)
    ;; otherwise, we try with `search/jql` first, and if it fails we try with `search`
    (let* ((success-cb
            (cl-function
             (lambda (endpoint)
               (cl-function
                (lambda (data response)
                  (setq jira-search-endpoint endpoint)
                  (when callback (funcall callback data response)))))))
           ;; some old Jira versions does not support the `search/jql` endpoint,
           ;; so we need to try with `search` if the first one fails with a 404.
           (error-cb
            (cl-function
             (lambda (&key response error-thrown &allow-other-keys)
               (if (and response (= (request-response-status-code response) 404))
                   (jira-api-call "GET" "search"
                                  :params params
                                  :sync sync
                                  :callback (funcall success-cb "search")
                                  :error errback)
                 ;; if there is another error, we call the errback if it exists
                 (if errback
                     (funcall errback :response response :error-thrown error-thrown)
                   (jira-api--callback-error-log response error-thrown)))))))
      (jira-api-call "GET" "search/jql"
                     :params params
                     :sync sync
                     :callback (funcall success-cb "search/jql")
                     :error error-cb))))

(cl-defun jira-api-tempo-call (verb endpoint &key params callback)
  "Perform a VERB request to the Jira Tempo API ENDPOINT.
Calling CALLBACK if successful and passing PARAMS."
  (message "[Jira Tempo Call]: %s %s" verb endpoint)
  (let ((auth (jira-api--tempo-auth-header (jira-api--tempo-token))))
    (request
      (concat jira-tempo-url endpoint)
      :type verb
      :headers `(("Authorization" . ,auth)
                 ("Content-Type" . "application/json"))
      :params (or params '())
      :parser 'json-read
      :success (cl-function
                (lambda (&key data response &allow-other-keys)
		  (when jira-debug
                    (jira-api--callback-success-log data response))
		  (when callback
		    (funcall callback data response))))
      :error (cl-function
              (lambda (&key response error-thrown &allow-other-keys)
                (jira-api--callback-error-log response error-thrown))))))

(cl-defun jira-api-get-account-id (&key force callback)
  "Retrive the account ID of the current user.

FORCE will force the request even if the account ID is already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-account-id))
      (let ((success (lambda (data _response)
                       (setq jira-account-id (cdr (assoc 'accountId data)))))
            (next (lambda (&rest _args) (when callback (funcall callback)))))
        (jira-api-call "GET" "myself" :callback success :complete next))
    (when callback (funcall callback))))

(cl-defun jira-api-get-fields (&key force callback)
  "Retrive the available custom fields for the current user.

FORCE will force the request even if the fields are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-fields))
      (let ((success (lambda (data _response)
                       (setq jira-fields
			     (mapcar (lambda (field)
                                       (cons (cdr (assoc 'name field))
					     (cdr (assoc 'key field))))
				     data))))
            (next (lambda (&rest _args) (when callback (funcall callback)))))
        (jira-api-call "GET" "field" :callback success :complete next))
    (when callback (funcall callback))))

(defun jira-api-get-transitions (issue-keys)
  "Get the transitions available for a list of ISSUE-KEYS."
  ;; Jira API version 2 does not support bulk transitions
  (let* ((bulk (if (= jira-api-version 3) t nil))
	 (format-transition
	  (if bulk
              (lambda (tr) (cons (alist-get 'statusName (alist-get 'to tr))
				 (alist-get 'transitionId tr)))
	    (lambda (tr) (cons (alist-get 'name (alist-get 'to tr))
                               (alist-get 'id tr)))))
         (extract
	  (if bulk
              (lambda (data _response)
		(let* ((trdata (car (append (alist-get 'availableTransitions data) nil)))
		       (transitions (alist-get 'transitions trdata)))
		  (mapcar format-transition transitions)))
	    (lambda (data _response)
              (let* ((transitions (alist-get 'transitions data)))
		(mapcar format-transition transitions))))))
    (if bulk
	(jira-api-call
	 "GET" "bulk/issues/transition"
	 :params `(("issueIdsOrKeys" . ,(mapconcat #'identity issue-keys ",")))
	 :callback (lambda (data response)
                     (setq jira-active-issue-transitions
			   (funcall extract data response))))
      (jira-api-call "GET" (format "issue/%s/transitions" (cl-first issue-keys))
                     :callback (lambda (data response)
				 (setq jira-active-issue-transitions
                                       (funcall extract data response)))))))

(cl-defun jira-api-get-statuses (&key force callback)
  "Get the list of allowed issues statuses.

FORCE will force the request even if the statuses are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-statuses))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'id s)))))
        (jira-api-call
         "GET" "status"
         :callback
         (lambda (data _response) (setq jira-statuses (mapcar fmt data)))
         :complete
         (lambda (&rest _args) (when callback (funcall callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-resolutions (&key force callback)
  "Get the list of allowed resolutions.

FORCE will force the request even if the resolutions are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-resolutions))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'id s)))))
        (jira-api-call
         "GET" "resolution"
         :callback
         (lambda (data _response)
           (let* ((resolutions (mapcar fmt data)))
	     ;; Jira UI represents with the "Unresolved" key the
	     ;; resolutions for fields that don't have a resolution set.
	     ;; If we find this value, we will send a nil
             (setq jira-resolutions (cons (cons "Unresolved" nil) resolutions))))
         :complete
         (lambda (&rest _args) (when callback (funcall callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-filters (&key force callback)
  "Get the list of user filters.

FORCE will force the request even if the filters are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-filters))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'jql s)))))
        (jira-api-call
         "GET" "filter/my?includeFavourites=true"
         :callback
         (lambda (data _response) (setq jira-filters (mapcar fmt data)))
         :complete
         (lambda (&rest _args) (when callback (funcall callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-issue-types (&key force callback)
  "Get the list of available issue types.

FORCE will force the request even if the issue types are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-issue-types))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'id s)))))
        (jira-api-call
         "GET" "issuetype"
         :callback
         (lambda (data _response) (setq jira-issue-types (mapcar fmt data)))
         :complete
         (lambda (&rest _args) (when callback (funcall callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-projects (&key force callback)
  "Get the 10 most recent projects.

FORCE will force the request even if the projects are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-projects))
      (let* ((fmt (lambda (s) (cons (alist-get 'key s) (alist-get 'name s)))))
        (jira-api-call
         "GET" "project"
         :params `(("recent" . ,10))
         :callback
         (lambda (data _)
           (setq jira-projects (mapcar fmt data))
           (jira-api-get-versions :force t :callback callback))
         :complete
         (lambda (&rest _args) (when callback (funcall callback)))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-versions (&key force callback)
  "Get the list of versions for the stored projects.

FORCE will force the request even if the versions are already stored.
CALLBACK is the function to call after the request is done."
  (if (or force (not jira-projects-versions))
      (let* ((fmt (lambda (s) (cons (alist-get 'name s) (alist-get 'id s)))))
        (setq jira-projects-versions nil)
        (dolist (project jira-projects)
          (jira-api-call
           "GET" (concat "project/" (car project) "/version")
           :params `(("orderBy" . "-sequence")
                     ("maxResults" . 50))
           :callback
           (lambda (data _)
             (let* ((versions (mapcar fmt (alist-get  'values data))))
               (setq jira-projects-versions
                     (cons (cons (car project) versions) jira-projects-versions)))
             (when callback (funcall callback))))))
    (when callback (funcall callback))))

(cl-defun jira-api-get-users (&key force)
  "Fetch the list of all Jira user names and IDs and store it in `jira-users'.

If FORCE is non-nil, re-fetches the user list."
  (interactive)
  (when (or (not jira-users)
            force)
    (let ((table (make-hash-table :test #'equal)))
      ;; Theses params are undocumented but work:
      ;; https://stackoverflow.com/a/64786638
      (jira-api-call "GET"
                     "users/search"
                     :params
                     `((query . "+")
                       (maxResults . ,jira-users-max-results))
                     :callback
                     (lambda (data _response)
                       (mapc (lambda (u)
                                 (let ((id (alist-get 'accountId u))
                                       (name (alist-get 'displayName u)))
                                   (unless (eq :json-false (alist-get 'active u))
                                     (setf (gethash name table) id))))
                             data)))
      (setq jira-users table))))

(cl-defun jira-api-get-create-metadata (project-key issue-type-id &key callback sync)
  "Get the metadata for creating an issue in PROJECT-KEY with ISSUE-TYPE-ID.
CALLBACK is the function to call after the request is done.
SYNC determines if the call should be synchronous."
  (let* ((url (format "issue/createmeta/%s/issuetypes/%s"
		      project-key issue-type-id))
         (response (jira-api-call "GET" url :params `(("maxResults" . 100)) :callback callback :sync sync)))
    (when sync
      (request-response-data response))))

(cl-defun jira-api-get-project-issue-types (project-key &key callback sync)
  "Get the issue types available for PROJECT-KEY.
Optionally calls CALLBACK if provided and makes the request synchronous if SYNC."
  (let* ((url (format "issue/createmeta/%s/issuetypes" project-key))
	 (response (jira-api-call "GET" url :callback callback :sync sync)))
    (when sync (request-response-data response))))

(cl-defun jira-api-get-linked-issues (issue-key &key callback sync)
  "Get linked issues for ISSUE-KEY.

CALLBACK is the function to call after the request is done.
SYNC determines if the request should be synchronous."
  (jira-api-call "GET"
                 (format "issue/%s" issue-key)
                 :params `(("fields" . "issuelinks"))
                 :callback callback
                 :sync sync))

(cl-defun jira-api-get-basic-data (&key force)
  "Get some basic data (custom fields, projects, statuses, etc) from JIRA API.

FORCE will force the request even if the data is already stored."
  ;; one call after the other, to avoid request burst
  (let* ((users (lambda () (jira-api-get-users :force force)))
         (fds (lambda () (jira-api-get-fields :force force :callback users)))
         (st (lambda () (jira-api-get-statuses :force force :callback fds)))
         (res (lambda () (jira-api-get-resolutions :force force :callback st)))
         (flt (lambda () (jira-api-get-filters :force force :callback res)))
         (types (lambda () (jira-api-get-issue-types :force force :callback flt)))
         (prj (lambda () (jira-api-get-projects :force force :callback types)))
         (account (lambda () (jira-api-get-account-id :force force :callback prj))))
    (funcall account)))


(provide 'jira-api)

;;; jira-api.el ends here

# jira.el

Emacs integration for [Atlassian's Jira](https://www.atlassian.com/software/jira).

[![MELPA Stable](https://stable.melpa.org/packages/jira-badge.svg)](https://stable.melpa.org/#/jira)

> If you have no choice but to use Jira, at least do it without leaving Emacs.

Supports listing and filtering issues, viewing issue details, modifying certain
properties, adding worklogs or exporting issues to `Markdown`, `Org-mode` or `CSV`.

Additionally, it provides support for displaying all worklogs from the
[Tempo](https://www.tempo.io/products/jira-time-tracking) integration.

## 🎉 Version 2 Release! 🎉

We're excited to announce a major update to `jira.el`, bringing significant enhancements
to the Jira Detail view and overall usability:

- **Create issue subtasks**
  - Press `S` from the detail view of an issue to create a subtask.
- **Enhanced Issue Management from Detail View:**
  - Press `?` to reveal a comprehensive menu of all available actions and keybindings.
  - Update various issue fields effortlessly using `U`. Enjoy **intelligent suggestions**
    for many fields, pulled directly from the Jira API, making updates faster and more
    accurate.
  - Seamlessly change issue statuses with a quick press of `C`.

Upgrade today for a more powerful and efficient Jira experience within Emacs!


## Screenshots

- List issues

![List issues](doc/list-issues.png)

- Filter issues

![Filter issues](doc/list-issues-filter.png)

- Change selected issue

![Change issue](doc/change-issue.png)

- List [Tempo](https://www.tempo.io/) worklogs

![List Worklogs](doc/list-worklogs.png)

## Installation
This package is available in [MELPA](https://github.com/milkypostman/melpa),
so you just need to do:

```elisp
(use-package jira
  :config
  (setq jira-base-url "https://acme.atlassian.net") ;; Jira instance URL
  (setq jira-username "johndoe@acme.com") ;; Jira username (usually, an email)
  ;; API token for Jira
  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
  (setq jira-token "foobar123123")
  (setq jira-token-is-personal-access-token nil)
  (setq jira-api-version 3) ;; Version 2 is also allowed
  ;; (Optional) API token for JIRA TEMPO plugin
  ;; See https://apidocs.tempo.io/
  (setq jira-tempo-token "foobar123123"))
```

You can also install it using
[straight.el](https://github.com/radian-software/straight.el)

```elisp
(use-package jira
  :straight (:host github :repo "unmonoqueteclea/jira.el")
  :demand t
  :config ...)
```

## Authentication
`jira.el` supports two methods for authenticating with the Jira REST API:

1.  **Configuration variables:** You can directly set the
    `jira-username` and `jira-token` variables (and `jira-tempo-token`
    if you use it) in your Emacs configuration (e.g., your `init.el`
    or `config.el`).  This is often the simplest way to get started.

    ```elisp
      (setq jira-username "johndoe@acme.com") ;; Jira username (usually, an email)
	  ;; API token for Jira
	  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
	  (setq jira-token "foobar123123")
    ```

    While convenient, storing your token directly in your Emacs
    configuration can pose a security risk if your configuration file
    is not properly protected. Consider using `auth-source` for more
    secure storage.

2.  **`auth-source`:** This is the recommended and more secure
    method. `auth-source` is an Emacs library for managing
    credentials.  It allows you to store your Jira username and API
    token in a secure location (like `~/.authinfo.gpg`) and `jira.el`
    will retrieve them when needed.

    -  **Configure `auth-source`:** Add an entry to your
    `~/.authinfo` or `~/.authinfo.gpg` file (create the file if it
    doesn't exist).

	```
	machine acme.atlassian.net login johndoe@acme.com port https password foobar123123
	```

	And, if you use `tempo.io`:

	```
	machine tempo.io port https password foobar123123
	```

    **`jira.el` will automatically use `auth-source` if `jira-username` and `jira-token` are not explicitly set.**

	⚠️ If you are using a Jira [**Personal Access Token (PAT)**](https://confluence.atlassian.com/enterprise/using-personal-access-tokens-1026032365.html) instead
    of an **API Token**, you will need to set `jira-token-is-personal-access-token`:

	```elisp
	(setq jira-token-is-personal-access-token t)
	```

🌐 For multiple Jira hosts, add entries for each host in your auth-source file and
configure `jira-secondary-urls`. Press `H` in the issues list to switch between hosts.

## Quickstart
Use `M-x jira-issues` to check the list of issues assigned to the
configured user for the active sprint. Once the list is loaded, press
`?` to see the available actions. You can modify the filters, update
the selected issue, add worklogs, and more.

Press `l` to select the filters to be applied to the list of
issues. You can filter by several fields, use one of your starred filters,
or even write your own `JQL` filter.

- Thanks to
[transient](https://magit.vc/manual/transient/Saving-Values.html#Saving-Values),
all the transients arguments can be set temporarily or
permanently. See [transient
docs](https://magit.vc/manual/transient/Saving-Values.html#Saving-Values)
for more information.

- Check out [tablist docs](https://github.com/politza/tablist) for additional
keybindings to manage the table (sorting, filtering, exporting, etc)


To export issues, press `E` to open the export menu, or use `M-x jira-export-issues` to
export all visible issues to your preferred format (`Markdown`, `Org-mode`, or
`CSV`). Issue keys and URLs are automatically converted to clickable links.

If you configured [Tempo integration](https://www.tempo.io/), you can
also run `jira-tempo` to view the list of worklogs for the current
week.

## FAQ

- ❓ **Does it work with my old on-premise instance?**

	I try to provide support as much as possible, but I don’t have access to this kind of
	JIRA installation, so I rely on the official documentation and user PRs and issues.
	If you’re using API version 2, make sure to set `jira-api-version` (see the
	**Customization** section). Version `v2.8.2` fixes an issue with older on-premise
	instances related to the endpoint that lists issues. I’m also working on making issue
	transitions work (see https://github.com/unmonoqueteclea/jira.el/issues/37).

## Customization

This is the list of customizations you can set:

- `jira-base-url`: **Mandatory**: Jira instance URL, like: https://acme.atlassian.net
- `jira-username`: Jira username (usually, an email)
- `jira-token`: Jira REST API token
- `jira-api-version`: Jira REST API version. Can be `2` or `3` (default: `3`)
   - ⚠️ Some Jira instances only allow REST API version 2
- `jira-tempo-token`: Jira [tempo.io](https://www.tempo.io/) API token
- `jira-debug`: Whether to log jira.el internal processes data, including API responses
- `jira-users-max-results`:  Maximum number of Jira usernames to retrieve (default: `1000`)
- `jira-issues-table-fields`: Fields to show in the issues table.
   Allowed values are defined in `jira-issues-fields`. Example:
   `'(:key :issue-type-name :status-name :assignee-name :progress-percent :work-ratio
      :remaining-time :summary)`
- `jira-issues-max-results`: Maximum number of Jira issues to retrieve
- `jira-issues-default-type`: Default issue type filter for the issues list.
  When set to a type name (e.g., "Epic", "Story", "Bug"), the issues list will
  automatically filter to show only that type unless explicitly overridden via
  the transient menu. Set to nil for no default filter. (default: `nil`)
- `jira-tempo-max-results`: Maximum number of Tempo worklogs to retrieve
- `jira-comments-display-recent-first`: The order to display Jira comments in
  issue detail view.
- `jira-use-color-marks`: If true, display color marks in Jira text.
  (default: `t`)
- `jira-detail-reuse-buffer`: If non-nil, reuse a single Jira detail buffer
  instead of creating separate buffers for each issue. When enabled, all issue
  details will be displayed in one buffer named "*Jira Issue Detail*" instead
  of creating separate buffers like "*Jira Issue Detail: [PROJ-123]*". (default: `nil`)
- `jira-detail-show-announcements`: Whether to show announcements in Jira
   detail view. Useful to know new features of `jira.el`. (default: `t`)
- `jira-status-faces`: Alist mapping status names to faces to override
  default styling (faces are automatically assigned to statuses based
  on the status category: `To Do`, `In Progress` or `Done`). For example:
  ```elisp
  (defface jira-face-pr
  '((t (:foreground "white" :background "orange" :weight bold)))
  "Face for pull-request status."
  :group 'jira)
  (setq jira-status-faces '(("Pull-request" . jira-face-pr)))
  ```
- `jira-datetime-format`: Format string for displaying datetimes (default: `%c`)
- `jira-jira-issues-sort-key`: Default tabulated-list sort key for jira-issues-mode. Set to nil to preserve order from JQL (default: ("Status" . nil)).

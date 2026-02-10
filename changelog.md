# Changelog
## 2.19.0 (2026-02-10)
- Add `jira-issues-default-type` customization variable to set a default issue type filter
- Add "Any" option to Issue Type filter to allow disabling the default type filter

## 2.18.0 (2026-02-10)
- In detail view, update the handling of linked issues to match the handling of subtasks

## 2.17.0 (2026-01-24)
- Add issue type filter to transient menu

## 2.16.0 (2026-01-24)
- Add interactive subtask section in detail view
- ADF: Display and create action item lists

## 2.15.0 (2025-12-08)
- Expose `tabulated-list-sort-key`
- Press `h` to open transient actions menu

## 2.14.1 (2025-11-28)
-  Fix multi-paragraph code blocks and misrecognized text marks in ADF

## 2.14.0 (2025-11-14)
-  Add export functionality for JIRA issues with support for clickable links. Export to
   Markdown, Org-mode, and CSV formats. Accessible via `jira-export-issues` or from the
   Actions menu.

## 2.13.2 (2025-11-14)
- Add `jira-detail-reuse-buffer` configuration option to enable reusing a single Jira
  detail buffer instead of creating separate buffers for each issue. When enabled, all
  issue details are displayed in one buffer named "*Jira Issue Detail*" instead of
  separate buffers like "*Jira Issue Detail: [PROJ-123]*".

## 2.13.1 (2025-11-14)
- Add `complete` callback parameter to `jira-api-call` for better async handling.
- Refactor API functions to separate success and complete callbacks for improved error handling.

## 2.13.0 (2025-11-05)
- Allow combining custom JQL filters with other filter arguments using AND operator.
- Move named filters to Actions section of List Jira Issues menu to avoid conflicts.

## 2.12.2 (2025-11-04)
- Use ADX format for description field during subtask creation.

## 2.12.1 (2025-11-01)
- Fix compilation for Emacs 29, broken due to an unsupported rx expression.

## 2.12.0 (2025-10-31)
- Add multi-host support: Press `H` to switch between Jira instances
- Configure secondary hosts with `jira-secondary-urls`

## 2.11.0 (2025-10-26)
- Show linked inssues in in Jira Detail view.

## 2.10.3 (2025-10-25)
- Fix JIRA API deprecation issue #46. Replace deprecated
  `issue/createmeta/?projectKeys=%s` endpoint with `issue/createmeta/%s/issuetypes` to
  restore subtask creation functionality.
- Add validation to prevent 400 errors when issue type ID is missing.

## 2.10.2 (2025-10-11)
- Display `mediaInline` nodes as attachments. When user adds a file to a comment, one of
these mediaInline elements are created.

## 2.10.1 (2025-10-11)
- Better Error Handling On `jira-issues--fetch-and-display`.
It fixes the following problem reported by @Gchouchou:

```
I am having issues recovering from errors running jira-issues--fetch-and-display.
If, say you run with an invalid jql, jira-issues--loading-p would be set to t
because the callback will never be evaluated to set it back to nil. I tried to see
if this is a simple fix but it requires some refactoring of jira-api-search to
customize error handling and make sure we set jira-issues--loading-p back to nil
after an error. The manual fix is not too hard (just set it to t) but it would
be greatly helpful to people that do not want to go into the source code and
wonder issues stopped reloading.
```

## 2.10.0 (2025-10-06)
- Add support for tables in comments and issue descriptions

## 2.9.3 (2025-09-30)
- Automatically convert raw URLs to ADF links in the editor.

## 2.9.2 (2025-09-30)
- Allow specialized modes to work with Jira attachments.
- jira-complete-ask-issue: put default-input before the colon.

## 2.9.1 (2025-09-22)
- Restore support for transitioning issue status in Jira API version 2.

## 2.9.0 (2025-09-15)
- Add syntax highlighting for jira-edit-mode. Thanks @danielcmccarthy!

## 2.8.2 (2025-09-06)
- Restore compatibility of `jira.el` with older JIRA API versions where the `search/jql`
  endpoint does not exist.

## 2.8.1 (2025-09-01)
- Apply font-lock-face to faces to fix faces not being rendered in detail view.

## 2.8.0 (2025-08-29)
- Add pagination to the Jira issues list view. Now you can navigate between pages
  using `M-n` and `M-p`. Also, a message is shown when there are no more pages.

## 2.7.4 (2025-08-27)
- Add `jira-datetime-format` setting to configure format string for displaying datetimes
  in issues.

## 2.7.3 (2025-08-27)
- Add `parent-summary` to list of available issue fields.

## 2.7.2 (2025-08-24)
- Fix deprecated `search` endpoint, use `search/jql` instead.

## 2.7.1 (2025-07-23)
- Add `EMPTY` option to assignee filter in issues view to find unassigned issues

## 2.7.0 (2025-07-22)
- Add assignee and reporter filters to issues list

## 2.6.0 (2025-07-21)
- Add ability to bulk change issues status

## 2.5.0 (2025-07-21)
- Add ability to add/remove watchers on an issue

## 2.4.0 (2025-07-19)
- Add support for favorite JQL filters.

## 2.3.0 (2025-07-19)
- Support panel block formatting in issue detail view.

## 2.2.0 (2025-07-17)
- Allow the creation of subtasks from issue detail view.

## 2.1.0 (2025-07-08)
- Add new section for subtasks in the detail view.
- Prepend appropriate emojis to headings in the detail view.

## 2.0.5 (2025-07-07)
- Add `P` keybinding to jump to the detail view of the parent issue.
- Add "Show parent issue" to the transient menu.

## 2.0.4 (2025-07-04)
- Find an issue by key or URL from the issues list view by pressing 'f'.

## 2.0.3 (2025-06-30)
- Add `jira-detail-show-announcements` custom variable to control announcements in detail view.
- Add announcement in Jira detail view about new update field feature.

## 2.0.2 (2025-06-30)
- Fix: Labels can now be updated correctly, handling the API's expectation of a list of strings.
## 2.0.1 (2025-06-30)
- Fix bug while updating the status of an issue. The original estimation was updated
  instead of the remaining time
## 2.0.0 (2025-06-29)
- **Major Version Update**
- **Enhanced Issue Management from Detail View:**
  - Seamlessly change issue statuses with a quick press of `C`.
  - Update various issue fields effortlessly using `U`. Enjoy intelligent
    suggestions for many fields, pulled directly from the Jira API, making
	updates faster and more accurate.
  - Press `?` to reveal a comprehensive menu of all available actions and keybindings.

## 1.1.5 (2025-06-27)
- Press `g` in Jira Detail mode to refresh the issue

## 1.1.4 (2025-06-21)
- Provide an `Unresolved` resolution value for issues.  Jira UI represents with
  the "Unresolved" key the resolutions for fields that don't have a resolution
  set.

## 1.1.3 (2025-06-08)
- Fix another critical bug with formatting of issue fields that can't be empty

## 1.1.2 (2025-06-06)
- Fix critical bug with formatting of issue fields that can't be empty

## 1.1.1 (2025-06-04)
- Fix critical bug that wasn't allowing to open Jira Issue Detail view

## 1.1.0 (2025-06-03)
- Add keybinding to remove a comment from issue detail view
- Add help transient in Issue Detail mode, press '?' to open it

## 1.0.3 (2025-05-26)
- Fix another formatting issue in detail view

## 1.0.2 (2025-05-26)
- Fix formatting issues with progress fields

## 1.0.1 (2025-05-26)
- Fix formatting issues in `jira-tempo`

## 1.0.0 (2025-05-23)
- **Breaking Change**: `jira-statuses-done`, `jira-statuses-progress`,
`jira-statuses-todo`, `jira-statuses-error` doesn't work any more.

	- Replaced status lists by status categories to **group statuses** in
      `Todo`, `In progress` and `Done`, improving the maintainability of the
      config. Each category have an associated face.

    - Added possibility to custom faces based on status names, allowing to
      override status categories.

## 0.11.0 (2025-05-19)
- Display more kinds of markup in comments and descriptions: numeric labels for
  orderedLists, indentation for nested lists, user mentions, emoji, calendar
  dates, italic, underline, strike-through, inline code, super/subscript,
  foreground color, adapting to dark backgrounds


## 0.10.0 (2025-05-17):
- Write comments in a new buffer

## 0.9.1 (2025-04-17)
- Add logging messages to find problems with API auth

## 0.9.0 (2025-04-17)
- Display times in locale's preferred format
- Add defcustom to display comments in chronological order

## 0.8.0 (2025-04-09):
- Add support for attachments (thank you @danielcmccarthy!)

## 0.7.0 (2025-03-29):
- Add comments from jira detail view

## 0.6.0 (2025-03-28)
- Add keybinding in issue to jump to tempo and the other way around.
- Close the jira issues buffer before I start the jira-tempo one, to avoid issues
- Avoid deleting other windows when opening jira-issues

## 0.5.0 (2025-03-24)
- Add support for bold text and code in docs
- Show comments in issue detail view

## 0.4.0 (2025-03-23)
- Add `jira-issues-table-fields` so that users can select the fields
  to be visualized in the table
- `--current-sprint` is not set by default any more when retrieving
  issues

## 0.3.0 (2024-03-21)
- Add support for Jira Personal Access Token (PAT)

## 0.2.2 (2025-03-20)
- Convert windows line endings to unix ones in issues detail

## 0.2.1 (2025-03-20)
- Add constant with package version: `jira-version`

## 0.2.0 (2025-03-19)
- Add support for authentication with `auth-source`

## 0.1.1 (2025-03-18)
- Fix bug for missing fields in issue detail view

## 0.1.0 (2025-03-18)
- Allow configuring Jira REST API Version

## 0.0.2 (2025-03-17)
- Log requests input data (if `jira-debug`)
- Provide date suggestions while adding a worklog

## 0.0.1 (2025-03-16)
- First working version

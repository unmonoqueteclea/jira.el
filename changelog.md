# Changelog

## 0.8.0 (2024-04-09):
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

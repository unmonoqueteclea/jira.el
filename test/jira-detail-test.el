;;; jira-detail-test.el --- Tests for jira-detail  -*- lexical-binding: t -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -f package-initialize -L . -l test/jira-detail-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'jira-detail)

(defun jira-detail-test--field-on-line (text)
  "Return the field at point on the line containing TEXT in a detail buffer."
  (with-temp-buffer
    (magit-section-mode)
    (let ((inhibit-read-only t))
      (insert (jira-detail--header "Status") "IN PROGRESS\n")
      (insert (jira-detail--header "Time") "1h (remaining: 1h)\n")
      (insert (jira-detail--header "Summary") "Fix the thing\n")
      (insert (jira-detail--header "Parent") "\n")
      (insert (jira-detail--header "Priority") "Medium\n")
      (insert "Priority is high, said a comment\n")
      (insert (jira-fmt-set-face "Labels" 'italic) " in an italic comment\n"))
    (goto-char (point-min))
    (search-forward text)
    (jira-detail--field-at-point)))

(ert-deftest jira-detail-test-field-at-point ()
  (should (equal (jira-detail-test--field-on-line "Fix the thing") "Summary"))
  (should (equal (jira-detail-test--field-on-line "Medium") "Priority"))
  ;; headers with a different field name
  (should (equal (jira-detail-test--field-on-line "Parent") "Parent Issue"))
  (should (equal (jira-detail-test--field-on-line "remaining") "Remaining Estimate")))

(ert-deftest jira-detail-test-no-field-at-point ()
  ;; a header that can't be updated
  (should-not (jira-detail-test--field-on-line "IN PROGRESS"))
  ;; text that only looks like a header
  (should-not (jira-detail-test--field-on-line "said a comment"))
  (should-not (jira-detail-test--field-on-line "italic comment")))

(provide 'jira-detail-test)
;;; jira-detail-test.el ends here

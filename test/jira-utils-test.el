;;; jira-utils-test.el --- Tests for jira-utils  -*- lexical-binding: t -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -f package-initialize -L . -l test/jira-utils-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'jira-detail)
(require 'jira-utils)

(ert-deftest jira-utils-test-marked-items-in-detail-buffer ()
  ;; point on text that is not a tablist entry
  (with-temp-buffer
    (jira-detail-mode)
    (let ((inhibit-read-only t))
      (insert "Summary  Fix the thing\n"))
    (goto-char (point-min))
    (setq jira-detail--current-key "TEST-1")
    (should-not (jira-utils-marked-items))
    (should (equal (jira-utils-marked-item) "TEST-1"))))

(ert-deftest jira-utils-test-marked-items-in-list ()
  (with-temp-buffer
    (tabulated-list-mode)
    (tablist-minor-mode)
    (setq tabulated-list-format [("Key" 10 t)]
          tabulated-list-entries '(("TEST-1" ["TEST-1"]) ("TEST-2" ["TEST-2"])
                                   ("TEST-3" ["TEST-3"])))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (goto-char (point-min))
    (should (equal (jira-utils-marked-items) '("TEST-1")))
    (tablist-put-mark)
    (forward-line 2)
    (tablist-put-mark)
    (goto-char (point-min))
    (should (equal (jira-utils-marked-items) '("TEST-1" "TEST-3")))))

(provide 'jira-utils-test)
;;; jira-utils-test.el ends here

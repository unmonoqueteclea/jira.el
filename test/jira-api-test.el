;;; jira-api-test.el --- Tests for jira-api  -*- lexical-binding: t -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -f package-initialize -L . -l test/jira-api-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'jira-api)

(defmacro jira-api-test--with-secret (secret &rest body)
  "Run BODY with `auth-source-search' returning SECRET for every host."
  (declare (indent 1))
  `(let ((jira-base-url "https://acme.atlassian.net")
         (jira-current-url nil)
         (jira-token "")
         (jira-tempo-token ""))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest _) (list (list :secret ,secret)))))
       ,@body)))

(ert-deftest jira-api-test-token-from-secret-function ()
  (jira-api-test--with-secret (lambda () "fn-token")
    (should (equal (jira-api--token) "fn-token"))
    (should (equal (jira-api--tempo-token) "fn-token"))))

(ert-deftest jira-api-test-token-from-secret-string ()
  ;; Some auth-source backends return the secret as a plain string
  (jira-api-test--with-secret "str-token"
    (should (equal (jira-api--token) "str-token"))
    (should (equal (jira-api--tempo-token) "str-token"))))

(ert-deftest jira-api-test-token-from-variable ()
  (let ((jira-token "var-token"))
    (should (equal (jira-api--token) "var-token"))))

(provide 'jira-api-test)
;;; jira-api-test.el ends here

;;; jira-doc-test.el --- Tests for jira-doc  -*- lexical-binding: t -*-

;;; Commentary:
;; Run with:
;;   emacs -Q --batch -f package-initialize -L . -l test/jira-doc-test.el \
;;         -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'jira-doc)

(defun jira-doc-test--card (type)
  "Return the first node of TYPE built from the card markup examples."
  (let* ((markup (concat "Inline [https://a.com|https://a.com|smart-link] text.\n\n"
                         "[https://b.com|https://b.com|smart-card]\n\n"
                         "[https://c.com|https://c.com|smart-embed]"))
         (doc (jira-doc-build markup))
         (nodes (append (alist-get "content" doc nil nil #'equal) nil))
         (nodes (append nodes
                        (mapcan (lambda (n)
                                  (append (alist-get "content" n nil nil #'equal) nil))
                                nodes))))
    (seq-find (lambda (n) (equal (alist-get "type" n nil nil #'equal) type)) nodes)))

(ert-deftest jira-doc-test-build-cards ()
  (dolist (case '(("inlineCard" . "https://a.com")
                  ("blockCard" . "https://b.com")
                  ("embedCard" . "https://c.com")))
    (let* ((node (jira-doc-test--card (car case)))
           (attrs (alist-get "attrs" node nil nil #'equal)))
      (should node)
      (should (equal (alist-get "url" attrs nil nil #'equal) (cdr case))))))

(ert-deftest jira-doc-test-embed-card-has-layout ()
  (let ((attrs (alist-get "attrs" (jira-doc-test--card "embedCard") nil nil #'equal)))
    (should (equal (alist-get "layout" attrs nil nil #'equal) "center"))))

(ert-deftest jira-doc-test-card-markup-roundtrip ()
  (let ((adf '((type . "doc") (version . 1)
               (content . [((type . "blockCard")
                            (attrs (url . "https://b.com")))
                           ((type . "embedCard")
                            (attrs (layout . "center") (url . "https://c.com")))]))))
    (should (equal (jira-doc-markup adf)
                   (concat "[https://b.com|https://b.com|smart-card]\n\n"
                           "[https://c.com|https://c.com|smart-embed]")))))

(provide 'jira-doc-test)
;;; jira-doc-test.el ends here

;;; claude-archive-consult-test.el --- Tests for claude-archive-consult -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for claude-archive-consult.el

;;; Code:

(require 'ert)
(require 'claude-archive-consult)

(defun claude-archive-consult-test--conversation (title text)
  "Return a conversation called TITLE whose only message says TEXT."
  (claude-archive--conversation-create
   :uuid title :title title :branches 1
   :ended (date-to-time "2025-08-27T06:19:24Z")
   :messages (list (claude-archive--message-create
                    :sender 'human
                    :blocks (list (list :type 'text :text text))))))

(ert-deftest claude-archive-consult-test-matches ()
  "Only conversations matching every regexp become candidates."
  (let* ((tuscany (claude-archive-consult-test--conversation
                   "Trip" "Tuscany by train, Tuscany by car"))
         (paris (claude-archive-consult-test--conversation "City" "Paris"))
         (candidates (claude-archive-consult--matches
                      (list tuscany paris) '("tuscany"))))
    (should (= (length candidates) 1))
    (should (eq (get-text-property 0 'claude-archive-consult--conversation
                                   (car candidates))
                tuscany))
    (should (equal (get-text-property 0 'claude-archive-consult--hits
                                      (car candidates))
                   2))
    (should-not (claude-archive-consult--matches (list tuscany paris) nil))))

(ert-deftest claude-archive-consult-test-annotate ()
  "Candidates are annotated with message and match counts."
  (let ((conversation (claude-archive-consult-test--conversation "T" "x")))
    (should (equal (substring-no-properties
                    (claude-archive-consult--annotate
                     (claude-archive-consult--candidate conversation)))
                   "  1 msgs"))
    (should (equal (substring-no-properties
                    (claude-archive-consult--annotate
                     (claude-archive-consult--candidate conversation 4)))
                   "  1 msgs, 4 hits"))))

(provide 'claude-archive-consult-test)
;;; claude-archive-consult-test.el ends here

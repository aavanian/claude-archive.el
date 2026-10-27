;;; claude-archive-test.el --- Tests for the claude.ai archive -*- lexical-binding: t -*-

;;; Commentary:

;; Tests for claude-archive.el

;;; Code:

(require 'ert)
(require 'claude-archive)

(defvar claude-archive-test--root "00000000-0000-4000-8000-000000000000"
  "Parent uuid the export gives to the first message of a conversation.")

(defun claude-archive-test--json (string)
  "Parse STRING the way an export is parsed."
  (apply #'json-parse-string string claude-archive--json-args))

(defun claude-archive-test--message (uuid parent sender created text)
  "Return the JSON of message UUID under PARENT from SENDER at CREATED saying TEXT."
  (apply #'format "{\"uuid\":%s,\"parent_message_uuid\":%s,\"sender\":%s,
                   \"created_at\":%s,\"attachments\":[],\"files\":[],
                   \"content\":[{\"type\":\"text\",\"text\":%s}]}"
         (mapcar #'json-encode (list uuid parent sender created text))))

(defun claude-archive-test--conversation (name &rest messages)
  "Return a parsed conversation called NAME made of MESSAGES' JSON."
  (claude-archive-test--json
   (format "{\"uuid\":\"c1\",\"name\":%S,
             \"created_at\":\"2025-08-27T06:19:22Z\",
             \"updated_at\":\"2025-08-28T06:19:22Z\",
             \"chat_messages\":[%s]}"
           name (string-join messages ","))))

(defvar claude-archive-test--branched
  (claude-archive-test--conversation
   ""
   (claude-archive-test--message
    "m1" claude-archive-test--root "human"
    "2025-08-27T06:19:24Z" "First question about Tuscany")
   (claude-archive-test--message
    "m2" "m1" "assistant" "2025-08-27T06:19:30Z" " Old answer")
   (claude-archive-test--message
    "m3" "m1" "assistant" "2025-08-27T06:25:00Z" " Regenerated answer"))
  "A parsed conversation whose answer was regenerated once.")

(defun claude-archive-test--write-export (directory name age)
  "Write an empty conversations file under DIRECTORY/NAME, AGE days old.
NAME nil writes it in DIRECTORY itself.  Return the file name."
  (let ((file (expand-file-name "conversations.json"
                                (if name (expand-file-name name directory)
                                  directory))))
    (make-directory (file-name-directory file) t)
    (write-region "[]" nil file nil 'silent)
    (set-file-times file (time-subtract nil (days-to-time age)))
    file))

(ert-deftest claude-archive-test-latest-export ()
  "The most recently modified conversations file wins."
  (let ((directory (make-temp-file "claude-archive" t)))
    (unwind-protect
        (progn
          (should-not (claude-archive--latest-export directory))
          (claude-archive-test--write-export directory "export-b" 3)
          (let ((newest (claude-archive-test--write-export
                         directory "export-a" 1)))
            (claude-archive-test--write-export directory "export-c" 2)
            (make-directory (expand-file-name "empty" directory))
            (should (equal (claude-archive--latest-export directory) newest)))
          (let ((own (claude-archive-test--write-export directory nil 0)))
            (should (equal (claude-archive--latest-export directory) own))))
      (delete-directory directory t))))

(ert-deftest claude-archive-test-current-branch ()
  "The branch ending at the most recent leaf is kept, root first."
  (pcase-let ((`(,branch . ,leaves)
               (claude-archive--current-branch
                (gethash "chat_messages" claude-archive-test--branched))))
    (should (equal (mapcar (lambda (message) (gethash "uuid" message)) branch)
                   '("m1" "m3")))
    (should (= leaves 2))))

(ert-deftest claude-archive-test-current-branch-empty ()
  "A conversation without messages has an empty branch."
  (should (equal (claude-archive--current-branch nil) '(nil . 0))))

(ert-deftest claude-archive-test-normalize-conversation ()
  "An untitled conversation is named after its first prompt."
  (let ((conversation (claude-archive--normalize-conversation
                       claude-archive-test--branched)))
    (should (equal (claude-archive--conversation-title conversation)
                   "First question about Tuscany"))
    (should (= (length (claude-archive--conversation-messages conversation))
               2))
    (should (= (claude-archive--conversation-branches conversation) 2))
    (should (equal (claude-archive--conversation-ended conversation)
                   (date-to-time "2025-08-27T06:25:00Z")))))

(ert-deftest claude-archive-test-title ()
  "A name wins over the first prompt, which is shortened to one line."
  (let ((messages (list (claude-archive--message-create
                         :sender 'human
                         :blocks (list (list :type 'text
                                             :text "line one\nline two"))))))
    (should (equal (claude-archive--title " Named " messages) "Named"))
    (should (equal (claude-archive--title "" messages)
                   "line one line two"))
    (should (equal (claude-archive--title nil nil) "(untitled)"))
    (let ((claude-archive-title-length 6))
      (should (equal (claude-archive--title "" messages) "line …")))))

(ert-deftest claude-archive-test-normalize-blocks ()
  "Content blocks become plists; bookkeeping blocks are dropped."
  (let ((message
         (claude-archive--normalize-message
          (claude-archive-test--json
           "{\"sender\":\"assistant\",\"created_at\":\"2025-08-27T06:19:24Z\",
             \"attachments\":[{\"file_name\":\"\",\"extracted_content\":\"pasted\"}],
             \"files\":[{\"file_name\":\"photo.jpeg\"},{\"file_name\":\"\"}],
             \"content\":[
               {\"type\":\"thinking\",\"thinking\":\"hmm\"},
               {\"type\":\"tool_use\",\"name\":\"web_search\",
                \"input\":{\"query\":\"tuscany trains\"}},
               {\"type\":\"tool_result\",\"name\":\"web_search\",\"is_error\":false,
                \"content\":[{\"type\":\"knowledge\",\"text\":\"result\"}]},
               {\"type\":\"token_budget\"},
               {\"type\":\"text\",\"text\":\"  \"},
               {\"type\":\"text\",\"text\":\" Answer\"}]}"))))
    (should (eq (claude-archive--message-sender message) 'assistant))
    (should (equal (claude-archive--message-blocks message)
                   (list '(:type attachment :name "pasted text" :text "pasted")
                         '(:type file :name "photo.jpeg")
                         '(:type thinking :text "hmm")
                         (list :type 'tool-use :name "web_search"
                               :summary "tuscany trains"
                               :input (plist-get
                                       (nth 3 (claude-archive--message-blocks
                                               message))
                                       :input))
                         '(:type tool-result :name "web_search" :error nil
                                 :text "result")
                         '(:type text :text "Answer"))))))

(ert-deftest claude-archive-test-tool-summary ()
  "A tool call is summarized by its most telling string argument."
  (should (equal (claude-archive--tool-summary
                  (claude-archive-test--json
                   "{\"id\":\"x\",\"url\":\"https://example.com\"}"))
                 "https://example.com"))
  (should (equal (claude-archive--tool-summary
                  (claude-archive-test--json "{\"n\":1,\"other\":\"a\\nb\"}"))
                 "a b"))
  (should (equal (claude-archive--tool-summary (make-hash-table)) "")))

(ert-deftest claude-archive-test-parse-query ()
  "Queries split on spaces into literal regexps, keeping quoted phrases."
  (should (equal (claude-archive--parse-query "Tuscany  \"Car Rental\" a.b")
                 '("Tuscany" "Car Rental" "a\\.b")))
  (should-not (claude-archive--parse-query "   ")))

(ert-deftest claude-archive-test-match-count ()
  "Every term must occur; the count adds up all occurrences."
  (let ((conversation (claude-archive--normalize-conversation
                       claude-archive-test--branched)))
    (should (= (claude-archive--match-count conversation '("answer")) 1))
    (should (= (claude-archive--match-count
                conversation '("tuscany" "regenerated"))
               3))
    (should-not (claude-archive--match-count conversation '("old")))
    (should-not (claude-archive--match-count
                 conversation '("tuscany" "paris")))))

(ert-deftest claude-archive-test-fence ()
  "A fence is longer than any backtick run in its content."
  (should (equal (claude-archive--fence "a\n" "json")
                 "```json\na\n```\n"))
  (should (equal (claude-archive--fence "x ```` y" nil)
                 "`````\nx ```` y\n`````\n")))

(ert-deftest claude-archive-test-render-block-verbosity ()
  "Thinking and tool results only render when verbose."
  (let ((thinking '(:type thinking :text "hmm"))
        (result '(:type tool-result :name "web_search" :error t :text ""))
        (attachment '(:type attachment :name "notes.txt" :text "abc")))
    (should-not (claude-archive--render-block thinking nil))
    (should (equal (claude-archive--render-block thinking t)
                   "> *Thinking*\n> \n> hmm\n"))
    (should-not (claude-archive--render-block result nil))
    (should (equal (claude-archive--render-block result t)
                   "> ↳ web_search (error)\n"))
    (should (equal (claude-archive--render-block attachment nil)
                   "📎 *notes.txt* (3 chars)\n"))
    (should (equal (claude-archive--render-block attachment t)
                   "📎 *notes.txt* (3 chars)\n\n```\nabc\n```\n"))))

(ert-deftest claude-archive-test-render ()
  "Each message renders under a heading; branches are announced."
  (let ((rendered (claude-archive--render
                   (claude-archive--normalize-conversation
                    claude-archive-test--branched)
                   nil)))
    (should (string-prefix-p "> This conversation has 2 branches" rendered))
    (should (string-match-p "^# Human · .*\n\nFirst question about Tuscany\n"
                            rendered))
    (should (string-match-p "^# Claude · .*\n\nRegenerated answer\n" rendered))
    (should-not (string-search "Old answer" rendered))))

(ert-deftest claude-archive-test-view-highlights ()
  "A conversation opened with terms highlights them and starts at the first."
  (let* ((conversation (claude-archive--normalize-conversation
                        claude-archive-test--branched))
         (buffer (claude-archive--view-buffer conversation '("regenerated"))))
    (unwind-protect
        (with-current-buffer buffer
          (should buffer-read-only)
          (should (looking-at "Regenerated"))
          (should (= 1 (length (seq-filter
                                (lambda (overlay)
                                  (overlay-get overlay 'claude-archive-match))
                                (overlays-in (point-min) (point-max)))))))
      (kill-buffer buffer))))

(ert-deftest claude-archive-test-view-message-navigation ()
  "Message navigation skips headings inside a message."
  (let* ((conversation (claude-archive--normalize-conversation
                        (claude-archive-test--conversation
                         "Named"
                         (claude-archive-test--message
                          "m1" claude-archive-test--root "human"
                          "2025-08-27T06:19:24Z" "Question")
                         (claude-archive-test--message
                          "m2" "m1" "assistant" "2025-08-27T06:19:30Z"
                          "# Not a message\n\nAnswer"))))
         (buffer (claude-archive--view-buffer conversation nil)))
    (unwind-protect
        (with-current-buffer buffer
          (claude-archive-view-next-message 1)
          (should (looking-at "# Claude · "))
          (claude-archive-view-next-message 1)
          (should (eobp))
          (claude-archive-view-previous-message 1)
          (should (looking-at "# Claude · "))
          (claude-archive-view-previous-message 1)
          (should (looking-at "# Human · ")))
      (kill-buffer buffer))))

(ert-deftest claude-archive-test-count ()
  "Matches are counted ignoring case; empty matches do not loop."
  (should (= (claude-archive--count "ab" "Ab ab aB") 3))
  (should (= (claude-archive--count "a[0-9]+" "a1 a22 b3") 2))
  (should (= (claude-archive--count "x*" "ab") 2))
  (should (= (claude-archive--count "z" "") 0)))

(ert-deftest claude-archive-test-check-regexp ()
  "Invalid and empty regexps are user errors."
  (should (equal (claude-archive--check-regexp "a.*b") "a.*b"))
  (should-error (claude-archive--check-regexp "a\\(") :type 'user-error)
  (should-error (claude-archive--check-regexp "") :type 'user-error))

(ert-deftest claude-archive-test-org-link ()
  "An Org link points at claude.ai and drops brackets from the title."
  (let ((conversation (claude-archive--conversation-create
                       :uuid "c1" :title "Notes [draft]")))
    (should (equal (claude-archive--url conversation)
                   "https://claude.ai/chat/c1"))
    (should (equal (claude-archive--org-link conversation)
                   "[[https://claude.ai/chat/c1][Notes draft]]"))))

(ert-deftest claude-archive-test-view-highlights-regexp ()
  "Regexps are highlighted; match navigation stops at the last match."
  (let* ((conversation (claude-archive--normalize-conversation
                        claude-archive-test--branched))
         (buffer (claude-archive--view-buffer conversation '("[a-z]+ answer"))))
    (unwind-protect
        (with-current-buffer buffer
          (should (looking-at "Regenerated answer"))
          (goto-char (point-min))
          (claude-archive-view-next-match)
          (should (looking-at "Regenerated answer"))
          (claude-archive-view-next-match)
          (should (looking-at "Regenerated answer"))
          (goto-char (point-max))
          (claude-archive-view-previous-match)
          (should (looking-at "Regenerated answer")))
      (kill-buffer buffer))))

(provide 'claude-archive-test)
;;; claude-archive-test.el ends here

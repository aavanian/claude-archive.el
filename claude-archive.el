;;; claude-archive.el --- Browse claude.ai conversation exports -*- lexical-binding: t -*-

;; Copyright (C) 2026 Alexandre Avanian

;; Author: Alexandre Avanian
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.2") (markdown-mode "2.5"))
;; Keywords: convenience, outlines
;; URL: https://github.com/aavanian/claude-archive.el

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Browse the conversations of a claude.ai data export.
;;
;; `claude-archive' lists every conversation of the most recent export
;; under `claude-archive-directory', with its title, start and end
;; dates.  `claude-archive-search' lists only the conversations
;; containing all the given keywords, `claude-archive-search-regexp'
;; those matching a regular expression.  Opening a conversation renders
;; it read-only in another window, matches highlighted when it was
;; opened from a search.
;;
;; A conversation edited or regenerated on claude.ai keeps every
;; branch in the export; only the most recent branch is shown.
;;
;; claude-archive-consult.el adds consult commands with live preview.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'text-property-search)
(require 'markdown-mode)

(defgroup claude-archive nil
  "Browse claude.ai conversation exports."
  :group 'applications
  :prefix "claude-archive-")

(defcustom claude-archive-directory nil
  "Directory holding claude.ai exports.
It is either an unpacked export, holding `conversations.json', or a
directory of unpacked exports, one per subdirectory.  The most
recently modified `conversations.json' is read."
  :type '(choice (const :tag "Unset" nil) directory))

(defcustom claude-archive-title-length 80
  "Maximum length of a title derived from a conversation's first prompt."
  :type 'natnum)

(defcustom claude-archive-tool-result-length 1500
  "Maximum length of a tool result shown in verbose rendering."
  :type 'natnum)

(defface claude-archive-match
  '((t :inherit match))
  "Face highlighting search matches in a rendered conversation.")

(defvar claude-archive--json-args
  '(:object-type hash-table :array-type list
    :null-object nil :false-object nil)
  "Arguments to `json-parse-buffer' for reading an export.")

(defvar claude-archive--cache nil
  "Loaded export, as (FILE MODIFICATION-TIME . CONVERSATIONS).")

(cl-defstruct (claude-archive--conversation
               (:constructor claude-archive--conversation-create)
               (:copier nil))
  "A conversation of the export, reduced to its most recent branch."
  uuid title created ended messages branches haystack)

(cl-defstruct (claude-archive--message
               (:constructor claude-archive--message-create)
               (:copier nil))
  "A message: its SENDER (`human' or `assistant'), CREATED time and BLOCKS.
Each block is a plist whose :type is one of `text', `thinking',
`tool-use', `tool-result', `attachment' or `file'."
  sender created blocks)

;;;; Reading the export

(defun claude-archive--latest-export (directory)
  "Return the most recently modified conversations file in DIRECTORY.
The file is either DIRECTORY's own `conversations.json' or that of
one of its subdirectories.  Return nil when there is none."
  (let ((files (append
                (file-expand-wildcards
                 (expand-file-name "conversations.json" directory))
                (file-expand-wildcards
                 (expand-file-name "*/conversations.json" directory)))))
    (car (sort files #'file-newer-than-file-p))))

(defun claude-archive--parse-time (iso)
  "Return the Lisp time of ISO, an ISO 8601 string, or nil."
  (when (and (stringp iso) (not (string-empty-p iso)))
    (ignore-errors (date-to-time iso))))

(defun claude-archive--format-time (time)
  "Format TIME as a local date and time, or an empty string if nil."
  (if time (format-time-string "%Y-%m-%d %H:%M" time) ""))

(defun claude-archive--one-line (string length)
  "Return STRING collapsed to one line and truncated to LENGTH."
  (truncate-string-to-width
   (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " string))
   length nil nil "…"))

(defun claude-archive--current-branch (messages)
  "Return the most recent branch of MESSAGES and their leaf count.
MESSAGES are the raw message tables of a conversation.  The value
is (BRANCH . LEAVES), BRANCH ordered from the first message to the
most recently created leaf."
  (let ((by-uuid (make-hash-table :test #'equal))
        (parents (make-hash-table :test #'equal)))
    (dolist (message messages)
      (puthash (gethash "uuid" message) message by-uuid)
      (puthash (gethash "parent_message_uuid" message) t parents))
    (let ((leaves (seq-remove (lambda (message)
                                (gethash (gethash "uuid" message) parents))
                              messages))
          (branch nil))
      (when-let* ((leaf (car (last (sort (copy-sequence leaves)
                                         (lambda (a b)
                                           (string< (gethash "created_at" a)
                                                    (gethash "created_at" b))))))))
        (let ((seen (make-hash-table :test #'equal))
              (message leaf))
          (while (and message (not (gethash (gethash "uuid" message) seen)))
            (puthash (gethash "uuid" message) t seen)
            (push message branch)
            (setq message (gethash (gethash "parent_message_uuid" message)
                                   by-uuid)))))
      (cons branch (length leaves)))))

(defun claude-archive--tool-summary (input)
  "Return a one-line summary of INPUT, a tool call's argument table."
  (if-let* ((value (or (seq-some (lambda (key)
                                   (let ((value (gethash key input)))
                                     (and (stringp value) value)))
                                 '("query" "url" "title" "description"
                                   "command" "path"))
                       (seq-find #'stringp (hash-table-values input)))))
      (claude-archive--one-line value 100)
    ""))

(defun claude-archive--tool-result-text (content)
  "Return the text of CONTENT, a tool result's item list."
  (if (stringp content)
      content
    (mapconcat #'identity
               (delq nil (mapcar (lambda (item)
                                   (and (hash-table-p item)
                                        (stringp (gethash "text" item))
                                        (gethash "text" item)))
                                 content))
               "\n")))

(defun claude-archive--normalize-block (block)
  "Return BLOCK, a raw content block, as a block plist, or nil."
  (pcase (gethash "type" block)
    ("text"
     (let ((text (string-trim (or (gethash "text" block) ""))))
       (unless (string-empty-p text)
         (list :type 'text :text text))))
    ("thinking"
     (let ((text (string-trim (or (gethash "thinking" block) ""))))
       (unless (string-empty-p text)
         (list :type 'thinking :text text))))
    ("tool_use"
     (let ((input (gethash "input" block)))
       (list :type 'tool-use
             :name (gethash "name" block)
             :summary (claude-archive--tool-summary
                       (if (hash-table-p input) input (make-hash-table)))
             :input input)))
    ("tool_result"
     (list :type 'tool-result
           :name (gethash "name" block)
           :error (gethash "is_error" block)
           :text (claude-archive--tool-result-text
                  (gethash "content" block))))
    ((or "image" "document")
     (list :type 'file
           :name (or (gethash "title" block) (gethash "type" block))))))

(defun claude-archive--normalize-attachment (attachment)
  "Return ATTACHMENT, pasted or uploaded text, as a block plist."
  (let ((name (gethash "file_name" attachment)))
    (list :type 'attachment
          :name (if (string-empty-p (or name "")) "pasted text" name)
          :text (or (gethash "extracted_content" attachment) ""))))

(defun claude-archive--normalize-file (file)
  "Return FILE, an uploaded file reference, as a block plist, or nil.
A file without a name stands for pasted text, which the message's
attachments already carry."
  (let ((name (gethash "file_name" file)))
    (unless (string-empty-p (or name ""))
      (list :type 'file :name name))))

(defun claude-archive--normalize-message (message)
  "Return MESSAGE, a raw message table, as a message struct.
Content blocks that `claude-archive--normalize-block' drops are
left out; attachments and files come before the content."
  (claude-archive--message-create
   :sender (if (equal (gethash "sender" message) "human") 'human 'assistant)
   :created (claude-archive--parse-time (gethash "created_at" message))
   :blocks (append
            (mapcar #'claude-archive--normalize-attachment
                    (gethash "attachments" message))
            (delq nil (mapcar #'claude-archive--normalize-file
                              (gethash "files" message)))
            (delq nil (mapcar #'claude-archive--normalize-block
                              (gethash "content" message))))))

(defun claude-archive--title (name messages)
  "Return NAME, or a title derived from MESSAGES when NAME is empty.
The derived title is the start of the first prompt's text."
  (if (not (string-empty-p (string-trim (or name ""))))
      (string-trim name)
    (if-let* ((text (seq-some
                     (lambda (message)
                       (and (eq (claude-archive--message-sender message) 'human)
                            (seq-some (lambda (block)
                                        (and (eq (plist-get block :type) 'text)
                                             (plist-get block :text)))
                                      (claude-archive--message-blocks message))))
                     messages)))
        (claude-archive--one-line text claude-archive-title-length)
      "(untitled)")))

(defun claude-archive--normalize-conversation (conversation)
  "Return CONVERSATION, a raw conversation table, as a conversation struct."
  (pcase-let* ((`(,branch . ,leaves)
                (claude-archive--current-branch
                 (gethash "chat_messages" conversation)))
               (messages (mapcar #'claude-archive--normalize-message branch))
               (created (claude-archive--parse-time
                         (gethash "created_at" conversation))))
    (claude-archive--conversation-create
     :uuid (gethash "uuid" conversation)
     :title (claude-archive--title (gethash "name" conversation) messages)
     :created created
     :ended (or (and messages
                     (claude-archive--message-created (car (last messages))))
                (claude-archive--parse-time (gethash "updated_at" conversation))
                created)
     :messages messages
     :branches leaves)))

(defun claude-archive--read (file)
  "Return the conversations of FILE, most recently ended first."
  (with-temp-buffer
    (insert-file-contents file)
    (sort (mapcar #'claude-archive--normalize-conversation
                  (apply #'json-parse-buffer claude-archive--json-args))
          (lambda (a b)
            (time-less-p (claude-archive--conversation-ended b)
                         (claude-archive--conversation-ended a))))))

(defun claude-archive--conversations (&optional force)
  "Return the conversations of the most recent export.
The export is read again when it changed since it was cached, or
when FORCE is non-nil."
  (unless claude-archive-directory
    (user-error "Set `claude-archive-directory' to your claude.ai exports"))
  (let* ((file (or (claude-archive--latest-export claude-archive-directory)
                   (user-error "No conversations.json in %s"
                               claude-archive-directory)))
         (modified (file-attribute-modification-time (file-attributes file))))
    (unless (and (not force)
                 (equal (car claude-archive--cache) file)
                 (equal (cadr claude-archive--cache) modified))
      (message "Reading %s..." file)
      (setq claude-archive--cache
            (cons file (cons modified (claude-archive--read file))))
      (message "Reading %s...done" file))
    (cddr claude-archive--cache)))

;;;; Searching

(defun claude-archive--parse-query (query)
  "Return the regexps matching the search terms of QUERY.
Terms are separated by spaces; double quotes group a phrase."
  (mapcar #'regexp-quote
          (seq-remove #'string-empty-p (split-string-and-unquote query))))

(defun claude-archive--check-regexp (regexp)
  "Return REGEXP, or signal a user error if it is invalid or empty."
  (when (string-empty-p regexp)
    (user-error "Empty search"))
  (condition-case err
      (progn (string-match-p regexp "") regexp)
    (invalid-regexp (user-error "Invalid regexp: %s" (cadr err)))))

(defun claude-archive--haystack (conversation)
  "Return the searchable text of CONVERSATION.
It covers the title, the messages' text and the attachments' text,
and is computed once."
  (or (claude-archive--conversation-haystack conversation)
      (setf (claude-archive--conversation-haystack conversation)
            (mapconcat
             #'identity
             (cons (claude-archive--conversation-title conversation)
                   (mapcan (lambda (message)
                             (mapcan (lambda (block)
                                       (and (memq (plist-get block :type)
                                                  '(text attachment))
                                            (list (plist-get block :text))))
                                     (claude-archive--message-blocks message)))
                           (claude-archive--conversation-messages conversation)))
             "\n"))))

(defun claude-archive--count (regexp haystack)
  "Return the number of matches of REGEXP in HAYSTACK, ignoring case."
  (let ((case-fold-search t)
        (count 0)
        (start 0))
    (while (and (< start (length haystack))
                (string-match regexp haystack start))
      (setq count (1+ count)
            start (max (match-end 0) (1+ (match-beginning 0)))))
    count))

(defun claude-archive--match-count (conversation regexps)
  "Return how often REGEXPS match in CONVERSATION, or nil.
Nil means at least one of REGEXPS does not match."
  (let ((haystack (claude-archive--haystack conversation))
        (total 0))
    (and (seq-every-p (lambda (regexp)
                        (let ((count (claude-archive--count regexp haystack)))
                          (setq total (+ total count))
                          (> count 0)))
                      regexps)
         total)))

;;;; Rendering

(defun claude-archive--fence (content language)
  "Return CONTENT as a fenced code block tagged LANGUAGE.
The fence is longer than any backtick run inside CONTENT."
  (let ((longest 2)
        (start 0))
    (while (string-match "`+" content start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (let ((fence (make-string (1+ longest) ?`)))
      (format "%s%s\n%s\n%s\n" fence (or language "") (string-trim-right content)
              fence))))

(defun claude-archive--quote (text)
  "Return TEXT as a markdown block quote."
  (concat (replace-regexp-in-string "^" "> " (string-trim text)) "\n"))

(defun claude-archive--render-block (block verbose)
  "Return BLOCK rendered as markdown, or nil when hidden.
Thinking, tool results and attachments' text are only shown when
VERBOSE is non-nil."
  (pcase (plist-get block :type)
    ('text (concat (plist-get block :text) "\n"))
    ('thinking
     (when verbose
       (claude-archive--quote (concat "*Thinking*\n\n" (plist-get block :text)))))
    ('tool-use
     (concat (format "> 🔧 **%s** %s\n" (plist-get block :name)
                     (plist-get block :summary))
             (when (and verbose (plist-get block :input))
               (concat "\n" (claude-archive--fence
                             (let ((json-encoding-pretty-print t))
                               (json-encode (plist-get block :input)))
                             "json")))))
    ('tool-result
     (when verbose
       (let ((text (plist-get block :text)))
         (concat (format "> ↳ %s%s\n" (plist-get block :name)
                         (if (plist-get block :error) " (error)" ""))
                 (unless (string-empty-p (string-trim text))
                   (concat "\n" (claude-archive--fence
                                 (truncate-string-to-width
                                  text claude-archive-tool-result-length
                                  nil nil "…")
                                 nil)))))))
    ('attachment
     (concat (format "📎 *%s* (%d chars)\n" (plist-get block :name)
                     (length (plist-get block :text)))
             (when verbose
               (concat "\n" (claude-archive--fence (plist-get block :text) nil)))))
    ('file (format "📎 *%s*\n" (plist-get block :name)))))

(defun claude-archive--render-message (message verbose)
  "Return MESSAGE rendered as markdown under a level-one heading.
The heading carries the `claude-archive-message' property, telling
it apart from headings inside the message.  VERBOSE is passed to
`claude-archive--render-block'."
  (concat
   (propertize (format "# %s · %s"
                       (if (eq (claude-archive--message-sender message) 'human)
                           "Human" "Claude")
                       (claude-archive--format-time
                        (claude-archive--message-created message)))
               'claude-archive-message t)
   "\n\n"
   (mapconcat #'identity
              (delq nil (mapcar (lambda (block)
                                  (claude-archive--render-block block verbose))
                                (claude-archive--message-blocks message)))
              "\n")
   "\n"))

(defun claude-archive--render (conversation verbose)
  "Return CONVERSATION rendered as markdown.
VERBOSE is passed to `claude-archive--render-block'."
  (concat
   (let ((branches (claude-archive--conversation-branches conversation)))
     (when (> branches 1)
       (format "> This conversation has %d branches; showing the most recent.\n\n"
               branches)))
   (mapconcat (lambda (message)
                (claude-archive--render-message message verbose))
              (claude-archive--conversation-messages conversation)
              "")))

;;;; Links

(defun claude-archive--url (conversation)
  "Return the claude.ai address of CONVERSATION."
  (concat "https://claude.ai/chat/" (claude-archive--conversation-uuid conversation)))

(defun claude-archive--org-link (conversation)
  "Return an Org link to CONVERSATION on claude.ai, described by its title."
  (format "[[%s][%s]]" (claude-archive--url conversation)
          (replace-regexp-in-string
           "[][]" "" (claude-archive--conversation-title conversation))))

(defvar-local claude-archive--viewed nil
  "The conversation shown in this buffer.")

(defun claude-archive--current ()
  "Return the conversation at point in a list or view buffer."
  (or claude-archive--viewed
      (tabulated-list-get-id)
      (user-error "No conversation here")))

(defun claude-archive-copy-url ()
  "Copy the claude.ai address of the conversation at point."
  (interactive)
  (let ((url (claude-archive--url (claude-archive--current))))
    (kill-new url)
    (message "Copied %s" url)))

(defun claude-archive-copy-org-link ()
  "Copy an Org link to the conversation at point on claude.ai."
  (interactive)
  (let ((link (claude-archive--org-link (claude-archive--current))))
    (kill-new link)
    (message "Copied %s" link)))

(defun claude-archive-browse ()
  "Open the conversation at point on claude.ai."
  (interactive)
  (browse-url (claude-archive--url (claude-archive--current))))

;;;; Viewing a conversation

(defvar-local claude-archive--terms nil
  "Search regexps highlighted in this buffer.")

(defvar-local claude-archive--verbose nil
  "Whether this buffer shows thinking, tool results and attachments.")

(defun claude-archive--header-line (conversation)
  "Return the header line describing CONVERSATION."
  (format " %s   %s → %s   %d messages"
          (claude-archive--conversation-title conversation)
          (claude-archive--format-time
           (claude-archive--conversation-created conversation))
          (claude-archive--format-time
           (claude-archive--conversation-ended conversation))
          (length (claude-archive--conversation-messages conversation))))

(defun claude-archive--highlight (regexps)
  "Highlight REGEXPS in the current buffer and return the first match position."
  (remove-overlays nil nil 'claude-archive-match t)
  (let ((case-fold-search t)
        (first nil))
    (save-excursion
      (dolist (regexp regexps)
        (goto-char (point-min))
        (while (and (not (eobp)) (re-search-forward regexp nil t))
          (if (= (match-beginning 0) (match-end 0))
              (forward-char 1)
            (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
              (overlay-put overlay 'claude-archive-match t)
              (overlay-put overlay 'face 'claude-archive-match)
              (setq first (min (or first (match-beginning 0))
                               (match-beginning 0))))))))
    first))

(defun claude-archive--fill-view ()
  "Render the current buffer's conversation and highlight its terms."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (claude-archive--render claude-archive--viewed claude-archive--verbose))
    (goto-char (point-min))
    (if-let* ((first (claude-archive--highlight claude-archive--terms)))
        (goto-char first)
      (when (and claude-archive--terms (not claude-archive--verbose))
        (message "Matches only in hidden content; press v to show it")))))

(defun claude-archive-view-toggle-verbose ()
  "Show or hide thinking, tool results and attachments' text."
  (interactive)
  (setq claude-archive--verbose (not claude-archive--verbose))
  (let ((line (line-number-at-pos)))
    (claude-archive--fill-view)
    (unless claude-archive--terms
      (goto-char (point-min))
      (forward-line (1- line))))
  (message "Verbose %s" (if claude-archive--verbose "on" "off")))

(defun claude-archive-view-next-message (count)
  "Move to the heading of the COUNTth next message."
  (interactive "p")
  (end-of-line)
  (let ((match nil))
    (dotimes (_ count)
      (setq match (or (text-property-search-forward 'claude-archive-message t t)
                      match)))
    (if match
        (goto-char (prop-match-beginning match))
      (goto-char (point-max)))))

(defun claude-archive-view-previous-message (count)
  "Move to the heading of the COUNTth previous message."
  (interactive "p")
  (beginning-of-line)
  (let ((match nil))
    (dotimes (_ count)
      (setq match (or (text-property-search-backward 'claude-archive-message t t)
                      match)))
    (if match
        (goto-char (prop-match-beginning match))
      (goto-char (point-min)))))

(defun claude-archive--match-starts ()
  "Return the sorted start positions of the highlighted matches."
  (sort (mapcar #'overlay-start
                (seq-filter (lambda (overlay)
                              (overlay-get overlay 'claude-archive-match))
                            (overlays-in (point-min) (point-max))))
        #'<))

(defun claude-archive-view-next-match ()
  "Move to the next highlighted match."
  (interactive)
  (if-let* ((next (seq-find (lambda (start) (> start (point)))
                            (claude-archive--match-starts))))
      (goto-char next)
    (message "No further match")))

(defun claude-archive-view-previous-match ()
  "Move to the previous highlighted match."
  (interactive)
  (if-let* ((previous (car (last (seq-filter (lambda (start) (< start (point)))
                                             (claude-archive--match-starts))))))
      (goto-char previous)
    (message "No previous match")))

(defvar-keymap claude-archive-view-mode-map
  :doc "Keymap for `claude-archive-view-mode'."
  "q" #'quit-window
  "n" #'claude-archive-view-next-message
  "p" #'claude-archive-view-previous-message
  "]" #'claude-archive-view-next-match
  "[" #'claude-archive-view-previous-match
  "v" #'claude-archive-view-toggle-verbose
  "w" #'claude-archive-copy-url
  "W" #'claude-archive-copy-org-link
  "b" #'claude-archive-browse
  "SPC" #'scroll-up-command
  "S-SPC" #'scroll-down-command
  "DEL" #'scroll-down-command)

(define-derived-mode claude-archive-view-mode gfm-mode "Claude-Conv"
  "Read-only view of a claude.ai conversation.
Each message is a level-one heading, so outline commands fold them.

\\{claude-archive-view-mode-map}"
  (setq buffer-read-only t)
  (visual-line-mode 1))

(defun claude-archive--view-buffer-name (conversation)
  "Return the name of the buffer rendering CONVERSATION."
  (format "*Claude: %s*"
          (claude-archive--one-line
           (claude-archive--conversation-title conversation) 50)))

(defun claude-archive--view-buffer (conversation regexps)
  "Return a buffer rendering CONVERSATION with REGEXPS highlighted."
  (let ((buffer (get-buffer-create (claude-archive--view-buffer-name conversation))))
    (with-current-buffer buffer
      (unless (and (eq claude-archive--viewed conversation)
                   (equal claude-archive--terms regexps))
        (unless (derived-mode-p 'claude-archive-view-mode)
          (claude-archive-view-mode))
        (setq claude-archive--viewed conversation
              claude-archive--terms regexps
              header-line-format (claude-archive--header-line conversation))
        (claude-archive--fill-view)))
    buffer))

;;;; Listing conversations

(defvar-local claude-archive--query nil
  "Search regexps restricting this list, or nil for every conversation.")

(defvar-local claude-archive--query-label nil
  "Description of `claude-archive--query' for the buffer name.")

(defun claude-archive--entry (conversation hits)
  "Return the tabulated list entry of CONVERSATION.
HITS is its match count, or nil in an unrestricted list."
  (list conversation
        (vconcat
         (list (claude-archive--format-time
                (claude-archive--conversation-created conversation))
               (claude-archive--format-time
                (claude-archive--conversation-ended conversation))
               (number-to-string
                (length (claude-archive--conversation-messages conversation))))
         (and hits (list (number-to-string hits)))
         (list (claude-archive--conversation-title conversation)))))

(defun claude-archive--numeric-sorter (column)
  "Return a predicate sorting entries by the number in COLUMN."
  (lambda (a b)
    (< (string-to-number (aref (cadr a) column))
       (string-to-number (aref (cadr b) column)))))

(defun claude-archive--refresh ()
  "Recompute the entries of the current list buffer."
  (let ((regexps claude-archive--query)
        (entries nil))
    (dolist (conversation (claude-archive--conversations))
      (if (null regexps)
          (push (claude-archive--entry conversation nil) entries)
        (when-let* ((hits (claude-archive--match-count conversation regexps)))
          (push (claude-archive--entry conversation hits) entries))))
    (setq tabulated-list-format
          (vconcat
           (list '("Started" 16 t) '("Ended" 16 t)
                 (list "Msgs" 5 (claude-archive--numeric-sorter 2)
                       :right-align t :pad-right 2))
           (and regexps
                (list (list "Hits" 5 (claude-archive--numeric-sorter 3)
                            :right-align t :pad-right 2)))
           (list '("Title" 0 t)))
          tabulated-list-entries (nreverse entries))
    (tabulated-list-init-header)))

(defun claude-archive-list-open ()
  "Show the conversation at point in another window and select it."
  (interactive)
  (pop-to-buffer (claude-archive--view-buffer (claude-archive--current)
                                              claude-archive--query)
                 t))

(defun claude-archive-list-display ()
  "Show the conversation at point in another window, staying in the list."
  (interactive)
  (display-buffer (claude-archive--view-buffer (claude-archive--current)
                                               claude-archive--query)
                  t))

(defun claude-archive-list-next ()
  "Move to the next conversation and show it."
  (interactive)
  (forward-line 1)
  (when (tabulated-list-get-id)
    (claude-archive-list-display)))

(defun claude-archive-list-previous ()
  "Move to the previous conversation and show it."
  (interactive)
  (forward-line -1)
  (when (tabulated-list-get-id)
    (claude-archive-list-display)))

(defun claude-archive--show-list (regexps label)
  "Pop to a list of the conversations matching all REGEXPS.
LABEL describes REGEXPS in the buffer name.  With REGEXPS nil,
list every conversation."
  (let ((buffer (get-buffer-create
                 (if regexps
                     (format "*Claude archive: %s*" label)
                   "*Claude archive*"))))
    (with-current-buffer buffer
      (claude-archive-list-mode)
      (setq claude-archive--query regexps
            claude-archive--query-label label)
      (claude-archive--refresh)
      (tabulated-list-print)
      (goto-char (point-min))
      (message "%d conversations" (length tabulated-list-entries)))
    (pop-to-buffer-same-window buffer)))

(defun claude-archive-list-narrow (query &optional regexp)
  "Restrict the current list to conversations also matching QUERY.
QUERY is read as search terms, or as a regexp when REGEXP is
non-nil, interactively with a prefix argument."
  (interactive
   (list (read-string (if current-prefix-arg "Narrow to regexp: " "Narrow to: "))
         current-prefix-arg))
  (let ((added (if regexp
                   (list (claude-archive--check-regexp query))
                 (or (claude-archive--parse-query query)
                     (user-error "Empty search")))))
    (claude-archive--show-list
     (append claude-archive--query added)
     (string-join (delq nil (list claude-archive--query-label
                                  (if regexp (format "/%s/" query) query)))
                  " "))))

(defun claude-archive-list-reload ()
  "Read the export again and refresh the list."
  (interactive)
  (claude-archive--conversations t)
  (revert-buffer))

(defvar-keymap claude-archive-list-mode-map
  :doc "Keymap for `claude-archive-list-mode'."
  :parent tabulated-list-mode-map
  "RET" #'claude-archive-list-open
  "o" #'claude-archive-list-open
  "C-o" #'claude-archive-list-display
  "SPC" #'claude-archive-list-display
  "M-n" #'claude-archive-list-next
  "M-p" #'claude-archive-list-previous
  "s" #'claude-archive-search
  "r" #'claude-archive-search-regexp
  "/" #'claude-archive-list-narrow
  "G" #'claude-archive-list-reload
  "w" #'claude-archive-copy-url
  "W" #'claude-archive-copy-org-link
  "b" #'claude-archive-browse)

(define-derived-mode claude-archive-list-mode tabulated-list-mode "Claude-Archive"
  "List of claude.ai conversations from an export.

\\{claude-archive-list-mode-map}"
  (setq tabulated-list-sort-key nil
        tabulated-list-padding 1)
  (add-hook 'tabulated-list-revert-hook #'claude-archive--refresh nil t))

;;;###autoload
(defun claude-archive ()
  "List the conversations of the most recent claude.ai export."
  (interactive)
  (claude-archive--show-list nil nil))

;;;###autoload
(defun claude-archive-search (query)
  "List the conversations containing every term of QUERY.
Terms are separated by spaces, double quotes group a phrase, and
matching ignores case."
  (interactive (list (read-string "Search conversations: ")))
  (if-let* ((regexps (claude-archive--parse-query query)))
      (claude-archive--show-list regexps query)
    (user-error "Empty search")))

;;;###autoload
(defun claude-archive-search-regexp (regexp)
  "List the conversations matching REGEXP, ignoring case."
  (interactive (list (read-regexp "Search conversations for regexp")))
  (claude-archive--show-list (list (claude-archive--check-regexp regexp))
                             (format "/%s/" regexp)))

;;;###autoload
(defun claude-archive-open (conversation)
  "Show CONVERSATION, read with completion on titles."
  (interactive
   (let ((candidates
          (mapcar (lambda (conversation)
                    (cons (format "%s  %s"
                                  (claude-archive--format-time
                                   (claude-archive--conversation-ended conversation))
                                  (claude-archive--conversation-title conversation))
                          conversation))
                  (claude-archive--conversations))))
     (list (cdr (assoc (completing-read
                        "Conversation: "
                        (lambda (string predicate action)
                          (if (eq action 'metadata)
                              '(metadata (display-sort-function . identity))
                            (complete-with-action action candidates
                                                  string predicate)))
                        nil t)
                       candidates)))))
  (pop-to-buffer (claude-archive--view-buffer conversation nil)))

(provide 'claude-archive)
;;; claude-archive.el ends here

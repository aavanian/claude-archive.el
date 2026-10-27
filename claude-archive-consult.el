;;; claude-archive-consult.el --- Consult commands for claude-archive -*- lexical-binding: t -*-

;; Copyright (C) 2026 Alexandre Avanian

;; Author: Alexandre Avanian
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.2") (claude-archive "0.1.0") (consult "2.0"))
;; Keywords: convenience, matching
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

;; Consult commands over claude.ai conversation exports, previewing
;; the conversation under point:
;;
;; - `claude-archive-consult' picks a conversation by title.
;; - `claude-archive-consult-search' searches the conversations as
;;   you type, highlighting the terms in the preview.

;;; Code:

(require 'consult)
(require 'claude-archive)

(defvar claude-archive-consult--history nil
  "History of `claude-archive-consult-search' inputs.")

(defun claude-archive-consult--candidate (conversation &optional hits)
  "Return the completion candidate of CONVERSATION.
HITS is its match count in a search, or nil."
  (propertize (format "%s  %s"
                      (claude-archive--format-time
                       (claude-archive--conversation-ended conversation))
                      (claude-archive--conversation-title conversation))
              'claude-archive-consult--conversation conversation
              'claude-archive-consult--hits hits))

(defun claude-archive-consult--annotate (candidate)
  "Return the annotation of CANDIDATE: its message and match counts."
  (let ((conversation (get-text-property 0 'claude-archive-consult--conversation
                                         candidate))
        (hits (get-text-property 0 'claude-archive-consult--hits candidate)))
    (concat "  "
            (propertize
             (format "%d msgs%s"
                     (length (claude-archive--conversation-messages conversation))
                     (if hits (format ", %d hits" hits) ""))
             'face 'completions-annotations))))

(defun claude-archive-consult--matches (conversations regexps)
  "Return the candidates of CONVERSATIONS matching all REGEXPS."
  (when regexps
    (delq nil (mapcar (lambda (conversation)
                        (when-let* ((hits (claude-archive--match-count
                                           conversation regexps)))
                          (claude-archive-consult--candidate conversation hits)))
                      conversations))))

(defun claude-archive-consult--state (regexps-function)
  "Return a consult state function previewing conversations.
REGEXPS-FUNCTION returns the regexps to highlight in the preview.
Buffers created only for previewing are killed on exit."
  (let ((window (selected-window))
        (original (window-buffer))
        (existing (buffer-list))
        (previews nil))
    (lambda (action conversation)
      (pcase action
        ('preview
         (when (window-live-p window)
           (if conversation
               (let ((buffer (claude-archive--view-buffer
                              conversation (funcall regexps-function))))
                 (unless (memq buffer existing)
                   (cl-pushnew buffer previews))
                 (set-window-buffer window buffer))
             (when (buffer-live-p original)
               (set-window-buffer window original)))))
        ('exit
         (mapc #'kill-buffer (seq-filter #'buffer-live-p previews)))))))

(defun claude-archive-consult--read (prompt table regexps-function &rest options)
  "Read a conversation with PROMPT from TABLE, previewing it.
REGEXPS-FUNCTION is passed to `claude-archive-consult--state'.
OPTIONS are passed to `consult--read'."
  (apply #'consult--read table
         :prompt prompt
         :state (claude-archive-consult--state regexps-function)
         :lookup (apply-partially #'consult--lookup-prop
                                  'claude-archive-consult--conversation)
         :annotate #'claude-archive-consult--annotate
         :require-match t
         :sort nil
         :category 'claude-archive
         options))

;;;###autoload
(defun claude-archive-consult ()
  "Pick a conversation by title, previewing it, and show it."
  (interactive)
  (when-let* ((conversation
               (claude-archive-consult--read
                "Conversation: "
                (mapcar #'claude-archive-consult--candidate
                        (claude-archive--conversations))
                #'ignore)))
    (pop-to-buffer-same-window (claude-archive--view-buffer conversation nil))))

;;;###autoload
(defun claude-archive-consult-search (&optional initial)
  "Search conversations as you type, previewing them, and show the one picked.
The input is read as for `claude-archive-search'.  INITIAL is the
initial input."
  (interactive)
  (let* ((conversations (claude-archive--conversations))
         (regexps nil)
         (conversation
          (claude-archive-consult--read
           "Search conversations: "
           (consult--dynamic-collection
             (lambda (input)
               (setq regexps (claude-archive--parse-query input))
               (claude-archive-consult--matches conversations regexps)))
           (lambda () regexps)
           :initial initial
           :history '(:input claude-archive-consult--history))))
    (when conversation
      (pop-to-buffer-same-window
       (claude-archive--view-buffer conversation regexps)))))

(provide 'claude-archive-consult)
;;; claude-archive-consult.el ends here

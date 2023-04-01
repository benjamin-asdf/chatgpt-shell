;;; chatgpt-shell.el --- Interaction mode for ChatGPT  -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alvaro Ramirez

;; Author: Alvaro Ramirez
;; URL: https://github.com/xenodium/chatgpt-shell
;; Version: 0.3
;; Package-Requires: ((emacs "27.1")
;;                    (markdown-mode "2.5"))

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Note: This is very much a proof of concept (and very rough!).  Much
;; of the code is based on `ielm'.
;;
;; You must set `chatgpt-shell-openai-key' to your key before using.
;;
;; Run `chatgpt-shell' to get a ChatGPT shell.

;;; Code:

(require 'comint)
(require 'map)
(require 'markdown-mode)
(require 'seq)

(eval-when-compile
  (require 'cl-lib)
  (declare-function json-pretty-print "ext:json" (begin end &optional minimize)))

(defcustom chatgpt-shell-openai-key nil
  "OpenAI key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-prompt "ChatGPT> "
  "Prompt text."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-language-mapping '(("elisp" . "emacs-lisp")
                                            ("objective-c" . "objc")
                                            ("cpp" . "c++"))
  "Maps external language names to Emacs names.

Use only lower-case names.

For example:

                  lowercase      Emacs mode (without -mode)
Objective-C -> (\"objective-c\" . \"objc\")"
  :type '()
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-version "gpt-4"
  "The used OpenAI model.

The list of models supported by /v1/chat/completions endpoint is
documented at
https://platform.openai.com/docs/models/model-endpoint-compatibility."
  :type 'string
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-model-temperature nil
  "What sampling temperature to use, between 0 and 2, or nil.

Higher values like 0.8 will make the output more random, while
lower values like 0.2 will make it more focused and
deterministic.  Value of nil will not pass this configuration to
the model.

See
https://platform.openai.com/docs/api-reference/completions\
/create#completions/create-temperature
for details."
  :type '(choice integer
                 (const nil))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-transmitted-context-length nil
  "Controls the amount of context provided to chatGPT.

This context needs to be transmitted to the API on every request.
ChatGPT reads the provided context on every request, which will
consume more and more prompt tokens as your conversation grows.
Models do have a maximum token limit, however.

A value of nil will send full chat history (the full contents of
the comint buffer), to ChatGPT.

A value of 0 will not provide any context.  This is the cheapest
option, but ChatGPT can't look back on your conversation.

A value of 1 will send only the latest prompt-completion pair as
context.

A Value >1 will send that amount of prompt-completion pairs to
ChatGPT."
  :type '(choice integer
                 (const nil))
  :group 'chatgpt-shell)

(defvar-local chatgpt-additional-prompts nil
  "A function returning a list of additional prompts.")

(defvar chatgpt-shell--log-buffer-name "*chatgpt-shell-log*")

(defvar chatgpt-shell--input)

(defvar-local chatgpt-shell--busy nil)

(defvar chatgpt-shell--prompt-internal "ChatGPT> ")

(defvar chatgpt-shell--current-request-id 0)

(defvar chatgpt-shell--show-invisible-markers nil)

(defvaralias 'inferior-chatgpt-mode-map 'chatgpt-shell-map)

(defconst chatgpt-shell-font-lock-keywords
  `(;; Markdown triple backticks source blocks
    ("\\(^\\(```\\)\\([^`\n]*\\)\n\\)\\(\\(?:.\\|\n\\)*?\\)\\(^\\(```\\)$\\)"
     ;; (2) ``` (3) language (4) body (6) ```
     (0 (progn
          ;; Hide ```
          (overlay-put (make-overlay (match-beginning 2)
                                     (match-end 2)) 'invisible t)
          ;; Language box.
          (overlay-put (make-overlay (match-beginning 3)
                                     (match-end 3)) 'face '(:box t))
          ;; Additional newline after language box.
          (overlay-put (make-overlay (match-end 3)
                                     (1+ (match-end 3))) 'display "\n\n")
          ;; Hide ```
          (overlay-put (make-overlay (match-beginning 6)
                                     (match-end 6)) 'invisible t)
          nil)))
    ;; Markdown single backticks
    ("`\\([^`\n]+\\)`"
     (1 'markdown-inline-code-face))))

(defvar chatgpt-shell-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    (define-key map "\C-m" 'chatgpt-shell-return)
    (define-key map "\C-c\C-c" 'chatgpt-shell-interrupt)
    map)
  "Keymap for ChatGPT mode.")

(defalias 'chatgpt-shell-clear-buffer 'comint-clear-buffer)

;;;###autoload
(defun chatgpt-shell (&optional arg)
  "Start a ChatGPT shell."
  (interactive "p")
  (let ((old-buf (current-buffer))
        (old-point)
        (buf-name (if (not (eq arg 1))
                      (generate-new-buffer-name
                       "*chatgpt*")
                    "*chatgpt*")))
    (unless (comint-check-proc buf-name)
      (with-current-buffer
          (get-buffer-create buf-name)
        (setq chatgpt-shell--busy nil)
        (unless (zerop (buffer-size))
          (setq old-point (point)))
        (inferior-chatgpt-mode)
        (when (eq arg 2)
          (cl-pushnew
           (chatgpt-shell--context-buffer-item old-buf)
           chatgpt-shell-contexts))))
    (pop-to-buffer-same-window buf-name)
    (when old-point
      (push-mark old-point))))

(define-derived-mode inferior-chatgpt-mode comint-mode "CHATGPT"
  "Major mode for interactively evaluating ChatGPT prompts.
Uses the interface provided by `comint-mode'"
  (visual-line-mode +1)
  (setq comint-prompt-regexp (concat "^" (regexp-quote chatgpt-shell-prompt)))
  (setq-local paragraph-separate "\\'")
  (setq-local paragraph-start comint-prompt-regexp)
  (setq comint-input-sender 'chatgpt-shell--input-sender)
  (setq comint-process-echoes nil)
  (setq-local chatgpt-shell--prompt-internal chatgpt-shell-prompt)
  (setq-local comint-prompt-read-only t)
  (setq comint-get-old-input 'chatgpt-shell--get-old-input)
  (setq-local comint-completion-addsuffix nil)

  (unless (comint-check-proc (current-buffer))
    (condition-case nil
        (start-process "chatgpt" (current-buffer) "hexl")
      (file-error (start-process "chatgpt" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
    (goto-char (point-max))
    (setq-local comint-inhibit-carriage-motion t)

    (chatgpt-shell--set-pm (point-max))
    (unless comint-use-prompt-regexp
      (let ((inhibit-read-only t))
        (add-text-properties
         (point-min) (point-max)
         '(rear-nonsticky t field output inhibit-line-move-field-capture t))))
    (comint-output-filter (chatgpt-shell--process) chatgpt-shell--prompt-internal)
    (set-marker comint-last-input-start (chatgpt-shell--pm))
    (set-process-filter (get-buffer-process (current-buffer)) 'comint-output-filter))

  (font-lock-add-keywords nil chatgpt-shell-font-lock-keywords))

(defvar-local chatgpt-shell-contexts nil
  "A list of pairs (ITEM . CONTENT-FN) representing context items from files or buffers.")

(defun chatgpt-shell--context-file-messages (contexts)
  "Return a list of context messages based on the provided CONTEXTS."
  (interactive)
  (cl-mapcar (lambda (ctx)
               (list (cons 'role "system")
                     (cons 'content (format "file: %s, content: %s"
                                            (car ctx)
                                            (funcall (cdr ctx))))))
             contexts))

(defun chatgpt-shell--context-buffer-item (buf)
  (cons
   (buffer-name buf)
   (lambda ()
     (when-let ((b (get-buffer buf)))
       (with-current-buffer b (buffer-substring-no-properties
                               (point-min)
                               (point-max)))))))

(defun chatgpt-shell-shell-add-context-file ()
  "Add context items (files or buffers) to ChatGPT."
  (interactive)
  ;; when consult is loaded, else completing-read
  (let*
      ((selected
        (consult--multi
         consult-buffer-sources
         :prompt "Chat-GPT context: "
         :history 'consult--buffer-history
         :state nil
         :require-match t
         :sort nil))
       (item (car selected)))
    (cl-pushnew
     `(,item .
             ,(pcase
                  (plist-get (cdr selected) :name)
                ("File"
                 (lambda ()
                   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
                ("Buffer"
                 (lambda ()
                   (when-let ((b (get-buffer item)))
                     (with-current-buffer b (buffer-substring-no-properties
                                             (point-min)
                                             (point-max))))))
                ;; ""
                ;; "Bookmarks"

                ))
     chatgpt-shell-contexts)))
(defun chatgpt-clear-contexts ()
  (interactive)
  (setf chatgpt-shell-contexts nil))

(defun chatgpt-clear-some-contexts ()
  (interactive)
  (if (not chatgpt-shell-contexts)
      (message "contexts emtpy")
      (let ((to-remove (completing-read-multiple "Remove: " (mapcar #'car chatgpt-shell-contexts)))
            new-list)
        (dolist (context chatgpt-shell-contexts)
          (unless (member (car context) to-remove)
            (push context new-list)))
        (setq chatgpt-shell-contexts (nreverse new-list)))))

(defun chatgpt-shell-return ()
  "RET binding."
  (interactive)
  (chatgpt-shell--send-input))

(defun chatgpt-shell-prompt ()
  "Make a ChatGPT request from the minibuffer."
  (interactive)
  (chatgpt-shell-send-to-buffer (read-string chatgpt-shell-prompt))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-describe-code ()
  "Describe code from region using ChatGPT."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (concat "What does the following code do?\n\n"
           (buffer-substring (region-beginning) (region-end))))
  (chatgpt-shell--send-input))

(defun chatgpt-shell-send-region ()
  "Send region to ChatGPT."
  (interactive)
  (chatgpt-shell-send-to-buffer
   (buffer-substring (region-beginning) (region-end)) nil t))

(defun chatgpt-shell-send-to-buffer (text &optional submit save-excursion)
  "Send TEXT to *chatgpt* buffer.
Set SUBMIT to automatically submit to ChatGPT.
Set SAVE-EXCURSION to prevent point from moving."
  (chatgpt-shell)
  (when chatgpt-shell--busy
    (chatgpt-shell-interrupt))
  (goto-char (point-max))
  (if save-excursion
      (save-excursion
        (insert text))
    (insert text))
  (when submit
    (chatgpt-shell--send-input)))

(defun chatgpt-shell-interrupt ()
  "Interrupt current request."
  (interactive)
  (chatgpt-shell--increment-request-id)
  (comint-send-input)
  (goto-char (point-max))
  (comint-output-filter (chatgpt-shell--process)
                        (concat (propertize "<gpt-end-of-prompt>\n<gpt-ignored-response>"
                                            'invisible (not chatgpt-shell--show-invisible-markers))
                                "\n"
                                chatgpt-shell--prompt-internal))
  (when (and chatpt-shell-stream-process (process-live-p chatpt-shell-stream-process))
    (kill-process chatpt-shell-stream-process))
  (setq chatgpt-shell--busy nil)
  (message "interrupted!"))


(defvar-local chatgpt-shell-dwim-p nil)

(defun chatgpt-shell-dwim-prompt ()
  (list
   (cons 'role "system")
   (cons 'content
         "You are an emacs ai package.
The user asks you to 'do what I mean' (dwim) with the current context.
Output a snippet of elisp that helps the user make progress on their programming task.
Consider that the running emacs program is evaluating your output in a temp buffer.
If your desire is to show the user a message, consider calling
message, or
(with-current-buffer-window nil nil (insert \"message\"))


Example:

Input:

file: fib.clj,  content: ;; fib implementation with tail recursion

User: dwim

Output:

(with-current-buffer
    (find-file-noselect \"fib.clj\")
  (goto-char (point-max))
  (insert
   \"(defn fib-tail-rec [n]
  (loop [current-n 0 a 0 b 1]
    (if (= current-n n)
      a
      (recur (inc current-n) b (+ a b)))))\"))

Input:

User: dwim shell command to list files

Output:

(async-shell-command \"ls\")

Input:

User: dwim open user init

Output:

(find-file user-init-file)

User: dwim greeting function

Output:

(progn
  (defun greeting (name)
    \"Make a greeting with NAME\"
    (message \"Hello, \" name))
  (message \"defined greeting\"))")))

(defun chatgpt-shell--eval-input (input-string)
  "Evaluate the Lisp expression INPUT-STRING, and pretty-print the result."
  (unless chatgpt-shell--busy
    (setq chatgpt-shell--busy t)
    (setq chatgpt-shell-dwim-p
          (string-prefix-p "dwim" (string-trim input-string)))
    (let ((buffer (current-buffer)))
      (cond ((string-equal
              "clear"
              (string-trim input-string))
             (call-interactively
              'comint-clear-buffer)
             (comint-output-filter
              (chatgpt-shell--process)
              chatgpt-shell--prompt-internal)
             (setq chatgpt-shell--busy nil))
            ((not (json-available-p))
             (chatgpt-shell--write-reply
              "Emacs needs to be compiled with --with-json")
             (setq chatgpt-shell--busy nil))
            ((not (chatgpt-shell--curl-version-supported))
             (chatgpt-shell--write-reply
              "You need curl version 7.67 or newer.")
             (setq chatgpt-shell--busy nil))
            ((not chatgpt-shell-openai-key)
             (chatgpt-shell--write-reply
              "Variable `chatgpt-shell-openai-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-openai-key

or

(setq chatgpt-shell-openai-key \"my-key\")"
              t)
             (setq chatgpt-shell--busy nil))
            ((string-empty-p
              (string-trim input-string))
             (comint-output-filter
              (chatgpt-shell--process)
              (concat
               "\n"
               chatgpt-shell--prompt-internal))
             (setq chatgpt-shell--busy nil))
            (t
             ;; For viewing prompt delimiter (used to handle multiline prompts).
             ;; (comint-output-filter (chatgpt-shell--process) "<gpt-end-of-prompt>")
             (comint-output-filter
              (chatgpt-shell--process)
              (propertize
               "<gpt-end-of-prompt>"
               'invisible
               (not chatgpt-shell--show-invisible-markers)))
             (when-let
                 ((key
                   (cond ((stringp
                           chatgpt-shell-openai-key)
                          chatgpt-shell-openai-key)
                         ((functionp
                           chatgpt-shell-openai-key)
                          (condition-case err
                              (funcall
                               chatgpt-shell-openai-key)
                            (error
                             (chatgpt-shell--write-reply
                              (error-message-string err)
                              t)
                             (comint-output-filter
                              (chatgpt-shell--process)
                              (propertize
                               "\n<gpt-ignored-response>"
                               'invisible
                               (not chatgpt-shell--show-invisible-markers)))
                             (setq chatgpt-shell--busy nil)
                             nil))))))
               (chatgpt-shell--write-partial-reply
                "\n")
               (chatgpt-shell--async-shell-command
                (chatgpt-shell--make-request-command-list
                 (vconcat
                  (when chatgpt-additional-prompts
                    (funcall chatgpt-additional-prompts))
                  (when chatgpt-shell-dwim-p
                    (list
                     (chatgpt-shell-dwim-prompt)))
                  (chatgpt-shell--context-file-messages chatgpt-shell-contexts)
                  (last
                   (chatgpt-shell--extract-commands-and-responses)
                   (if (null chatgpt-shell-transmitted-context-length)
                       ;; If variable above is nil, send "full" context
                       2048
                     ;; Send in pairs of prompt and completion by
                     ;; multiplying by 2
                     (1+ (*
                          2
                          chatgpt-shell-transmitted-context-length)))))
                 key)
                (lambda (response)
                  (with-current-buffer buffer
                    (chatgpt-shell--write-partial-reply
                     response)))
                (lambda ()
                  (with-current-buffer buffer
                    (chatgpt-shell--write-prompt)
                    (setq chatgpt-shell--busy nil)
                    (when chatgpt-shell-dwim-p
                      (chatgpt-eval-what-the-assistant-just-said
                       buffer))))
                (lambda (error)
                  (message "error")
                  (with-current-buffer buffer
                    (chatgpt-shell--write-reply error t)
                    (setq chatgpt-shell--busy nil))))))))))

(defun chatgpt-shell-dwim ()
  (interactive)
  (chatgpt-shell--eval-input "dwim"))

(defvar-local chatpt-shell-stream-process nil)
(defun chatgpt-shell--async-shell-command (command filter-callback callback error-callback)
  "Run shell COMMAND asynchronously.
Calls filter-callback for with each chunk string from the stream.
Calls CALLBACK and ERROR-CALLBACK with its output when finished."
  (let* ((output-buffer (generate-new-buffer " *temp*"))
         (last-point nil)
         (filter (defalias 'openai-stream-filter
                   (lambda (process string)
                     (with-temp-buffer
                       (insert string)
                       (goto-char (point-min))
                       (while
                           (when-let*
                               ((data
                                 (ignore-errors
                                   (oai-read-a-data)))
                                (choices
                                 (assoc-default 'choices data))
                                (msg
                                 (or
                                  (car (cl-map
                                        'list
                                        (lambda (d)
                                          (assoc-default
                                           'content
                                           (assoc-default 'delta d)))
                                        choices))
                                  (car (cl-map
                                        'list
                                        (lambda (d)
                                          (assoc-default
                                           'role
                                           (assoc-default 'delta d)))
                                        choices))))
                                (msg (if
                                         (string= "assistant" msg)
                                         "assistant: "
                                       msg)))
                             (funcall filter-callback msg)
                             t)))))))
    (setf chatpt-shell-stream-process
     (make-process
      :name "*openai-api-chat-stream*"
      :buffer output-buffer
      :filter filter
      :command command
      :sentinel (lambda (process _event)
                  (let ((output (with-current-buffer
                                    (process-buffer process)
                                  (buffer-string))))
                    (if (= (process-exit-status process)
                           0)
                        (funcall callback)
                      (funcall error-callback output))
                    (kill-buffer output-buffer)))))))

(defun oai-read-a-data ()
  (when (re-search-forward "data: " nil t)
    (json-read)))

(defun chatgpt-shell--increment-request-id ()
  "Increment `chatgpt-shell--current-request-id'."
  (if (= chatgpt-shell--current-request-id most-positive-fixnum)
      (setq chatgpt-shell--current-request-id 0)
    (setq chatgpt-shell--current-request-id (1+ chatgpt-shell--current-request-id))))

(defun chatgpt-shell--set-pm (pos)
  "Set the process mark in the current buffer to POS."
  (set-marker (process-mark (get-buffer-process (current-buffer))) pos))

(defun chatgpt-shell--pm nil
  "Return the process mark of the current buffer."
  (process-mark (get-buffer-process (current-buffer))))

(defun chatgpt-shell--input-sender (_proc input)
  "Set the variable `chatgpt-shell--input' to INPUT.
Used by `chatgpt-shell--send-input's call."
  (setq chatgpt-shell--input input))

(defun chatgpt-shell--send-input ()
  "Send text after the prompt."
  (interactive)
  (let (chatgpt-shell--input)
    (comint-send-input)
    (chatgpt-shell--eval-input chatgpt-shell--input)))

(defun chatgpt-shell--write-partial-reply (reply)
  "Write REPLY to prompt."
  (comint-output-filter (chatgpt-shell--process)
                        reply))

(defun chatgpt-shell--write-prompt ()
  "Write REPLY to prompt."
  (comint-output-filter (chatgpt-shell--process)
                        (concat "\n\n" chatgpt-shell--prompt-internal)))

(defun chatgpt-shell--write-reply (reply &optional failed)
  "Write REPLY to prompt.  Set FAILED to record failure."
  (comint-output-filter (chatgpt-shell--process)
                        (concat "\n"
                                (string-trim reply)
                                (if failed
                                    (propertize "\n<gpt-ignored-response>"
                                                'invisible (not chatgpt-shell--show-invisible-markers))
                                  "")
                                "\n\n"
                                chatgpt-shell--prompt-internal)))

(defun chatgpt-shell--get-old-input nil
  "Return the previous input surrounding point."
  (save-excursion
    (beginning-of-line)
    (unless (looking-at-p comint-prompt-regexp)
      (re-search-backward comint-prompt-regexp))
    (comint-skip-prompt)
    (buffer-substring (point) (progn (forward-sexp 1) (point)))))

(defun chatgpt-shell--make-request-command-list (messages key)
  "Build ChatGPT curl command list using MESSAGES and KEY."
  (cl-assert chatgpt-shell-openai-key nil "`chatgpt-shell-openai-key' needs to be set with your key")
  (let ((request-data `((model . ,chatgpt-shell-model-version)
                        (messages . ,messages)
                        (stream . t))))
    (when chatgpt-shell-model-temperature
      (push `(temperature . ,chatgpt-shell-model-temperature) request-data))
    (list "curl"
          "https://api.openai.com/v1/chat/completions"
          "--fail" "--no-progress-meter"
          "-H" "Content-Type: application/json"
          "-H" (format "Authorization: Bearer %s" key)
          "-d" (json-serialize request-data))))

(defun chatgpt-shell--curl-version-supported ()
  "Return t if curl version is 7.67 or newer, nil otherwise."
  (let ((curl-version-string (shell-command-to-string "curl --version 2>/dev/null")))
    (when (string-match "\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" curl-version-string)
      (let ((version (match-string 1 curl-version-string)))
        (version<= "7.67" version)))))

(defun chatgpt-shell--json-parse-string (json)
  "Parse JSON and return the parsed data structure, nil otherwise."
  (condition-case nil
      (json-parse-string json :object-type 'alist)
    (json-parse-error nil)))

(defun chatgpt-shell--extract-content (json)
  "Extract ChatGPT response from JSON."
  (when-let (parsed (chatgpt-shell--json-parse-string json))
    (string-trim
     (map-elt (map-elt (seq-first (map-elt parsed 'choices))
                       'message)
              'content))))

(defun chatgpt-shell--extract-commands-and-responses ()
  "Extract all command and responses in buffer."
  (let ((result))
    (mapc (lambda (item)
            (let* ((values (split-string item "<gpt-end-of-prompt>"))
                   (lines (split-string item "\n"))
                   (prompt (string-trim (nth 0 values)))
                   (response (string-trim (progn
                                            (if (> (length values) 1)
                                                (nth 1 values)
                                              (string-join
                                               (cdr lines) "\n"))))))
              (unless (string-match "<gpt-ignored-response>" response)
                (when (not (string-empty-p prompt))
                  (push (list (cons 'role "user")
                              (cons 'content prompt)) result))
                (when (not (string-empty-p response))
                  (push (list (cons 'role "system")
                              (cons 'content response)) result)))))
          (split-string (substring-no-properties (buffer-string))
                        chatgpt-shell--prompt-internal))
    (nreverse result)))

(defun chatgpt-shell--write-output-to-log-buffer (output)
  "Write curl process OUTPUT to log buffer.

Create the log buffer if it does not exist.  Pretty print output
if `json' is available."
  (let ((buffer (get-buffer chatgpt-shell--log-buffer-name)))
    (unless buffer
      ;; Create buffer
      (setq buffer (get-buffer-create chatgpt-shell--log-buffer-name))
      (with-current-buffer buffer
        ;; Use `js-json-mode' if available, fall back to `js-mode'.
        (if (fboundp #'js-json-mode)
            (js-json-mode)
          (js-mode))))
    (with-current-buffer buffer
      (let ((beginning-of-input (goto-char (point-max))))
        (insert output)
        (when (and (require 'json nil t)
                   (ignore-errors (json-parse-string output)))
          (json-pretty-print beginning-of-input (point)))))))

(defun chatgpt-shell--buffer ()
  "Get *chatgpt* buffer."
  (get-buffer-create "*chatgpt*"))

(defun chatgpt-shell--process nil
  "Get *chatgpt* process."
  (get-buffer-process (current-buffer)))

(defun chatgpt-shell-ibuffer-buffers ()
  (interactive)
  (let ((buffers (cl-remove-if-not
                  (lambda (it)
                    (when it
                      (string-match-p "*chatgpt*" it)))
                  (mapcar #'buffer-name (buffer-list)))))
    (ibuffer t "*Chatpt shells Ibuffer*"
             `((predicate . (member (buffer-name) ',buffers))))))

(defun chatgpt-jump-to-context-shell ()
  "Reverse buffer context shell jump."
  (interactive)
  (when-let*
      ((orig-buffname (buffer-name))
       (buffers
        (cl-remove-if-not
         (lambda (it)
           (with-current-buffer it
             (car
              (cl-remove-if-not
               (lambda (s) (string= orig-buffname s))
               (mapcar #'car chatgpt-shell-contexts)))))
         (cl-remove-if-not
          (lambda (it)
            (when it
              (string-match-p "*chatgpt*" (buffer-name it))))
          (buffer-list))))
       (buff (car buffers)))
    (display-buffer buff)))

(defun chatgpt-eval-what-the-assistant-just-said (buff)
    (interactive)
    (with-temp-buffer
      (insert
       (with-current-buffer
           buff
         (string-remove-prefix
          "assistant: "
          (assoc-default
           'content
           (car
            (last
             (chatgpt-shell--extract-commands-and-responses)))))))
      (eval-buffer)))

(provide 'chatgpt-shell)

;;; chatgpt-shell.el ends here

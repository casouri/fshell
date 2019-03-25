;;; fshell.el --- Fake Shell      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;
;; Some extra features on top of M-x shell, YMMV.

;;; Code:
;;

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'shell)

(require 'fsh-autosuggest)
(require 'fish-completion)


(defgroup fshell nil
  "Fake shell."
  :group 'fshell)

(defface fshell-valid-command-face
  '((((background dark)) (:foreground "green"))
    (((background light)) (:foreground "DarkGreen")))
  "Face of a valid command in `fshell-mode.'"
  :group 'fshell)

(defface fshell-invalid-command-face
  '((t (:inherit 'error)))
  "Face of a valid command in `fshell-mode.'"
  :group 'fshell)

(defvar fshell-prompt-regexp "^.*> "
  "The regexp pattern that matches prompt.")

(defvar fshell-shell-file-name "zsh"
  "The shell used in fshell-mode.")

(defvar fshell-history-file "~/.fhistory"
  "Path to the history file of fshell.")

(defvar fshell--which-path (concat (file-name-directory (or load-file-name buffer-file-name))
                                   "fshell-which.sh")
  "Path to `fshell-which.sh'.")

(define-derived-mode fshell-mode shell-mode
  "fshell" "Fake shell."
  ;; comint setting
  (setq-local comint-process-echoes t) ; remove echo
  (setq-local comint-redirect-echo-input t) ; remove echo
  (setq-local shell-prompt-pattern fshell-prompt-regexp)
  (setq-local comint-prompt-regexp fshell-prompt-regexp)
  (setq-local comint-input-ring-file-name fshell-history-file)
  ;; disable prompt highlight, bold instead
  (face-remap-set-base 'comint-highlight-prompt :inherit nil :weight 'bold)
  ;; don't bold user input
  (face-remap-set-base 'comint-highlight-input :weight 'normal)
  ;; auto suggest
  (fsh-autosuggest-companyless-mode)
  ;; validate command
  (fshell--prepare-which)
  (add-hook 'post-command-hook #'fshell-validate-command t t)
  ;; sync pwd
  (fshell-sync-dir-buffer-name)
  ;; (add-function :after (local 'shell-resync-dirs) #'fshell-sync-dir-buffer-name)
  (add-hook 'post-command-hook #'fshell-sync-dir-buffer-name t t)
  ;; completion
  (when (require 'company nil t)
    (company-mode)
    (setq-local company-auto-complete nil)
    (setq-local company-idle-delay 99999999)
    (define-key fshell-mode-map (kbd "<tab>") #'company-complete))
  ;; xterm-256 color
  (when (require 'xterm-color nil t)
    (setq-local comint-output-filter-functions
                (remove 'ansi-color-process-output comint-output-filter-functions))
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))
  ;; completion fallback
  (fish-completion-mode)
  ;; load history
  (comint-read-input-ring)
  ;; save history when kill buffer
  (add-hook 'kill-buffer-hook #'comint-write-input-ring t t)
  ;; prevent indentation, otherwise echo is not cleaned properly
  (setq-local electric-indent-inhibit t))

(define-key fshell-mode-map (kbd "C-c C-l") #'comint-clear-buffer)
(define-key fshell-mode-map (kbd "C-c C-b") #'fshell-switch-buffer)
(define-key fshell-mode-map (kbd "M-p") #'comint-previous-matching-input-from-input)
(define-key fshell-mode-map (kbd "M-n") #'comint-next-matching-input-from-input)

;;;; Helpers

(defun fshell--send (command)
  "Send COMMAND to process.
COMMAND doesn't include newline.
(In fact, it depends on `comint-input-sender-no-newline')."
  (funcall comint-input-sender (get-buffer-process (current-buffer))
           command))

(defun fshell-buffer-list ()
  "Return a list of fshell buffers."
  (cl-remove-if-not
   (lambda (buf)
     (eq (buffer-local-value 'major-mode buf)
         'fshell-mode))
   (buffer-list)))

(defun fshell--cd (dir)
  "Cd into DIR."
  (fshell--send (concat "cd " dir))
  (cd dir))

(defun fshell--prepare-which ()
  "Prepare the `which' command used to determine command validness."
  (with-temp-buffer
    (insert (format "#!%s\nwhich $1" (executable-find fshell-shell-file-name)))
    (write-region 1 (1+ (buffer-size)) fshell--which-path))
  nil)

;;;; Commands

(defun fshell-switch-buffer (buffer)
  "Switch to an fshell buffer BUFFER."
  (interactive
   (list (completing-read "Choose buffer: "
                          (mapcar (lambda (buf)
                                    (buffer-name buf))
                                  (fshell-buffer-list)))))
  (switch-to-buffer buffer))

;;;###autoload
(defun fshell-toggle (&optional arg)
  "Toggle Fshell.

ARG: C-u: open the fshell buffer with the same dir of current buffer
If there exists an Fshell buffer with current directory, use that,
otherwise create one.

C-u C-u: same as C-u, but reuse a existing fshell buffer instead of
creating one."
  (interactive "p")
  (if (equal major-mode 'fshell-mode)
      ;; toggle off
      (while (equal major-mode 'fshell-mode)
        (switch-to-prev-buffer))
    ;; toggle on
    (let ((buffer-list (fshell-buffer-list)))
      (cond ((or (eq arg 4) ; C-u
                 (eq arg 16)) ; C-u C-u
             ;; open in current dir
             (let* ((dir default-directory)
                    (buffer-with-same-dir
                     (catch 'found
                       (dolist (buffer buffer-list)
                         (when (equal dir (buffer-local-value 'default-directory
                                                              buffer))
                           (throw 'found buffer))))))
               ;; found the buffer with the same dir
               ;; or create a new one
               (if buffer-with-same-dir
                   (switch-to-buffer buffer-with-same-dir)
                 (switch-to-buffer (if (eq arg 4)
                                       (progn (message "No valid fshell buffer found, reuse one.")
                                              (car buffer-list))
                                     (message "No valid fshell buffer found, create a new one.")
                                     (fshell-new)))
                 (fshell--cd dir))))
            ;; simply open
            (t (switch-to-buffer (or (car buffer-list)
                                     (fshell-new))))))))

(defun fshell-new ()
  "Create new fshell buffer."
  (let ((explicit-shell-file-name fshell-shell-file-name))
    (ignore explicit-shell-file-name)
    (save-excursion
      (save-window-excursion
        (shell (generate-new-buffer "*fshell*"))
        (fshell-mode)
        (current-buffer)))))

(defun fshell (&optional dir)
  "Switch to a new fshell, cd to DIR.
If DIR is nil, use current directory."
  (interactive)
  (let ((current-dir default-directory))
    (switch-to-buffer (fshell-new))
    (run-hooks 'fshell-before-hook)))

(defun fshell-next ()
  "Select next fshell buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (next-buffer)
    (while (and (not (eq major-mode 'fshell-mode))
                (not (eq buf (current-buffer))))
      (next-buffer))))

(defun fshell-prev ()
  "Select previous fshell buffer."
  (interactive)
  (let ((buf (current-buffer)))
    (previous-buffer)
    (while (and (not (eq major-mode 'fshell-mode))
                (not (eq buf (current-buffer))))
      (previous-buffer))))

(defalias 'fshell-clear-buffer 'comint-clear-buffer
  "Clear fshell buffer.")


;;;; Extensions

;;;;; Validate command

(defun fshell-validate-command ()
  "Validate current command."
  ;; overlay is slow, so we use text property here
  (when (eq (point) (point-max))
    (save-excursion
      (let ((inhibit-field-text-motion t))
        ;; otherwise beginning of line and line end position
        ;; doesn't work right
        (beginning-of-line)
        (let ((match (re-search-forward
                      (format "%s\\([^ \t\r\n\v\f]*\\)" fshell-prompt-regexp)
                      (line-end-position)
                      t)))
          (when match
            (let ((beg (match-beginning 1))
                  (end (match-end 1))
                  (command (match-string 1)))
              (put-text-property
               beg end
               'font-lock-face (if (or (executable-find command)
                                       ;; Or  ../. ?
                                       (or (equal command "..")
                                           (equal command "."))
                                       ;; Or a file in current dir?
                                       (member (file-name-base command) (directory-files default-directory))
                                       ;; Command exists (even though ‘executable-find’ returns nil) or is an alias?
                                       ;; this is slow so we put it at the end
                                       (eq (call-process fshell--which-path nil nil nil command) 0)
                                       )
                                   'fshell-valid-command-face
                                 'fshell-invalid-command-face))
              (put-text-property beg end 'rear-nonsticky t)
              command)))))))

;;;;; Sync buffer name with current directory

(defun fshell-sync-dir-buffer-name (&rest _)
  "Change fshell buffer name by directory change."
  (rename-buffer (format "fshell @ %s" default-directory)
                 t))

(provide 'fshell)

;;; fshell.el ends here

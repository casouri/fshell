;;; fshell.el --- Fake Shell      -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:
;;

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
  "Face of a valid command in `fshell-mode.'")

(defface fshell-invalid-command-face
  '((t (:inherit 'error)))
  "Face of a valid command in `fshell-mode.'")

(defvar fshell-prompt-regexp "^.*> "
  "The regexp pattern that matches prompt.")

(defvar fshell-shell-file-name "zsh"
  "The shell used in fshell-mode.")

(define-derived-mode fshell-mode shell-mode
  "fshell" "Fake shell."
  (setq-local shell-prompt-pattern fshell-prompt-regexp)
  (setq-local comint-prompt-regexp fshell-prompt-regexp)
  (face-remap-set-base 'comint-highlight-prompt :inherit nil)
  (esh-autosuggest-companyless-mode)
  (add-hook 'post-command-hook #'fshell-validate-command t t)
  (fshell-sync-dir-buffer-name)
  (advice-add #'shell-resync-dirs :after #'fshell-sync-dir-buffer-name)
  (when (featurep 'company)
    (company-mode)
    (setq-local company-auto-complete nil)
    (setq-local company-idle-delay 99999999)
    (define-key fshell-mode-map (kbd "<tab>") #'company-complete))
  (fish-completion-mode))

(define-key fshell-mode-map (kbd "C-c C-l") #'fshell-clear-buffer)
(define-key fshell-mode-map (kbd "C-c C-b") #'fshell-switch-buffer)


;;;; Helpers

(defun fshell-buffer-list ()
  "Return a list of fshell buffers."
  (cl-remove-if-not
   (lambda (buf)
     (eq (buffer-local-value 'major-mode buf)
         'fshell-mode))
   (buffer-list)))

;;;; Commands

(defun fshell-switch-buffer (buffer)
  "Switch to an fshell buffer BUFFER."
  (interactive
   (list (completing-read "Choose buffer: "
                          (mapcar (lambda (buf)
                                    (buffer-name buf))
                                  (fshell-buffer-list)))))
  (switch-to-buffer buffer))

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
                 (shell-cd dir))))
            ;; simply open
            (t (switch-to-buffer (or (car buffer-list)
                                     (fshell-new))))))))

(defun fshell-new ()
  "Create new fshell buffer."
  (let ((explicit-shell-file-name fshell-shell-file-name))
    (ignore explicit-shell-file-name)
    (save-excursion
      (shell (generate-new-buffer "*fshell*"))
      (fshell-mode)
      (current-buffer))))

(defun fshell ()
  "Switch to a new fshell."
  (interactive)
  (switch-to-buffer (fshell-new)))

(defun fshell-next ()
  "Select next fshell buffer.
Create new one if no fshell buffer exists."
  (interactive)
  (let ((buf (current-buffer)))
    (next-buffer)
    (while (and (not (eq major-mode 'fshell-mode))
                (not (eq buf (current-buffer))))
      (next-buffer))))

(defun fshell-prev ()
  "Select previous fshell buffer.
Create new one if no fshell buffer exists."
  (interactive)
  (let ((buf (current-buffer)))
    (previous-buffer)
    (while (and (not (eq major-mode 'fshell-mode))
                (not (eq buf (current-buffer))))
      (previous-buffer))))

(defun fshell-clear-buffer ()
  "Clear fshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (comint-send-input)))


;;;; Extensions

;;;;; Validate command

(defun fshell-validate-command ()
  "Validate current command."
  ;; overlay is slow, so we use text property here
  (save-excursion
    (forward-line 0) ; beginning-of-line but ignores boundaries
    (re-search-forward (format "%s\\([^ \t\r\n\v\f]*\\)" fshell-prompt-regexp)
                       (line-end-position)
                       t)
    (let ((beg (match-beginning 1))
          (end (match-end 1))
          (command (match-string 1)))
      (when command
        (put-text-property
         beg end
         'face (if (or
                    ;; Command exists?
                    (executable-find command)
                    ;; Or command is an alias?
                    ;; TODO
                    ;; (seq-contains (eshell-alias-completions "") command)
                    ;; Or  ../. ?
                    (or (equal command "..")
                        (equal command ".")
                        (equal command "exit"))
                    ;; Or a file in current dir?
                    (member (file-name-base command) (directory-files default-directory))
                    ;; Or a elisp function?
                    (functionp (intern command)))
                   'fshell-valid-command-face
                 'fshell-invalid-command-face))
        (put-text-property beg end 'rear-nonsticky t)
        command))))

;;;;; Sync buffer name with current directory

(defun fshell-sync-dir-buffer-name (&rest _)
  "Change fshell buffer name by directory change."
  (rename-buffer (format "fshell @ %s" default-directory)
                 t))



(provide 'fshell)

;;; fshell.el ends here

;;; fsh-autosuggest.el --- History autosuggestions for fshell -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Diego A. Mundo
;; Author: Diego A. Mundo <diegoamundo@gmail.com>
;; URL: http://github.com/dieggsy/fsh-autosuggest
;; Git-Repository: git://github.com/dieggsy/fsh-autosuggest.git
;; Created: 2017-10-28
;; Version: 1.2.2
;; Keywords: completion company matching convenience abbrev
;; Package-Requires: ((emacs "24.4") (company "0.9.4"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Provides a company backend that implements functionality similar to fish
;; shell history autosuggestions.

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup fsh-autosuggest nil
  "Fish-like autosuggestions for fshell."
  :group 'company)

(defcustom fsh-autosuggest-delay 0
  "Delay for history autosuggestion."
  :group 'fsh-autosuggest
  :type 'number)

(defcustom fsh-autosuggest-use-company-map nil
  "Instead of overriding `company-active-map', use as-is.

This is disabled by default, as bindings in `company-active-map'
to RET and TAB may interfere with command input and completion
respectively."
  :group 'fsh-autosuggest
  :type 'boolean)

(defvar fsh-autosuggest-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<right>") 'company-complete-selection)
    (define-key keymap (kbd "C-f") 'company-complete-selection)
    (define-key keymap (kbd "M-<right>") 'fsh-autosuggest-complete-word)
    (define-key keymap (kbd "M-f") 'fsh-autosuggest-complete-word)
    keymap)
  "Keymap that is enabled during an active history
  autosuggestion.")

(defun fsh-reload-shell-history ()
  (with-temp-message ""
    (let* ((shell-command (getenv "SHELL")))
      (cond ((string-equal shell-command "/bin/bash")
             (shell-command "history -r"))
            ((string-equal shell-command "/bin/zsh")
             (shell-command "fc -W; fc -R"))))))

(defun fsh-parse-bash-history ()
  "Parse the bash history."
  (if (file-exists-p "~/.bash_history")
      (let (collection bash_history)
        (fsh-reload-shell-history)
        (setq collection
              (nreverse
               (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.bash_history"))
                                               (buffer-string))
                             "\n"
                             t)))
        (when (and collection (> (length collection) 0)
                   (setq bash_history collection))
          bash_history))
    nil))

(defun fsh-parse-zsh-history ()
  "Parse the bash history."
  (if (file-exists-p "~/.zsh_history")
      (let (collection zsh_history)
        (fsh-reload-shell-history)
        (setq collection
              (nreverse
               (split-string (with-temp-buffer (insert-file-contents (file-truename "~/.zsh_history"))
                                               (replace-regexp-in-string "^:[^;]*;" "" (buffer-string)))
                             "\n"
                             t)))
        (when (and collection (> (length collection) 0)
                   (setq zsh_history collection))
          zsh_history))
    nil))

(defun fsh-parse-shell-history ()
  "Parse history from fshell/bash/zsh/ ."
  (delete-dups
   (mapcar
    (lambda (str)
      (string-trim (substring-no-properties str)))
    (append
     (ring-elements comint-input-ring)
     (fsh-parse-bash-history)
     (fsh-parse-zsh-history)))))

(defun fsh-autosuggest-candidates (prefix)
  "Select the first fshell history candidate that starts with PREFIX."
  (let* ((most-similar (cl-find-if
                        (lambda (str)
                          (string-prefix-p prefix str))
                        (fsh-parse-shell-history))))
    (when most-similar
      `(,most-similar))))

(defun fsh-autosuggest-complete-word ()
  (interactive)
  (save-excursion
    (let ((pos (point)))
      (company-complete-selection)
      (goto-char pos)
      (forward-word)
      (unless (or (eobp) (eolp))
        (kill-line))))
  (end-of-line)
  (ignore-errors
    (let ((inhibit-message t))
      (company-begin-backend 'fsh-autosuggest))))

(defun fsh-autosuggest--prefix ()
  "Get current fshell input."
  (catch 'no-prompt
    (let* ((point-min (point-min))
           (input-start
            (save-excursion
              (let ((inhibit-field-text-motion t))
                ;; disable field and go to real beg of line
                (beginning-of-line)
                (while (and (not (looking-at-p fshell-prompt-regexp))
                            (not (eq point-min (point))))
                  (forward-line -1))
                (when (eq point-min (point))
                  (throw 'no-prompt 'stop)))
              ;; enable field and go to end of line,
              ;; now we are at end of prompt/beg of user input
              (line-end-position)))
           (prefix
            (string-trim-left
             (buffer-substring-no-properties
              input-start
              (line-end-position)))))
      (if (not (string-empty-p prefix))
          prefix
        'stop))))

;;;###autoload
(defun fsh-autosuggest (command &optional arg &rest ignored)
  "`company-mode' backend to provide fshell history suggestion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'fsh-autosuggest))
    (prefix (and (eq major-mode 'fshell-mode)
                 (fsh-autosuggest--prefix)))
    (candidates (fsh-autosuggest-candidates arg))))

;;;###autoload
(define-minor-mode fsh-autosuggest-mode
  "Enable fish-like autosuggestions in fshell.

You can use <right> to select the suggestion. This is
customizable through `fsh-autosuggest-active-map'. If
you prefer to use the default value of `company-active-map', you
may set the variable
`fsh-autosuggest-use-company-map', though this isn't
recommended as RET and TAB may not work as expected (send input,
trigger completions, respectively) when there is an active
suggestion.

The delay defaults to 0 seconds to emulate fish shell's
instantaneous suggestions, but is customizable with
`fsh-autosuggest-delay'.

Note: This assumes you want to use something other than company
for shell completion, e.g. `fshell-pcomplete',
`completion-at-point', or helm-fsh-pcomplete, since
`company-active-map', `company-backends', and `company-frontends'
will be locally overriden and company will be used solely for
history autosuggestions."
  :init-value nil
  :group 'fsh-autosuggest
  (if fsh-autosuggest-mode
      (progn
        (company-mode 1)
        (unless fsh-autosuggest-use-company-map
          (setq-local company-active-map fsh-autosuggest-active-map))
        (setq-local company-idle-delay fsh-autosuggest-delay)
        (setq-local company-backends '(fsh-autosuggest))
        (setq-local company-frontends '(company-preview-frontend)))
    (company-mode -1)
    (kill-local-variable 'company-active-map)
    (kill-local-variable 'company-idle-delay)
    (kill-local-variable 'company-backends)
    (kill-local-variable 'company-frontends)))

;;;; Companyless

(defvar fsh-autosuggest--companyless-overlay nil
  "Overlay used to display auto suggestion")

(defface fsh-autosuggest-companyless '((((background dark)) . (:foreground "gray50"))
                                       (((background light)) . (:foreground "silver")))
  "Face of auto suggestion when using companyless mode.")

(defvar fsh-autosuggest--companyless-override-map (let ((map (make-sparse-keymap)))
                                                    (define-key map (kbd "C-f") #'fsh-autosuggest--companyless-complete)
                                                    map)
  "The map used on overlay so you can complete with C-f.")

;;;###autoload
(define-minor-mode fsh-autosuggest-companyless-mode
  "`fsh-autosuggest-mode' but don't use company as front end."
  :keymap (make-sparse-keymap)
  :group 'fsh-autosuggest
  (when fsh-autosuggest-mode
    (fsh-autosuggest-mode -1))
  (if fsh-autosuggest-companyless-mode
      (progn (add-hook 'post-command-hook #'fsh-autosuggest--companyless-post-command-hook t t)
             (add-hook 'company-completion-started-hook #'fsh-autosuggest-companyless-mode-off-hook t t)
             (add-hook 'company-completion-cancelled-hook #'fsh-autosuggest-companyless-mode-on-hook t t)
             (add-hook 'company-completion-finished-hook #'fsh-autosuggest-companyless-mode-on-hook t t))
    (remove-hook 'post-command-hook #'fsh-autosuggest--companyless-post-command-hook t)
    (remove-hook 'company-completion-started-hook #'fsh-autosuggest-companyless-mode-off-hook t)
    (remove-hook 'company-completion-cancelled-hook #'fsh-autosuggest-companyless-mode-on-hook t)
    (remove-hook 'company-completion-finished-hook #'fsh-autosuggest-companyless-mode-on-hook t)
    ;; clean up overlay and binding
    (fsh-autosuggest--companyless-cleanup)))


(defun fsh-autosuggest--companyless-cleanup ()
  "Remove overlay and keybinding."
  (when fsh-autosuggest--companyless-overlay
    (delete-overlay fsh-autosuggest--companyless-overlay)
    (setq fsh-autosuggest--companyless-overlay nil))
  (define-key fsh-autosuggest-companyless-mode-map
    (kbd "C-f") nil))

(defun fsh-autosuggest--companyless-post-command-hook ()
  "Add autosuggest to overlay."
  (when (and (eq major-mode 'fshell-mode)
             (not (minibufferp)))
    ;; remove overlay and later (if needed) create a new one
    (fsh-autosuggest--companyless-cleanup)
    (let ((prefix (if (or (eq (char-after) nil) ; only complete when at end of symbol
                          (eq (char-after) ?\n))
                      (fsh-autosuggest--prefix)
                    'stop))
          suggest)
      (when (and (not (eq prefix 'stop))
                 (setq suggest (car (fsh-autosuggest-candidates prefix))))
        (setq fsh-autosuggest--companyless-overlay
              (make-overlay (point) (point)))
        (define-key fsh-autosuggest-companyless-mode-map
          (kbd "C-f") #'fsh-autosuggest--companyless-complete)
        (overlay-put
         fsh-autosuggest--companyless-overlay
         'after-string ; use after sting to display suggestion
         (propertize (substring suggest (length prefix)) ; remove prefix from suggestion
                     ;; without 'cursor property, the cursor is displayed at the end of
                     ;; the overlay
                     'cursor 0 'face 'fsh-autosuggest-companyless))))))

(defun fsh-autosuggest-companyless-mode-off-hook (&rest _)
  "Turn off, used in company hooks."
  (fsh-autosuggest-companyless-mode -1)
  (add-hook 'company-completion-started-hook #'fsh-autosuggest-companyless-mode-off-hook t t)
  (add-hook 'company-completion-cancelled-hook #'fsh-autosuggest-companyless-mode-on-hook t t)
  (add-hook 'company-completion-finished-hook #'fsh-autosuggest-companyless-mode-on-hook t t))

(defun fsh-autosuggest-companyless-mode-on-hook (&rest _)
  "Turn on, used in company hooks."
  (fsh-autosuggest-companyless-mode))

(defun fsh-autosuggest--companyless-complete ()
  "Insert the auto suggestion."
  (interactive)
  (when (and (eq major-mode 'fshell-mode)
             fsh-autosuggest--companyless-overlay)
    (insert (substring-no-properties
             (or (overlay-get fsh-autosuggest--companyless-overlay 'after-string)
                 "")))))

(provide 'fsh-autosuggest)

;;; fsh-autosuggest.el ends here

;;; run-in-dir.el --- Override the default directory in the next command  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; URL: https://github.com/abougouffa/run-in-dir

;; This program is free software; you can redistribute it and/or modify
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

;; Override `default-directory' in the next command

;;; Code:

(require 'savehist)


;;; Customization

(defgroup run-in-dir nil
  "Override the default directory in the next command."
  :group 'convenience)

(defcustom run-in-dir-directory nil
  "This variable can be configured to set the default directory directory."
  :type '(choice directory (const nil))
  :group 'run-in-dir)
(make-variable-buffer-local 'run-in-dir-directory)

(defcustom run-in-dir-remember-last-directory t
  "Remember the last used directory in the current buffer."
  :type 'boolean
  :group 'run-in-dir)

;;; Internal stuff

(defvar run-in-dir-histroy nil)

;; Integrate with `savehist-mode'
(add-to-list 'savehist-additional-variables 'run-in-dir-histroy)

(defun run-in-dir--pch ()
  "Pre-command hook for the command `run-in-dir-next-command-prefix'."
  (let ((cmd this-command)
        (new-dir run-in-dir-directory)
        (minibuf-depth (minibuffer-depth)))
    (unless (> (minibuffer-depth) minibuf-depth) ; until leaving the minibuffer
      (remove-hook 'pre-command-hook #'run-in-dir--pch))
    (unless run-in-dir-remember-last-directory
      (setq run-in-dir-directory nil))
    (setq this-command
          (lambda ()
            (interactive)
            (let ((default-directory (or new-dir default-directory)))
              (call-interactively cmd))))))

;;; Commands

;;;###autoload
(defun run-in-dir-next-command-prefix (directory)
  "Run next command with `default-directory' set to DIRECTORY."
  (interactive (list (if (or current-prefix-arg (not run-in-dir-directory))
                         (setq run-in-dir-directory
                               (read-directory-name "Select a `default-directory': "))
                       run-in-dir-directory)))
  (prefix-command-preserve-state)
  (setq run-in-dir-directory directory)
  (add-to-history 'run-in-dir-histroy run-in-dir-directory)
  (add-hook 'pre-command-hook #'run-in-dir--pch)
  (message "Run next command in %S -- " (abbreviate-file-name directory)))

;;;###autoload
(defalias 'run-in-dir-prefix #'run-in-dir-next-command-prefix)


(provide 'run-in-dir)
;;; run-in-dir.el ends here

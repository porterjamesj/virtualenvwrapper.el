;;; virtualenvwrapper.el -- a modern venv tool for Emacs

;; Copyright (C) 2013 James J Porter
;; Author: James J Porter <porterjamesj@gmail.com>

;;; Commentary:
;; The python situation in Emacs has historically been a mess, but
;; with the adoption of Fabián Gallina's python.el as the canonical
;; python major mode, hopefully this will be fixed soon.  The out of
;; the box virtualenv support in python.el, however, doesn't do quite
;; everything I want.  virtualenv.el has been historically been the
;; tool for this, but it's hundreds of lines of code, many of which
;; are there for supporting the legacy python modes.  I wanted a
;; minimal mode for working with virtualenvs that works effectively
;; with the new python.el and only with the new python.el; here it is.

;;; Code:

(defcustom venv-dir
  (expand-file-name "~/.virtualenvs/")
  "The directory in which your virtualenvs are located.")

(defvar venv-history nil "History of venvs switched to.")

(defvar venv-current-name nil "Name of current virtualenv.")

(defvar venv-current-dir nil "Directory of current virtualenv.")

(defvar venv-configure-shell t)
(defvar venv-configure-eshell t)

(defun venv-deactivate ()
  "Deactivate the current venv."
  (interactive)
  (setq python-shell-virtualenv-path nil)
  (setq exec-path (-filter (lambda (s) (not (s-contains? venv-dir s)))
                           exec-path))
  (setenv "PATH" (venv-get-stripped-path))
  (setenv "VIRTUAL_ENV" nil)
  (setq venv-current-name nil)
  (setq venv-current-dir nil)
  (setq eshell-path-env (getenv "PATH"))
  (message "virtualenv deactivated"))

(defun venv-get-candidates (dir)
  "Given a directory containing virtualenvs, return a list
of candidates to match against in the completion."
  (let ((proper-dir (file-name-as-directory dir)))
    (-filter (lambda (s) (car (file-attributes (concat dir s))))
             (directory-files proper-dir nil "^[^.]"))))

(defun venv-get-stripped-path ()
  "Return what the PATH environment variable would look like if
we weren't in a virtualenv."
  (s-join ":" (-filter (lambda (s) (not (s-contains? venv-dir s)))
                       (s-split ":" (getenv "PATH")))))

(defun venv-workon (&optional venv-to-switch-to)
  "Interactively switch to a virtualenv."
  (interactive)
  ;; first deactivate
  (venv-deactivate)
  (if venv-to-switch-to
      ;; if called with argument, make sure it is valid
      (progn
        (when (not (-contains? (venv-get-candidates venv-dir) venv-to-switch-to))
          (error "Invalid virtualenv specified!"))
        ;; then switch to it
        (setq venv-current-name venv-to-switch-to))
    ;; if called without argument, prompt for completion
  (setq venv-current-name
          (completing-read "Switch to virtualenv: "
                           (venv-get-candidates venv-dir) nil t nil
                           'venv-history
                           (car venv-history))))
  (setq venv-current-dir
        (file-name-as-directory
         (concat (file-name-as-directory venv-dir) venv-current-name)))
  (message venv-current-name)
  ;; push it onto the history
  (add-to-list 'venv-history venv-current-name)
  ;; setup the python shell
  (setq python-shell-virtualenv-path venv-current-dir)
  ;; setup emacs exec-path
  (add-to-list 'exec-path (concat venv-current-dir "bin"))
  ;; setup the environment for subprocesses
  (setenv "PATH" (concat venv-current-dir "bin:" (getenv "PATH")))
  (setenv "VIRTUAL_ENV" venv-current-dir)
  ;; set eshell path
  (setq eshell-path-env (getenv "PATH"))
  (message (concat "Switched to virtualenv: " venv-current-name)))


;; Advice for the shell so it doesn't blow up

(defun venv-shell-init (process)
  "Startup the current virtualenv in a newly opened shell."
  (comint-send-string
   process
   (concat "if command -v workon >/dev/null 2>&1; then workon "
           venv-current-name
           "; else source "
           venv-current-dir
           "bin/activate; fi \n")))


(defun venv-initialize ()
  (when venv-configure-shell
    (defadvice shell (around strip-env ())
      "Use the environment without the venv to start up a new shell."
      (let* ((buffer-name (or buffer "*shell*"))
             (buffer-exists-already (get-buffer buffer-name)))
        (if (or buffer-exists-already (not venv-current-name))
            ad-do-it
          (progn (setenv "PATH" (venv-get-stripped-path))
                 (setenv "VIRTUAL_ENV" nil)
                 ad-do-it
                 (venv-shell-init buffer-name)
               (setenv "PATH" (concat venv-current-dir "bin:" (getenv "PATH")))
               (setenv "VIRTUAL_ENV" venv-current-dir)))))
    (ad-activate 'shell))
  (when venv-configure-eshell
    (defun eshell/workon (arg) (venv-workon arg))
    (defun eshell/deactivate () (venv-deactivate))))

(provide 'virtualenvwrapper)
;;; venvwrapper.el ends here

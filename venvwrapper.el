;;; venvwrapper.el -- a modern venv tool for Emacs

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

(defvar venv-wrapper t "Whether or not you use the virtualenv wrapper.")

(defun venv-deactivate ()
  "Deactivate the current venv."
  (interactive)
  (setq python-shell-virtualenv-path nil)
  (setq exec-path (-filter (lambda (s) (not (s-contains? venv-dir s)))
                           exec-path))
  (setenv "PATH" (venv-get-stripped-path))
  (setenv "VIRTUAL_ENV" nil))

(defun venv-get-candidates (dir)
  "Given a directory containing virtualenvs, return a list
of candidates to match against in the completion."
  (let ((proper-dir (file-name-as-directory dir)))
    (-filter (lambda (s) (car (file-attributes (concat dir s))))
             (directory-files proper-dir nil "^[^.]"))))

(defun venv-get-stripped-path ()
  "Returns what the PATH environment variable would look like if
we weren't in a virtualenv."
  (s-join ":" (-filter (lambda (s) (not (s-contains? venv-dir s)))
                       (s-split ":" (getenv "PATH")))))

(defun venv-workon ()
  "Interactively switch to a virtualenv."
  (interactive)
  ;; first deactivate
  (venv-deactivate)
  ;; now find completions and prompt for one
  (setq venv-current-name
          (completing-read "Switch to virtualenv: "
                           (venv-get-candidates venv-dir) nil t nil
                           'venv-history
                           (car venv-history)))
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
    (setenv "VIRTUAL_ENV" venv-current-dir))


;; Advice for the shell so it doesn't blow up

(when venv-wrapper
  (defadvice shell (before strip-env ())
    "Use the environment without the venv to start up a new shell."
    (when (not (get-buffer "*shell*"))
        (setenv "PATH" (venv-get-stripped-path))
        (setenv "VIRTUAL_ENV" nil)))
  (defadvice make-comint-in-buffer (after restore-env ())
    "Tell the shell to turn the virtualenv on and
     restore the environment in emacs."
    (when (get-buffer "*shell*")
      (comint-send-string "*shell*" (concat "workon " venv-current-name "\n"))
      (setenv "PATH" (concat venv-current-dir "bin:" (getenv "PATH")))
      (setenv "VIRTUAL_ENV" venv-current-dir)))
    (ad-activate 'shell)
    (ad-activate 'make-comint-in-buffer))

(provide 'venvwrapper)
;;; venvwrapper.el ends here

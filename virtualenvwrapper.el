;;; virtualenvwrapper.el --- a featureful virtualenv tool for Emacs

;; Copyright (C) 2013 James J Porter

;; Author: James J Porter <porterjamesj@gmail.com>
;; URL: http://github.com/porterjamesj/virtualenvwrapper.el
;; Version: 20131514
;; Keywords: python, virtualenv, virtualenvwrapper
;; Package-Requires: ((dash "1.5.0") (s "1.6.1"))

;;; Commentary:

;; A powerful virtualenv tool for Emacs.  See documentation at
;; https://github.com/porterjamesj/virtualenvwrapper.el

;;; POTENTIAL TODOS:
;; - Figure out a better way to make M-x shell work than
;;   advising it.  This could be done if Emacs had pre-
;;   and post- shell activation hooks.
;; - Implement the option to have eshell work in a separate
;;   namespace.  This would be a substantial refactor.
;; - Add an option for `venv-location' to be an alist.
;; - Propertize the venv names in the output of `venv-lsvirtualenv'
;;   so that clicking or pressing RET on one will switch to it.

;;; VERSION HISTORY
;; 20130921
;; - Fix a bug that caused an error if exec-path was nil (Thanks Steven Huwig).
;; - Fix a bug that prevented cpvirtualenv from working in eshell.
;; - Fix a bug in which deleted virtualenvs sometimes still showed up
;;   in completions.
;; - Eshell commands now tab complete virtualenv names where appropriate.
;; - mkvirtualenv and rmvirtualenv can now accept multiple names.


;;; Code:

(require 'dash)
(require 's)

;; customizable variables

(defgroup virtualenvwrapper nil
  "Virtualenvwrapper for Emacs."
  :group 'python)

(defcustom venv-location
  (expand-file-name (or (getenv "WORKON_HOME") "~/.virtualenvs/"))
  "The location(s) of your virtualenvs. This
can be either a string, which indicates a single directory in which
you keep all your virutalenvs, or a list of strings, in which case it
specifies disparate locations in which all your virtualenvs are kept.
The default location is ~/.virtualenvs/, which is where your virtualenvs
are stored if you use virtualenvwrapper in the shell."
  :group 'virtualenvwrapper)

;; hooks

(defvar venv-premkvirtualenv-hook nil
  "Hook run before creating a new virtualenv.")

(defvar venv-postmkvirtualenv-hook nil
  "Hook run after creating a new virtualenv.")

(defvar venv-prermvirtualenv-hook nil
  "Hook run before deleting a virtualenv.")

(defvar venv-postrmvirtualenv-hook nil
  "Hook run after deleting a virtualenv.")

(defvar venv-preactivate-hook nil
  "Hook run before a virtualenv is activated.")

(defvar venv-postactivate-hook nil
  "Hook run after a virtualenv is activated.")

(defvar venv-predeactivate-hook nil
  "Hook run before a virtualenv is deactivated.")

(defvar venv-postdeactivate-hook nil
  "Hook run after a virtualenv is deactivated.")


;; internal variables that you probably shouldn't mess with

(defvar venv-history nil "The history of venvs we have worked on.")

(defvar venv-current-name nil "Name of current virtualenv.")

(defvar venv-current-dir nil "Directory of current virtualenv.")


;; copy from virtualenv.el
(defvar venv-executables-dir
  (if (eq system-type 'windows-nt) "Scripts" "bin")
  "The name of the directory containing executables. It is system dependent.")

;; internal utility functions

(defun venv-clear-history ()
  (setq venv-history nil))

(defun venv-dir-to-name (dir)
  "Extract the name of a virtualenv from a path."
  (car (last (--filter (not (s-blank? it))
                 (s-split "/" dir)))))

(defun venv-name-to-dir (name)
  "Given the name of a virtualenv, translate it
to the directory where that virtualenv is located."
  (file-name-as-directory
   (let ((potential-dir
          (if (stringp venv-location)
              (concat (file-name-as-directory
                       (expand-file-name venv-location)) name)
            (car (-filter
                  (lambda (d)
                    (s-equals? name (venv-dir-to-name d)))
                  venv-location)))))
     (if (and potential-dir
              (file-exists-p
               (concat (file-name-as-directory
                        (expand-file-name potential-dir)) venv-executables-dir)))
         (file-name-as-directory
          (expand-file-name potential-dir))
       (error (concat "No such virtualenv: " name))))))

(defun venv-get-candidates ()
  "Wrapper to call get-candidates-list or
get-candidates-string depending on which
is appropriate for how venv-location is
specified."
  (let ((candidates
         (if (stringp venv-location)
             (venv-get-candidates-dir venv-location)
           (venv-get-candidates-list venv-location))))
    (when (not (eq (length (-distinct candidates))
                   (length candidates)))
      (error "Some virtualenvs have the same name!"))
    candidates))

(defun venv-get-candidates-list (list)
  "Given LIST of virtualenv directories,
return a list of names that can be used in, e.g.
a completing read. This trusts the caller to only
pass directories with are actually virtualenvs."
   (-map (lambda (dir)
           (car (last (-filter (lambda (s) (not (s-blank? s)))
                               (s-split "/" dir)))))
         (-filter
          (lambda (s) (car (file-attributes
                            (concat (file-name-as-directory
                                     (expand-file-name s)) venv-executables-dir))))
                  list)))

(defun venv-get-candidates-dir (dir)
  "Given a directory DIR containing virtualenvs, return a list
of names that can be used in the completing read."
  (let ((proper-dir (file-name-as-directory (expand-file-name dir))))
    (-filter (lambda (s)
               (let ((subdir (concat proper-dir s)))
                 (car (file-attributes
                       (concat (file-name-as-directory subdir) venv-executables-dir)))))
             (directory-files proper-dir nil "^[^.]"))))

(defun venv-get-stripped-path (path)
  "Return what the PATH would look like if we weren't in a
virtualenv. PATH should be a list of strings specifiying directories."
  (let ((func (if (stringp venv-location)
                  (lambda (s) (not (s-contains?
                                    (file-name-as-directory
                                     (expand-file-name
                                      venv-location)) (or s default-directory))))
                (lambda (execs)
                  (not (-filter (lambda (locs)
                                  (s-contains?
                                   (file-name-as-directory
                                    (expand-file-name locs))
                                   (file-name-as-directory
                                    (expand-file-name execs))))
                                venv-location))))))
  (-filter func path)))


(defun venv--purge-history (candidates)
  "Remove history candidates that are not present in the list CANDIDATES"
  (setq venv-history (-filter (lambda (s) (not (-contains? candidates s)))
                              venv-history)))

(defun venv-is-valid (name)
  "Test if NAME is a valid virtualenv specifier"
  (-contains? (venv-get-candidates) name))

(defun venv-read-name (prompt)
  "Do a completing read to get the name of a candidate,
prompting the user with the string PROMPT"
  (let ((candidates (venv-get-candidates)))
    ;; purge history of no longer existant candidates first
    (venv--purge-history candidates)
    (completing-read prompt
                     candidates nil t nil
                     'venv-history
                     (or (car venv-history)
                         (car candidates)))))

(defun venv-list-virtualenvs ()
  (s-join "\n" (venv-get-candidates)))


;; potentially interactive user-exposed functions

;;;###autoload
(defun venv-deactivate ()
  "Deactivate the current venv."
  (interactive)
  (run-hooks 'venv-predeactivate-hook)
  (setq python-shell-virtualenv-path nil)
  (setq exec-path (venv-get-stripped-path exec-path))
  (setenv "PATH" (s-join path-separator
                  (venv-get-stripped-path
                   (s-split path-separator (getenv "PATH")))))
  (setenv "VIRTUAL_ENV" nil)
  (setq venv-current-name nil)
  (setq venv-current-dir nil)
  (setq eshell-path-env (getenv "PATH"))
  (run-hooks 'venv-postdeactivate-hook)
  (when (called-interactively-p 'interactive)
    (message "virtualenv deactivated")))

;;;###autoload
(defun venv-workon (&optional name)
  "Interactively switch to virtualenv NAME. Prompts for name if called
interactively."
  (interactive)
  (if name
      ;; if called with argument, make sure it is valid
      (progn
        (when (not (venv-is-valid name))
          (error "Invalid virtualenv specified!"))
        ;; then deactivate
        (venv-deactivate)
        ;; then switch
        (setq venv-current-name name))
    (progn
      ;; if without argument, deactivate first
      (venv-deactivate)
      ;; then read
      (setq venv-current-name
            (venv-read-name "Virtualenv to switch to: "))))
  (run-hooks 'venv-preactivate-hook)
  (setq venv-current-dir
        (venv-name-to-dir venv-current-name))
  ;; push it onto the history
  (add-to-list 'venv-history venv-current-name)
  ;; setup the python shell
  (setq python-shell-virtualenv-path venv-current-dir)
  ;; setup emacs exec-path
  (add-to-list 'exec-path (concat venv-current-dir venv-executables-dir))
  ;; setup the environment for subprocesses
  (setenv "PATH" (concat venv-current-dir venv-executables-dir path-separator (getenv "PATH")))
  ;; keep eshell path in sync
  (setq eshell-path-env (getenv "PATH"))
  (setenv "VIRTUAL_ENV" venv-current-dir)
  (run-hooks 'venv-postactivate-hook)
  (when (called-interactively-p 'interactive)
    (message (concat "Switched to virtualenv: " venv-current-name))))

;;;###autoload
(defun venv-mkvirtualenv (&rest names)
"Create new virtualenvs NAMES. If venv-location is a single
directory, the new virtualenvs are made there; if it is a list of
directories, the new virtualenvs are made in the current
default-directory."
  (interactive)
  (let ((parent-dir (if (stringp venv-location)
                        (file-name-as-directory
                         (expand-file-name venv-location))
                      default-directory)))
    (when (not names)
      (setq names (list (read-from-minibuffer "New virtualenv: "))))
    ;; map over all the envs we want to make
    (--each names
      ;; error if this env already exists
      (when (-contains? (venv-get-candidates) it)
        (error "A virtualenv with this name already exists!"))
      (run-hooks 'venv-premkvirtualenv-hook)
      (shell-command (concat "virtualenv " parent-dir it))
      (when (listp venv-location)
        (add-to-list 'venv-location (concat parent-dir it)))
      (venv-with-virtualenv it
                            (run-hooks 'venv-postmkvirtualenv-hook))
      (when (called-interactively-p 'interactive)
        (message (concat "Created virtualenv: " it)))))
  ;; workon the last venv we made
    (venv-workon (car (last names))))

;;;###autoload
(defun venv-rmvirtualenv (&rest names)
"Delete virtualenvs NAMES."
  (interactive)
  ;; deactivate first
  (venv-deactivate)
  ;; check validity and read names if necessary
  (if names
      (--map (when (not (venv-is-valid it))
               (error "Invalid virtualenv specified!"))
             names)
    (setq names (list (venv-read-name "Virtualenv to delete: "))))
  ;; map over names, deleting the appropriate directory
  (--each names
    (run-hooks 'venv-prermvirtualenv-hook)
    (delete-directory (venv-name-to-dir it) t)
    ;; get it out of the history so it doesn't show up in completing reads
    (setq venv-history (-filter
                        (lambda (s) (not (s-equals? s it))) venv-history))
    ;; if location is a list, delete it from the list
    (when (listp venv-location)
      (setq venv-location
            (-filter (lambda (locs) (not (s-equals?
                                          it
                                          (venv-dir-to-name locs))))
                     venv-location)))
    (run-hooks 'venv-postrmvirtualenv-hook)
    (message (concat "Deleted virtualenv: " it))))

;;;###autoload
(defun venv-lsvirtualenv ()
  "List all available virtualenvs in a temp buffer."
  (interactive)
  (with-output-to-temp-buffer
      "*Virtualenvs*"
      (princ (venv-list-virtualenvs))))

;;;###autoload
(defun venv-cdvirtualenv (&optional subdir)
  "Change to the directory of current virtualenv. If
SUBDIR is passed, append that to the path such that
we are immediately in that directory."
  (interactive)
  (if venv-current-dir
      (let ((going-to (concat (file-name-as-directory
                               (expand-file-name venv-current-dir))
                              subdir)))
        (cd going-to)
        (when (called-interactively-p 'interactive)
          (message (concat "Now in directory: " going-to))))
    (error "No virtualenv is currently active.")))

;;;###autoload
(defun venv-cpvirtualenv (&optional name newname)
  "Copy virtualenv NAME to NEWNAME. Any arguments not passed will be
prompted for This comes with the same caveat as cpvirtualenv in the
original virtualenvwrapper, which is that is far from guarenteed to
work well. Many packages hardcode absolute paths in various places an
will break if moved to a new location. Use with caution. If used with
a single virtualenv directory, behaves just like cpvirtualenv in
virtualenvwrapper.sh.  If used with virtualenvs spread around the
filesystem, creates the new virtualenv in the current default
directory."
  (interactive)
  (let ((parent-dir (if (stringp venv-location)
                        (file-name-as-directory
                         (expand-file-name venv-location))
                      default-directory)))
    (when (not name) (setq name (venv-read-name "Virtualenv to copy from: ")))
    (when (not newname) (setq newname
                              (read-from-minibuffer "Virtualenv to copy to: ")))
    ;; throw an error if newname already exists
    (when (file-exists-p (concat parent-dir newname))
      (error "A virtualenv with the proposed name already exists!"))
    ;; make the copy
    (copy-directory (venv-name-to-dir name)
                    (concat parent-dir newname))
    ;; if the location specifier is a list, add to it.
    (when (listp venv-location)
      (add-to-list 'venv-location (concat parent-dir newname)))
    (when (called-interactively-p 'interactive)
      (message (format "Copied virtualenv %s to %s" name newname)))
    (venv-workon newname)))


;; macros and functions supporting executing elisp or
;; shell commands in a particular venv

(defmacro venv-with-virtualenv (name &rest forms)
  "Evaluate FORMS with venv NAME active. NAME must be a string
identifying a virtualenv."
  `(progn
     (let ((prev-dir default-directory)
           (prev-env venv-current-name))
       (venv-workon ,name) ;; switch it up
       (cd venv-current-dir)
       ,@forms ;; evalulate forms
       (if prev-env ;; switch back
           (venv-workon prev-env)
           (venv-deactivate))
       (cd prev-dir))))

(defmacro venv-allvirtualenv (&rest forms)
  "For each virtualenv, activate it, switch to its directory,
and then evaluate FORMS."
  `(progn
     (--each (venv-get-candidates)
             (venv-with-virtualenv it
                                   ,@forms))))

(defun venv-with-virtualenv-shell-command (name command)
  "Execute the string COMMAND in virtualenv NAME."
  (venv-with-virtualenv name
                        (shell-command command)))

(defun venv-allvirtualenv-shell-command (&optional command)
  "Just like venv-allvirtulenv, but executes a shell
command (COMMAND) rather than elisp forms."
  (interactive)
  (when (not command)
    (setq command (read-from-minibuffer "Shell command to execute: ")))
  (-map (lambda (name)
          (venv-with-virtualenv-shell-command name command))
        (venv-get-candidates))
  (message (concat "Executed " command " in all virtualenvs")))


;; Code for setting up interactive shell and eshell

;; interactive shell

;;;###autoload
(defun venv-shell-init (process)
  "Activate the current virtualenv in a newly opened shell."
  (comint-send-string
   process
   (concat "if command -v workon >/dev/null 2>&1; then workon "
           venv-current-name
           "; else source "
           venv-current-dir venv-executables-dir
           "/activate; fi \n")))

;;;###autoload
(defun venv-initialize-interactive-shells ()
  "Configure interactive shells for use with
virtualenvwrapper.el."
    (defadvice shell (around strip-env ())
      "Use the environment without the venv to start up a new shell."
      (let* ((buffer-name (or buffer "*shell*"))
             (buffer-exists-already (get-buffer buffer-name)))
        (if (or buffer-exists-already (not venv-current-name))
            ad-do-it
          (progn (setenv "PATH" (s-join path-separator (venv-get-stripped-path
                                         (s-split path-separator (getenv "PATH")))))
                 (setenv "VIRTUAL_ENV" nil)
                 ad-do-it
                 (venv-shell-init buffer-name)
               (setenv "PATH" (concat venv-current-dir venv-executables-dir path-separator (getenv "PATH")))
               (setenv "VIRTUAL_ENV" venv-current-dir)))))
    (ad-activate 'shell))


;; eshell

(eval-and-compile
  (defun venv--gen-fun (command)
    `(defun ,(intern (format "pcomplete/eshell-mode/%s" command)) ()
       (pcomplete-here* (venv-get-candidates)))))

(defmacro venv--make-pcompletions (commands)
  `(progn ,@(-map #'venv--gen-fun commands)))

(defun venv-initialize-eshell ()
  "Configure eshell for use with virtualenvwrapper.el."
  ;; make emacs and eshell share an environment
  (setq eshell-modify-global-environment t)
  ;; set eshell path
  (setq eshell-path-env (getenv "PATH"))
  ;; alias functions
  (defun eshell/workon (arg) (venv-workon arg))
  (defun eshell/deactivate () (venv-deactivate))
  (defun eshell/rmvirtualenv (&rest args) (apply #'venv-rmvirtualenv args))
  (defun eshell/mkvirtualenv (&rest args) (apply #'venv-mkvirtualenv args))
  (defun eshell/cpvirtualenv (&rest args) (apply #'venv-cpvirtualenv args))
  (defun eshell/cdvirtualenv (&optional arg) (venv-cdvirtualenv arg))
  (defun eshell/lsvirtualenv () (venv-list-virtualenvs))
  (defun eshell/allvirtualenv (&rest command)
    (venv-allvirtualenv-shell-command
     (s-join " " (eshell-stringify-list command))))
  ;; make completions work
  (venv--make-pcompletions ("workon" "rmvirtualenv"
                            "cdvirtualenv" "cpvirtualenv"))
  (message "Eshell virtualenv support initialized."))


(provide 'virtualenvwrapper)
;;; virtualenvwrapper.el ends here

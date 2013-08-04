;;; virtualenvwrapper.el --- a modern virtualenv tool for Emacs

;; Copyright (C) 2013 James J Porter

;; Author: James J Porter <porterjamesj@gmail.com>
;; URL: http://github.com/porterjamesj/virtualenvwrapper.el
;; Version: 0.0.1
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
;; - Add autocompletion of virtualenvs at the eshell prompt.
;; - Add ability to create and destroy multiple virtualenvs
;;   at once.


;;; Code:

;; customizable variables

(defgroup virtualenvwrapper nil
  "Virtualenvwrapper for Emacs."
  :group 'python)

(defcustom venv-location
  (expand-file-name "~/.virtualenvs/")
  "The location(s) of your virtualenvs. This
can be either a string, which indicates a single directory in which
you keep all your virutalenvs, or a list of strings, in which case it
specifies disparate locations in which all your virtualenvs are kept."
  :group 'virtualenvwrapper)


;; internal variables that you probably shouldn't mess with

(defvar venv-history nil "The history of venvs we have worked on.")

(defvar venv-current-name nil "Name of current virtualenv.")

(defvar venv-current-dir nil "Directory of current virtualenv.")


;; internal utility functions

(defun venv-clear-history ()
  (setq venv-history nil))

(defun venv-dir-to-name (dir)
  "Extract the name of a virtualenv from a path."
  (car (last (-filter (lambda (s) (not (s-blank? s)))
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
                        (expand-file-name potential-dir)) "bin")))
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
                                     (expand-file-name s)) "bin"))))
                  list)))

(defun venv-get-candidates-dir (dir)
  "Given a directory DIR containing virtualenvs, return a list
of names that can be used in the completing read."
  (let ((proper-dir (file-name-as-directory (expand-file-name dir))))
    (-filter (lambda (s)
               (let ((subdir (concat proper-dir s)))
                 (car (file-attributes
                       (concat (file-name-as-directory subdir) "bin")))))
             (directory-files proper-dir nil "^[^.]"))))

(defun venv-get-stripped-path (path)
  "Return what the PATH would look like if we weren't in a
virtualenv. PATH should be a list of strings specifiying directories."
  (let ((func (if (stringp venv-location)
                  (lambda (s) (not (s-contains?
                                    (file-name-as-directory
                                     (expand-file-name
                                      venv-location)) s)))
                (lambda (execs)
                  (not (-filter (lambda (locs)
                                  (s-contains?
                                   (file-name-as-directory
                                    (expand-file-name locs))
                                   (file-name-as-directory
                                    (expand-file-name execs))))
                                venv-location))))))
  (-filter func path)))

(defun venv-is-valid (name)
  "Test if NAME is a valid virtualenv specifier"
  (-contains? (venv-get-candidates) name))

(defun venv-read-name (prompt)
  "Do a completing read to get the name of a candidate,
prompting the user with the string PROMPT"
  (let ((candidates (venv-get-candidates)))
    (completing-read prompt
                     candidates nil t nil
                     'venv-history
                     (or (car venv-history)
                         (car candidates)))))

(defun venv-list-virtualenvs ()
  (s-join "\n" (venv-get-candidates)))


;; potentially interactive user-exposed functions

(defun venv-deactivate ()
  "Deactivate the current venv."
  (interactive)
  (setq python-shell-virtualenv-path nil)
  (setq exec-path (venv-get-stripped-path exec-path))
  (setenv "PATH" (s-join ":"
                  (venv-get-stripped-path
                   (s-split ":" (getenv "PATH")))))
  (setenv "VIRTUAL_ENV" nil)
  (setq venv-current-name nil)
  (setq venv-current-dir nil)
  (setq eshell-path-env (getenv "PATH"))
  (message "virtualenv deactivated"))

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
  (setq venv-current-dir
        (venv-name-to-dir venv-current-name))
  ;; push it onto the history
  (add-to-list 'venv-history venv-current-name)
  ;; setup the python shell
  (setq python-shell-virtualenv-path venv-current-dir)
  ;; setup emacs exec-path
  (add-to-list 'exec-path (concat venv-current-dir "bin"))
  ;; setup the environment for subprocesses
  (setenv "PATH" (concat venv-current-dir "bin:" (getenv "PATH")))
  ;; keep eshell path in sync
  (setq eshell-path-env (getenv "PATH"))
  (setenv "VIRTUAL_ENV" venv-current-dir)
  (message (concat "Switched to virtualenv: " venv-current-name)))

(defun venv-mkvirtualenv (&optional name)
"Create new virtualenv NAME. If venv-location is a single
directory, the new virtualenv is made there; if it is a list of
directories, the new virtualenv is made in the current
default-directory."
  (interactive)
  (let ((parent-dir (if (stringp venv-location)
                        (file-name-as-directory
                         (expand-file-name venv-location))
                      default-directory)))
    (when (not name)
      (setq name (read-from-minibuffer "New virtualenv: ")))
    ;; error if this env already exists
    (when (-contains? (venv-get-candidates) name)
      (error "A virtualenv with this name already exists!"))
    ;; should this be asynchronous?
    (shell-command (concat "virtualenv " parent-dir name))
    (when (listp venv-location)
        (add-to-list 'venv-location (concat parent-dir name)))
    (venv-workon name)
    (message (concat "Created virtualenv: " name))))

(defun venv-rmvirtualenv (&optional name)
"Delete virtualenv NAME."
  (interactive)
  ;; deactivate first
  (venv-deactivate)
  (if name
      (when (not (venv-is-valid name))
        (error "Invalid virtualenv specified!"))
    (setq name (venv-read-name "Virtualenv to delete: ")))
  (delete-directory (venv-name-to-dir name) t)
  ;; get it out of the history so it doesn't show up in completing reads
  (setq venv-history (-filter
                      (lambda (s) (not (s-equals? s name))) venv-history))
  ;; if location is a list, delete it from the list
  (when (listp venv-location)
    (setq venv-location
          (-filter (lambda (locs) (not (s-equals?
                                        name
                                        (venv-dir-to-name locs))))
                   venv-location)))
  (message (concat "Deleted virtualenv: " name)))

(defun venv-lsvirtualenv ()
  "List all available virtualenvs in a temp buffer."
  (interactive)
  (with-output-to-temp-buffer
      "*Virtualenvs*"
      (princ (venv-list-virtualenvs))))

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
        (message (concat "Now in directory: " going-to)))
    (error "No virtualenv is currently active.")))

(defun venv-cpvirtualenv (&optional name newname)
  "Copy virtualenv NAME to NEWNAME. Any arguments not passed will be
prompprted for This comes with the same caveat as cpvirtualenv in the
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
    (venv-workon newname)))


;; macros and functions supporting executing elisp or
;; shell commands in a particular venv

(defmacro venv-with-virtualenv (name &rest forms)
  "Evaluate FORMS with venv NAME active. NAME must be a string
identifying a virtualenv."
  `(progn
     (let ((prev-dir default-directory))
       (venv-workon ,name)
       (cd venv-current-dir)
       ,@forms
       (venv-deactivate)
       (cd prev-dir)
       (message (concat "Evaluated in venv: " ,name)))))

(defmacro venv-allvirtualenv (&rest forms)
  "For each virtualenv, activate it, switch to its directory,
and then evaluate FORMS."
  `(progn
     (-map (lambda (name)
             (venv-with-virtualenv name
                                   ,@forms))
           (venv-get-candidates))
     (message "Ran command in all virtualenvs.")))

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


;; Code for setting up shell and eshell

(defun venv-shell-init (process)
  "Activate the current virtualenv in a newly opened shell."
  (comint-send-string
   process
   (concat "if command -v workon >/dev/null 2>&1; then workon "
           venv-current-name
           "; else source "
           venv-current-dir
           "bin/activate; fi \n")))

(defun venv-initialize-interactive-shells ()
  "Configure interactive shells for use with
virtualenvwrapper.el."
    (defadvice shell (around strip-env ())
      "Use the environment without the venv to start up a new shell."
      (let* ((buffer-name (or buffer "*shell*"))
             (buffer-exists-already (get-buffer buffer-name)))
        (if (or buffer-exists-already (not venv-current-name))
            ad-do-it
          (progn (setenv "PATH" (s-join ":" (venv-get-stripped-path
                                         (s-split ":" (getenv "PATH")))))
                 (setenv "VIRTUAL_ENV" nil)
                 ad-do-it
                 (venv-shell-init buffer-name)
               (setenv "PATH" (concat venv-current-dir "bin:" (getenv "PATH")))
               (setenv "VIRTUAL_ENV" venv-current-dir)))))
    (ad-activate 'shell))

(defun venv-initialize-eshell ()
  "Configure eshell for use with virtualenvwrapper.el."
  ;; make emacs and eshell share an environment
  (setq eshell-modify-global-environment t)
  ;; set eshell path
  (setq eshell-path-env (getenv "PATH"))
  ;; alias functions
  (defun eshell/workon (arg) (venv-workon arg))
  (defun eshell/deactivate () (venv-deactivate))
  (defun eshell/rmvirtualenv (arg) (venv-rmvirtualenv arg))
  (defun eshell/mkvirtualenv (arg) (venv-mkvirtualenv arg))
  (defun eshell/cpvirtualenv (arg) (venv-cpvirtualenv arg))
  (defun eshell/cdvirtualenv (&optional arg) (venv-cdvirtualenv arg))
  (defun eshell/lsvirtualenv () (venv-list-virtualenvs))
  (defun eshell/allvirtualenv (&rest command)
    (venv-allvirtualenv-shell-command
     (s-join " " (eshell-stringify-list command))))
  (message "Eshell virtualenv support initialized."))


(provide 'virtualenvwrapper)
;;; venvwrapper.el ends here

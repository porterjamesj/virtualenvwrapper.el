;; Rudimentary test suite for virtualenvwrapper.el

(load (expand-file-name "virtualenvwrapper.el" default-directory))
(require 's)
(require 'noflet)
(require 'with-simulated-input)

;; unclear why this is required, we get `(void-function string-trim)'
;; errors without, probably has something to do with byte-compiling
(require 'subr-x)

(setq venv-tmp-env "emacs-venvwrapper-test")

(defmacro with-temp-location (&rest forms)
  `(let ((venv-location temporary-file-directory))
     (unwind-protect
         (progn
           ,@forms))))


(defmacro with-temp-env (name &rest forms)
  `(let ((venv-location temporary-file-directory))
     (unwind-protect
         (progn
           (venv-mkvirtualenv ,name)
           ,@forms)
       (venv-rmvirtualenv ,name))))


(defmacro with-temp-dir (&rest forms)
  `(let ((temp-dir (file-name-as-directory (make-temp-file nil t))))
     (unwind-protect
         (progn
           ,@forms)
       (delete-directory temp-dir t))))


(defun assert-venv-activated ()
  "Runs various assertions to check if a venv is activated."
  ;; M-x pdb should ask to run "python -m pdb"
  (should (equal gud-pdb-command-name "python -m pdb"))
  ;; we store the name correctly
  (should (s-contains? venv-tmp-env venv-current-name))
  ;; assert that the current dir exists and is asbolute
  (should (file-name-absolute-p venv-current-dir))
  (should (file-directory-p venv-current-dir))
  ;; we change the path for python mode
  (should (s-contains? venv-tmp-env python-shell-virtualenv-path))
  ;; we set PATH for shell and subprocesses
  (should (s-contains? venv-tmp-env (getenv "PATH")))
  ;; we set VIRTUAL_ENV for jedi and whoever else needs it
  (should (s-contains? venv-tmp-env (getenv "VIRTUAL_ENV")))
  ;; we add our dir to exec-path
  (should (s-contains? venv-tmp-env (car exec-path))))

(ert-deftest venv-mkvirtualenv-works ()
  (with-temp-location
   (venv-mkvirtualenv venv-tmp-env)
   (should (equal venv-current-name venv-tmp-env))
   (venv-deactivate)
   (venv-rmvirtualenv venv-tmp-env)))

(ert-deftest venv-rmvirtualenv-works ()
  (let ((venv-location temporary-file-directory))
    (venv-mkvirtualenv venv-tmp-env)
    (venv-deactivate)
    (venv-rmvirtualenv venv-tmp-env)
    (should-error (venv-workon venv-tmp-env))))

(ert-deftest venv-mkvirtualenv-select-default-interpreter ()
  (with-temp-location
   (let ((current-prefix-arg '(4)))
     (with-simulated-input
      "RET"
      (venv-mkvirtualenv venv-tmp-env))
     (should (equal venv-current-name venv-tmp-env))
     (venv-deactivate)
     (venv-rmvirtualenv venv-tmp-env))))

(ert-deftest venv-mkvirtualenv-select-different-interpreter ()
  (with-temp-location
   (let ((current-prefix-arg '(4)))
     (with-simulated-input
         '((insert (executable-find "python")) "RET")
       (venv-mkvirtualenv venv-tmp-env))
     (should (equal venv-current-name venv-tmp-env))
     (venv-deactivate)
     (venv-rmvirtualenv venv-tmp-env))))

(ert-deftest venv-mkvirtualenv-using-default-interpreter-works ()
  (with-temp-location
   (venv-mkvirtualenv-using nil venv-tmp-env)
   (should (equal venv-current-name venv-tmp-env))
   (venv-deactivate)
   (venv-rmvirtualenv venv-tmp-env)))

(ert-deftest venv-mkvirtualenv-using-different-interpreter-works ()
  (with-temp-location
   (venv-mkvirtualenv-using (executable-find "python") venv-tmp-env)
   (should (equal venv-current-name venv-tmp-env))
   (venv-deactivate)
   (venv-rmvirtualenv venv-tmp-env)))

(ert-deftest venv-mkvirtualenv-using-select-default-interpreter ()
  (with-temp-location
   (with-simulated-input
    "RET"
   (let ((current-prefix-arg '(4)))
     (venv-mkvirtualenv-using "some invalid interpreter" venv-tmp-env)))
   (should (equal venv-current-name venv-tmp-env))
   (venv-deactivate)
   (venv-rmvirtualenv venv-tmp-env)))

(ert-deftest venv-mkvirtualenv-using-select-different-interpreter ()
  (with-temp-location
   (with-simulated-input
       '((insert (executable-find "python")) "RET")
     (let ((current-prefix-arg '(4)))
       (venv-mkvirtualenv-using "some invalid interpreter" venv-tmp-env)))
   (should (equal venv-current-name venv-tmp-env))
   (venv-deactivate)
   (venv-rmvirtualenv venv-tmp-env)))

(ert-deftest venv-workon-works ()
  (with-temp-env
   venv-tmp-env
   (venv-deactivate)
   (venv-workon venv-tmp-env)
   (assert-venv-activated)))

(ert-deftest venv-deactivate-works ()
  (with-temp-env
   venv-tmp-env
   (venv-deactivate)
   ;; M-x pdb should ask to run "pdb"
   (should (equal gud-pdb-command-name "pdb"))
   ;; we remove the name correctly
   (should (equal venv-current-name nil))
   ;; we change the python path back
   (should (equal python-shell-virtualenv-path nil)))
   ;; we reset the PATH correctly
   (should (not (s-contains? venv-tmp-env (getenv "PATH"))))
   ;; we reset VIRTUAL_ENV
   (should (equal nil (getenv "VIRTUAL_ENV")))
   ;; we remove out dir to exec-path
   (should (not (s-contains? venv-tmp-env (car exec-path)))))

(ert-deftest venv-workon-errors-for-nonexistence ()
  (should-error (venv-workon "i-hopefully-do-not-exist")))

(ert-deftest venv-list-virtualenvs-works ()
  (with-temp-env
   venv-tmp-env
   (should (s-contains? venv-tmp-env (venv-list-virtualenvs)))))

(ert-deftest venv-cdvirtualenv-works ()
  (with-temp-env
   venv-tmp-env
   (let ((old-wd default-directory))
     (unwind-protect
         (progn
           (venv-cdvirtualenv)
           (should (s-contains? venv-tmp-env default-directory)))
       (cd old-wd)))))

(ert-deftest venv-cpvirtualenv-works ()
  (with-temp-env
   venv-tmp-env
   (unwind-protect
       (progn
         (venv-cpvirtualenv venv-tmp-env "copy-of-tmp-env")
         (should (s-contains? "copy-of-tmp-env" (venv-list-virtualenvs))))
     (venv-rmvirtualenv "copy-of-tmp-env"))))


;; tests for hooks


(ert-deftest venv-activate-hooks ()
  (let ((preactivate nil)
        (postactivate nil)
        (venv-preactivate-hook '((lambda () (setq preactivate "yes"))))
        (venv-postactivate-hook '((lambda () (setq postactivate "yes")))))
  (with-temp-env
   venv-tmp-env
   (should (equal preactivate "yes"))
   (should (equal postactivate "yes")))))

(ert-deftest venv-mkvenv-hooks ()
  (let ((venv-premkvirtualenv-hook '((lambda ()
                                       (setq preactivated "yes"))))
        (venv-postmkvirtualenv-hook '((lambda ()
                                        (setq postactivated "yes")
                                        (setq name venv-current-name)))))
    (with-temp-env
     venv-tmp-env
     (venv-deactivate)
     (should (equal preactivated "yes"))
     (should (equal postactivated "yes"))
     (should (equal name venv-tmp-env)))))

(ert-deftest venv-set-location-works ()
  (let ((expected-venv-location "test location")
        (original-venv-location venv-location))
    (venv-set-location expected-venv-location)
    (should (equal venv-location expected-venv-location))
    (setq venv-location original-venv-location)))

(ert-deftest venv-projectile-auto-workon-works ()
  (with-temp-env
    venv-tmp-env
    ;; the reason for setting a bogus venv-location here is that the
    ;; venv-location shouldn't matter, projectile-auto-workon should happen
    ;; indepedent of it's being set or not
    (let ((venv-location "bogus"))
      (noflet ((projectile-project-root () temporary-file-directory))
        (setq venv-dirlookup-names (list venv-tmp-env))
        (venv-deactivate)
        (venv-projectile-auto-workon)
        (assert-venv-activated)))))

(ert-deftest venv-test-auto-cd-to-project-dir-works ()
  (with-temp-env
    venv-tmp-env
    (with-temp-dir
      (venv-deactivate)
      (should (not (equal default-directory temp-dir)))
      ;; set the project dir to be `temp-dir'
      (append-to-file temp-dir nil
        (s-concat (venv-name-to-dir venv-tmp-env) ".project"))
      (venv-workon venv-tmp-env)
      ;; TODO should probably set these up to reset current-directory
      ;; when done for hygeine purposes
      (should (equal default-directory temp-dir)))))

(ert-deftest venv-test-workon-does-not-cd-to-project-when-disabled ()
  (with-temp-env
    venv-tmp-env
    (with-temp-dir
      (let ((venv-workon-cd nil))
        (venv-deactivate)
        (should (not (equal default-directory temp-dir)))
        ;; set the project dir to be `temp-dir'
        (append-to-file temp-dir nil
          (s-concat (venv-name-to-dir venv-tmp-env) ".project"))
        (venv-workon venv-tmp-env)
        (should (not (equal default-directory temp-dir)))))))

(ert-deftest venv-projectile-auto-workon-works-with-text-file ()
  (with-temp-env
   venv-tmp-env
   ;; the reason for setting a bogus venv-location here is that the
   ;; venv-location shouldn't matter, projectile-auto-workon should happen
   ;; indepedent of it's being set or not
   (let* ((venv-location "bogus")
          ;; Create a file in the projectile-project-root with
          ;; the text content of the venv to be activated
          (venv-tmp-text-file (make-temp-file "venv" nil nil venv-tmp-env))
          (venv-tmp-text-name (file-name-nondirectory venv-tmp-text-file)))
     (noflet ((projectile-project-root () temporary-file-directory))
       (setq venv-dirlookup-names (list venv-tmp-text-name))
       (venv-deactivate)
       (venv-projectile-auto-workon)
       (assert-venv-activated)
       (delete-file venv-tmp-text-file)))))

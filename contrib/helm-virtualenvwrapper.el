;;; helm-virtualenvwrapper.el --- A helm-source for virtualenvwrapper.el

;; Copyright (C) 2014 Javier Olaechea

;;; Commentary:

;; To start using define a python-local keybinding or a global one such as:
;;
;; (define-key python-mode-map (kbd "C-c v") 'helm-venv-workon)
;; (global-set-key (kbd "C-c v") 'helm-venv-workon)
;;
;; Then C-c v away


;;;###autoload
(defun helm-venv-workon ()
  "Like venv-work, for helm."
  (interactive)
  (helm :sources '(helm-source-venv)))

(defvar helm-source-venv
  `((name . "Virtual env completion")
    (candidates . ,(cl-loop
                    for venv in (venv-get-candidates)
                    collect (cons venv venv)))
    (action . (("activate" . venv-workon)))
    (persistent-action . venv-workon)
    (persistent-help . "Activate the virtualenv.")))

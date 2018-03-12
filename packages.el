;;; packages.el --- elpy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(setq elpy-packages
      '(
        company
        ;; counsel-gtags
        cython-mode
        elpy
        evil-matchit
        flycheck
        (nose :location local)
        ;; nose
        ;; ob-ipython
        org
        pony-mode
        ;; py-autopep8
        py-isort
        ;; pyenv-mode
        ;; pygen
        (pylookup :location local)
        pytest
        ;; (python :location built-in)
        ;; pyvenv
        ;; vimish-fold
        yapfify
        ))

(setq elpy-excluded-packages '())

(defun elpy/init-elpy ()
  (use-package elpy
    :diminish elpy-mode
    :init
    (progn
      (spacemacs/register-repl 'python 'spacemacs/elpy-start-or-switch-repl "python")
      (add-hook 'python-mode-hook #'spacemacs//elpy-default)
      ;; call `spacemacs//python-setup-shell' once, don't put it in a hook
      ;; (see issue #5988)
      (spacemacs//elpy-setup-shell))
    :config

    ;; Elpy removes the modeline lighters. Let's override this
    (defun elpy-modules-remove-modeline-lighter (mode-name))

    (setq elpy-modules '(elpy-module-sane-defaults
                         elpy-module-company
                         elpy-module-eldoc
                         ;; elpy-module-flymake
                         elpy-module-highlight-indentation
                         elpy-module-pyvenv
                         elpy-module-yasnippet
                         elpy-module-django))

    ;; (when (configuration-layer/layer-usedp 'auto-completion)
    ;;   (add-to-list 'elpy-modules 'elpy-module-company)
    ;;   (add-to-list 'elpy-modules 'elpy-module-yasnippet))

    ;; (setq elpy-modules (delq 'elpy-model-company elpy-modules))
    (elpy-enable)

    ;; (add-hook 'python-mode-hook
    ;;           (lambda ()
    ;;             (company-mode)
    ;;             (add-to-list 'company-backends
    ;;                          (company-mode/backend-with-yas 'elpy-company-backend))))

    (spacemacs/declare-prefix-for-mode 'python-mode "mc" "execute")
    (spacemacs/declare-prefix-for-mode 'python-mode "md" "debug")
    (spacemacs/declare-prefix-for-mode 'python-mode "me" "errors")
    (spacemacs/declare-prefix-for-mode 'python-mode "mp" "project")
    (spacemacs/declare-prefix-for-mode 'python-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'python-mode "mi" "pygen")
    (spacemacs/declare-prefix-for-mode 'python-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'python-mode "mj" "pony")
    (spacemacs/declare-prefix-for-mode 'python-mode "ms" "send to REPL")
    (spacemacs/declare-prefix-for-mode 'python-mode "mt" "test")
    (spacemacs/declare-prefix-for-mode 'python-mode "mr" "refactor")
    (spacemacs/declare-prefix-for-mode 'python-mode "mv" "pyvenv")
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "'" 'spacemacs/elpy-start-or-switch-repl
      "cc" 'spacemacs/elpy-execute-file
      "cC" 'spacemacs/elpy-execute-file-focus
      "db" 'spacemacs/elpy-toggle-breakpoint
      "ec" 'elpy-check
      ;; "en" 'elpy-flymake-next-error
      ;; "ep" 'elpy-flymake-previous-error
      ;; "el" 'elpy-flymake-show-error
      ;; "ee" 'elpy-flymake-error-at-point
      "gb" 'spacemacs/elpy-go-back
      "gg" 'spacemacs/elpy-goto-definition
      "gG" 'elpy-goto-definition-other-window
      "gl" 'elpy-goto-location
      "go" 'elpy-occur-definitions
      "hh" 'elpy-doc
      "pf" 'elpy-find-file
      "pc" 'spacemacs/elpy-django-command
      "pr" 'spacemacs/elpy-django-runserver
      "pi" 'spacemacs/elpy-remove-unused-imports
      "re" 'elpy-multiedit-python-symbol-at-point
      "rf" 'elpy-format-code
      "ri" 'elpy-importmagic-fixup
      "rp" 'spacemacs/elpy-remove-unused-imports
      "rr" 'elpy-refactor
      "sb" 'elpy-shell-send-region-or-buffer
      "sf" 'python-shell-send-defun
      "si" 'spacemacs/python-start-or-switch-repl
      "sk" 'elpy-shell-kill
      "sr" 'elpy-shell-send-region-or-buffer
      ;; "tt" 'spacemacs/python-test-and-switch
      "va" 'pyvenv-activate
      "vd" 'pyvenv-deactivate
      "vw" 'pyvenv-workon
      "vr" 'pyvenv-restart-python
      )
    ))

(defun elpy/post-init-company ()
  (spacemacs|add-company-hook python-mode)
  (spacemacs|add-company-hook inferior-python-mode)
  (push '(company-files company-capf) company-backends-inferior-python-mode)
  (add-hook 'inferior-python-mode-hook (lambda ()
                                         (setq-local company-minimum-prefix-length 0)
                                         (setq-local company-idle-delay 0.5))))

(defun elpy/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'python-mode))

(defun elpy/post-init-ggtags ()
  (add-hook 'python-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun elpy/init-cython-mode ()
  (use-package cython-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'cython-mode
        "hh" 'elpy-doc))))

(defun elpy/post-init-evil-matchit ()
  (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(defun elpy/post-init-flycheck ()
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  )

(defun elpy/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode
    :init
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "l" 'live-py-mode)))

(defun elpy/init-nose ()
  (use-package nose
    :commands (nosetests-one
               nosetests-pdb-one
               nosetests-all
               nosetests-pdb-all
               nosetests-module
               nosetests-pdb-module
               nosetests-suite
               nosetests-pdb-suite)
    ;; :init (spacemacs//bind-python-testing-keys)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil))))

(defun elpy/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(python . t))))

(defun elpy/init-pony-mode ()
  (use-package pony-mode
    :defer t
    :init (progn
            (spacemacs/set-leader-keys-for-major-mode 'python-mode
                                        ; d*j*ango f*a*bric
              "jaf" 'pony-fabric
              "jad" 'pony-fabric-deploy
                                        ; d*j*ango *f*iles
              "jfs" 'pony-goto-settings
              "jfc" 'pony-setting
              "jft" 'pony-goto-template
              "jfr" 'pony-resolve
                                        ; d*j*ango *i*nteractive
              "jid" 'pony-db-shell
              "jis" 'pony-shell
                                        ; d*j*ango *m*anage
                                        ; not including one-off management commands like "flush" and
                                        ; "startapp" even though they're implemented in pony-mode,
                                        ; because this is much handier
              "jm" 'pony-manage
                                        ; d*j*ango *r*unserver
              "jrd" 'pony-stopserver
              "jro" 'pony-browser
              "jrr" 'pony-restart-server
              "jru" 'pony-runserver
              "jrt" 'pony-temp-server
                                        ; d*j*ango *s*outh/*s*yncdb
              "jsc" 'pony-south-convert
              "jsh" 'pony-south-schemamigration
              "jsi" 'pony-south-initial
              "jsm" 'pony-south-migrate
              "jss" 'pony-syncdb
                                        ; d*j*ango *t*est
              "jtd" 'pony-test-down
              "jte" 'pony-test-goto-err
              "jto" 'pony-test-open
              "jtt" 'pony-test
              "jtu" 'pony-test-up))))

;; (defun elpy/post-init-py-autopep8 ()
;;   (use-package py-autopep8
;;     :defer t
;;     :init
;;     (progn
;;       (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))))


(defun elpy/init-py-isort ()
  (use-package py-isort
    :defer t
    :init
    (progn
      (add-hook 'before-save-hook 'spacemacs//elpy-sort-imports)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "rI" 'py-isort-buffer))))

(defun elpy/init-pygen ()
  (use-package pygen
    :defer t
    :init
    (progn
      (add-hook 'python-mode-hook 'pygen-mode)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "ic" 'pygen-generate-class
        "if" 'pygen-generate-function
        "ik" 'pygen-make-keyword-argument
        "isa" 'pygen-make-sequence-argument
        "isf" 'pygen-generate-static-function
        "iss" 'pygen-insert-super
        "ist" 'pygen-toggle-selfify-symbol
        "iv" 'pygen-extract-variable
        "i@" 'pygen-add-decorator-to-function
        ))))

(defun elpy/init-pylookup ()
  (use-package pylookup
    :commands (pylookup-lookup pylookup-update pylookup-update-all)
    :init
    (progn
      (evilified-state-evilify pylookup-mode pylookup-mode-map)
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "hH" 'pylookup-lookup))
    :config
    (progn
      (let ((dir (configuration-layer/get-layer-local-dir 'elpy)))
        (setq pylookup-dir (concat dir "pylookup/")
              pylookup-program (concat pylookup-dir "pylookup.bat")
              pylookup-db-file (concat pylookup-dir "pylookup.db")))
      (setq pylookup-completing-read 'completing-read))))

(defun elpy/init-pytest ()
  (use-package pytest
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-module
               pytest-pdb-module)
    :init (spacemacs//bind-elpy-testing-keys)
    :config (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun elpy/init-vimish-fold ()
  (use-package vimish-fold
    :config
    (progn
      (setq evil-fold-list (cl-remove-if
                            #'(lambda (e) (eq (caar e) 'python-mode))
                            evil-fold-list))
      (add-to-list 'evil-fold-list
                   `((python-mode)
                     :delete     vimish-fold-delete
                     :open-all   vimish-fold-unfold-all
                     :close-all  vimish-fold-refold-all
                     :toggle     vimish-fold-toggle
                     :open       vimish-fold-unfold
                     :open-rec   nil
                     :close      vimish-fold-refold)))))

(defun elpy/init-yapfify ()
  (use-package yapfify
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "=" 'yapfify-buffer)
      (when python-enable-yapf-format-on-save
        (add-hook 'python-mode-hook 'yapf-mode)))
    :config (spacemacs|hide-lighter yapf-mode)))

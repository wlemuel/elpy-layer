;;; funcs.el --- Elpy Layer functions File for Spacemacs

(defun spacemacs/python-setup-shell (&rest args)
  (if (spacemacs/pyenv-executable-find "ipython")
      (progn (setq python-shell-interpreter "ipython")
             (if (version< (replace-regexp-in-string "\n$" "" (shell-command-to-string "ipython --version")) "5")
                 (setq python-shell-interpreter-args "-i")
               (setq python-shell-interpreter-args "--simple-prompt -i")))
    (progn
      (setq python-shell-interpreter-args "-i")
      (setq python-shell-interpreter "python"))))

(defun spacemacs/pyenv-executable-find (command)
  "Find executable taking pyenv shims into account."
  (if (executable-find "pyenv")
      (progn
        (let ((pyenv-string (shell-command-to-string (concat "pyenv which " command))))
          (unless (string-match "not found" pyenv-string)
            pyenv-string)))
    (executable-find command)))


(defun spacemacs/python-toggle-breakpoint ()
  "Add a break point, highlight it."
  (interactive)
  (let ((trace (cond ((spacemacs/pyenv-executable-find "wdb") "import wdb; wdb.set_trace()")
                     ((spacemacs/pyenv-executable-find "ipdb") "import ipdb; ipdb.set_trace()")
                     ((spacemacs/pyenv-executable-find "pudb") "import pudb; pudb.set_trace()")
                     ((spacemacs/pyenv-executable-find "ipdb3") "import ipdb; ipdb.set_trace()")
                     ((spacemacs/pyenv-executable-find "pudb3") "import pudb; pudb.set_trace()")
                     (t "import pdb; pdb.set_trace()")))
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (insert "\n")
        (python-indent-line)))))

(defun spacemacs/python-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))


(defun spacemacs//python-sort-imports ()
  ;; py-isort-before-save checks the major mode as well, however we can prevent
  ;; it from loading the package unnecessarily by doing our own check
  (when (and python-sort-imports-on-save
             (derived-mode-p 'python-mode))
    (py-isort-before-save)))

(defun spacemacs/elpy-goto-definition ()
  (interactive)
  (condition-case ex
      (elpy-goto-definition)
    ('error (message (format "%s" ex)))))

(defun spacemacs/python-shell-send-buffer-switch ()
  " Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (elpy-shell-send-region-or-buffer)
  (elpy-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/python-shell-send-defun-switch ()
  "Send function content to shell and switch to it in insert mode."
  (interactive)
  (python-shell-send-defun nil)
  (elpy-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/python-start-or-switch-repl ()
  " Switch to shell in insert mode."
  (interactive)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/elpy-start-or-switch-repl ()
  " Switch to elpy shell in insert mode."
  (interactive)
  (elpy-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/python-test-and-switch ()
  " Run tests and switch to test-buffer."
  (interactive)
  (elpy-test)
  (switch-to-buffer-other-window "*compilation*"))

(defun spacemacs/python-shell-send-region-switch (start end)
  " Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/python-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "python %s" (file-name-nondirectory
                                              buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)))))

(defun spacemacs/python-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
  (interactive "P")
  (spacemacs/python-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

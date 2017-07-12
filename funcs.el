;;; funcs.el --- Elpy Layer functions File for Spacemacs

(defun spacemacs/elpy-annotate-pdb ()
  "Highlight break point lines."
  (interactive)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\|wdb\\)")
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\|wdb\\).set_trace()"))

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

(defun spacemacs//elpy-setup-checkers (&rest args)
  (when (fboundp 'flycheck-set-checker-executable)
    (let ((pylint (spacemacs/pyenv-executable-find "pylint"))
          (flake8 (spacemacs/pyenv-executable-find "flake8")))
      (when pylint
        (flycheck-set-checker-executable "python-pylint" pylint))
      (when flake8
        (flycheck-set-checker-executable "python-flake8" flake8)))))

(defun spacemacs/elpy-toggle-breakpoint ()
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

(defun spacemacs//pyenv-mode-set-local-version ()
  "Set pyenv version from \".python-version\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".python-version")))
    (when root-path
      (let* ((file-path (expand-file-name ".python-version" root-path))
             (version
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (nth 0 (split-string (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position)))))))
        (if (member version (pyenv-mode-versions))
            (pyenv-mode-set version)
          (message "pyenv: version `%s' is not installed (set by %s)"
                   version file-path))))))


(defun spacemacs/elpy-remove-unused-imports()
  "Use Autoflake to remove unused function"
  "autoflake --remove-all-unused-imports -i unused_imports.py"
  (interactive)
  (if (executable-find "autoflake")
      (progn
        (shell-command (format "autoflake --remove-all-unused-imports -i %s"
                               (shell-quote-argument (buffer-file-name))))
        (revert-buffer t t t))
    (message "Error: Cannot find autoflake executable.")))

(defun spacemacs//pyvenv-mode-set-local-virtualenv ()
  "Set pyvenv virtualenv from \".venv\" by looking in parent directories."
  (interactive)
  (let ((root-path (locate-dominating-file default-directory
                                           ".venv")))
    (when root-path
      (let* ((file-path (expand-file-name ".venv" root-path))
             (virtualenv
              (with-temp-buffer
                (insert-file-contents-literally file-path)
                (buffer-substring-no-properties (line-beginning-position)
                                                (line-end-position)))))
        (pyvenv-workon virtualenv)))))

(defun spacemacs//elpy-sort-imports ()
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

(defun spacemacs/elpy-go-back ()
  (interactive)
  (xref-pop-marker-stack))

(defun spacemacs/elpy-shell-send-buffer-switch ()
  " Send buffer content to shell and switch to it in insert mode."
  (interactive)
  (elpy-shell-send-region-or-buffer)
  (elpy-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/elpy-shell-send-defun-switch ()
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

(defun spacemacs/elpy-test-and-switch ()
  " Run tests and switch to test-buffer."
  (interactive)
  (elpy-test)
  (switch-to-buffer-other-window "*compilation*"))

(defun spacemacs/elpy-shell-send-region-switch (start end)
  " Send region content to shell and switch to it in insert mode."
  (interactive "r")
  (python-shell-send-region start end)
  (python-shell-switch-to-shell)
  (evil-insert-state))

(defun spacemacs/elpy-execute-file (arg)
  "Execute a python script in a shell."
  (interactive "P")
  ;; set compile command to buffer-file-name
  ;; universal argument put compile buffer in comint mode
  (let ((universal-argument t)
        (compile-command (format "%s %s"
                                 (spacemacs/pyenv-executable-find python-shell-interpreter)
                                 (file-name-nondirectory buffer-file-name))))
    (if arg
        (call-interactively 'compile)
      (compile compile-command t)
      (with-current-buffer (get-buffer "*compilation*")
        (inferior-python-mode)
        (evil-normal-state)))))

(defun spacemacs/elpy-execute-file-focus (arg)
  "Execute a python script in a shell and switch to the shell buffer in
`insert state'."
  (interactive "P")
  (spacemacs/elpy-execute-file arg)
  (switch-to-buffer-other-window "*compilation*")
  (end-of-buffer)
  (evil-insert-state))

(defun spacemacs/elpy-test-all (arg)
  "Run all tests."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function arg '((pytest . pytest-all)
                                           (nose . nosetests-all))))

(defun spacemacs/elpy-test-pdb-all (arg)
  "Run all tests in debug mode."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function arg '((pytest . pytest-pdb-all)
                                           (nose . nosetests-pdb-all))))

(defun spacemacs/elpy-test-module (arg)
  "Run all tests in the current module."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function arg '((pytest . pytest-module)
                                           (nose . nosetests-module))))

(defun spacemacs/elpy-test-pdb-module (arg)
  "Run all tests in the current module in debug mode."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function
   arg
   '((pytest . pytest-pdb-module)
     (nose . nosetests-pdb-module))))

(defun spacemacs/elpy-test-suite (arg)
  "Run all tests in the current suite."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function arg '((nose . nosetests-suite))))

(defun spacemacs/elpy-test-pdb-suite (arg)
  "Run all tests in the current suite in debug mode."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function arg '((nose . nosetests-pdb-suite))))

(defun spacemacs/elpy-test-one (arg)
  "Run current test."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function arg '((pytest . pytest-one)
                                           (nose . nosetests-one))))

(defun spacemacs/elpy-test-pdb-one (arg)
  "Run current test in debug mode."
  (interactive "P")
  (spacemacs//elpy-call-correct-test-function arg '((pytest . pytest-pdb-one)
                                           (nose . nosetests-pdb-one))))

(defun spacemacs//bind-elpy-testing-keys ()
  "Bind the keys for testing in Python."
  (spacemacs/declare-prefix-for-mode 'python-mode "mt" "test")
  (spacemacs/set-leader-keys-for-major-mode 'python-mode
    "tA" 'spacemacs/elpy-test-pad-all
    "ta" 'spacemacs/elpy-test-all
    "tB" 'spacemacs/elpy-test-pdb-module
    "tb" 'spacemacs/elpy-test-module
    "tT" 'spacemacs/elpy-test-pdb-one
    "tt" 'spacemacs/elpy-test-one
    "tM" 'spacemacs/elpy-test-pdb-module
    "tm" 'spacemacs/elpy-test-module
    "tS" 'spacemacs/elpy-test-pdb-suite
    "ts" 'spacemacs/elpy-test-suite))

(defun spacemacs//elpy-get-main-testrunner ()
  "Get the main test runner."
  (if (listp python-test-runner) (car python-test-runner) python-test-runner))

(defun spacemacs//elpy-get-secondary-testrunner ()
  "Get the secondary test runner"
  (cdr (assoc (spacemacs//elpy-get-main-testrunner) '((pytest . nose)
                                             (nose . pytest)))))

(defun spacemacs//elpy-call-correct-test-function (arg funcalist)
  "Call a test function based on the chosen test framework.
ARG is the universal-argument which chooses between the main and
the secondary test runner. FUNCALIST is an alist of the function
to be called for each testrunner. "
  (let* ((test-runner (if arg
                          (spacemacs//elpy-get-secondary-testrunner)
                        (spacemacs//elpy-get-main-testrunner)))
         (test-function (assq test-runner funcalist)))
    (if test-function
        (funcall (cdr (assoc test-runner funcalist)))
      (user-error "This test function is not available with the `%S' runner."
                  test-runner))))

(defun spacemacs//elpy-django--get-commands ()
  "Return list of django commands."
  (let ((dj-commands-str nil)
        (help-output
         (shell-command-to-string
          (format "%s %s -h"
                  (spacemacs/pyenv-executable-find python-shell-interpreter)
                  elpy-django-command))))
    (setq dj-commands-str
          (with-temp-buffer
            (progn
              (insert help-output)
              (goto-char (point-min))
              (delete-region (point) (search-forward "Available subcommands:" nil nil nil))
              ;; cleanup [auth] and stuff
              (goto-char (point-min))
              (save-excursion
                (replace-regexp "\\[.*\\]" ""))
              (buffer-string))))
    ;; get a list of commands from the output of manage.py -h
    ;; What would be the pattern to optimize this ?
    (setq dj-commands-str (split-string dj-commands-str "\n"))
    (setq dj-commands-str (-remove (lambda (x) (string= x "")) dj-commands-str))
    (setq dj-commands-str (mapcar (lambda (x) (s-trim x)) dj-commands-str))
    (sort dj-commands-str 'string-lessp)))

(defun spacemacs/elpy-django-command (cmd)
  "Prompt user for Django command. If called with `C-u',
it will prompt for other flags/arguments to run."
  (interactive (list (completing-read "Command: "
                                      (spacemacs//elpy-django--get-commands) nil nil)))
  ;; Called with C-u, variable is set or is a cmd that requires an argument
  (when (or current-prefix-arg
            elpy-django-always-prompt
            (member cmd elpy-django-commands-with-req-arg))
    (setq cmd (concat cmd " " (read-shell-command (concat cmd ": ")))))
  (compile (format "%s %s %s"
                   (spacemacs/pyenv-executable-find python-shell-interpreter)
                   elpy-django-command
                   cmd)))

(defun spacemacs/elpy-django-runserver (arg)
  "Start the server and automatically add the ipaddr and port.
Also create it's own special buffer so that we can have multiple
servers running per project.

When called with a prefix (C-u), it will prompt for additional args."
  (interactive "P")
  (let* ((cmd (format "%s %s %s"
                      (spacemacs/pyenv-executable-find python-shell-interpreter)
                      elpy-django-command
                      elpy-django-server-command))
         (proj-root (file-name-base (directory-file-name (elpy-project-root))))
         (buff-name (format "*runserver[%s]*" proj-root)))
    ;; Kill any previous instance of runserver since we might be doing something new
    (when (get-buffer buff-name)
      (kill-buffer buff-name))
    (setq cmd (concat cmd " " elpy-django-server-ipaddr ":" elpy-django-server-port))
    (when (or arg elpy-django-always-prompt)
      (setq cmd (concat cmd " "(read-shell-command (concat cmd ": ")))))
    (compile cmd)
    (with-current-buffer "*compilation*"
      (rename-buffer buff-name))))

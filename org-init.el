(message "Tangling org-init.org file.")

(message "org-init.org: Creating customize group rhol-emacs/rhol-general")
(defgroup rhol-general nil
  "General customization settings."
  :group 'rhol-emacs
  :tag "General")

(message "org-init.org: Emacs Server")
(add-hook 'after-init-hook
          (lambda ()
            "Start Emacs server if not already running."
            (use-package server
              :config
              (unless (server-running-p)
                (server-start)))))

(message "org-init.org: Fish Environment")

(defun rhol--get-fish-env ()
  "Return the environment variables set by fish config"
  (let ((fish-shell (executable-find "fish")))
    (shell-command-to-string (format "%s -ilc 'set -L | grep -v history'" fish-shell))))

(defun rhol--format-fish-env-for-emacs (env-line)
  "Convert fish set variable to emacs process-environment var.

   ENV-LINE A line from the 'set' command in fish shell.

   Emacs process-environment expects variables in the form of:
   VAR=VAL. But fish outputs them in the form VAR VAL1 VAL2. This
   command converts from the fish syntax to the emacs syntax."
  (let ((space-split-env-var (split-string env-line "[[:space:]]+")))
    (let ((no-single-quotes (mapcar (lambda (s) (remove (aref "'" 0) s)) space-split-env-var)))
      (if (= (length no-single-quotes) 2)
          (format "%s=%s"
                  (car no-single-quotes)
                  (car (last no-single-quotes)))
        (format "%s=%s" (car no-single-quotes)
                (mapconcat 'identity (cdr no-single-quotes) ":"))))))

(defun rhol--fish-apply-set-string (raw-set-str)
  "Set environment variables from env string.

   RAW-SET-STR the output of 'set' in fish shell"
  (let ((line-split-env-strings (split-string raw-set-str "\n")))
    (let ((emacs-env-strings (mapcar 'rhol--format-fish-env-for-emacs  line-split-env-strings)))
      (setq process-environment emacs-env-strings))))

(defun rhol-eval-fish-env ()
  "Overwrite `process-environment' with the environment variables set in fish config."
  (interactive)
  (rhol--fish-apply-set-string (rhol--get-fish-env)))

(defun rhol--get-fish-path ()
  "Return PATH variable after evaluating fish config."
  (let ((line-split-env-strings (split-string (rhol--get-fish-env) "\n")))
    (let ((filtered-env-strings (-filter (lambda (line) (string-prefix-p "PATH " line)) line-split-env-strings)))
      (let ((no-single-quotes (mapcar (lambda (s) (remove (aref "'" 0) s)) filtered-env-strings)))
        (cond
         ((= (length filtered-env-strings) 1)
          (cdr (split-string (car no-single-quotes) "[[:space:]]+")))
         ((> (length no-single-quotes) 1)
          (error "More than one PATH string found in fish env vars: %s" no-single-quotes))
         ((not no-single-quotes)
          (error "No PATH string found in fish env vars")))))))

(defun rhol-eval-fish-path ()
  "Set `exec-path' based on fish PATH variable."
  (interactive)
  (let ((fish-path (rhol--get-fish-path)))
    (let ((fish-path-with-exec (append fish-path (list exec-directory))))
      (setq exec-path fish-path-with-exec))))

(message "org-init.org: Bash Environment")

(defun rhol--get-bash-env ()
  "Return the environment variable string set by .bash_profile."
  (shell-command-to-string "/bin/bash -c \"source ~/.bash_profile; env\""))

(defun rhol--bash-apply-env-string (raw-env-str)
  "Set environment variables from env string.

  RAW-ENV-STR the output of env command"
  (let ((env-strings (split-string raw-env-str "\n")))
    (setq process-environment env-strings)))

(defun rhol-eval-bash-env ()
  "Overwrite `process-environment' with the environment variables set by .bash_profile."
  (interactive)
  (rhol--bash-apply-env-string (rhol--get-bash-env)))

(defun rhol--get-bash-path ()
  "Return PATH variable after evaluating bash profile."
  (replace-regexp-in-string "\n" ""
                            (shell-command-to-string "/bin/bash -c \"source ~/.bash_profile >/dev/null 2>&1; echo $PATH\"")))

(defun rhol-eval-bash-path ()
  "Set exec path based on bash path variable."
  (interactive)
  (let ((env-strings (split-string (rhol--get-bash-path) ":")))
    (setq exec-path env-strings)))

(cond
 ((string= "/usr/bin/fish" (getenv "SHELL"))
  (message "Getting env and path from fish shell")
  (rhol-eval-fish-env)
  (rhol-eval-fish-path))
 ((string= "/bin/bash" (getenv "SHELL"))
  (message "Getting env and path from bash shell")
  (rhol-eval-bash-env)
  (rhol-eval-bash-path))
 (t
  (error "Unknown shell type: %s" (getenv "SHELL"))))

(message "org-init.org: Open Default Directory")

(defun rhol-open-default-directory ()
  "Open the directory of the current buffer.

  Useful for quickly opening a dired buffer for the containing
  folder of the current buffer."
  (interactive)
  (find-file (file-name-directory buffer-file-name)))

(global-set-key (kbd "C-x C-j") 'rhol-open-default-directory)

(message "org-init.org: General Keybindings")
;; unbind the sleep button
(global-unset-key [(control z)])
(global-unset-key [(control x)(control z)])

;; Windows Style undo
(global-set-key (kbd "s-z") 'undo)

;; Windows Style cut, copy and paste
(global-set-key (kbd "s-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "s-x") 'clipboard-kill-region)
(global-set-key (kbd "s-v") 'clipboard-yank)

;; quickly enable and disable auto-fill-mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(message "org-init.org: Common Lisp package")
(use-package cl
  :ensure t)

(message "org-init.org: General Tweaks")
;; enable line highlighting
(global-hl-line-mode t)

;; get rid of yes or no prompt
(fset 'yes-or-no-p 'y-or-n-p)
;; remove confirmation if file or buffer does not exist
(setq confirm-nonexistent-file-or-buffer nil)
;; remove splash screen and echo area message
(setq inhibit-startup-message t inhibit-startup-echo-area-message t)

;; Add autofill for all text mode buffers
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(message "org-init.org: Sudo Edit")
;; Allow editing of a buffer that requires sudo privileges
(defun sudo-edit (&optional arg)
  "Edit currently visited file as root.

With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
(global-set-key (kbd "C-x C-r") 'sudo-edit)

(message "org-init.org: Spellcheck")

(defcustom rhol-default-spellchecker "hunspell"
  "Executable name for default spellchecker."
  :type '(choice (string :tag "hunspell" :value "hunspell")
                 (string :tag "aspell" :value "aspell")
                 (string :tag "Custom"))
  :group 'rhol-general
  :tag "Default spellchecker")

(defcustom rhol-default-spellchecker-args '("-d en_US")
  "List of extra args to provide to spellchecker."
  :type 'list
  :group 'rhol-general
  :tag "Default spellchecker args")

(defun rhol--config-spellchecker ()
  "Configure spellchecker using saved config."
  (if (executable-find rhol-default-spellchecker)
      (progn
        (setq ispell-program-name rhol-default-spellchecker)
        (setq ispell-extra-args rhol-default-spellchecker-args)
        (if (string= "aspell" rhol-default-spellchecker)
            (setq ispell-list-command "--list")))
    (user-error "Default spellchecker %s not found" rhol-default-spellchecker)))

(rhol--config-spellchecker)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(savehist-mode 1)

(defun rhol-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun rhol-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the 
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (rhol-reload-dir-locals-for-current-buffer)))))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'rhol-reload-dir-locals-for-all-buffer-in-this-directory))))

(message "org-init.org: Creating customize group rhol-emacs/rhol-appearance")
(defgroup rhol-appearance nil
  "Appearance customization settings."
  :group 'rhol-emacs
  :tag "Appearance")

(message "org-init.org: Font")

(defcustom rhol-default-font nil
  "Font name used in graphical mode."
  :type 'string
  :group 'rhol-appearance
  :tag "Default Font")

(if rhol-default-font
    nil
  (customize-save-variable 'rhol-default-font
                           (let ((choice (completing-read "What font do you want to use? ((D)efault, (I)nconsolata, (S)ource Code Pro, (O)ther): "
                                                          '(("D" 1)
                                                            ("I" 2)
                                                            ("S" 3)
                                                            ("O" 4))
                                                          nil t "D")))
                             (cond
                              ((string= choice "D")
                               "DejaVu Sans Mono")
                              ((string= choice "I")
                               "Inconsolata")
                              ((string= choice "S")
                               "Source Code Pro")
                              ((string= choice "O")
                               (read-string "Enter desired font-name: "))))))

(defcustom rhol-default-font-size nil
  "Font size used in graphical mode."
  :type 'integer
  :group 'rhol-appearance
  :tag "Font Size")

(if rhol-default-font-size
    nil
  (customize-save-variable 'rhol-default-font-size
                           (read-number "Enter font size: " 9)))

(defun rhol--configure-font ()
  "Refresh font configuration."
  (if (member rhol-default-font (font-family-list))
      (let ((font-name (format "%s %d" rhol-default-font rhol-default-font-size)))
        (set-frame-font font-name))
    (user-error "Font %s does not exist" rhol-default-font)))

(add-hook 'after-make-frame-functions '(lambda (frame)
                                         (select-frame frame)
                                         (if (display-graphic-p frame)
                                               (rhol--configure-font))))

(message "org-init.org: Theme")
(use-package darkokai-theme
  :ensure t
  :config
  (add-to-list 'custom-safe-themes "196cc00960232cfc7e74f4e95a94a5977cb16fd28ba7282195338f68c84058ec")
  (load-theme 'darkokai-custom t))

(message "loading emojify")
(use-package emojify
  :ensure t
  :config
  (emojify-download-emoji-maybe)
  (add-hook 'after-init-hook #'global-emojify-mode))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(message "org-init.org: Creating customize group rhol-emacs/rhol-appearance")
(defgroup rhol-third-party nil
  "Enable and disable third-party packages."
  :group 'rhol-emacs
  :tag "Third-Party Packages")

(message "org-init.org: ibuffer")
(defcustom rhol-use-ibuffer t
  "Set true to enable ibuffer package."
  :type 'boolean
  :group 'rhol-third-party
  :tag "Use ibuffer")

(if rhol-use-ibuffer
    (use-package ibuffer
      :ensure t
      :config
      (global-set-key (kbd "C-x C-b") 'ibuffer)
      (setq ibuffer-saved-filters
            '(("C/C++" ((or (mode . c++-mode)
                            (mode . c-mode)
                            (mode . cmake-mode))))
              ("Python" ((mode . python-mode)))
              ("Emacs" ((or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$"))))
              ("Org" ((or
                       (name . "^\\*Org Agenda\\*$")
                       (mode . org-mode))))
              ("Dired" ((mode . dired-mode)))
              ("Helm" ((name . "^\\*helm")))
              ("Magit" ((name . "^\\*magit")))))
      (setq ibuffer-saved-filter-groups
            '(("Default"
               ("Dired" (mode . dired-mode))
               ("Python" (mode . python-mode))
               ("Org" (or
                       (name . "^\\*Org Agenda\\*$")
                       (mode . org-mode)))
               ("C/C++" (or
                         (mode . c++-mode)
                         (mode . c-mode)
                         (mode . cmake-mode)))
               ("Emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("Helm" (name . "^\\*helm"))
               ("Magit" (name . "^\\*magit")))))
      (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "Default")))))

;; (message "org-init.org: direnv")
;; (defcustom rhol-use-direnv t
;;   "Set true to enable direnv parsing."
;;   :type 'boolean
;;   :group 'rhol-third-party
;;   :tag "Use direnv")

;; (defun rhol--find-envrc (path)
;;   "Find path to .envrc.

;;   PATH path to the buffer that you want to find .envrc for."
;;   (locate-dominating-file path ".envrc"))

;; (defun rhol--get-bash-direnv-str (envrc-path)
;;   "Get the direnv environment vars.

;;   ENVRC-PATH path to the .envrc to load."
;;   (message "%s" envrc-path)
;;   (make-local-variable 'process-environment)
;;   (shell-command-to-string (format "pushd %s > /dev/null; source .envrc; /usr/bin/env" envrc-path)))

;; (defun rhol-load-bash-direnv-for-buffer ()
;;   "Parse environment specified by direnv for a given buffer."
;;   (interactive)
;;   (cond
;;    ;; Special handling of dired mode files
;;    ((eq 'dired-mode major-mode)
;;     (let ((envrc-path (rhol--find-envrc (expand-file-name default-directory))))
;;       (message "%s" envrc-path)
;;       (if envrc-path
;;           (let ((env-str (rhol--get-bash-direnv-str envrc-path)))
;;             (message "%s" env-str)
;;             (rhol--bash-apply-env-string env-str)))))
;;    ;; Other files can be handled normally
;;    (t
;;     (let ((envrc-path (rhol--find-envrc (buffer-file-name))))
;;       (if envrc-path
;;           (let ((env-str (rhol--get-bash-direnv-str envrc-path)))
;;             ;; (message "%s" env-str)
;;             (rhol--bash-apply-env-string env-str)))))))

;; (defun rhol--get-fish-direnv-str (envrc-path)
;;   "Get the direnv environment vars.

;;   ENVRC-PATH path to .envrc to load."
;;   (message "%s" envrc-path)
;;   (make-local-variable 'process-environment)
;;   (shell-command-to-string (format "fish -ilc 'cd %s; and eval (direnv export fish); and set -L'" envrc-path)))

;; (defun rhol-load-fish-direnv-for-buffer ()
;;   "Parse direnv for given buffer."
;;   (interactive)
;;   (cond
;;    ((eq 'dired-mode major-mode)
;;     (let ((envrc-path (rhol--find-envrc (expand-file-name default-directory))))
;;       (message "%s" envrc-path)
;;       (if envrc-path
;;           (let ((env-str (rhol--get-fish-direnv-str envrc-path)))
;;             (rhol--fish-apply-set-string env-str)))))
;;    (t
;;     (let ((envrc-path (rhol--find-envrc (buffer-file-name))))
;;       (if envrc-path
;;           (let ((env-str (rhol--get-fish-direnv-str envrc-path)))
;;             (rhol--fish-apply-set-string env-str)))))))

;; (defun rhol-load-direnv-for-buffer ()
;;   "Call the appropriate direnv load function for the current shell."
;;   (interactive)
;;   (cond
;;    ((string= "/usr/bin/fish" (getenv "SHELL"))
;;     (rhol-load-fish-direnv-for-buffer))
;;    ((string= "/bin/bash" (getenv "SHELL"))
;;     (rhol-load-bash-direnv-for-buffer))
;;    (t
;;     (error "Unknown shell type: %s" (getenv "SHELL")))))

;; (defun rhol-unload-direnv ()
;;   "Unload changed env from direnv."
;;   (interactive)
;;   (if (local-variable-p 'process-environment)
;;       (kill-local-variable 'process-environment)))

;; (add-hook 'find-file-hook 'rhol-load-direnv-for-buffer)

(use-package direnv
  :ensure t
  :config (direnv-mode))

(message "org-init.org: ace-window")
(defcustom rhol-use-ace-window t
  "Set true to enable ace-window package."
  :type 'boolean
  :group 'rhol-third-party
  :tag "Use ace-window")
(if rhol-use-ace-window
    (use-package ace-window
      :ensure t))

(message "org-init.org: ripgrep")
(defcustom rhol-use-ripgrep t
  "Set true to enable ripgrep package if rg installed on system."
  :type 'boolean
  :group 'rhol-third-party
  :tag "Use ripgrep")
(if rhol-use-ripgrep
    (if (executable-find "cargo")
        (use-package rg
          :ensure t
          :ensure-system-package 
          (rg . "cargo install --force ripgrep")
          :config (rg-enable-default-bindings))
      (message "cargo not installed. Skipping rg package.")))

(message "org-init.org: flyspell")
(defcustom rhol-use-flyspell t
  "Set true to enable flyspell package."
  :type 'boolean
  :group 'rhol-third-party
  :tag "Use flyspell")
(if rhol-use-flyspell
    (use-package flyspell
      :ensure t
      :config (add-hook 'org-mode-hook '(lambda () (flyspell-mode)))))

(message "org-init.org: Flycheck")

(defcustom rhol-use-flycheck t
  "Set true to enable flycheck package."
  :type 'boolean
  :group 'rhol-third-party
  :tag "Use flycheck")

(if rhol-use-flycheck
    (use-package flycheck
      :ensure t
      :config
      (add-hook 'after-init-hook #'global-flycheck-mode)))

(message "org-init.org: YASnippets")
  (defcustom rhol-use-yasnippet t
    "Set true to enable yasnippet package."
    :type 'boolean
    :group 'rhol-third-party
    :tag "Use yasnippet")

(if rhol-use-yasnippet
    (use-package yasnippet
      :ensure t
      :config
      (use-package yasnippet-snippets)
      (yas-global-mode 1)
      (add-to-list 'yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets"))
      ;; (let ((yasnippet-lib-dir (f-dirname (buffer-file-name (find-library "yasnippet")))))
      ;;   (kill-buffer)
      ;;   (add-to-list 'yas-snippet-dirs (format "%s/snippets" yasnippet-lib-dir)))
      (yas-reload-all)))

(message "org-init.org: Company")

(defcustom rhol-use-company t
  "Enable use of company package."
  :type 'boolean
  :group 'rhol-third-party
  :tag "Enable company")

(if rhol-use-company
    (use-package company
      :ensure t
      :config
      (setq company-idle-delay 0.5)
      (add-hook 'after-init-hook 'global-company-mode)
      (global-set-key (kbd "C-<tab>") 'company-complete-common)
      (company-auctex-init)
      (add-to-list 'company-backends 'company-math-symbols-unicode)))

(message "org-init.org: Helm")
(defun rhol/helm-config ()
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when
                                        ;      reaching top or bottom of source
        helm-ff-search-library-in-sexp        t ; search for library in `require` and `declare-function` sexp
        helm-scroll-amount                    8 ; scroll 8 lines
        helm-ff-file-name-history-use-recentf t)

  (helm-mode 1)

  ;; Replace normal M-x
  (global-set-key (kbd "M-x") 'helm-M-x)
  (setq helm-M-x-fuzzy-match t)

  ;; Replace normal kill-ring cycle
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)

  ;; Replace normal find files
  (global-set-key (kbd "C-x C-f") 'helm-find-files))

(use-package helm
  :ensure t
  :config
  (use-package helm-config)
  (rhol/helm-config))

(message "org-init.org: Helm Swoop")
(use-package helm-swoop
  :ensure t
  :config
  (global-set-key (kbd "M-i") 'helm-swoop)
  (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
  (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
  (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch) ; when doing isearch, hand the word over to helm-swoop
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))

(message "org-init.org: Smartparens")
(use-package smartparens-config
  :ensure smartparens
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  (defun wrap-with-parens ()
    (sp-wrap-with-pair "("))
  (bind-keys
   :map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)

   ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)

   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ("C-M-d" . delete-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-x C-t" . sp-transpose-hybrid-sexp)))

(message "org-init.org: Magit")
(use-package magit
             :ensure t
             :config
             (setq magit-auto-revert-mode t))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(message "org-init.org: Monky")
(use-package monky
  :ensure t
  :config
  (setq monky-process-type 'cmdserver))

(message "org-init.org: Nyan-mode")
(use-package nyan-mode
  :ensure t
  :config
  (define-globalized-minor-mode rhol/nyan-mode nyan-mode
    (lambda () (nyan-mode)))
  (rhol/nyan-mode 1))

(setq zathura-procs ())
(defun zathura-forward-search ()
  ;; Open the compiled pdf in Zathura with synctex. This is complicated since
  ;; 1) Zathura refuses to acknowledge Synctex directive if the pdf is not
  ;; already opened
  ;; 2) This means we have to bookkeep open Zathura processes ourselves: first
  ;; open a new pdf from the beginning, if it is not already open. Then call
  (interactive)
  (let* ((zathura-launch-buf (get-buffer-create "*Zathura Output*"))
         (pdfname (TeX-master-file "pdf"))
         (zatentry (assoc pdfname zathura-procs))
         (zatproc (if (and zatentry (process-live-p (cdr zatentry)))
                      (cdr zatentry)
                    (progn
                      (let ((proc (progn (message "Launching Zathura")
                                         (start-process "zathura-launch"
                                                        zathura-launch-buf "zathura"
                                                        "-x" "emacsclient +%{line} %{input}" pdfname))))
                        (when zatentry
                          (setq zathura-procs (delq zatentry zathura-procs)))
                        (add-to-list 'zathura-procs (cons pdfname proc))
                        (set-process-query-on-exist-flag proc nil)
                        proc))))
         (synctex (format "%s:0:%s"
                          (TeX-current-line)
                          (TeX-current-file-name-master-relative)))
         )
    (start-process "zathura-synctex" zathura-launch-buf "zathura" "--synctex-forward" synctex pdfname)
    (start-process "raise-zathura-wmctrl" zathura-launch-buf "wmctrl" "-a" pdfname)))

(message "org-init.org: Auctex")
(defun rhol/latex-setup ()
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-PDF-mode t)

  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

  (setq reftex-plug-into-AUCTeX t)

  ;; setup the viewer
  (if (eq system-type 'darwin)
      (progn
        (setq TeX-view-program-list '())
        (setq TeX-view-program-selection '())
        (add-to-list 'TeX-view-program-list '("skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
        (add-to-list 'TeX-view-program-selection '(output-pdf "skim")))
    (progn
      (setq TeX-view-program-selection '())
      (setq TeX-view-program-list '())
      (add-to-list 'TeX-view-program-list '("zathura-custom" zathura-forward-search))
      (add-to-list 'TeX-view-program-selection '(output-pdf "zathura-custom"))))

  ;; Shortcut to jump to line in PDF Viewer
  (add-hook 'LaTeX-mode-hook (lambda () (local-set-key (kbd "<S-s-mouse-1>") #'TeX-view))))

(use-package latex
  :defer nil
  :config (rhol/latex-setup))

(message "org-init.org: Latexmk")
(use-package auctex-latexmk
             :ensure t
             :ensure-system-package latexmk
             :config
             (auctex-latexmk-setup)
             (add-hook 'TeX-mode-hook '(lambda ()
                                         (setq TeX-command-default "LatexMk"))))

(message "org-init.org: Yaml")
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml$" "\\.yaml$")
  :config
  (add-hook 'yaml-mode '(lambda () (auto-fill-mode -1))))

(message "org-init.org: Markdown Mode")
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode))
  :init (setq markdown-command "multimarkdown"))

;; Usage Example:
;;  
;; <!-- BEGIN RECEIVE ORGTBL ${1:YOUR_TABLE_NAME} -->
;; <!-- END RECEIVE ORGTBL $1 -->
;;  
;; <!-- 
;; #+ORGTBL: SEND $1 orgtbl-to-gfm
;; | $0 | 
;; -->

(defun orgtbl-to-gfm (table params)
  "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
  (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
			       org-table-last-alignment ""))
         (params2
         (list
           :splice t
	   :hline (concat alignment "|")
           :lstart "| " :lend " |" :sep " | ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(use-package json-mode
  :ensure t
  :config
  (add-hook 'json-mode '(lambda () (auto-fill-mode -1))))
(use-package json-reformat
  :ensure t)

(message "org-init.org: EPA")

(defcustom rhol-gpg-default-key nil
  "Address of default GPG key."
  :type 'string
  :group 'rhol-general
  :group 'rhol-third-party
  :tag "GPG Default Key")

(if rhol-gpg-default-key
    nil
  (customize-save-variable 'rhol-gpg-default-key (read-string "Enter your GPG email: ")))

(use-package epa-file
  :config
  (epa-file-enable)
  (setq epa-file-select-keys rhol-gpg-default-key))

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t) 
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
                                        ;resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
                                        ;resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))  
(setq auto-mode-alist
      (append '(("\\.gpg$" . sensitive-mode))
              auto-mode-alist))

(use-package pyenv-mode
  :ensure t
  :config (pyenv-mode))

(defun ssbb-pyenv-hook ()
  "Automatically activates pyenv version if .python-version file exists."
  (f-traverse-upwards
   (lambda (path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
           (pyenv-mode-set (s-trim (f-read-text pyenv-version-path 'utf-8))))))))

(add-hook 'find-file-hook 'ssbb-pyenv-hook)

(use-package elpy
  :ensure t
  :init (with-eval-after-load 'python
          (progn              
            (elpy-enable)
            (if (executable-find "jupyter")
                (setq python-shell-interpreter "jupyter"
                      python-shell-interpreter-args "console --simple-prompt")
              (setq elpy-rpc-backend "jedi")
              (setq elpy-test-runner 'elpy-test-pytest-runner)))))

(add-hook 'python-mode-hook (lambda () (auto-fill-mode 1)
                                        (setq-local comment-auto-fill-only-comments t)))

(defun rhol/hs-hide (x)
  "Hide block. Or if C-u pressed then hide all"
  (interactive "P")
  (message "%s" x)
  (if x
      (hs-hide-all)
    (hs-hide-block)))

(defun rhol/hs-show (x)
  "Show block. Or if C-u pressed then show all"
  (interactive "P")
  (message "%s" x)
  (if x
      (hs-show-all)
    (hs-show-block)))

(add-hook 'python-mode-hook (lambda ()
                              (hs-minor-mode 1)
                              (local-set-key (kbd "C-=") 'rhol/hs-hide)
                              (local-set-key (kbd "C-+") 'rhol/hs-show)))

(message "Loading flycheck-mypy")
(use-package flycheck-mypy
  :ensure t)

(message "org-init.org: Cmake mode")
(use-package cmake-mode
  :ensure t
  :config

  ;; I have to do this because I already have .txt files set to org-mode
  ;; in auto-mode-alist
  (add-hook 'org-mode-hook '(lambda ()
                              (let ((filename (buffer-file-name)))
                                (if (not (eq nil filename))
                                    (let ((basename (car (last (split-string filename "/")))))
                                      (if (string= "CMakeLists.txt" basename)
                                          (cmake-mode))))))))

(message "org-init.org: C/C++ Mode")
(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(add-hook 'c++-mode-hook 'eldoc-mode)
(add-hook 'c-mode-hook 'eldoc-mode)

(use-package clang-format
  :config
  (setq clang-format-executable "/usr/bin/clang-format-6.0")
  (add-hook 'c++-mode-hook '(lambda()
                            (local-set-key (kbd "C-c C-r f") 'clang-format-region)
                            (local-set-key (kbd "C-c C-r b") 'clang-format-buffer))))

(load "~/.emacs.d/lisp/third-party/clang-rename.el")
(add-hook 'c++-mode-hook '(lambda() (local-set-key (kbd "C-c C-r r") 'clang-rename)))

(load "~/.emacs.d/lisp/third-party/clang-include-fixer.el")

(message "org-init.org: Rtags")
(use-package rtags 
  :ensure t
  :if (executable-find "rdm")
  :load-path "/usr/local/share/emacs/site-lisp/rtags"
  :config
  (defun rhol/rtags-hook ()
    (message "rhol/rtags-hook on %s" (buffer-name))
    (rtags-start-process-unless-running)
    (local-set-key (kbd "M-.") 'rtags-find-symbol-at-point)
    (local-set-key (kbd "M-,") 'rtags-find-references-at-point)
    (local-set-key (kbd "C-M-;") 'rtags-find-file)
    (local-set-key (kbd "C-.") 'rtags-find-symbol)
    (local-set-key (kbd "C-,") 'rtags-find-references)
    (local-set-key (kbd "C-<") 'rtags-find-virtuals-at-point)
    (local-set-key (kbd "M-i") 'rtags-imenu)
    (local-set-key (kbd "C-M-,") 'rtags-location-stack-back)
    (local-set-key (kbd "C-M-.") 'rtags-location-stack-forward)
    )
  (add-hook 'c-mode-hook 'rhol/rtags-hook)
  (add-hook 'c++-mode-hook 'rhol/rtags-hook))

(use-package company-rtags
  :config
  (add-to-list 'company-backends 'company-rtags))

(use-package irony-eldoc
  :ensure t
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(defun rhol/hs-hook ()
  (hs-minor-mode 1)
  (local-set-key (kbd "C-=") 'hs-hide-block)
  (local-set-key (kbd "C-+") 'hs-show-block)
  )

(add-hook 'c++-mode-hook 'rhol/hs-hook)

(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(use-package ansi-color
  :config
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(message "org-init.org: Compilation mode")
(setq compilation-scroll-output t)
(add-hook 'c-mode-common-hook '(lambda () (local-set-key (kbd "<f5>") 'compile)))

(use-package rtags
  :ensure t
  :defer nil)
(use-package cmake-ide
  :ensure t
  :config
  (cmake-ide-setup))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))

(fset 'original-cmake-ide--get-compile-command (symbol-function 'cmake-ide--get-compile-command))

(defun rhol/default-cmake-ide-test-func (dir)
  (format "%s && CTEST_OUTPUT_ON_FAILURE=1 %s test"
          (original-cmake-ide--get-compile-command dir)
          (original-cmake-ide--get-compile-command dir)))

(defvar rhol/cmake-ide-test-func
  (symbol-function 'rhol/default-cmake-ide-test-func)
  "Function that receives a directory and returns the test command.")

(defun cmake-ide--get-compile-command (dir)
  "Return the compile command or the compile and test command.

DIR: The path to the build folder used by cmake-ide"

  (let ((cmake-compile-command (original-cmake-ide--get-compile-command dir))
        (cmake-test-command (funcall rhol/cmake-ide-test-func dir)))
    (cond ((and (boundp 'rhol/is-test-run) rhol/is-test-run) cmake-test-command)
          ((and (boundp 'rhol/is-clean-run) rhol/is-clean-run) (format "%s clean" cmake-compile-command))
          (t cmake-compile-command))))

(defun cmake-ide-run-tests ()
  "Compile and run test."
  (interactive)
  (let ((rhol/is-test-run t))
    (cmake-ide-compile)))

(defun cmake-ide-run-clean ()
  "Clean the build directory."
  (interactive)
  (let ((rhol/is-clean-run t))
    (cmake-ide-compile)))

(defvar rhol/cmake-compile-test-target-name
  ""
  "Name of the test target.")

(defun rhol/cmake-ide-run-test-target (dir)
  "Compile and run specified test target.

DIR: Path to build folder"
  (format "%s %s && cd %s && ./bin/%s"
          (original-cmake-ide--get-compile-command dir)
          rhol/cmake-compile-test-target-name
          dir
          rhol/cmake-compile-test-target-name))

(defun cmake-ide-clang-include-fixer ()
  "Invoke the Include Fixer to insert missing C++ headers."
  (interactive)
  (message (concat "Calling the include fixer using cmake-ide-build-dir. "
                   "This might take some seconds. Please wait."))
  (let ((default-directory cmake-ide-build-dir))
    (clang-include-fixer--start #'clang-include-fixer--add-header
                                "-output-headers")))

(add-hook 'c++-mode-hook '(lambda() (local-set-key (kbd "C-c C-r i") 'cmake-ide-clang-include-fixer)))

(defun rhol/cmakelists-hook ()
  (message "rhol/cmakelists-hook %s" (buffer-name))
  (condition-case ex
      (if (string-equal (buffer-name) "CMakeLists.txt")
          (progn
            (message "buffer is main cmake file")
            (local-set-key (kbd "<f5>") (lambda () cmake-ide-compile))
            (local-set-key (kbd "C-<f5>") 'cmake-ide-run-cmake)
            (local-set-key (kbd "<f6>") 'cmake-ide-run-tests)
            (local-set-key (kbd "C-<f6>") 'cmake-ide-run-clean))
        (message "buffer is not main cmake file"))
    ('error (message "Failed to run rhol/cmakelists-hook on buffer %s. [%s]" (buffer-name) ex))))

(defun rhol/c++-hook ()
  (message "rhol/c++-hook %s" (buffer-name))
  (condition-case ex
      (progn
        (local-set-key (kbd "<f5>") 'cmake-ide-compile)
        (local-set-key (kbd "C-<f5>") 'cmake-ide-run-cmake)
        (local-set-key (kbd "<f6>") 'cmake-ide-run-tests)
        (local-set-key (kbd "C-<f6>") 'cmake-ide-run-clean))
    ('error (message "Failed to run rhol/c++-hook on buffer %s. [%s]" (buffer-name) ex))))

(add-hook 'c++-mode-hook 'rhol/c++-hook)
(add-hook 'cmake-mode-hook 'rhol/cmakelists-hook)

(message "org-init.org: Transpose frame")
(use-package transpose-frame
  :ensure t
  :demand t
  :config (global-set-key (kbd "C-c t") 'transpose-frame))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(message "org-init.org: Projectile")
(use-package helm-projectile
  :ensure t)
(use-package projectile
  :ensure t
  :demand
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(add-to-list 'auto-mode-alist '("\\.launch$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xacro$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.sdf$" . xml-mode))
(add-to-list 'auto-mode-alist '("^model\\.config$" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.world$" . xml-mode))

(use-package rust-mode
  :ensure t
  :mode "\\.rs$"
  :config
  (add-hook 'rust-mode-hook '(lambda ()
                               (setq-local cargo-compile-command "cargo build")
                               (setq-local cargo-test-command "cargo test")
                               (setq-local compilation-read-command nil)
                               (local-set-key (kbd "<f5>") '(lambda () (interactive) (compile cargo-compile-command)))
                               (local-set-key (kbd "<f6>") '(lambda () (interactive) (compile cargo-test-command)))))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (add-hook 'racer-mode-hook #'company-mode)

  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(use-package cargo
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.toml$" . conf-mode))
  (add-hook 'conf-mode-hook '(lambda () (if (eq "toml" (file-name-extension (buffer-name)))
                                            (progn (cargo-minor-mode 1)                                                   
                                                   (setq-local cargo-compile-command "cargo build")
                                                   (setq-local cargo-test-command "cargo test")
                                                   (setq-local compilation-read-command nil)
                                                   (local-set-key (kbd "<f5>") '(lambda ()
                                                                                  (interactive)
                                                                                  (compile cargo-compile-command)))
                                                   (local-set-key (kbd "<f6>") '(lambda ()
                                                                                  (interactive)
                                                                                  (compile cargo-test-command))))))))

(use-package hydra
  :ensure t
  :config
  (require 'hydra-examples)


  (global-set-key
   (kbd "C-S-n")
   (defhydra hydra-move
     (:pre (progn 
	     (linum-mode 1))
	   :post (linum-mode -1))
     "
   Movement
   ---------
   _n_ next line          _p_ previous line
   _f_ forward char       _b_ backward char
   _a_ beginning of line  _e_ end of line
   _v_ scroll up          _V_ scroll down
   _l_ recenter

   Resize
   ------
   _+_ Zoom In
   _-_ Zoom Out
   _r_ Reset Zoom

   Goto
   ----
   _g_ goto line
   _m_ mark

   _q_ quit
   "
     ("n" next-line)
     ("p" previous-line)
     ("f" forward-char)
     ("b" backward-char)
     ("a" beginning-of-line)
     ("e" move-end-of-line)
     ("v" scroll-up-command)
     ;; Converting M-v to V here by analogy.
     ("V" scroll-down-command)
     ("l" recenter-top-bottom)

     ("+" text-scale-increase "in")
     ("-" text-scale-decrease "out")
     ("r" (text-scale-adjust 0) "reset")

     ("g" goto-line "go")
     ("m" set-mark-command "mark" :bind nil)
     ("q" nil "quit")))

  (defun hydra-move-splitter-left (arg)
    "Move window splitter left."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'right))
	(shrink-window-horizontally arg)
      (enlarge-window-horizontally arg)))

  (defun hydra-move-splitter-right (arg)
    "Move window splitter right."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'right))
	(enlarge-window-horizontally arg)
      (shrink-window-horizontally arg)))

  (defun hydra-move-splitter-up (arg)
    "Move window splitter up."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'up))
	(enlarge-window arg)
      (shrink-window arg)))

  (defun hydra-move-splitter-down (arg)
    "Move window splitter down."
    (interactive "p")
    (if (let ((windmove-wrap-around))
	  (windmove-find-other-window 'up))
	(shrink-window arg)
      (enlarge-window arg)))

  (global-set-key
   (kbd "<f1>")
   (defhydra hydra-window ()
     "
Movement^^        ^Split^         ^Switch^      ^Resize^
----------------------------------------------------------------
_j_ left       _|_    vertical      _b_uffer        _J_ left
_k_ down       _\__   horizontal    _f_ind files    _K_ down
_i_ up         _z_    undo          _a_ce 1         _I_ up
_l_ right      _Z_    reset         _s_wap          _L_ right
_F_ollow      _D_lt   Other        _S_ave          _m_aximize
_q_ quit      _o_nly  this         _d_elete        _+_   balance
                                   _t_ranspose  
"
     ("+" balance-windows)
     ("j" windmove-left )
     ("k" windmove-down )
     ("i" windmove-up )
     ("l" windmove-right )
     ("J" hydra-move-splitter-left)
     ("K" hydra-move-splitter-down)
     ("I" hydra-move-splitter-up)
     ("L" hydra-move-splitter-right)
     ("b" helm-mini)
     ("f" helm-find-files)
     ("F" follow-mode)
     ("a" (lambda ()
	    (interactive)
	    (ace-window 1)
	    (add-hook 'ace-window-end-once-hook
		      'hydra-window/body))
      )
     ("|" (lambda ()
	    (interactive)
	    (split-window-right)
	    (windmove-right))
      )
     ("_" (lambda ()
	    (interactive)
	    (split-window-below)
	    (windmove-down))
      )
     ("s" (lambda ()
	    (interactive)
	    (ace-window 4)
	    (add-hook 'ace-window-end-once-hook
		      'hydra-window/body)))
     ("S" save-buffer)
     ("d" delete-window)
     ("D" (lambda ()
	    (interactive)
	    (ace-window 16)
	    (add-hook 'ace-window-end-once-hook
		      'hydra-window/body))
      )
     ("o" delete-other-windows)
     ("m" ace-maximize-window)
     ("t" transpose-frame)
     ("z" (progn
	    (winner-undo)
	    (setq this-command 'winner-undo))
      )
     ("Z" winner-redo)
     ("q" nil)))

  (global-set-key
   (kbd "C-x p")
   (defhydra hydra-projectile (:color teal
				      :hint nil)
     "
     PROJECTILE: %(projectile-project-root)

     Find File            Search/Tags          Buffers                Cache
------------------------------------------------------------------------------------------
  _f_: file            _a_: ag               _i_: Ibuffer           _c_: cache clear
  _F_: file dwim       _s_: ripgrep          _b_: switch to buffer  _x_: remove known project
_M-f_: file curr dir   _o_: multi-occur      _K_: Kill all buffers  _X_: cleanup non-existing
  _r_: recent file                                                  _z_: cache current

  _q_: quit
"
     ("a"    projectile-ag)
     ("b"    projectile-switch-to-buffer)
     ("c"    projectile-invalidate-cache)
     ("f"    projectile-find-file)
     ("F"    projectile-find-file-dwim)
     ("M-f"  projectile-find-file-in-directory)
     ;; ("g"   ggtags-update-tags)
     ;; ("s-g" ggtags-update-tags)
     ("i"   projectile-ibuffer)
     ("K"   projectile-kill-buffers)
     ("o"   projectile-multi-occur)
     ("p"   projectile-switch-project)
     ("r"   projectile-recentf)
     ("s"   rg-project)
     ("x"   projectile-remove-known-project)
     ("X"   projectile-cleanup-known-projects)
     ("z"   projectile-cache-current-file)
     ("`"   hydra-projectile-other-window/body "other window")
     ("q"   nil "cancel" :color blue)))


  (global-set-key
   (kbd "C-x SPC")
   (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
					:color pink
					:hint nil
					:post (deactivate-mark))
     "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
     ("k" previous-line)
     ("j" next-line)
     ("h" left-char)
     ("l" right-char)
     ("d" kill-rectangle)                    ;; C-x r k
     ("y" yank-rectangle)                    ;; C-x r y
     ("w" copy-rectangle-as-kill)            ;; C-x r M-w
     ("o" open-rectangle)                    ;; C-x r o
     ("t" string-rectangle)                  ;; C-x r t
     ("c" clear-rectangle)                   ;; C-x r c
     ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
     ("N" rectangle-number-lines)            ;; C-x r N
     ("r" (if (region-active-p)
	      (deactivate-mark)
	    (rectangle-mark-mode 1)))
     ("u" undo nil)
     ("g" nil))))

(use-package protobuf-mode
  :ensure t)

(use-package winner
  :ensure t
  :demand
  :config
  (winner-mode 1))

(message "org-init.org: undo-tree")
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1)
  ;; Windows Style undo
  (global-set-key (kbd "s-z") 'undo-tree-visualize)
  (global-set-key (kbd "C-x u") 'undo-tree-visualize)
  (add-hook 'python-mode-hook (lambda ()
                                (undo-tree-mode 1)))
  (add-hook 'c++-mode-hook (lambda () (undo-tree-mode 1))))

(message "org-init.org: flatbuffers")
(use-package flatbuffers
  :load-path "~/.emacs.d/lisp"
  :mode ("\\.fbs\\'" . flatbuffers-mode))

(setq flycheck-shellcheck-follow-sources nil)

(use-package lua-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.cfg\\'" . lua-mode))

(use-package ks
  :load-path "~/.emacs.d/lisp/third-party")

(use-package matlab-mode
  :mode "\\.m\\'"
  :config
  (setq matlab-indent-function t)
  (setq matlab-shell-command "/usr/local/MATLAB/R2017b/bin/matlab"))

(use-package auto-yasnippet
  :ensure t
  :bind (("<f9> e" . aya-create)
         ("<f9> y" . aya-expand)))

(use-package aggressive-indent
  :ensure t
  :config
  (aggressive-indent-global-mode 1))

(use-package ranger
  :ensure t
  :config
  (ranger-override-dired-mode t))

(message "org-init.org: Creating a customize group rhol-emacs/rhol-org-mode")
(defgroup rhol-org-mode nil
  "Org-mode customization settings."
  :group 'rhol-emacs
  :tag "Org-mode")

(defcustom rhol-org-directory nil
  "Folder where org-mode files are stored."
  :type 'directory
  :group 'rhol-org-mode
  :tag "Org Directory")

(defcustom rhol-org-agenda-files rhol-org-directory
  "List of directories that should be included in agenda."
  :type '(repeat 'file)
  :tag "Org Agenda Files")

(if rhol-org-directory nil
  (progn (customize-save-variable
	  'rhol-org-directory
	  (expand-file-name (read-directory-name "Org-mode Directory: "
						 "~/Dropbox/org-mode"
						 nil
						 t)))
         (customize-save-variable 'rhol-org-agenda-files (list rhol-org-directory))))

(defcustom rhol-org-default-notes-file nil
  "Default capture file."
  :type 'file
  :group 'rhol-org-mode
  :tag "Org Default Notes File")

(if rhol-org-default-notes-file nil
  (customize-save-variable
   'rhol-org-default-notes-file
   (expand-file-name
    (read-file-name "Org default notes file: " rhol-org-directory "refile.org" nil "refile.org"))))

(message "org-init.org: Org basic setup")
(use-package org-id)
(use-package org-habit)
(load-file "~/.emacs.d/lisp/norang-org.el")
(use-package plain-org-wiki
             :load-path "~/.emacs.d/lisp/")

(defun rhol/org-mode-hook ()
  (turn-on-flyspell)
  (auto-fill-mode 1))
(add-hook 'org-mode-hook 'rhol/org-mode-hook)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

                                        ; TODO: Check if the files exist before choosing this dir

(setq org-default-notes-file rhol-org-default-notes-file
      org-directory rhol-org-directory
      org-agenda-files rhol-org-agenda-files)

(setq org-support-shift-select t)

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org_archive$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

;; Make the latex preview fragments larger
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(defun rhol/reload-org-buffers ()
  (interactive)
  (mapc #'(lambda (buffer-name)
            (condition-case ex
                (with-current-buffer buffer-name
                  (if (derived-mode-p 'org-mode)
                      (revert-buffer t t)))
              ('error (message "Failed to revert buffer %s. [%s]" buffer-name ex))))
        (buffer-list)))

(message "org-init.org: Org todo setup")
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
              (sequence "WAITING(@w/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "EVENT"))))


(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("EVENT" :foreground "forest green" :weight bold))))

(setq org-use-fast-todo-selection t)
(setq org-treat-S-cursor-todo-selection-as-state-change nil)

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("HOLD" ("WAITING") ("HOLD" . t))
              ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
              ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
              ("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

(message "org-init.org: Org capture templates")
(setq org-capture-templates
      '(("t" "todo" entry (file (lambda () (expand-file-name (format "%s/refile.org" rhol-org-directory))))
         "* TODO %?\n%U\n" :clock-in t :clock-resume t)
        ("n" "note" entry (file (lambda ()(expand-file-name (format "%s/notes.org" rhol-org-directory))))
         "* %? :NOTE:\n%U\n" :clock-in t :clock-resume t)
        ("j" "Journal" entry (file+datetree (lambda () (expand-file-name (format "~/%s/diary.org" rhol-org-directory))))
         "* %? :crypt:\n%U\n" :clock-in t :clock-resume t)
        ("e" "Event" entry (file (lambda () (expand-file-name (format "%s/calendar.org" rhol-org-directory))))
         "* %^{Name} \n%U\n:PROPERTIES:\n:CATEGORY: Event\n:LOCATION: %^{Location}\n:END:\n%^T\n\n%?")
        ("h" "Habit" entry (file (lambda () (expand-file-name (format "%s/refile.org" rhol-org-directory))))
         "* NEXT %?\n%U\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ("c" "Contacts" entry (file (lambda () (expand-file-name (format "%s/contacts.org" rhol-org-directory))))
         "* %^{Name} \n:PROPERTIES:\n:EMAIL: %^{Email}\n:PHONE:\n:NICKNAME:\n:IGNORE:\n:ICON:\n:NOTE:\n:ADDRESS:\n:BIRTHDAY:\n:END:\n%?")
        ("i" "Ideas" entry (file (lambda () (expand-file-name (format "%s/ideas.org" rhol-org-directory))))
         "* %?\n%U\n" :clock-in t :clock-resume t)
        ))

(message "org-init.org: Refile settings")
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)

; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)

; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

(setq org-refile-target-verify-function 'bh/verify-refile-target)

(use-package org-super-agenda
  :config (org-super-agenda-mode))

(defun rhol/org-super-agenda-list ()
  (interactive)
  (let ((org-super-agenda-groups
         '((:order-multi (1 (:name "Done today"
                                   :and (:regexp "State \"DONE\""
                                                 :log t))
                            (:name "Clocked today"
                                   :log t)))
           (:name "Schedule"
                  :time-grid t)
           (:name "Today"
                  :scheduled today)
           (:name "Refile - Super"
                  :tag "REFILE")
           (:habit t)
           (:name "Due today"
                  :deadline today)
           (:name "Overdue"
                  :deadline past)
           (:name "Due soon"
                  :deadline future)
           (:name "Projects - Super"
                  :children t)
           (:name "Waiting..."
                  :todo "WAITING"
                  :order 98)
           (:name "On Hold..."
                  :todo "HOLD"
                  :order 98)
           (:name "Scheduled earlier"
                  :scheduled past)
           (:auto-group t))))
    (org-agenda)))

(message "org-init.org: Agenda settings")
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Custom agenda command definitions
(setq org-agenda-custom-commands
      (quote (("W" "Wiki" tags "WIKI"
               ((org-agenda-overriding-header "Wiki Pages")
                (org-tags-match-list-sublevels t)))
              ("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE-DISABLED"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED-DISABLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED-DISABLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED-DISABLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-DISABLED/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-DISABLED/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING-DISABLED|HOLD-DISABLED/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE-DISABLED/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

(setq org-agenda-auto-exclude-function 'bh/org-auto-exclude-function)

(setq org-agenda-log-mode-items (quote (closed state)))

(setq org-agenda-span 'day)
(setq org-stuck-projects (quote ("" nil nil "")))

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archived Tasks")

(message "org-init.org: Clocking")
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(setq org-agenda-clock-consistency-checks
      (quote (:max-duration "4:00"
              :min-duration 0
              :max-gap 0
              :gap-ok-around ("4:00"))))
;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
; global STYLE property values for completion
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))

(message "org-init.org: Tags")
(setq org-tag-alist (quote ((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?H)
                            ("@school" . ?s)
                            (:endgroup)
                            ("WIKI" . ?x)
                            ("WAITING" . ?w)
                            ("HOLD" . ?h)
                            ("PERSONAL" . ?P)
                            ("WORK" . ?W)
                            ("SCHOOL" . ?S)
                            ("ORG" . ?O)
                            ("NOTE" . ?n)
                            ("CANCELLED" . ?c)
                            ("DISABLED" . ?d)
                            ("FLAGGED" . ??))))

; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

(message "org-init.org: Babel")
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)

; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(org-babel-do-load-languages
 'org-babel-load-languages
 (if (<= emacs-major-version 25)
     '((clojure . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . t)
       (latex . t)
       (ledger . t)
       (octave . t)
       (org . t)
       (plantuml . t)
       (python . t)
       (ruby . t)
       (shell . t)
       (sql . t)
       (sqlite . t))
   ;; Emacs 26 babel versions of different
   '((clojure . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (gnuplot . t)
       (haskell . t)
       (latex . t)
       (ledger . t)
       (octave . t)
       (org . t)
       (plantuml . t)
       (python . t)
       (ruby . t)
       (sql . t)
       (sqlite . t))))

; Do not prompt to confirm evaluation
; This may be dangerous - make sure you understand the consequences
; of setting this -- see the docstring for details
(setq org-confirm-babel-evaluate nil)

; Use fundamental mode when editing plantuml blocks with C-c '
(add-to-list 'org-src-lang-modes '("plantuml" . fundamental))

;; initialize the shell
;; (org-babel-shell-initialize)

(message "org-init.org: Appointments")
;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'bh/org-agenda-to-appt 'append)


                                        ; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

                                        ; Activate appointments so we get notifications
(appt-activate t)

                                        ; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

(run-at-time "00:59" 3600 'org-save-all-org-buffers)

(message "org-init.org: Calfw")
(use-package calfw-cal)
(use-package calfw-ical)
;; (use-package calfw-howm)
(use-package calfw-org)

(defun rhol/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:ical-create-source
     "Personal"
     "https://calendar.google.com/calendar/ical/yifengzhu1129%40gmail.com/private-d59fe0ae9d9b1b5d99af42843cebcc00/basic.ics"
     "White")
    (cfw:ical-create-source
     "Courses"
     "https://calendar.google.com/calendar/ical/a83hfc0bja3k38p5pcemv0long%40group.calendar.google.com/public/basic.ics"
     "White")    
    (cfw:org-create-source "Yellow"))))

(message "org-init.org: org face customization")
;; Color =<text>= as hot pink
(set-face-attribute 'org-verbatim nil :foreground "#ff69b4")

;; Color =<text>= as hot pink
(set-face-attribute 'org-verbatim nil :foreground "#ff69b4")

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key "yifeng.zhu@utexas.edu")

(use-package org-ref
  :config
  ;; org-ref settings
  (setq org-ref-notes-directory (format "%s/bibliography/notes" rhol-org-directory)
        org-ref-bibliography-notes (format "%s/bibliography/articles.org" rhol-org-directory)
        org-ref-default-bibliography '(lambda (_ _ _) (format "%s/bibliography/references.bib" rhol-org-directory))
        org-ref-pdf-directory (format "%s/bibliography/bibtex-pdfs" rhol-org-directory))
  ;; helm-bibtex settings
  (setq bibtex-completion-bibliography (format "%s/bibliography/references.bib" rhol-org-directory)
        bibtex-completion-library-path (format "%s/bibliography/bibtex-pdfs" rhol-org-directory)
        bibtex-completion-notes-path (format "%s/bibliography/articles.org" rhol-org-directory))
  ;; pdf opener
  (setq bibtex-completion-pdf-open-function 'find-file)

  (unless (file-exists-p org-ref-pdf-directory)
    (make-directory org-ref-pdf-directory t))
  (use-package org-ref-pdf)
  (use-package org-ref-url-utils)
  (use-package org-ref-bibtex)
  (use-package org-ref-latex)
  (use-package org-ref-arxiv)
  (use-package org-ref-isbn)
  (setq biblio-download-directory org-ref-pdf-directory)

  (setq helm-bibtex-pdf-open-function 'org-open-file)
  (setq bibtex-completion-additional-search-fields '(tags keywords))
  )

(if (string-equal (system-name) "stryker")
    (require 'org-mu4e))

(use-package deft
  :config
  (setq deft-directory (concat rhol-org-directory "/wiki/")
	deft-recursive t
	deft-extensions '("org" "md")
	deft-default-extension "org"
	deft-use-filter-string-for-filename t))

(message "Loading mu4e")

(use-package mu4e)
(use-package smtpmail)

(setq
 ;; basic mu4e config
 mu4e-sent-messages-behavior 'delete
 mu4e-maildir "~/mail"
 mu4e-trash-folder "/yifengz/trash"
 mu4e-refile-folder "/yifengz/allmail"
 mu4e-sent-folder "/yifengz/sent"
 mu4e-drafts-folder "/yifengz/drafts"

 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-attachment-dir "~/Downloads"
 mu4e-use-fancy-chars t 

 user-mail-address "yifeng.zhu@utexas.edu"
 mu4e-reply-to-address "yifeng.zhu@utexas.edu"
 user-full-name "Yifeng Zhu"

 mu4e-compose-siganture (concat "--\n" "Yifeng\n")
 mu4e-compose-signature-auto-include nil 

 ;; smtp config
 message-send-mail-function 'smtpmail-send-it
 starttls-use-gnutls t
 smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
 smtpmail-auth-credentials
 '(("smtp.gmail.com" 587 "yifeng.zhu@utexas.edu" nil))
 smtpmail-default-smtp-server "smtp.gmail.com"
 smtpmail-smtp-server "smtp.gmail.com"
 smtpmail-smtp-service 587

 message-kill-buffer-on-exit t

 ;; html renderer
 mu4e-html2text-command "w3m -T text/html"

 mu4e-update-interval 120
 mu4e-headers-auto-update t

 ;; fix uid errors with mbsync
 mu4e-change-filenames-when-moving t

 mu4e-headers-draft-mark     '("D" . "⚒")
 mu4e-headers-flagged-mark   '("F" . "✚")
 mu4e-headers-new-mark       '("N" . "✱")
 mu4e-headers-passed-mark    '("P" . "❯")
 mu4e-headers-replied-mark   '("R" . "❮")
 mu4e-headers-seen-mark      '("S" . "✔")
 mu4e-headers-trashed-mark   '("T" . "✀")
 mu4e-headers-attach-mark    '("a" . "⚓")
 mu4e-headers-encrypted-mark '("x" . "⚴")
 mu4e-headers-signed-mark    '("s" . "☡")
 mu4e-headers-unread-mark    '("u" . "🖂")
 mu4e-headers-has-child-prefix    '("+"  . "◼")
 mu4e-headers-empty-parent-prefix '("-"  . "◽")
 mu4e-headers-first-child-prefix  '("\\" . "┗▶")
 mu4e-headers-duplicate-prefix    '("="  . "≡")
 mu4e-headers-default-prefix      '("|"  . "│"))

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(setq mu4e-maildir-shortcuts
      '(("/yifengz/INBOX" . ?i)
        ("/yifengz/sent" . ?s)
        ("/yifengz/drafts" . ?d)
        ("/yifengz/trash" . ?t)
        ("/yifengz/allmail" . ?a)))

(setq mu4e-get-mail-command "mbsync gmail")

(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(use-package org-mu4e)
(setq org-mu4e-convert-to-html t)

(use-package mu4e-alert
  :ensure t)


(mu4e-alert-set-default-style 'libnotify)
(add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
(add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

(use-package subr-x)

(use-package jl-encrypt
  :load-path "~/.emacs.d/lisp/third-party")

(message "org-init.org: determine repo type")
(defun rhol/get-repo-type (&optional filepath)
  "Returns the type of the repository for a given directory or file
   Will return \"git\", \"hg\", or nil if not a repo
  "
  (interactive)
  (let ((filepath (if (eq filepath nil)
		      (if (boundp 'default-directory)
			  (expand-file-name default-directory)
			(buffer-file-name))
		    filepath)))
    (if (locate-dominating-file filepath ".git")
	"git" 
      (if (locate-dominating-file filepath ".hg")
	  "hg"
	nil))))

(defun rhol/get-repo-path (&optional filepath)
  "Returns the type of the repository for a given directory or file
   Will return \"git\", \"hg\", or nil if not a repo
  "
  (interactive)
  (let ((filepath (if (eq filepath nil)
		      (if (boundp 'default-directory)
			  (expand-file-name default-directory)
			(buffer-file-name))
		    filepath)))
    (let ((git-repo-dir (locate-dominating-file filepath ".git")))
      (if git-repo-dir
	  git-repo-dir
	(let ((hg-repo-dir (locate-dominating-file filepath ".hg")))
	  (if hg-repo-dir
	      hg-repo-dir
	    nil))))))

(defun rhol/repo-status (&optional filepath)
  (interactive)
  (let ((filepath (if (eq filepath nil)
		      (if (boundp 'default-directory)
			  (expand-file-name default-directory)
			(buffer-file-name))
		    filepath)))
    (let ((repo-type (rhol/get-repo-type filepath))
	  (repo-dir (rhol/get-repo-path filepath)))
      (cond ((string= repo-type "git")
      	     (magit-status repo-dir))
      	    ((string= repo-type "hg")
	     (monky-status repo-dir))
      	    (t (message "%s is not a git or mercurial repo." filepath))))))

;; Org reveal
(require 'ox-reveal)
(setq org-reveal-root "file:///home/yifeng/slides/reveal.js")
(setq org-enable-reveal-js-support t)

;; other custom plugin
;; loaded function: lookup-wikipedia
(load "~/.emacs.d/custom-plugin.el")

(message "org-init.org: org keymap")
;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
(global-set-key (kbd "<f9> g") 'rhol/repo-status)
;; (global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
(global-set-key (kbd "<f9> b") 'helm-bibtex)
(global-set-key (kbd "<f9> B") (lambda () (interactive) (helm-bibtex (universal-argument))))
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> m") 'mu4e)
(global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)
(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)
(global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
(global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
(global-set-key (kbd "<f9> y") 'org-preview-latex-fragment)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-<f11>") 'org-clock-in)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
(global-set-key (kbd "C-c c") 'org-capture)

;; custom plugin keybinding
(global-set-key (kbd "<f6> w") 'lookup-wikipedia)
(global-set-key (kbd "<f6> g") 'lookup-google)
(global-set-key (kbd "<f6> e") 'lookup-etymology)
(global-set-key (kbd "<f6> d") 'lookup-definition)

(defun insert-eq ()
  (interactive)
  (insert "\\[ \\]")
  (backward-char)
  (backward-char))

(global-set-key (kbd "<f9> e") 'insert-eq)

(global-set-key (kbd "<f9> c") 'rhol/open-calendar)

(global-set-key (kbd "<f9> w") 'deft)

;; For some reason org-mode has changed what M-S-<up> etc do

(message "defining fix-org-keymap")
(defun fix-org-keymap ()
  "Set the proper keybindings in `org-mode'."
  (local-set-key (kbd "M-S-<up>") 'org-move-subtree-up)
  (local-set-key (kbd "M-S-<down>") 'org-move-subtree-down)
  (define-key pyenv-mode-map (kbd "C-c C-s") nil)
  (local-set-key (kbd "C-c C-s") 'org-schedule))

;; add to hook
(message "adding org-mode hook")
(add-hook 'org-mode-hook 'fix-org-keymap)

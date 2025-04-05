(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files)

(global-set-key "\C-xwb" 'switch-to-buffer-other-window)

(add-hook 'prog-mode-hook
	  (lambda()
	    (electric-pair-local-mode -1)))

(use-package smartparens
  :ensure t
  :hook  (prog-mode)
  :config
  (require 'smartparens-config)
  (sp-with-modes 'sh-mode
    (sp-local-pair "[" "]"   :actions '(wrap insert navigate))
    (sp-local-pair "[ " " ]" :actions '(wrap insert navigate)))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(defun pref/set-line-number-mode()
  (setq display-line-numbers-type 'relative)
  (display-line-numbers-mode))
(add-hook 'prog-mode-hook 'pref/set-line-number-mode)
(add-hook 'latex-mode-hook 'pref/set-line-number-mode)

(setq column-number-mode t)

(auto-revert-mode 1)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case    t)

(use-package vterm
  :ensure t
  :init
  (defun pref/new-terminal()
    (interactive)
    (split-window-below)
    (other-window 1)
    (vterm)
    (rename-uniquely))
  :bind
  ("C-c t" . pref/new-terminal))

(use-package pdf-tools
  :ensure t
  :hook   (doc-view-mode . pdf-tools-install))

(use-package vertico
  :ensure t
  :defer  t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :bind
  (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package corfu
  :ensure t
  :after  orderless
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto           t
	corfu-on-exact-match nil)
  (keymap-unset corfu-map "RET"))

(use-package yasnippet
  :ensure t
  :hook ((prog-mode  . yas-minor-mode)
         (org-mode   . yas-minor-mode)
	 (latex-mode . yas-minor-mode))
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas/keymap         (kbd "TAB") nil)
  (define-key yas/keymap         (kbd "<backtab>") nil)
  (define-key yas-minor-mode-map (kbd "C-<tab>") #'yas-expand)
  (define-key yas/keymap         (kbd "C-j") #'yas-next-field)
  (define-key yas/keymap         (kbd "C-S-j") #'yas-prev-field)
  (yas-reload-all))

(use-package orderless
  :ensure t
  :defer  t
  :custom
  (completion-category-defaults    nil)
  (completion-styles             '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  (setq	orderless-component-separator "[- ]"))

(use-package consult
  :ensure t
  :bind
  ("C-c c b" . consult-buffer)
  ("C-c c w" . consult-buffer-other-window)
  ("C-c c /" . consult-ripgrep))
(add-hook 'org-mode-hook
	  (lambda()
	    (local-set-key (kbd "C-c c o") 'consult-outline)))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding  nil
	evil-undo-system #'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

(require 'compile)
(require 'vterm)
(setq prefs/evil-emacs-state-modes
      '(minibuffer-mode
	minibuffer-inactive-mode
	messages-buffer-mode
	Buffer-menu-mode
	haskell-mode
	help-mode
	compilation-mode
	emacs-lisp-mode
	dired-mode
	vterm-mode
	inferior-python-mode
	fundamental-mode))
(setq evil-normal-state-modes '(prog-mode)
      evil-insert-state-modes  nil
      evil-emacs-state-modes   (append prefs/evil-emacs-state-modes
				       evil-emacs-state-modes))

(use-package lsp-mode
  :ensure t
  :custom
  (lsp-completion-provider :none)
  :init
  (defun myLsp/orderless-dispatch-flex-first(_pattern index _total)
    (and (eq index 0) 'orderless-flex))
  
  (defun myLsp/lsp-mode-setup-completion()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
	  '(orderless))
    (add-hook 'orderless-style-dispatchers #'myLsp/orderless-dispatch-flex-first nil 'local))
  
  :hook
  (lsp-completion-mode . myLsp/lsp-mode-setup-completion)
  
  (c-mode       . lsp-deferred)
  (c-ts-mode    . lsp-deferred)
  (haskell-mode . lsp-deferred)
  (c++-mode     . lsp-deferred)
  (c++-ts-mode  . lsp-deferred)
  (java-ts-mode . lsp-deferred)
  (lua-mode     . lsp-deferred)
  (python-mode  . lsp-deferred)
  (latex-mode   . lsp-deferred)
  
  :config
  (setq-default lsp-enable-on-type-formatting   nil
				lsp-java-format-on-type-enabled nil
				lsp-rename-use-prepare          nil)

  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t))

(use-package lsp-ui
  :ensure t
  :after  lsp-mode
  :config
  (setq lsp-ui-doc-show-with-mouse  nil
	lsp-ui-doc-show-with-cursor t
	lsp-ui-doc-delay            0.5
	lsp-ui-sideline-enable      nil
	lsp-eldoc-enable-hover      nil))

(use-package lsp-pyright
  :ensure t
  :defer  t)

(use-package dap-mode
  :ensure t
  :defer  t)
(use-package lsp-treemacs
  :ensure t
  :defer  t)
(use-package treemacs
  :ensure t
  :defer  t)
(use-package lsp-java
  :ensure t
  :defer  t)

(use-package lsp-haskell
  :ensure t
  :defer  t)

(use-package lsp-latex
  :ensure t
  :defer  t)

(use-package magit
  :ensure t
  :defer  t)

(use-package projectile
  :ensure t
  :bind ("C-c p" . projectile-command-map))

(use-package rainbow-delimiters
  :ensure t
  :hook prog-mode)
(use-package rainbow-identifiers
  :ensure t
  :hook emacs-lisp-mode)

(add-to-list 'auto-mode-alist '("\\.latex\\'" . latex-mode))

(setq auth-source-save-behavior nil)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold   t
        doom-themes-enable-italic t)
  (set-face-italic 'font-lock-comment-face t)
  (load-theme 'doom-city-lights t)
  (doom-themes-org-config))

(custom-set-faces
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight regular :height 143 :width normal)))))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))
(use-package all-the-icons
  :ensure t
  :after  doom-modeline)

(defun myLaTeX/is-project-root(directory counter)
  (if (file-exists-p (concat directory "cfg.cfg"))
      directory
    (if (< counter 3)
	(myLaTeX/is-project-root (file-name-parent-directory directory) (+ 1 counter))
      nil)))

(defun myLaTeX/get-project-root()
  (myLaTeX/is-project-root (file-name-directory buffer-file-name) 1))

(defvar myLaTeX/main-tex-file nil)
(defun myLaTeX/set-main-tex-file()
  (setq myLaTeX/main-tex-file (file-relative-name buffer-file-name))
  (remove-hook 'latex-mode-hook 'myLaTeX/set-main-tex-file))
(add-hook 'latex-mode-hook 'myLaTeX/set-main-tex-file)

(defun myLaTeX/single-file-compile()
  (interactive)
  (save-window-excursion
    (async-shell-command (concat "latexmk -quiet -lualatex -f -auxdir=$HOME/.texbuild/ -outdir=pdf/ "
				 myLaTeX/main-tex-file))))

(defun myLaTeX/project-complie()
  (interactive)
  (save-window-excursion
    (async-shell-command (concat (concat "cd " (myLaTeX/get-project-root)) " && mktex"))))

(defun myLaTeX/choose-file()
  (interactive)
  (read-file-name "Which PDF? "
		  (concat (myLaTeX/get-project-root) "pdf/")))

(defun myLaTeX/open-pdf-zathura()
  (interactive)
  (save-window-excursion
    (async-shell-command (concat "zathura --fork "
				 (myLaTeX/choose-file)))))

(add-hook 'latex-mode-hook
	  (lambda()
	    (local-set-key (kbd "C-c l r") 'myLaTeX/set-main-tex-file)
	    (local-set-key (kbd "C-c l c") 'myLaTeX/single-file-compile)
	    (local-set-key (kbd "C-c l m") 'myLaTeX/project-complie)
	    (local-set-key (kbd "C-c l z") 'myLaTeX/open-pdf-zathura)))

(add-hook 'org-mode-hook
          (lambda() (setq jit-lock-defer-time 0.15)))

(add-hook 'org-mode-hook
          (lambda()
            (setq org-latex-src-block-backend 'listings
                  org-latex-listings-options  '(("numbers" "left")
                                                ("breaklines" "true")
                                                ("upquote" "true")
                                                ("autogobble" "true")
                                                ("showstringspaces" "false")
                                                ("basicstyle" "\\ttfamily")))))

(add-hook 'org-mode-hook
          (lambda()
            (setq org-preview-latex-default-process 'dvipng
                  org-startup-with-latex-preview     t
                  org-format-latex-options           (plist-put org-format-latex-options :scale 2.0))))

(add-hook 'org-mode-hook
          (lambda()
            (org-babel-do-load-languages
             'org-babel-load-languages '((emacs-lisp . t)
                                         (C . t)
                                         (shell . t)
                                         (lua . t)))))

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(add-hook 'org-mode-hook
          (lambda()
            (setq org-startup-indented  t)))

(use-package olivetti
  :config
  (setq-default olivetti-body-width 120)
  :hook org-mode)

(use-package lua-mode
  :ensure t
  :defer  t)

(use-package haskell-mode
  :ensure t
  :defer  t
  :config
  (setq lsp-haskell-plugin-rename-config-cross-module t))

(use-package rustic
  :ensure t
  :defer  t
  :bind (:map rustic-mode-map
			  ("M-?"       . lsp-find-reference)
			  ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq lsp-eldoc-hook                 nil
		lsp-enable-symbol-highlighting nil
		lsp-signature-auto-activate    nil))

(use-package portage-modes
  :ensure t
  :defer  t)

(add-hook 'prog-mode-hook
          (lambda()
            (setq c-indentation-style 'k&r
                  c-basic-offset       4)))

(defun myProg/compile()
  (interactive) (compile compile-command))
(add-hook 'prog-mode-hook (lambda() (local-set-key (kbd "C-c r c") 'myProg/compile)))

(use-package elpy
  :ensure t
  :hook (python-mode . elpy-enable)
  :config
  (setenv "WORKON_HOME" "~/.venvs"))

(defun myJava/insert-compile-command()
  (interactive)
  (insert (concat "javac " (file-relative-name buffer-file-name))))
(add-hook 'java-ts-mode-hook (lambda() (local-set-key (kbd "C-c r C") 'myJava/insert-compile-command)))

(add-hook 'prog-mode-hook
          (lambda()
            (setq treesit-font-lock-level 4
                  c-ts-mode-indent-style 'k&r
                  c-ts-mode-indent-offset 4)))

(setq treesit-language-source-alist
      '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")
        (java "https://github.com/tree-sitter/tree-sitter-java")))

(setq major-mode-remap-alist
      '((c-mode    . c-ts-mode)
        (c++-mode  . c++-ts-mode)
        (bash-mode . bash-ts-mode)
        (java-mode . java-ts-mode)))

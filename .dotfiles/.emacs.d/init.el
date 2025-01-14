(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(recentf-mode 1)
(global-set-key "\C-xf" 'recentf-open-files)

(electric-pair-mode)
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)

(setq-default global-display-line-numbers-mode t)
(setq-default display-line-numbers 'relative)
(setq         column-number-mode    t)

(auto-revert-mode 1)

(use-package vterm
  :ensure t)

(use-package pdf-tools
  :ensure t
  :config
  (pdf-loader-install))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t
        evil-want-keybinding  nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(dolist (p '((prog-mode                . normal)
             (minibuffer-mode          . emacs)
             (minibuffer-inactive-mode . emacs)
             (haskell-mode             . emacs)
             (help-mode                . emacs)
             (emacs-lisp-mode          . emacs)
             (dired-mode               . emacs)
             (vterm-mode               . emacs)
             (fundamental-mode         . emacs)))
  (evil-set-initial-state (car p) (cdr p)))

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
  :defer  t
  :custom
  (corfu-cycle t)
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t))

(use-package lsp-mode
  :ensure t
  :hook
  (c-mode       . lsp-deferred)
  (haskell-mode . lsp-deferred)
  (c++-mode     . lsp-deferred)
  (lua-mode     . lsp-deferred))

(use-package magit
  :ensure t
  :defer  t)

(add-to-list 'auto-mode-alist '("\\.latex\\'" . latex-mode))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold   t
        doom-themes-enable-italic t)
  (set-face-italic 'font-lock-comment-face t)
  (load-theme 'doom-city-lights t)
  (doom-themes-org-config))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight regular :height 143 :width normal)))))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1))
(use-package all-the-icons
  :ensure t
  :after  doom-modeline)

(add-hook 'org-mode-hook
          (lambda() (setq jit-lock-defer-time 0.15)))

(add-hook 'org-mode-hook
          (lambda()
            (setq org-latex-src-block-backend 'listing
                  org-lagex-listings-options  '(("numbers" "left")
                                                ("breaklines" "true")
                                                ("upquote" "true")
                                                ("autogobble" "true")
                                                ("showstringspaces" "false")
                                                ("basicstyle" "\\ttfamily")))))

(add-hook 'org-mode-hook
          (lambda()
            (setq org-preview-latex-default-process 'dvipng
                  org-startup-with-latex-preview     t)))

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
            (display-line-numbers-mode -1)
            (setq org-startup-indented t)))

(add-hook 'org-mode-hook
          (lambda()
            (setq-local electric-pair-inhibit-predicate
                        `(lambda(c)
                           (if (char-equal c ?<) t
                             (,electric-pair-inhibit-predicate c))))))

(add-hook 'prog-mode-hook
          (lambda()
            (setq c-indentation-style 'k&r
                  c-basic-offset       4)))

(add-hook 'prog-mode-hook
          (lambda()
            (setq treesit-font-lock-level 4
                  c-ts-mode-indent-style 'k&r
                  c-ts-mode-indent-offset 4)))

(setq treesit-language-source-alist
      '((cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(vterm vertico slime rainbow-mode rainbow-delimiters rainbow-blocks pdf-tools org-bullets marginalia magit lsp-mode evil-collection doom-themes doom-modeline corfu all-the-icons)))

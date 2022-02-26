;; -*- lexical-binding: t -*-

;; ---------------------------------------------------
;; Package
;; ---------------------------------------------------
(require 'package)
(setq package-archives '(
			 ("gnu" . "http://elpa.zilongshanren.com/gnu/")
			 ("melpa" . "http://elpa.zilongshanren.com/melpa/")

			 ;; ("melpa" . "https://melpa.org/packages/")
			 ;; ("org"   . "https://orgmode.org/elpa/")
			 ;; ("elpa"  . "https://elpa.gnu.org/packags/")

			 ))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

(require 'use-package)
(setq use-package-always-ensure t)

;; ---------------------------------------------------
;; evil
;; ---------------------------------------------------
(defun rune/evil-hook()
  (dolist (mode '(
      custom-mode
      eshell-mode
      git-rebase-mode
      erc-mode
      circe-server-mode
      circe-chat-mode
      circe-query-mode
      sauron-mode
      term-mode
      ))
  (add-to-list 'evil-emacs-state-modes mode)))


(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :hook (evil-mode . rune/evil-hook)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(evil-mode t)

;

;; ---------------------------------------------------
;; all-the-icons
;; ---------------------------------------------------
(use-package all-the-icons)

;; ---------------------------------------------------
;; helpful
;; ---------------------------------------------------
(use-package helpful)

;; ---------------------------------------------------
;; General
;; ---------------------------------------------------
 (use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "SPC")

  (rune/leader-keys
   "t" '(:ignore t : which-key "toggles")
   "tt" '(load-theme : :which-key "choose theme"))) 





(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(column-number-mode)
(global-linum-mode t)
(dolist (mode '(
		org-mode-hook
		term-mode-hook
		eshell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(setq tab-always-indent 'complete)

;; (icomplete-mode t)

(electric-pair-mode t)

(toggle-frame-maximized)

(tool-bar-mode -1)
(tooltip-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell t)

(setq cursor-type '(bar . 5))

(setq inhibit-splash-screen t)

(setq mouse-wheel-progressive-speed nil)

;; ---------------------------------------------------
;; Font
;; ---------------------------------------------------

;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 180)
(set-face-attribute 'default nil :font "Source Code pro" :height 180)

;; ---------------------------------------------------
;; Theme
;; ---------------------------------------------------
(use-package doom-themes)

;; (load-theme 'wombat)
;; (load-theme 'tango-dark)
;; (load-theme 'vscode-dark-plus)
;; (load-theme 'doom-dark+)
;; (load-theme 'doom-palenight t)
(load-theme 'doom-dracula t)

(use-package vscode-dark-plus-theme)



;; ---------------------------------------------------
;; Key
;; ---------------------------------------------------
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)

(global-set-key (kbd "C-h C-f") 'find-function)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; (package-install 'keycast)
;; (keycast-mode t)

(use-package command-log-mode)    ;; toggle-command-log-buffer ;; "C-c o"


;; ---------------------------------------------------
;; Mouse
;; ---------------------------------------------------
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)


;; ---------------------------------------------------
;; Company
;; ---------------------------------------------------
(use-package company)
(global-company-mode t)
(setq company-minimum-prefix-length 1)
(setq company-idle-delay 0)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)


;; ---------------------------------------------------
;; Vertico
;; ---------------------------------------------------
(use-package vertico)
(vertico-mode t)

;; ---------------------------------------------------
;; Orderless
;; ---------------------------------------------------
(use-package orderless)
(setq completion-styles '(orderless))

;; ---------------------------------------------------
;; Marginalia
;; ---------------------------------------------------
(use-package marginalia)
(marginalia-mode t)

;; ---------------------------------------------------
;; Embark
;; ---------------------------------------------------
(use-package embark)
(global-set-key (kbd "C-;") 'embark-act)
(setq prefix-help-command 'embark-prefix-help-command)    ;; "C-x C-h" find the command you want

;; ---------------------------------------------------
;; Consult
;; ---------------------------------------------------
(use-package consult)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "M-y") 'consult-yank-pop)

;; ---------------------------------------------------
;; doom-modeline
;; ---------------------------------------------------
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

;; ---------------------------------------------------
;; rainbow-delimiters
;; ---------------------------------------------------
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; ---------------------------------------------------
;; which-key
;; ---------------------------------------------------
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.1))

;; ---------------------------------------------------
;; Hydra
;; ---------------------------------------------------
(use-package hydra)

;; ---------------------------------------------------
;; Projectile
;; ---------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'vertico))
  ;; TODO: binding failure
  ;;:bind-keymap
  ;; ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/git")
    (setq projectile-project-search-path '("~/git")))
  (setq projectile-switch-project-action #'projectile-dired))

;; (use-package consult-projectile)


;; ---------------------------------------------------
;; Magit
;; ---------------------------------------------------
(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;(use-package evil-magit
;;  :after magit)

;; ---------------------------------------------------
;; Treemacs
;; ---------------------------------------------------
(use-package treemacs)
(treemacs-mode t)

















;; ---------------------------------------------------
;; 
;; ---------------------------------------------------

;; ---------------------------------------------------
;; Custom
;; ---------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "a226e096b9c4924c93b920ba50e545fb2d37c6d95d6b62b44e62cb6f03b081fa" default))
 '(package-selected-packages
   '(consult-projectile tree-sitter treemacs magit hydra projectile general doom-themes helpful vscode-dark-plus-theme which-key rainbow-delimiters doom-modeline command-log-mode use-package consult embark marginalia orderless vertico keycast company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

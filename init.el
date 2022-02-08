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
  (package-install 'use-package))

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
  (setq evil-disable-insert-state-bindings t)
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

(setq evil-move-cursor-back nil)

;; (use-package evil-leader
;;   :ensure t
;;   :config
;;   (global-evil-leader-mode)
;;   (evil-leader/set-leader "SPC"))

;; ---------------------------------------------------
;; evil-surround
;; ---------------------------------------------------
(use-package evil-surround)

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
;; ui
;; ---------------------------------------------------
(savehist-mode t)

(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

(column-number-mode)
(global-display-line-numbers-mode t)
(setq display-line-numbers-type t)
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
;; (scroll-bar-mode -1)
;; (fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell t)

(setq-default cursor-type '(bar . 5))

(setq inhibit-splash-screen t)

(setq mouse-wheel-progressive-speed nil)

(show-paren-mode t)

(setq make-backup-files nil)

(setq auto-save-default nil)

(delete-selection-mode t)

(global-hl-line-mode t)

;; hungry-delete
;; (global-hungry-delete-mode t)


;; no lockfiles
(setq create-lockfiles nil)
;; fullscreen
;; (setq initial-frame-alist (quote ((fullscreen . maximized))))


;; indent
(setq indent-tabs-mode nil)
(setq default-tab-width 3)
(setq tab-width 3)
(setq c-default-style "ellemtel" c-basic-offset 3)

(define-abbrev-table 'global-abbrev-table '(
                                            ("zk" "--- ezhonke --- ")
                                            ("db" "TRACE_DEBUG(\"--- ezhonke --- : [%]\", ?);")
                                            ("dx" "TRACE_DEBUG_EX(\"--- ezhonke ---- : [\" << \? << \"]\");")
                                            ("et" "TRACE_FUNCTION_DEBUG(\"enter\");")
                                            ("ex" "TRACE_FUNCTION_DEBUG(\"exit\");")
                                            ("td" "// TODO: ezhonke")
                                            ))
;; define function
(add-hook 'c-mode-hook 'linux-c-mode)
;; (add-hook 'c-mode-hook 'lsp-mode)
(add-hook 'c++-mode-hook 'linux-c-mode)
;; (add-hook 'c++-mode-hook 'lsp-mode)
(defun linux-c-mode()
  (define-key c-mode-map [return] 'newline-and-indent)
  (interactive)
  (c-set-style "k&r")
  (c-toggle-hungry-state)
  (setq c-basic-offset 3)
  (setq c++-basic-offset 3)
  (imenu-add-menubar-index)
  (which-function-mode)
  )
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))



;; ---------------------------------------------------
;; Font
;; ---------------------------------------------------

;; (set-face-attribute 'default nil :font "Fira Code Retina" :height 180)
(set-face-attribute 'default nil :font "Source Code pro" :height 180)

;; ---------------------------------------------------
;; dashboard
;; ---------------------------------------------------
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
;; Set the title
(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
;; Set the banner
(setq dashboard-startup-banner [VALUE])
;; Value can be
;; 'official which displays the official emacs logo
;; 'logo which displays an alternative emacs logo
;; 1, 2 or 3 which displays one of the text banners
;; "path/to/your/image.gif", "path/to/your/image.png" or "path/to/your/text.txt" which displays whatever gif/image/text you would prefer

;; Content is not centered by default. To center, set
(setq dashboard-center-content t)

;; To disable shortcut "jump" indicators for each section, set
(setq dashboard-show-shortcuts nil)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(setq dashboard-set-navigator t)
(setq dashboard-set-init-info t)
(setq dashboard-set-footer nil)

;; ---------------------------------------------------
;; Theme
;; ---------------------------------------------------
(use-package doom-themes)

;; (load-theme 'wombat)
;; (load-theme 'tango-dark)
;; (load-theme 'vscode-dark-plus)
(load-theme 'doom-dark+ t)
;; (load-theme 'doom-palenight t)
;; (load-theme 'doom-dracula t)

(use-package vscode-dark-plus-theme)


;; ---------------------------------------------------
;; auto-highlight-symbol
;; ---------------------------------------------------
(use-package auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;; ---------------------------------------------------
;; highlight-indent-guides
;; ---------------------------------------------------
(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; ---------------------------------------------------
;; ace-window
;; ---------------------------------------------------
;; (use-package ace-window) 

;; ---------------------------------------------------
;; winum
;; ---------------------------------------------------
(use-package winum)
(winum-mode t)

;; ---------------------------------------------------
;; Key
;; ---------------------------------------------------
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "<f2>") 'open-init-file)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-key)

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
(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-cycle t))

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
(defun dw/get-project-root()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
;;  :straight t
  :demand t
  :bind (
         ("C-s" . consult-line)
         ("C-M-l" . consult-imenu)
         ("C-M-j" . persp-switch-to-buffer*)
         ("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         :map minibuffer-local-map
         ("C-r" . consult-history)
         )
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

;; ---------------------------------------------------
;; Embark-Consult
;; ---------------------------------------------------
(use-package embark-consult)

;; ---------------------------------------------------
;; Wgrep
;; ---------------------------------------------------
(use-package wgrep)

(setq wgrep-auto-save-buffer t)

(eval-after-load 'consult
  '(eval-after-load
       'embark
     '(progn
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook
                  #'consult-preview-at-point-mode))))

(defun embark-export-write ()
    "Export the current vertico results to a writable buffer if possible.
Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
    (interactive)
    (require 'embark)
    (require 'wgrep)
    (pcase-let ((`(,type . ,candidates)
                 (run-hook-with-args-until-success 'embark-candidate-collectors)))
      (pcase type
        ('consult-grep
         (let ((embark-after-export-hook #'wgrep-change-to-wgrep-mode))
           (embark-export)))
        ('file
         (let ((embark-after-export-hook #'wdired-change-to-wdired-mode))
           (embark-export)))
        ('consult-location
         (let ((embark-after-export-hook #'occur-edit-mode))
           (embark-export)))
        (x (user-error "embark category %S doesn't support writable export" x)))))


(define-key minibuffer-local-map (kbd "C-c C-e") 'embark-export-write)

;; ---------------------------------------------------
;; doom-modeline
;; ---------------------------------------------------
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t))

(setq doom-modeline-window-width-limit fill-column)
(setq doom-modeline-project-detection 'auto)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-buffer-state-icon t)

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
  (setq which-key-idle-delay 0))

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

  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

  :custom ((projectile-completion-system 'auto))
  ;; TODO: binding failure
  ;;:bind-keymap
  ;; ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "/tmp/ezhonke/vsapc/")
    (setq projectile-project-search-path '("/tmp/ezhonke/vsapc/")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'native))

(use-package consult-projectile)


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
;; ranger
;; ---------------------------------------------------
(use-package ranger) 


;; ---------------------------------------------------
;; Treemacs
;; ---------------------------------------------------
;; (use-package treemacs)
;; (treemacs-mode t)
;; (setq treemacs--width-is-locked nil)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))


;; ---------------------------------------------------
;; Nox
;; ---------------------------------------------------
(add-to-list 'load-path "/home/ezhonke/git/nox/")

(require 'posframe)
(require 'nox)

(add-to-list 'nox-server-programs '((c++-mode c-mode) . ("clangd"  "--background-index=false")))

(dolist (hook (list
                ;; 'js-mode-hook
                ;; 'rust-mode-hook
                ;; 'python-mode-hook
                ;; 'ruby-mode-hook
                ;; 'java-mode-hook
                ;; 'sh-mode-hook
                ;; 'php-mode-hook
                'c-mode-common-hook
                'c-mode-hook
                ;; 'csharp-mode-hook
                'c++-mode-hook
                ;; 'haskell-mode-hook
                ))
(add-hook hook '(lambda () (nox-ensure))))





;; ---------------------------------------------------
;; Go
;; ---------------------------------------------------
(use-package go-mode)


;; ---------------------------------------------------
;; Go
;; ---------------------------------------------------
;; (require 'lsp-mode)
;; (add-hook 'go-mode-hook #'lsp-deferred)
;; 
;; ;; Set up before-save hooks to format buffer and add/delete imports.
;; ;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))
;; (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
;; 
;; (lsp-register-custom-settings
;;  '(("gopls.completeUnimported" t t)
;;    ("gopls.staticcheck" t t)))




;; (use-package lsp-mode
;;   :init
;;   ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
;;          (go-mode . lsp)
;;          ;; if you want which-key integration
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :commands lsp)
;; 
;; ;; optionally
;; (use-package lsp-ui :commands lsp-ui-mode)
;; ;; if you are helm user
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; ;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; 
;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; 
;; ;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))


;; ---------------------------------------------------
;; Tabbar
;; ---------------------------------------------------
;; (use-package tabbar
;;   :ensure t
;;   :config
;;   (tabbar-mode 1))

;; ---------------------------------------------------
;; Recentf
;; ---------------------------------------------------
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 10)



;; ---------------------------------------------------
;; hungry-delete
;; ---------------------------------------------------
(use-package hungry-delete)
(global-hungry-delete-mode)

;; ---------------------------------------------------
;; transient
;; ---------------------------------------------------
(require 'transient)

;; ---------------------------------------------------
;; rg
;; ---------------------------------------------------
(use-package rg)
(rg-enable-default-bindings)


;; ---------------------------------------------------
;; General
;; ---------------------------------------------------
;; :config
;; (general-create-definer rune/leader-keys
;;   :keymaps '(normal insert visual emacs)
;;   :prefix "SPC"
;;   :global-prefix "SPC")
;;
;; (rune/leader-keys
;;  "t" '(:ignore t : which-key "toggles")
;;  "tt" '(load-theme : :which-key "choose theme")))

(use-package general)
;; (general-define-key
;;  :prefix "SPC"
;;  :non-normal-prefix "M-SPC"
;;  :keymaps 'normal
;;  "TAB" '(ido-switch-buffer :which-key "prev buffer")
;;  ;; unbind SPC and give it a title for which-key (see echo area)
;;  "" '(nil :which-key "my lieutenant general prefix")
;;  ;; bind nothing but give SPC f a description for which-key
;;  "f" '(:ignore t :which-key "file prefix")
;;  "f" '(:ignore t :wk "Files")
;;  ;; use a cons as a replacement
;;  "g" '(:ignore t :wk ("g-key" . "git prefix"))
;;  ;; toggle lispy; use a function as a replacement to show if currently on
;;  "l" '(lispy-mode :wk my-lispy-which-key-display)
;;  ;; for a keymap, only the keys will be matched;
;;  ;; :no-match-binding is not necessary
;;  "p" '(:keymap projectile-command-map :wk "projectile prefix"))

(general-define-key
 :states '(normal)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
  "1"   '(winum-select-window-1 :which-key "window 1")
  "2"   '(winum-select-window-2 :which-key "window 2")
  "3"   '(winum-select-window-3 :which-key "window 3")
  "'"   '(iterm-focus :which-key "iterm")
  "?"   '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
  "/"   '(counsel-ag :wich-key "ag")
  "TAB" '(evil-switch-to-windows-last-buffer :which-key "prev buffer")
  "."   '(avy-goto-word-or-subword-1  :which-key "go to word")
  "SPC" '(counsel-M-x :which-key "M-x")
  "a"   '(hydra-launcher/body :which-key "Applications")
  "b"   '(hydra-buffer/body t :which-key "Buffer")
  "c"   '(:ignore t :which-key "Comment")
  "cl"  '(comment-or-uncomment-line :which-key "comment line")

  "f"   '(:ignore t :which-key "Files")
  "fd"  '(counsel-git :which-key "find in git dir")
  "fr"  '(consult-recent-file :which-key "consult-recent-file")
  "ft"  '(treemacs :which-key "treemacs")

  ;; "g"   '(:keymap magit-mode-map :wk "Magit")

  ;; "o"   '(:keymap org-mode-map :wk "Org")
  "p"   '(:keymap projectile-command-map :wk "Projectile")

  "r"   '(:keymap rg-mode-map :wk "Ripgrep")
  ;; TODO search
  "s"   '(:keymap isearch-mode-map :wk "Search")

  "t"   '(:which-key "Toggles")


  "w"   '(:ignore t :which-key "Windows")
  "wd"  '(delete-window :which-key "delete-window")
  "wo"  '(delete-other-windows :which-key "delete-other-windows")
  "wv"  '(evil-window-vsplit :which-key "evil-window-vsplit")
  "ws"  '(evil-window-split :which-key "evil-window-split")
  )

;; ---------------------------------------------------
;; language
;; ---------------------------------------------------
(use-package yaml-mode
  :ensure t
  :config
  (setq yaml-indent-offset t))

;; ---------------------------------------------------
;; modern-cpp-font-lock
;; ---------------------------------------------------
(use-package modern-cpp-font-lock
  :ensure t)




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
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "0466adb5554ea3055d0353d363832446cd8be7b799c39839f387abb631ea0995" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "a226e096b9c4924c93b920ba50e545fb2d37c6d95d6b62b44e62cb6f03b081fa" default))
 '(package-selected-packages
   '(modern-cpp-font-lock ranger dashboard winum highlight-indent-guides evil-surround auto-highlight-symbol rg hungry-delete tabbar consult-projectile tree-sitter treemacs magit hydra projectile general doom-themes helpful vscode-dark-plus-theme which-key rainbow-delimiters doom-modeline command-log-mode use-package consult embark marginalia orderless vertico keycast company)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

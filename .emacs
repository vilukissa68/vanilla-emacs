;; PACKAGE STUFF
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure and load use-package
(setq use-package-always-ensure t)
(require 'diminish)

(eval-when-compile
  (defvar use-package-verbose t)
  (require 'use-package))
;; EVIL MODE
(use-package evil)
(use-package evil-leader)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
(use-package evil-collection)
(evil-mode t)
(evil-collection-init)


;; Leader key
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key 
  "<SPC>" 'execute-extended-command
  "bl" 'switch-to-buffer
  "bn" 'switch-to-next-buffer
  "bb" 'switch-to-previous-buffer
  "bd" 'kill-buffer
  "ff" 'find-file
  "ha" 'apropos-command
  "hi" 'info
  "hb" 'describe-bindings
  "hk" 'describe-key
  "ms" 'magit-status
  "mc" 'magit-commit
  "mp" 'magit-push
  "wb" 'balance-windows
  "wv" 'split-window-right
  "ws" 'split-window-below
  "wn" 'other-window
  "wb" 'previous-window
  "wd" 'delete-window)

;; UNDO TREE
(use-package undo-tree
  :diminish undo-tree-mode
  :config 
  (global-undo-tree-mode 1))

;; SMOOTH SCROLLING
(use-package smooth-scrolling
  :config
  (setq smooth-scroll-margin 5
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        scroll-margin 5)
  (smooth-scrolling-mode 1))


;; WHICH KEY
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5
        which-key-popup-type 'side-window
        which-key-side-window-location 'bottom))

;; MAGIT
(use-package magit)

;; COMPANY MODE
(use-package company)
;; Enable automatically for all buffers
(add-hook 'after-init-hook 'global-company-mode)

;; GENERAL
;; Line numbers
(global-display-line-numbers-mode)
;; Hide menu bar
(menu-bar-mode -1)
;; Hide tool bat
(tool-bar-mode -1)
;; Hide scroolbar
(toggle-scroll-bar -1)

;; Theming
(use-package dracula-theme)
;; Don't change the font size for some headings and titles (default t)
(setq dracula-enlarge-headings nil)
;; Adjust font size of titles level 1 (default 1.3)
(setq dracula-height-title-1 1.25)
;; Adjust font size of titles level 2 (default 1.1)
(setq dracula-height-title-1 1.15)
;; Adjust font size of titles level 3 (default 1.0)
(setq dracula-height-title-1 1.05)
;; Adjust font size of document titles (default 1.44)
(setq dracula-height-doc-title 1.4)
;; Use less pink and bold on the mode-line and minibuffer (default nil)
(setq dracula-alternate-mode-line-and-minibuffer t)
;; Load theme
(load-theme 'dracula t)


;; Keybindings
;; Cancel with <ESC>
(define-key key-translation-map (kbd "<ESC>") (kbd "C-g"))

;; Window switching
(evil-global-set-key 'motion (kbd "C-<up>") 'evil-window-up)
(evil-global-set-key 'motion (kbd "C-<down>") 'evil-window-down)
(evil-global-set-key 'motion (kbd "C-<left>") 'evil-window-left)
(evil-global-set-key 'motion (kbd "C-<right>") 'evil-window-right)
(evil-global-set-key 'motion (kbd "C-k") 'evil-window-up)
(evil-global-set-key 'motion (kbd "C-j") 'evil-window-down)
(evil-global-set-key 'motion (kbd "C-l") 'evil-window-left)
(evil-global-set-key 'motion (kbd "C-h") 'evil-window-right)

;; Brackets
;; Colored paranthesis
(use-package rainbow-delimiters)
;; Enable raindow delimiters for all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) 
;; Auto close paranthesis
(use-package smartparens)
;; Enable smart parenthesis for all programming modes
(add-hook 'prog-mode-hook 'smartparens-mode) 

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil-leader use-package evil diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

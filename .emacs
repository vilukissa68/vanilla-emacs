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
(use-package evil
    :init
    (setq evil-want-keybinding nil)
    :config
    (evil-mode 1)
    (setq evil-want-fine-undo 'fine))
(use-package evil-leader)
(use-package evil-surround
  :ensure t
  :config
    (global-evil-surround-mode 1))
(use-package evil-collection
  :after evil
  :config
    (evil-collection-init))



;; Leader key
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(evil-leader/set-key 
    "<SPC>" 'execute-extended-command
    "bl" 'switch-to-buffer
    "bn" 'switch-to-next-buffer
    "bb" 'switch-to-previous-buffer
    "bd" 'kill-current-buffer
    "ff" 'find-file
    "ft" 'neotree-toggle
    "ha" 'apropos-command
    "hi" 'info
    "hb" 'describe-bindings
    "hk" 'describe-key
    "gs" 'magit-status
    "gc" 'magit-commit
    "gp" 'magit-push
    "wb" 'balance-windows
    "wv" 'split-window-right
    "ws" 'split-window-below
    "wn" 'other-window
    "wb" 'previous-window
    "wd" 'delete-window
    "xx" 'eval-last-sexp
    "xe" 'eval-expression)

;; UNDO TREE
(use-package undo-tree
    :diminish undo-tree-mode
    :after evil
    :config 
    (evil-set-undo-system 'undo-tree)
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
(use-package magit
    :defer t)

;; COMPANY MODE
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0
	company-minimum-prefix-length 2
	company-show-numbers t
	company-tooltip-limit 7
	company-tooltip-align-annotations t
	company-tooltip-flip-when-above t)
  ;; Enable automatically for all buffers
  (global-company-mode t))

;; IVY
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))


(use-package counsel
	     :init
	     :ensure t
	     :config
	     (counsel-mode 1))


(use-package ivy-rich
  :after ivy
  :config
  (setq ivy-rich-path-style 'abbrev)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-initial-inputs-alist nil)
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
	(get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
	(if (symbolp icon)
	    (all-the-icons-icon-for-mode 'fundamental-mode)
	  icon))))
  (setq ivy-rich-display-transformers-list
	'(ivy-switch-buffer
	  (:columns
	   ((ivy-rich-switch-buffer-icon (:width 2))
	    (ivy-rich-candidate (:width 30))
	    (ivy-rich-switch-buffer-size (:width 7))
	    (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
	    (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
	    (ivy-rich-switch-buffer-project (:width 15 :face success))
	    (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
	   :predicate
	   (lambda (cand) (get-buffer cand)))))
  (ivy-rich-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :after ivy-rich
  :init (all-the-icons-ivy-rich-mode 1))
												     
;; NEOTREE
(use-package neotree)

;; YASNIPPET
(use-package yasnippet
  :config
  (yas-global-mode t))

;; ORG-MODE
(use-package org)
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))
(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; Configure leader key
  (evil-leader/set-ket-for-mode 'org-mode
    "ot" 'org-todo
    "oT" 'org-show-todo-tree
    "ov" 'org-mark-element
    "oa" 'org-agenda
    "ol" 'evil-org-open-links
    "oC" 'org-resolve-clocks
    "oih" 'org-insert-heading-respect-content
    "ois" 'org-insert-subheading
    "op" 'org-previous-visible-heading
    "on" 'org-next-visible-heading
    "ort" 'org-ali))

;; C/C++


;; PYTHON
;;(use-package elpy
;;  :ensure t
;;  :defer t
;;  :init
;;  (advice-add 'python-mode :before 'elpy-enable)
;;  :config
;;  (add-hook 'elpy-mode-hook '(lambda ()
	;;		       (progn
		;;		 (setq-local flymake-start-syntax-check-on-newline t)
			;;	 (setq-local flymake-no-changes-timeout 0.5))))
;;  (evil-leader/set-key-for-mode 'python-mode
;;    "cc" 'elpy-shell-send-buffer
;;    "gd" 'elpy-goto-definition))


;; PYvENV
(use-package pyvenv 
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/anaconda3/envs")
  (pyvenv-mode 1))

;; ANACONDA
(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (evil-leader/set-key-for-mode 'anaconda-mode
    "mc" 'anaconda-mode-complete))


;; JUPYTER NOTEBOOKS
(use-package ein
  :defer t
  :config
  (setq ein:worksheet-enable-undo t)
  (setq ein:output-area-inlined-images t)
  (evil-leader/set-key-for-mode 'Notebook
    "mca" 'ein:worksheet-insert-cell-above-km
    "mcb" 'ein:worksheet-insert-cell-below-km
    "mcc" 'ein:worksheet-copy-cell-km
    "mcy" 'ein:worksheet-yank-cell-km
    "mcd" 'ein:worksheet-kill-cell-km
    "mcc" 'ein:worksheet-copy-cell-km
    "mcm" 'ein:worksheet-merge-cell-km
    "moc" 'ein:worksheet-clear-all-output-km
    "mww" 'ein:notebook-save-notebook)
  (evil-define-key 'motion "S-<up>" 'ein:worksheet-move-cell-up-km)
  (evil-define-key 'motion "S-<down>" 'ein:worksheet-move-cell-down-km)
  (evil-define-key 'normal "RET" 'ein:worksheet-execute-cell-and-goto-next-km))

;; GENERAL
;; Line numbers
;;(global-display-line-numbers-mode)
;; Hide menu bar
(menu-bar-mode -1)
;; Hide tool bat
(tool-bar-mode -1)
;; Hide scroolbar
(toggle-scroll-bar -1)

;; Theming
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italics t)
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; Statusbar
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-bar-height 20)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline--battery-status t)
  )

;; Icons
(use-package all-the-icons)

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
(evil-global-set-key 'motion (kbd "C-h") 'evil-window-left)
(evil-global-set-key 'motion (kbd "C-l") 'evil-window-right)

;; Brackets
;; Colored paranthesis  
(use-package rainbow-delimiters
  :defer t)
;; Enable raindow delimiters for all programming modes
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode) 
;; Auto close paranthesis
(use-package smartparens)
;; Enable smart parenthesis for all programming modes
(add-hook 'prog-mode-hook 'smartparens-mode) 


;; ADVANCDED CUSTOMIZATION
;; Startup time
(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		     (time-subtract after-init-time before-init-time)))
	   gcs-done))
(add-hook 'emacs-startup-hook #'efs/display-startup-time)



;; SPLASH SCREEN

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#6272a4" "#bd93f9" "#8be9fd" "#f8f8f2"])
 '(custom-safe-themes
   '("549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default))
 '(package-selected-packages
   '(projectile counsel-world-clock counsel-spotify evil-leader use-package evil diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

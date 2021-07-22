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
  :ensure t
  :init
    (setq evil-want-keybinding nil)
    (evil-mode 1)
  :config
    (setq evil-want-fine-undo 'fine))

(use-package evil-leader)
(use-package evil-surround
  :ensure t
  :after evil
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
    "bb" 'switch-to-buffer
    "bn" 'switch-to-next-buffer
    "bp" 'switch-to-previous-buffer
    "bd" 'kill-current-buffer
    "cm" 'compile
    "ff" 'find-file
    "ft" 'treemacs
    "fr" 'revert-buffer
    "fed" 'my/open-configuration
    "fer" 'my/reload-configuration
    "hi" 'info
    "hb" 'describe-bindings
    "hf" 'describe-function
    "hk" 'describe-key
    "gs" 'magit-status
    "gc" 'magit-commit
    "gp" 'magit-push
    "oc" 'org-capture
    "os" 'org-schedule
    "ot" 'org-set-tags-command
    "oT" 'org-todo-list
    "ov" 'org-mark-element
    "oa" 'org-agenda
    "ol" 'evil-org-open-links
    "oC" 'org-resolve-clocks
    "oih" 'org-insert-heading-respect-content
    "ois" 'org-insert-subheading
    "op" 'org-previous-visible-heading
    "on" 'org-next-visible-heading
    "ort" 'org-ali
    "oS" 'org-tree-slide-mode
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
	company-tooltip-flip-when-above t
	)
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
												     
;; PROJECTILE
(use-package projectile
  :ensure t
  :init
  (projectile-mode 1)
  :config
    (evil-leader/set-key "p" projectile-command-map)
    (evil-normalize-keymaps))

;; TREEMACS
(use-package treemacs)

;; YASNIPPET
(use-package yasnippet
  :config
  (yas-global-mode t))

;; ORG-MODE
(use-package org
  :ensure t
  :config
  ;; Set agenda files
    (setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
				 "~/Dropbox/orgfiles/tasks.org"
				 "~/Dropbox/orgfiles/notes.org"))
  ;; Define capture templates
    (setq org-capture-templates
	'(("a" "Appointment" entry (file  "~/Dropbox/orgfiles/gcal.org" )
	"* %^{Brief Descriptions} %^G")
	("l" "Link" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Links")
	"* %? %^L %^g \n%T" :prepend t)
	("t" "To Do Item" entry (file "~/Dropbox/orgfiles/tasks.org")
	"* TODO %?\n%u" :prepend t)
	("n" "Note" entry (file+headline "~/Dropbox/orgfiles/notes.org" "Note space")
	"* %?\n%u" :prepend t)
    )))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))
  ;; Enable in all modes

;; Org-mode presentations
(use-package org-tree-slide
  :after org
  :custom
  (org-image-actual-width nil))


;; Load GCAL setup
(load "~/Dropbox/orgfiles/orgsetup.el")

;; C/C++
(use-package lsp-mode
  :ensure t
  :config
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
    ;;(evil-define-key '(normal visual) 'lsp-mode (kbd "SPC l") lsp-command-map)
    (evil-leader/set-key "l" lsp-command-map)
    (evil-normalize-keymaps))

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-ui)
(use-package dap-mode)
(use-package lsp-treemacs)

;; Hook to lsp
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'lsp-mode
	    '(lambda ()
		    (progn
				(setq-local flymake-start-syntax-check-on-newline t)
				(setq-local flymake-no-changes-timeout 0.5))))

;; PYTHON
(use-package elpy
  :ensure t
  :defer t
 :init
  (advice-add 'python-mode :before 'elpy-enable)
  ;; elpy syntax check new line
  (add-hook 'elpy-mode-hook
	    '(lambda ()
		       (progn
				 (setq-local flymake-start-syntax-check-on-newline t)
				 (setq-local flymake-no-changes-timeout 0.5))))
  :config
  (evil-leader/set-key-for-mode 'python-mode
    "me" 'elpy-shell-send-buffer
    "md" 'elpy-doc
    "gd" 'elpy-goto-definition))



;; PYvENV
(use-package pyvenv 
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/anaconda3/envs")
  (pyvenv-mode 1))


;; ANACONDA
;;(use-package anaconda-mode
;;  :config
 ;; (add-hook 'python-mode-hook 'anaconda-mode)
 ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
 ;; (evil-leader/set-key-for-mode 'anaconda-mode
  ;;  "mc" 'anaconda-mode-complete))


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


;; LATEX
(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "C-r") 'isearch-backward)
  (add-hook 'pdf-view-mode-hook (lambda ()
				  (bms/pdf-midnite-amber))) ; automatically turns on midnight-mode for pdfs
  )

(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

(use-package reftex
  :ensure t
  :defer t
  :config
  (setq reftex-cite-prompt-optional-args t)) ;; Prompt for empty optional arguments in cite

;(use-package auto-dictionary
;  :ensure t
;  :init(add-hook 'flyspell-mode-hook (lambda () (auto-dictionary-mode 1))))

(use-package company-auctex
  :ensure t
  :init (company-auctex-init))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\'" . latex-mode)
  :config (progn
	    (setq TeX-source-correlate-mode t)
	    (setq TeX-source-correlate-method 'synctex)
	    (setq TeX-auto-save t)
	    (setq TeX-parse-self t)
	    (setq-default TeX-master "paper.tex")
	    (setq reftex-plug-into-AUCTeX t)
	    (pdf-tools-install)
	    (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
		  TeX-source-correlate-start-server t)
	    ;; Update PDF buffers after successful LaTeX runs
	    (add-hook 'TeX-after-compilation-finished-functions
		      #'TeX-revert-document-buffer)
	    (add-hook 'LaTeX-mode-hook
		      (lambda ()
			(reftex-mode t)
			(flyspell-mode t)))
	    ))


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

;; Pywal integration
(use-package ewal
  :init (setq ewal-use-built-in-always-p nil
              ewal-use-built-in-on-failure-p t
              ewal-built-in-palette "sexy-material"))
(use-package ewal-spacemacs-themes
  :init (progn
          (setq spacemacs-theme-underline-parens t
                my:rice:font (font-spec
                              :family "Source Code Pro"
                              :weight 'semi-bold
                              :size 11.0))
          (show-paren-mode +1)
          (global-hl-line-mode)
          (set-frame-font my:rice:font nil t)
          (add-to-list  'default-frame-alist
                        `(font . ,(font-xlfd-name my:rice:font))))
  :config (progn
            (load-theme 'ewal-spacemacs-modern t)
            (enable-theme 'ewal-spacemacs-modern)))
(use-package ewal-evil-cursors
  :after (ewal-spacemacs-themes)
  :config (ewal-evil-cursors-get-colors
           :apply t :spaceline t))
(use-package spaceline
  :after (ewal-evil-cursors winum)
  :init (setq powerline-default-separator nil)
  :config (spaceline-spacemacs-theme))

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


;; SPALSH SCREEN
(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Emacs is Lisp!")
  ;;(setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-items '((recents . 10)
			  (bookmarks . 10)))
  (dashboard-setup-startup-hook))


;; USER DEFINED FUNCTION
(defun my/open-configuration ()
  (interactive)
  (find-file "~/.emacs"))
(defun my/reload-configuration ()
  (interactive)
  (load-file "~/.emacs"))

;; USER DEFINED HOOKS
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#282a36" "#ff5555" "#50fa7b" "#f1fa8c" "#6272a4" "#bd93f9" "#8be9fd" "#f8f8f2"])
 '(custom-safe-themes
   '("88f59acbeacefb4998f45126d4d8ae8b2184f2a48753db362a349fd55321c7e1" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "549ccbd11c125a4e671a1e8d3609063a91228e918ffb269e57bd2cd2c0a6f1c6" default))
 '(package-selected-packages
   '(projectile counsel-world-clock counsel-spotify evil-leader use-package evil diminish)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

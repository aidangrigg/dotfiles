;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package (we'll use this to install packages)
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Random options

;;; Clean up the ui

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 10) ; Give some breathing room
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)

(global-visual-line-mode 1)
(visual-line-mode 1)
(electric-pair-mode 1)
(global-hl-line-mode 1) ;; Cursor line

;;; Backups

(setq backup-directory-alist `(("." . "~/.cache/emacs")))
(setq make-backup-files t               ; backup of a file the first time it is saved.
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default t               ; auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)
      )

;; Dired

(setq dired-listing-switches "-aBhl  --group-directories-first")

;;; Theming & font
(set-face-attribute 'default nil :font "Roboto Mono" :height 115)

;;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; PLUGINS

;; Evil mode

(use-package undo-fu)

(use-package evil
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-undo-system 'undo-fu)
  :config

  ;; C-d & C-u center cursor on line
  (evil-define-command my-scroll-down ()
    (evil-scroll-down 0)
    (evil-scroll-line-to-center nil))

  (evil-define-command my-scroll-up ()
    (evil-scroll-up 0)
    (evil-scroll-line-to-center nil))

  (define-key evil-normal-state-map (kbd "C-d") 'my-scroll-down)
  (define-key evil-normal-state-map (kbd "C-u") 'my-scroll-up)

  (evil-mode 1))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental))

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (nvmap :prefix "SPC" :keymaps 'override
    "f"  '(projectile-command-map :which-key "projectile")
    "g"  '(magit-status :which-key "magit")

    "t"  '(:ignore t :which-key "theme")
    "td" '((lambda () (interactive) (load-theme 'tao-yin t)) :which-key "dark")   ;; dark theme
    "tl" '((lambda () (interactive) (load-theme 'tao-yang t)) :which-key "light")  ;; light theme
    "tc" '(counsel-load-theme :which-key "colourful")  ;; light theme
    "ts" '((lambda () (interactive) (load-theme 'doom-oksolar-light t)) :which-key "solarized_light")  ;; light theme

    "w" '(:ignore z :which-key "zen")
    "wm" '(writeroom-mode :which-key "mode")
    "wi" '(writeroom-increase-width :which-key "increase width")
    "wd" '(writeroom-increase-width :which-key "decrease width")

    "o"  '(:ignore o :which-key "org")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-capture :which-key "capture")

    "z" '(:ignore z :which-key "zettelkasten")
    "zc" '(org-roam-capture :which-key "capture")
    "zf" '(org-roam-node-find :which-key "find")
    "zi" '(org-roam-node-insert :which-key "insert link")

    "k" '(eldoc-doc-buffer :which-key "show documentation")
    "d" '(flymake-show-buffer-diagnostics :which-key "buffer diagnostics")
    "D" '(flymake-show-project-diagnostics :which-key "project diagnostics")
    "rn" '(eglot-rename :which-key "rename")
    "ca" '(eglot-code-actions :which-key "code action")))

(use-package diminish
  :diminish eldoc-mode)

(use-package evil-collection
  :diminish evil-collection-unimpaired-mode
  :after evil
  :config
  (evil-collection-init))

;; Projectile

(use-package projectile
  :diminish projectile-mode 
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Spellcheck [https://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs/]

(cond
 ;; try hunspell at first
  ;; if hunspell does NOT exist, use aspell
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_AU")
  (setq ispell-local-dictionary-alist
        ;; Please note the list `("-d" "en_AU")` contains ACTUAL parameters passed to hunspell
        ;; You could use `("-d" "en_AU,en_AU-med")` to check with multiple dictionaries
        '(("en_AU" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_AU") nil utf-8)))

  ;; new variable `ispell-hunspell-dictionary-alist' is defined in Emacs
  ;; If it's nil, Emacs tries to automatically set up the dictionaries.
  (when (boundp 'ispell-hunspell-dictionary-alist)
    (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)))

 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_AU"))))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit

(use-package magit)

;; Org

(use-package org
  :config

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-adapt-indentation t)
  (setq org-enforce-todo-dependencies t)

  ;; Capture templates

  (setq org-capture-templates
	'(("t" "Task" entry
	   (file+olp "~/notes/org/tasks.org" "Inbox")
	   "* TODO %?\n" :empty-lines 1)
	  ("r" "Recipe" entry
	   (file "~/notes/org/recipes.org")
	   "* %^{Name}\n\n - %^{Servings} Servings\n\n** Source\n -%? \n\n** Ingredients\n -\n\n** Recipe\n\n 1. \n\n" :empty-lines 1)))
  

  ;; Appearance
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars t)

  (let* ((variable-tuple
          (cond ((x-list-fonts "Roboto Serif")    '(:font "Roboto Serif"))
                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                ((x-list-fonts "Verdana")         '(:font "Verdana"))
                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (headline           `(:inherit default)))
    
    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (setq org-agenda-files
	'("~/notes/org/tasks.org")))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/notes/org/zettel"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (setq org-roam-capture-templates
	'(("m" "main" plain "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
	(file-name-nondirectory
	 (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
	(concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (org-roam-db-autosync-mode))

(use-package evil-org
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme '(textobjects insert return navigation additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package writeroom-mode)

(use-package ivy
  :diminish ivy-mode
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :diminish counsel-mode
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package swiper)

(use-package tao-theme)

(use-package doom-themes
  :init (load-theme 'doom-oksolar-light t)
  :config
  (setq doom-themes-enable-bold t   
        doom-themes-enable-italic t))

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode)
  :custom
  (simple-modeline-segments
   '((simple-modeline-segment-modified
      simple-modeline-segment-buffer-name
      simple-modeline-segment-position)
     (simple-modeline-segment-minor-modes
      simple-modeline-segment-vc
      simple-modeline-segment-misc-info
      simple-modeline-segment-process
      simple-modeline-segment-major-mode))))

(use-package direnv
  :config
  (direnv-mode))

(use-package which-key
  :config (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :font "Roboto Serif" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :font "Roboto Serif" :height 1.75))))
 '(org-level-2 ((t (:inherit default :font "Roboto Serif" :height 1.5))))
 '(org-level-3 ((t (:inherit default :font "Roboto Serif" :height 1.25))))
 '(org-level-4 ((t (:inherit default :font "Roboto Serif" :height 1.1))))
 '(org-level-5 ((t (:inherit default :font "Roboto Serif"))))
 '(org-level-6 ((t (:inherit default :font "Roboto Serif"))))
 '(org-level-7 ((t (:inherit default :font "Roboto Serif"))))
 '(org-level-8 ((t (:inherit default :font "Roboto Serif")))))

;; LSP Tings

(use-package eglot
  :config
  (define-key evil-normal-state-map (kbd "gd") 'xref-find-definitions))

(use-package company
  :diminish company-mode
  :bind ((:map company-active-map
               ("TAB" . company-complete-selection)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous))
         (:map company-search-map
               ("TAB" . company-complete-selection)
               ("C-n" . company-select-next)
               ("C-p" . company-select-previous)))
  :custom
  (global-company-mode 1)
  (company-global-modes
   '(not text-mode message-mode git-commit-mode org-mode magit-status-mode))
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 10)
  (company-tooltip-minimum-width 50))

;; this frontend properly renders propertized text, variable pitch font and
;; doesn't have to it within the parent-frame
(use-package company-box
  :diminish company-box-mode
  :after company
  :hook (company-mode . company-box-mode)
  :custom
  ;; Icons are huge!?
  (company-box-enable-icon nil)
  ;; Search doesn't scroll to focus the current selection
  (company-search-filtering t))

(use-package rust-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode)

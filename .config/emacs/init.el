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

;;; Clean up the ui

(setq inhibit-startup-message t)
(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 25) ; Give some breathing room
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)
(setq scroll-margin 8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d6d4e0512dcaae663f7bd304557d6bc8b78c576be5af9c0b62b8447fb79b5fde" "c3957b559cf3606c9a40777c5712671db3c7538e5d5ea9f63eb0729afeac832b" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "21101a7ee55bb8af0215f1735da8f3b48cf28025c2d86c0009175ce43ee01fb1" default))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'")))

(electric-pair-mode 0)
(global-hl-line-mode 0) ;; Cursor line

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

(setq auto-save-file-name-transforms
      `((".*" "~/.cache/emacs-saves/" t)))

;; Dired

(setq dired-listing-switches "-aBhl  --group-directories-first")
(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-dwim-target t)

;;; Theming & font
(set-face-attribute 'default nil :font "Roboto Mono" :height 115)
;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 110)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; jump to end of compilation buffer automatically
(setq compilation-scroll-output t)

(defun my-page-down ()
  (interactive)
  (next-line (/ (window-total-height) 2))
  (recenter))

(defun my-page-up ()
  (interactive)
  (previous-line (/ (window-total-height) 2))
  (recenter))

;; Random keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-v") 'my-page-up)
(global-set-key (kbd "C-v") 'my-page-down)
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)
(global-set-key (kbd "C-s") 'isearch-forward)

;; PLUGINS

;; MEOW

(use-package meow
  :config
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  (meow-setup)
  (meow-global-mode 1))

(use-package diminish
  :diminish visual-line-mode
  :diminish abbrev-mode
  :diminish auto-revert-mode)

;; Projectile

(use-package projectile
  :diminish projectile-mode 
  :config
  (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind (("C-c p" . 'projectile-command-map))
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package rg)

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

;; Magit

(use-package magit
  :bind (("C-c g" . 'magit)))

;; Org

(defun my-project-orgfile ()
  (interactive)
  (let ((default-directory "~/sync/notes/org/projects/")
	(project-name (projectile-project-name)))
    (find-file-other-window (expand-file-name (concat project-name ".org")))))

(defun my-project-agenda ()
  (interactive)
  (let ((org-agenda-files
	 (list
	  (concat "~/sync/notes/org/projects/"
		  (concat (projectile-project-name) ".org")))))
    (call-interactively #'org-agenda)))
  

(use-package org
  :bind (("C-c o a" . 'org-agenda)
	 ("C-c o c" . 'org-capture)
	 ("C-c o f" . 'my-project-orgfile)
	 ("C-c o p" . 'my-project-agenda)
	 :map org-mode-map
	 ("C-c C-c" . org-latex-export-to-latex))
  :config

  (setq org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-adapt-indentation t
	org-enforce-todo-dependencies t
	org-startup-with-inline-images t
	org-id-track-globally t)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.75))

  ;; Capture templates

  (setq org-capture-templates
	'(("t" "Task" entry
	   (file+olp "~/sync/notes/org/tasks.org" "Inbox")
	   "* TODO %?\n" :empty-lines 1)
	  ("r" "Recipe" entry
	   (file "~/sync/notes/org/recipes.org")
	   "* %^{Name}\n\n - %^{Servings} Servings\n\n** Source\n -%? \n\n** Ingredients\n -\n\n** Recipe\n\n 1. \n\n" :empty-lines 1)))

  ;; Appearance
  (setq org-hide-leading-stars t)

  ;; Babel stuff
  (org-babel-do-load-languages
   'org-babel-load-languages '((C . t)
			       (haskell . t)
			       (shell . t)))

  (setq org-agenda-files
	'("~/sync/notes/org/tasks.org"
	  "~/sync/notes/org/mobile.org"))

  (setq org-icalendar-include-todo t)
  (setq org-icalendar-use-scheduled '(todo-start event-if-todo))
  (setq org-icalendar-use-deadline '(todo-due event-if-todo)))

(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/sync/notes/org/zettel"))
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r g" . org-roam-graph)
         ("C-c r i" . org-roam-node-insert)
         ("C-c r c" . org-roam-capture)
         ;; Dailies
         ("C-c r j" . org-roam-dailies-capture-today))
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
  
  (org-roam-db-autosync-mode))

(use-package org-roam-ui)

(use-package org-download
  :bind (:map org-mode-map
	      ("C-c v" . 'org-download-clipboard))
  :config
  (setq-default org-download-image-dir "~/sync/notes/org/images"))

(use-package writeroom-mode)

(use-package expand-region
  :bind (("C-." . 'er/expand-region)))

(use-package multiple-cursors
  :bind (("C-c c s" . 'mc/mark-next-like-this)
	 ("C-c c l" . 'mc/edit-lines)
	 ("C-c c d" . 'mc/mark-all-like-this-in-defun)))

(use-package ivy
 :config
 (ivy-mode 1)
 (setq ivy-format-function 'ivy-format-function-arrow)
 (setq ivy-format-functions-alist '((t . ivy-format-function-arrow))))
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/") 

;; (load-theme 'eltbus t)

;; (use-package tao-theme)

(use-package eink-theme
  :config
  (load-theme 'eink))

(use-package quasi-monochrome-theme)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t   
        doom-themes-enable-italic t)
  (doom-themes-org-config)
  ;; (load-theme 'doom-plain-dark t)
  ;; M-x list-faces-display -> find face thats messed up -> add to below and fix
  ;; (custom-set-faces
  ;;  `(org-scheduled-previously ((t (:foreground ,(doom-color 'base7)))))
  ;;  `(secondary-selection ((t (:background ,(doom-color 'fg) :foreground ,(doom-color 'bg))))))
  )

(use-package which-key
  :config (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; LSP Tings

(defun my-flymake-diagnostics-vertical-split ()
  "`flymake-show-project-diagnostics', but forcing a vertical split.
    See `split-window-sensibly'."
  (interactive)
  (let ((split-width-threshold nil)
        (split-height-threshold 0))
    (call-interactively 'flymake-show-project-diagnostics)))

(use-package lsp-mode
  :bind ((:map lsp-mode-map
	       ("C-c l a" . lsp-execute-code-action)
	       ("C-c l e" . my-flymake-diagnostics-vertical-split)
	       ("M-q" . lsp-format-buffer)
	       ("C-c l r" . lsp-rename)))
  :hook ((rust-mode . lsp)
	 (js-mode . lsp)
	 (typescript-ts-mode . lsp)
	 (tsx-ts-mode . lsp)))

(use-package lsp-ui
  :after lsp-mode
  :bind ((:map lsp-ui-mode-map
	       ("C-c k" . lsp-ui-doc-glance))))

(use-package yasnippet
  :custom
  (yas-global-mode t))

(use-package company
  :diminish company-mode
  :bind ((:map company-mode-map
	       ("M-/" . company-complete)
	  :map company-active-map
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
  (company-idle-delay nil)
  (company-require-match nil)
  (company-show-numbers t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-minimum 10)
  (company-format-margin-function nil)
  (company-tooltip-minimum-width 50))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(require 'treesit)

(setq treesit-extra-load-path '("~/.config/emacs/treesitter"))

(use-package rainbow-mode)

(use-package pdf-tools
  :config
  (pdf-tools-install))

;; Programming modes!

(use-package rust-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode)

(use-package markdown-mode)

(use-package glsl-mode)

(use-package svelte-mode)

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))


(use-package gdscript-mode
  :straight (gdscript-mode
             :type git
             :host github
             :repo "godotengine/emacs-gdscript-mode"))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-scheduled-previously ((t (:foreground "#ffdddd"))))
 '(secondary-selection ((t (:background "#d7d5d1" :foreground "#222222")))))

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
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

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

;;; Line numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

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

(defun my-select-line ()
  (interactive)
  (move-beginning-of-line 1)
  (set-mark-command nil)
  (move-end-of-line 1))

;; Random keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-v") 'my-page-up)
(global-set-key (kbd "C-v") 'my-page-down)
(global-set-key (kbd "C-l") 'my-select-line)
(global-set-key (kbd "M-n") 'switch-to-next-buffer)
(global-set-key (kbd "M-p") 'switch-to-prev-buffer)
(global-set-key (kbd "C-s") 'isearch-forward)


(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; PLUGINS

;; Evil mode

;;(use-package undo-fu)

;; (use-package evil
;;   :init
;;   (setq evil-want-C-i-jump nil)
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-keybinding nil)
;;   (setq evil-want-integration t)
;;   (setq evil-undo-system 'undo-fu)
;;   :config

;;   ;; C-d & C-u center cursor on line
;;   (evil-define-command my-scroll-down ()
;;     (evil-scroll-down 0)
;;     (evil-scroll-line-to-center nil))

;;   (evil-define-command my-scroll-up ()
;;     (evil-scroll-up 0)
;;     (evil-scroll-line-to-center nil))

;;   (define-key evil-normal-state-map (kbd "C-d") 'my-scroll-down)
;;   (define-key evil-normal-state-map (kbd "C-u") 'my-scroll-up)

;;   (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
;;   (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;;   (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
;;   (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)

;;   (evil-mode 1))

;; (use-package evil-commentary
;;   :diminish evil-commentary-mode
;;   :config
;;   (evil-commentary-mode))

;; (use-package evil-surround
;;   :config
;;   (global-evil-surround-mode 1))

;; (use-package evil-numbers
;;   :after evil
;;   :config
;;   (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;;   (define-key evil-visual-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental))

;; (use-package general
;;   :after evil
;;   :config
;;   (general-evil-setup t)
;;   (nvmap :prefix "SPC" :keymaps 'override
;;     "f"  '(projectile-command-map :which-key "projectile")
;;     "g"  '(magit-status :which-key "magit")

;;     "t"  '(:ignore t :which-key "theme")
;;     "td" '((lambda () (interactive) (load-theme 'tao-yin t)) :which-key "dark")   ;; dark theme
;;     "tl" '((lambda () (interactive) (load-theme 'tao-yang t)) :which-key "light")  ;; light theme
;;     "tc" '(counsel-load-theme :which-key "colourful")  ;; light theme
;;     "ts" '((lambda () (interactive) (load-theme 'doom-oksolar-light t)) :which-key "solarized_light")  ;; light theme

;;     "w" '(:ignore z :which-key "zen")
;;     "wm" '(writeroom-mode :which-key "mode")
;;     "wi" '(writeroom-increase-width :which-key "increase width")
;;     "wd" '(writeroom-increase-width :which-key "decrease width")

;;     "o"  '(:ignore o :which-key "org")
;;     "oa" '(org-agenda :which-key "agenda")
;;     "oc" '(org-capture :which-key "capture")

;;     "z" '(:ignore z :which-key "zettelkasten")
;;     "zc" '(org-roam-capture :which-key "capture")
;;     "zf" '(org-roam-node-find :which-key "find")
;;     "ZI" '(org-roam-node-insert :which-key "insert link")

;;     "s" '(:ignore z :which-key "shell")
;;     "ss" '(shell :which-key "shell")
;;     "se" '(eshell :which-key "eshell")
;;     "st" '(term :which-key "term")

;;     "k" '(eglot-format-buffer :which-key "format buffer")
;;     "d" '(flymake-show-buffer-diagnostics :which-key "buffer diagnostics")
;;     "D" '(flymake-show-project-diagnostics :which-key "project diagnostics")
;;     "rn" '(eglot-rename :which-key "rename")
;;     "ca" '(eglot-code-actions :which-key "code action")
;;     "ev" '(eval-print-last-sexp :which-key "eval print")))

(use-package diminish
  :diminish visual-line-mode
  :diminish abbrev-mode
  :diminish auto-revert-mode)

;; Projectile

(use-package projectile
  :diminish projectile-mode 
  :config
  (projectile-mode)
  (setq projectile-enable-caching t)
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


(use-package hydra
  :config
  (defhydra hydra-mc (global-map "C-c c")
    "multi cursor"
    ("n" mc/mark-next-like-this "mark next")
    ("p" mc/mark-previous-like-this "mark previous")
    ("M-n" mc/unmark-next-like-this "unmark next")
    ("M-p" mc/unmark-previous-like-this "unmark previous")))

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
	 ("C-c o p" . 'my-project-agenda))
  :config

  (setq org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-adapt-indentation t
	org-enforce-todo-dependencies t
	org-startup-with-inline-images t
	org-startup-with-latex-preview t
	org-id-track-globally t)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 3))

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

  ;; (setq org-agenda-custom-commands
  ;; 	'(("p" "Projects"
  ;; 	   ((let ((org-files (file-expand-wildcards "~/sync/notes/org/projects/*.org")))
  ;; 	      (mapcar (lambda (file)
  ;; 		       (agenda "" ((org-agenda-files '(file))))
  ;; 		       org-files)))))))

  ;; (setq org-agenda-custom-commands
  ;; 	'(("p" "Projects"
  ;; 	   ((agenda "" ((org-agenda-files '("project1.org"))))
  ;; 	    (agenda "" ((org-agenda-files '("project2.org"))))))))

  ;; (setq org-agenda-custom-commands
  ;;     (let ((org-files (file-expand-wildcards "~/sync/notes/org/projects/*.org")))
  ;;       (mapcar (lambda (file)
  ;;                 (let ((name (file-name-base file)))
  ;;                   `(,(intern name) ,(format "Agenda for %s" name)
  ;;                     ((agenda "" ((org-agenda-files '(,file))))))))
  ;;               org-files)))

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

;; (use-package ivy-rich
;;   :after ivy
;;   :init
;;  (ivy-rich-mode 1))

;; (use-package counsel
;;   :diminish counsel-mode
;;   :bind (("C-M-j" . 'counsel-switch-buffer)
;;          :map minibuffer-local-map
;;          ("C-r" . 'counsel-minibuffer-history))
;;   :config
;;   (counsel-mode 1))

;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

(use-package swiper
  :bind (("C-M-s" . swiper)
	 ("C-s" . isearch-forward)))

(add-to-list 'custom-theme-load-path "~/.config/emacs/themes/") 

;; (load-theme 'eltbus t)

;; (use-package tao-theme)

(use-package quasi-monochrome-theme)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t   
        doom-themes-enable-italic t)
  (doom-themes-org-config)
  (load-theme 'doom-plain-dark t)
  ;; M-x list-faces-display -> find face thats messed up -> add to below and fix
  (custom-set-faces
  `(org-scheduled-previously ((t (:foreground ,(doom-color 'base7)))))
  `(secondary-selection ((t (:background ,(doom-color 'fg) :foreground ,(doom-color 'bg)))))))

(use-package which-key
  :config (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; LSP Tings

;; Typescript sucks : https://notes.alexkehayias.com/setting-up-typescript-and-eslint-with-eglot/
;; (cl-defmethod project-root ((project (head eglot-project)))
;;   (cdr project))

;; (defun my-project-try-tsconfig-json (dir)
;;   (when-let* ((found (locate-dominating-file dir "tsconfig.json")))
;;     (cons 'eglot-project found)))
;; (add-hook 'project-find-functions
;;           'my-project-try-tsconfig-json nil nil)

;; (use-package eglot
;;   :diminish eldoc-mode
;;   :bind (:map eglot-mode-map
;; 	      ("C-c f" . eglot-format-buffer)
;; 	      ("C-c a" . eglot-code-actions))
;;   :config
;;   ;; (define-key evil-normal-state-map (kbd "gd") 'xref-find-definitions)
;;   (eldoc-add-command 'c-electric-paren)
;;   (add-to-list 'eglot-server-programs
;; 	       `(svelte-mode . ("svelteserver" "--stdio")))
;;   (add-to-list 'eglot-server-programs
;; 	       `(typescript-mode . ("typescript-language-server" "--stdio")))
;;   (add-hook 'c-mode-hook 'eglot-ensure))

;; switched to lsp mode because it just works with godot, which eglot absolutely refused to
(use-package lsp-mode
  :bind ((:map lsp-mode-map
	       ("C-c a" . lsp-execute-code-action)
	       ("C-c e" . flymake-show-project-diagnostics)))
  :hook ((rust-mode . lsp)))

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
  (company-tooltip-minimum-width 50))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package rainbow-mode)

(use-package pdf-tools)

;; Programming modes!

(use-package rust-mode)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package haskell-mode)

(use-package markdown-mode)

(use-package glsl-mode)

(use-package svelte-mode)

(use-package typescript-mode)

(use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("c3957b559cf3606c9a40777c5712671db3c7538e5d5ea9f63eb0729afeac832b" "801a567c87755fe65d0484cb2bded31a4c5bb24fd1fe0ed11e6c02254017acb2" "dbade2e946597b9cda3e61978b5fcc14fa3afa2d3c4391d477bdaeff8f5638c5" "4b6cc3b60871e2f4f9a026a5c86df27905fb1b0e96277ff18a76a39ca53b82e1" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290" "3de5c795291a145452aeb961b1151e63ef1cb9565e3cdbd10521582b5fd02e9a" "e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "21101a7ee55bb8af0215f1735da8f3b48cf28025c2d86c0009175ce43ee01fb1" default)))

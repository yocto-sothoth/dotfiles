;; general settings

(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      crowd-dir "~/Dropbox/emacs"
      custom-file "~/.emacs.d/custom.el"
      delete-old-versions t
      diary-file (if (file-directory-p crowd-dir) (expand-file-name "diary" crowd-dir) "~/.emacs.d/diary")
      display-time-string-forms '((format "  %s:%s" 24-hours minutes))
      eshell-hist-ignoredups t
      eshell-history-size 1000
      load-prefer-newer t
      logo-file (if (file-directory-p crowd-dir) (expand-file-name "logo.png" crowd-dir))
      os-font-height (if (eq system-type 'gnu/linux) 120 160)
      ring-bell-function 'ignore
      version-control t)

(setq-default indent-tabs-mode nil
              indicate-buffer-boundaries 'right)

(delete-selection-mode t)
(display-battery-mode t)
(display-time-mode t)
(fringe-mode '(0))
(global-display-line-numbers-mode)
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Sarasa Mono J" :height os-font-height)
(toggle-frame-fullscreen)
(tool-bar-mode -1)
(tooltip-mode -1)

;; use-package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(bind-key "C-h" 'delete-backward-char)
(bind-key "C-M-s-v" 'scroll-other-window-down)
(setq use-package-compute-statistics t)

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  :ensure t)

;; each packages

(use-package all-the-icons
  :ensure t)

(use-package beacon
  :config
  (beacon-mode t)
  :custom
  (beacon-blink-when-window-changes t)
  (beacon-color "olive")
  (beacon-size 8)
  :ensure t)

(use-package company
  :bind
  ("C-c C-c" . company-complete)
  :custom
  (company-minimum-prefix-length 4)
  :diminish company-mode
  :ensure t)

(use-package counsel
  :bind ("C-s-o" . counsel-rhythmbox)
  :config
  (counsel-mode 1)
  :custom
  (counsel-yank-pop-separator "\n--------------------------------\n")
  :diminish counsel-mode
  :ensure t)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-footer-icon ">")
  (dashboard-items '((recents  . 10)
                     (projects . 10)
                     (bookmarks . 10)))
  (dashboard-startup-banner (if (file-exists-p logo-file) logo-file 'official))
  :ensure t)

(use-package doom-modeline
  :config
  (doom-modeline-mode t)
  (set-face-attribute 'mode-line nil :family "Sarasa Mono J" :height os-font-height)
  (set-face-attribute 'mode-line-inactive nil :family "Sarasa Mono J" :height os-font-height)
  :custom
  (doom-modeline-height 1)
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes t)
  :ensure t)

(use-package doom-themes
  :config
  (load-theme 'doom-sourcerer t)
  :custom
  (doom-themes-enable-bold nil)
  (doom-themes-enable-italic nil)
  :ensure t)

(use-package dired
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             ("r" . dired-subtree-remove)))

(use-package dired-subtree
  :commands (dired-subtree-insert dired-subtree-remove)
  :ensure t)

(use-package disable-mouse
  :config
  (global-disable-mouse-mode)
  :diminish disable-mouse-global-mode
  :ensure t)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  :ensure t)

(use-package flycheck
  :bind (("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error))
  :custom
  (flycheck-idle-change-delay 1)
  :ensure t)

(use-package ivy
  :config
  (ivy-mode 1)
  :diminish ivy-mode
  :ensure t)

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  :ensure t)

(use-package japanese-holidays
  :config
  (setq calendar-holidays (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (defun my/japanese-holiday-show (&rest _args)
    (let* ((date (calendar-cursor-to-date t))
           (calendar-date-display-form '((format "%s %s" monthname day)))
           (date-string (calendar-date-string date))
           (holiday-list (calendar-check-holidays date)))
      (when holiday-list
        (message "%s: %s" date-string (mapconcat #'identity holiday-list "; ")))))
  (add-hook 'calendar-move-hook 'my/japanese-holiday-show)
  :custom
  (calendar-mark-holidays-flag t)
  :ensure t)

(use-package lsp-mode
  :diminish lsp-mode
  :ensure t
  :hook (enh-ruby-mode . lsp))

(use-package magit
  :commands magit-status
  :ensure t)

(use-package mozc
  :custom
  (default-input-method "japanese-mozc-im")
  (mozc-candidate-style 'echo-area)
  :ensure t)

(use-package mozc-im
  :ensure t)

(use-package prog-mode
  :config
  (add-hook 'enh-ruby-mode-hook (lambda () (setq prettify-symbols-alist my/prettify-symbols-alist)))
  (add-hook 'eshell-mode-hook (lambda () (setq prettify-symbols-alist my/prettify-symbols-alist)))
  (add-hook 'shell-mode-hook (lambda () (setq prettify-symbols-alist my/prettify-symbols-alist)))
  (global-prettify-symbols-mode)
  :custom
  (my/prettify-symbols-alist '(("Infinity" . 8734)
                               ("Float::INFINITY" . 8734)
                               ("!=" . 8800)
                               ("<=" . 8804)
                               (">=" . 8805))))

(use-package projectile
  :ensure t)

(use-package rainbow-delimiters
  :config
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30)))
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package swiper
  :bind ("C-s" . swiper)
  :ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode
  :ensure t)

(use-package which-key
  :diminish which-key-mode
  :ensure t
  :hook (after-init . which-key-mode))

(use-package whitespace
  :custom
  (whitespace-action '(auto-cleanup))
  (whitespace-space-regexp "\\(\u3000+\\)")
  (whitespace-style '(face trailing spaces tab-mark))
  :custom-face
  (whitespace-space ((t (:background "tomato"))))
  :diminish whitespace-mode
  :hook ((prog-mode . whitespace-mode) (yaml-mode . whitespace-mode)))

(use-package xclip
  :config
  (xclip-mode 1)
  :ensure t
  :if (eq system-type 'gnu/linux))

(use-package yasnippet
  :bind ("C-c y" . 'yas-describe-tables)
  :config
  (yas-global-mode 1)
  :custom
  (yas-new-snippet-default (concat "# -*- mode: snippet -*-\n"
                                   "# name: ${}\n"
                                   "# group: customize\n"
                                   "# --\n"
                                   "${}"))
  :diminish yas-minor-mode
  :ensure t)

(use-package yasnippet-snippets
  :config
  (require 'cl-lib)
  (defun reject-snippets ()
    "Reject some yasnippet-snippets snippets."
    (interactive)
    (cl-loop for e in reject-snippets-list
             do (delete-file (expand-file-name e yasnippet-snippets-dir)))
    (yas-reload-all))
  :custom
  (reject-snippets-list '("prog-mode/todo"
                          "ruby-mode/pry"
                          "ruby-mode/when"))
  :ensure t)

;; crystal

(use-package crystal-mode
  :mode (("\\.cr\\'" . crystal-mode))
  :ensure t)

(use-package flycheck-crystal
  :config
  (add-hook 'crystal-mode-hook 'flycheck-mode)
  :ensure t)

;; lisp

(use-package slime
  :commands slime
  :custom
  (inferior-lisp-program "clisp")
  :ensure t)

(use-package slime-company
  :ensure t)

;; ruby

(use-package enh-ruby-mode
  :custom-face
  (erm-syn-errline ((t (:underline nil))))
  (erm-syn-warnline ((t (:underline nil))))
  :ensure t
  :mode (("\\.rb\\'" . enh-ruby-mode)))

(use-package inf-ruby
  :custom
  (inf-ruby-default-implementation "pry")
  :ensure t
  :hook (enh-ruby-mode . inf-ruby-minor-mode))

(use-package ruby-electric
  :diminish ruby-electric-mode
  :ensure t
  :hook (enh-ruby-mode . ruby-electric-mode))

;; web develop

(use-package slim-mode
  :ensure t
  :mode (("\\.slim\\'" . slim-mode)))

(use-package web-mode
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  (web-mode-style-padding 2)
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.jsx?\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.s?css\\'" . web-mode)))

;; document

(use-package markdown-mode
  :custom
  (markdown-command "pandoc")
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)))

(use-package yatex
  :ensure t
  :mode (("\\.tex\\'" . yatex-mode)))

;; orgmode

(use-package org
  :bind (("C-c a" . 'org-agenda)
         ("C-c c" . 'org-capture)
         ("C-c l" . 'org-store-link))
  :custom
  (org-agenda-current-time-string "now")
  (org-agenda-files '("~/Dropbox/emacs/inbox.org"))
  (org-agenda-time-grid '((weekly today required-timed) (1000 1900) "-" "-"))
  (org-agenda-time-leading-zero t)
  (org-capture-templates
   '(("i" "Inbox" entry (file+headline "inbox.org" "Inbox") "* INBOX %?")
     ("w" "Wishlist" entry (file+headline "inbox.org" "Wishlist") "* INBOX %?")
     ("n" "Note" entry (file+headline "note.org" "Note") "* %?")
     ("e" "English" entry (file+headline "word.org" "Word") "* %?")))
  (org-default-notes-file "inbox.org")
  (org-directory (if (file-directory-p crowd-dir) crowd-dir "~/org"))
  (org-log-done 'time)
  (org-todo-keywords
   '((sequence "INBOX(i)" "TODO(t)" "SOMEDAY(s)" "HIGH(h)" "LOW(l)" "|" "CANCELLED(c)" "DONE(d)"))))

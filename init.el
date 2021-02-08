;;; init.el --- Summary
;;; Commentary:
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(defvar *crowd-dir* "~/Dropbox/emacs")
(defvar *os-font-height* (if (eq system-type 'darwin) 160 120))
(defvar diary-file (if (file-directory-p *crowd-dir*) (expand-file-name "diary" *crowd-dir*) "~/.emacs.d/diary"))

(setq backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      custom-file "~/.emacs.d/custom.el"
      delete-old-versions t
      load-prefer-newer t
      ring-bell-function 'ignore
      ;; use-package-compute-statistics t
      version-control t)
(setq-default indent-tabs-mode nil)

(delete-selection-mode t)
(electric-pair-mode 1)
(global-display-line-numbers-mode)
(menu-bar-mode 0)
(set-face-attribute 'default nil :family "Sarasa Mono J" :height *os-font-height*)

(when (and (eq system-type 'darwin) (eq window-system nil))
  (defun paste-to-macos (text &optional push)
    (let ((process-connection-type nil))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc text)
        (process-send-eof proc))))
  (setq interprogram-cut-function 'paste-to-macos))

(when window-system
  (defvar *alpha* 80)
  (defvar display-time-string-forms '((format "  %s %s %s %s:%s" dayname monthname day 24-hours minutes)))
  (setq-default indicate-buffer-boundaries 'right)

  (if (eq system-type 'darwin) (add-hook 'after-init-hook (lambda () (call-process "osascript" nil nil nil "-e" "tell application \"System Events\" to key code 102"))))
  (add-hook 'after-init-hook (lambda () (toggle-frame-maximized)))
  (fringe-mode '(0))
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (set-frame-parameter nil 'alpha 100)
  (tool-bar-mode -1)
  (tooltip-mode -1)

  (defun set-alpha (value)
    "Set alpha."
    (interactive "nAlpha: ")
    (if (eq (frame-parameter nil 'fullscreen) 'fullboth) (set-frame-parameter nil 'fullscreen 'maximized))
    (unless (= value 100) (setq *alpha* value))
    (set-frame-parameter nil 'alpha value))

  (defun toggle-alpha ()
    "Toggle alpha."
    (interactive)
    (if (eq (frame-parameter nil 'fullscreen) 'fullboth) (set-frame-parameter nil 'fullscreen 'maximized))
    (set-frame-parameter nil 'alpha (if (eq (frame-parameter nil 'alpha) 100) *alpha* 100)))

  (defun toggle-modeline ()
    "Toggle mode line."
    (interactive)
    (display-battery-mode (if display-battery-mode -1 1))
    (display-time-mode (if display-time-mode -1 1))))

(defun flush-ruby-comments ()
  "Delete ruby comment lines."
  (interactive)
  (flush-lines "^\\s-*#.*$"))

(defun flush-empty-lines ()
  "Delete empty lines."
  (interactive)
  (flush-lines "^$"))

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

(global-unset-key (kbd "C-t"))
(bind-key "<f5>" 'revert-buffer-no-confirm)
(bind-key "C-h" 'delete-backward-char)
(bind-key "C-x ;" 'comment-line)
(bind-key "C-x c" 'calendar)

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  :ensure t)

(use-package anki-editor
  :commands anki-editor-push-notes
  :ensure t)

(use-package beacon
  :config
  (beacon-mode t)
  :custom
  (beacon-blink-when-window-changes t)
  (beacon-size 20)
  :ensure t)

(use-package company
  :bind
  (("C-c C-c" . company-complete)
   (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))
  :custom
  (company-minimum-prefix-length 4)
  :diminish company-mode
  :ensure t
  :hook (after-init . global-company-mode))

(use-package counsel
  :config
  (counsel-mode 1)
  :custom
  (counsel-yank-pop-separator "\n----------------------------------------\n")
  :diminish counsel-mode
  :ensure t)

(use-package crystal-mode
  :mode "\\.cr\\'"
  :ensure t)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-footer-icon ">")
  (dashboard-items '((recents  . 10)
                     (projects . 10)
                     (bookmarks . 10)))
  (dashboard-startup-banner "~/dotfiles/banner.txt")
  (initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :ensure t)

(use-package doom-modeline
  :config
  (doom-modeline-mode t)
  (set-face-attribute 'mode-line nil :family "Sarasa Mono J" :height *os-font-height*)
  (set-face-attribute 'mode-line-inactive nil :family "Sarasa Mono J" :height *os-font-height*)
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
  :bind
  (:map dired-mode-map
        ("i" . dired-subtree-insert)
        ("r" . dired-subtree-remove))
  :custom (dired-use-ls-dired nil))

(use-package dired-subtree
  :commands (dired-subtree-insert dired-subtree-remove)
  :ensure t)

(use-package disable-mouse
  :config
  (global-disable-mouse-mode)
  :diminish disable-mouse-global-mode
  :ensure t
  :if window-system)

(use-package enh-ruby-mode
  :ensure t
  :mode "\\(?:\\.rb\\|.ru\\|\\.pryrc\\|\\(?:Gem\\|Rake\\|Brew\\)file\\)\\'")

(use-package flycheck
  :bind (("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (global-flycheck-mode)
  :custom
  (flycheck-idle-change-delay 1)
  :ensure t)

(use-package flycheck-crystal
  :ensure t)

(use-package inf-ruby
  :ensure t
  :hook (enh-ruby-mode . inf-ruby-minor-mode))

(use-package ivy
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-selectable-prompt t)
  :diminish ivy-mode
  :ensure t)

(use-package ivy-rich
  :config
  (ivy-rich-mode 1)
  :ensure t)

(use-package japanese-holidays
  :config
  (setq calendar-holidays (append japanese-holidays holiday-local-holidays holiday-other-holidays))
  (defun japanese-holiday-show (&rest _args)
    (let* ((date (calendar-cursor-to-date t))
           (calendar-date-display-form '((format "%s %s" monthname day)))
           (date-string (calendar-date-string date))
           (holiday-list (calendar-check-holidays date)))
      (when holiday-list
        (message "%s: %s" date-string (mapconcat #'identity holiday-list "; ")))))
  (add-hook 'calendar-move-hook 'japanese-holiday-show)
  :custom
  (calendar-mark-holidays-flag t)
  :ensure t)

(use-package js
  :custom
  (js-indent-level 2)
  :mode ("\\.js\\'" . js-mode))

(use-package lsp-mode
  :diminish lsp-mode
  :ensure t
  :hook (enh-ruby-mode . lsp)
  :init
  (defvar lsp-headerline-arrow ">"))

(use-package magit
  :bind ("C-c g" . magit)
  :ensure t)

(use-package markdown-mode
  :custom
  (markdown-command "pandoc")
  :custom-face
  (markdown-code-face ((t (:background "#222222"))))
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)))

(use-package mozc
  :custom
  (default-input-method "japanese-mozc-im")
  (mozc-candidate-style 'echo-area)
  :ensure t)

(use-package mozc-im
  :ensure t)

(use-package org
  :bind (("C-c a" . 'org-agenda)
         ("C-c c" . 'org-capture)
         ("C-c l" . 'org-store-link)
         ("C-c b" . 'org-switchb))
  :custom
  (org-agenda-current-time-string "now")
  (org-agenda-files (if (file-directory-p *crowd-dir*) (list (expand-file-name "inbox.org" *crowd-dir*))
                      '("~/org/inbox.org")))
  (org-agenda-include-diary t)
  (org-agenda-time-grid '((weekly today required-timed) (1000 1900) "-" "-"))
  (org-agenda-time-leading-zero t)
  (org-capture-templates
   '(("a" "Anki - Main" entry (file+headline "anki.org" "Main")
      "* Card\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:END:\n** Front\n   %?\n** Back")
     ("d" "Anki - DUO" entry (file+headline "anki.org" "DUO")
      "* Card\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic (and reversed card)\n:END:\n** Front\n   %?\n** Back")
     ("e" "Event" entry (file+headline "inbox.org" "Event") "* %?")
     ("i" "Inbox" entry (file+headline "inbox.org" "Inbox") "* %?")
     ("n" "Note" entry (file+headline "note.org" "Note") "* %?")
     ("w" "Wishlist" entry (file+headline "inbox.org" "Wishlist") "* %?")))
  (org-default-notes-file "inbox.org")
  (org-directory (if (file-directory-p *crowd-dir*) *crowd-dir* "~/org"))
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . "firefox %s")
                   ("\\.pdf\\'" . default)))
  (org-log-done 'time))

(use-package projectile
  :ensure t)

(use-package projectile-rails
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode)
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook ((prog-mode slime-repl-mode) . rainbow-delimiters-mode))

(use-package ruby-electric
  :diminish ruby-electric-mode
  :ensure t
  :hook (enh-ruby-mode . ruby-electric-mode))

(use-package slim-mode
  :ensure t
  :mode "\\.slim\\'")

(use-package slime
  :commands slime
  :config
  (setq inferior-lisp-program "clisp")
  :ensure t)

(use-package slime-company
  :after (company slime)
  :ensure t)

(use-package sudo-edit
  :commands (sudo-edit sudo-edit-find-file)
  :ensure t)

(use-package swiper
  :bind ("C-s" . swiper)
  :ensure t)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :diminish undo-tree-mode
  :ensure t)

(use-package web-mode
  :custom
  (web-mode-auto-close-style 2)
  (web-mode-auto-quote-style 2)
  (web-mode-block-padding 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-quoting t)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 2)
  :ensure t
  :mode "\\(?:\\.erb\\|\\.html?\\|.s?css\\)\\'")

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

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish yas-minor-mode
  :ensure t)

(use-package yatex
  :ensure t
  :mode (("\\.tex\\'" . yatex-mode)))

;;; init.el ends here

;; general settings

(setq alpha 80
      backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      crowd-dir "~/Dropbox/emacs"
      custom-file "~/.emacs.d/custom.el"
      default-directory "~/"
      delete-old-versions t
      dired-use-ls-dired (not (eq system-type 'darwin))
      display-time-string-forms '((format "  %s %s %s %s:%s" dayname monthname day 24-hours minutes))
      eshell-hist-ignoredups t
      eshell-history-size 10000
      load-prefer-newer t
      os-font-height (if (eq system-type 'gnu/linux) 120 160)
      ring-bell-function 'ignore
      version-control t)

(if (file-directory-p crowd-dir)
    (setq diary-file (expand-file-name "diary" crowd-dir)
          logo-file (expand-file-name "logo.png" crowd-dir)))

(setq-default indent-tabs-mode nil
              indicate-buffer-boundaries 'right)

(delete-selection-mode t)
(fringe-mode '(0))
(global-display-line-numbers-mode)
(global-unset-key (kbd "C-t"))
(scroll-bar-mode -1)
(set-face-attribute 'default nil :family "Sarasa Mono J" :height os-font-height)
(set-frame-parameter nil 'alpha 100)
(tool-bar-mode -1)
(tooltip-mode -1)
(toggle-frame-maximized)
(if (eq window-system nil) (menu-bar-mode -1))

(defun set-alpha (value)
  "Set alpha."
  (interactive "nAlpha: ")
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth) (set-frame-parameter nil 'fullscreen 'maximized))
  (unless (equal value 100) (setq alpha value))
  (set-frame-parameter nil 'alpha value))

(defun toggle-alpha ()
  "Toggle alpha."
  (interactive)
  (if (eq (frame-parameter nil 'fullscreen) 'fullboth) (set-frame-parameter nil 'fullscreen 'maximized))
  (set-frame-parameter nil 'alpha (if (eq (frame-parameter nil 'alpha) 100) alpha 100)))

(defun toggle-modeline ()
  "Toggle mode line."
  (interactive)
  (display-battery-mode (if display-battery-mode -1 1))
  (display-time-mode (if display-time-mode -1 1)))

(defun flush-ruby-comments ()
  "Delete ruby comment lines."
  (interactive)
  (flush-lines "^\\s-*#.*$"))

(defun flush-empty-lines ()
  "Delete empty lines."
  (interactive)
  (flush-lines "^$"))

;; use-package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))

(bind-key "C-h" 'delete-backward-char)
(bind-key "C-M-s-v" 'scroll-other-window-down)

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'eldoc-mode)
  :ensure t)

;; each packages

(use-package all-the-icons
  :ensure t)

(use-package anki-editor
  :commands anki-editor-push-notes
  :ensure t)

(use-package beacon
  :config
  (beacon-mode t)
  :custom
  (beacon-blink-when-window-changes t)
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
  :mode (("\\.cr\\'" . crystal-mode))
  :ensure t)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-footer-icon ">")
  (dashboard-items '((recents  . 10)
                     (projects . 10)
                     (bookmarks . 10)))
  (dashboard-startup-banner (if (and logo-file (file-exists-p logo-file)) logo-file 'official))
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
  :bind
  (:map dired-mode-map
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

(use-package enh-ruby-mode
  :custom-face
  (erm-syn-errline ((t (:underline nil))))
  (erm-syn-warnline ((t (:underline nil))))
  :ensure t
  :mode (("\\(?:\\.rb\\|.ru\\|\\.pryrc\\|\\(?:Gem\\|Rake\\|Brew\\)file\\)\\'" . enh-ruby-mode)))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "RUBY_THREAD_VM_STACK_SIZE")
  :ensure t)

(use-package eshell
  :config
  (defun pry-on-eshell-bol ()
    "Goes to the beginning of line, then skips past the prompt, if any."
    (interactive)
    (beginning-of-line)
    (unless (re-search-forward "\\[[0-9]+\\] pry(.+)\\(:[0-9]+\\)?> " nil t)
      (move-end-of-line 1)
      (eshell-bol)))

  (defun pry-on-eshell-next-input ()
    "Show next input history of pry on eshell"
    (interactive)
    (cond ((<= pry-history-index 1) (setq pry-history-index 1))
          ((or (equal last-command 'pry-on-eshell-previous-input)
               (equal last-command 'pry-on-eshell-next-input)) (cl-decf pry-history-index))
          (t (setq pry-history-index 1)))
    (shell-command (concat "cat " pry-history-file " | tail -"
                           (number-to-string pry-history-index)
                           " | head -1 | "
                           (if (eq system-type 'darwin) "ghead" "head")
                           " -c -1") "*pry-on-eshell-history*" "*pry-on-eshell-history-error*")
    (beginning-of-line)
    (if (re-search-forward "\\[[0-9]+\\] pry(.+)\\(:[0-9]+\\)?> " nil t)
        (progn (delete-region (point) (line-end-position))
               (insert-buffer "*pry-on-eshell-history*")
               (end-of-line)
               (message (concat "Pry history item: " (number-to-string pry-history-index))))
      (progn (move-end-of-line 1) (eshell-next-input 1))))

  (defun pry-on-eshell-previous-input ()
    "Show previous input history of pry on eshell"
    (interactive)
    (if (or (equal last-command 'pry-on-eshell-previous-input)
            (equal last-command 'pry-on-eshell-next-input)) (cl-incf pry-history-index)
      (setq pry-history-index 1))
    (shell-command (concat "cat " pry-history-file " | tail -"
                           (number-to-string pry-history-index)
                           " | head -1 | "
                           (if (eq system-type 'darwin) "ghead" "head")
                           " -c -1") "*pry-on-eshell-history*" "*pry-on-eshell-history-error*")
    (beginning-of-line)
    (if (re-search-forward "\\[[0-9]+\\] pry(.+)\\(:[0-9]+\\)?> " nil t)
        (progn (delete-region (point) (line-end-position))
               (insert-buffer "*pry-on-eshell-history*")
               (end-of-line)
               (message (concat "Pry history item: " (number-to-string pry-history-index))))
      (progn (move-end-of-line 1) (eshell-previous-input 1))))
  :custom
  (pry-history-file "~/.local/share/pry/pry_history")
  (pry-history-index 1)
  :init
  (add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "C-a") 'pry-on-eshell-bol)))
  (add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "M-n") 'pry-on-eshell-next-input)))
  (add-hook 'eshell-mode-hook (lambda () (define-key eshell-mode-map (kbd "M-p") 'pry-on-eshell-previous-input))))

(use-package flycheck
  :bind (("C-c n" . flycheck-next-error)
         ("C-c p" . flycheck-previous-error))
  :custom
  (flycheck-idle-change-delay 1)
  :hook ((enh-ruby-mode . flycheck-mode) (crystal-mode . flycheck-mode))
  :ensure t)

(use-package flycheck-crystal
  :ensure t)

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-eshell-pry (global-map "<f5>")
    "pry-byebug"
    ("c" (lambda () (interactive) (insert "continue") (eshell-send-input)) "continue")
    ("e" (lambda () (interactive) (insert "exit") (eshell-send-input)) "exit")
    ("f" (lambda () (interactive) (insert "finish") (eshell-send-input)) "finish")
    ("n" (lambda () (interactive) (insert "next") (eshell-send-input)) "next")
    ("s" (lambda () (interactive) (insert "step") (eshell-send-input)) "step")
    ("g" nil "cancel")))

(use-package inf-ruby
  :custom
  (inf-ruby-default-implementation "pry")
  :ensure t
  :hook (enh-ruby-mode . inf-ruby-minor-mode))

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

(use-package lsp-mode
  :diminish lsp-mode
  :ensure t
  :hook (enh-ruby-mode . lsp))

(use-package magit
  :bind ("C-c g" . magit)
  :ensure t)

(use-package markdown-mode
  :custom
  (markdown-command "pandoc")
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
  (org-agenda-files (if (file-directory-p crowd-dir) (list (expand-file-name "inbox.org" crowd-dir))
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
  (org-directory (if (file-directory-p crowd-dir) crowd-dir "~/org"))
  (org-file-apps '((auto-mode . emacs)
                   ("\\.mm\\'" . default)
                   ("\\.x?html?\\'" . "firefox %s")
                   ("\\.pdf\\'" . default)))
  (org-log-done 'time))

(use-package prog-mode
  :config
  (add-hook 'enh-ruby-mode-hook (lambda () (setq prettify-symbols-alist my/prettify-symbols-alist)))
  (add-hook 'eshell-mode-hook (lambda () (setq prettify-symbols-alist my/prettify-symbols-alist)))
  (global-prettify-symbols-mode)
  :custom
  (my/prettify-symbols-alist '(("Infinity" . 8734)
                               ("Float::INFINITY" . 8734)
                               ("!=" . 8800)
                               ("<=" . 8804)
                               (">=" . 8805))))

(use-package projectile
  :ensure t)

(use-package projectile-rails
  :bind-keymap
  ("C-c r" . projectile-rails-command-map)
  :config
  (projectile-rails-global-mode)
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

(use-package ruby-electric
  :diminish ruby-electric-mode
  :ensure t
  :hook (enh-ruby-mode . ruby-electric-mode))

(use-package slim-mode
  :ensure t
  :mode (("\\.slim\\'" . slim-mode)))

(use-package slime
  :commands slime
  :custom
  (inferior-lisp-program "clisp")
  :ensure t)

(use-package slime-company
  :after (company slime)
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
  (web-mode-markup-indent-offset 2)
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.s?css\\'" . web-mode)))

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
  :mode (("\\.yml\\'" . yaml-mode)))

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

(use-package yatex
  :ensure t
  :mode (("\\.tex\\'" . yatex-mode)))

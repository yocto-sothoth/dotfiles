;; general settings

(toggle-frame-fullscreen)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(if (eq window-system nil) (menu-bar-mode -1))

(setq crowd-dir "~/Dropbox/emacs"
      os-font-height (if (eq system-type 'gnu/linux) 120 160)
      custom-file "~/.emacs.d/custom.el"
      backup-directory-alist '((".*" . "~/.emacs.d/backup"))
      delete-old-versions t
      version-control t
      load-prefer-newer t
      display-time-string-forms '((format "  %s:%s" 24-hours minutes))
      eshell-history-size 1024
      eshell-hist-ignoredups t
      ring-bell-function 'ignore)

(if (file-directory-p crowd-dir)
    (setq diary-file (expand-file-name "diary" crowd-dir)
          logo-file (expand-file-name "logo.png" crowd-dir)))

(setq-default indicate-buffer-boundaries 'right
              indent-tabs-mode nil
              tab-width 4)

(set-face-attribute 'default nil :family "Sarasa Mono J" :height os-font-height)
(fringe-mode '(0))
(global-display-line-numbers-mode)
(delete-selection-mode t)
(display-battery-mode t)
(display-time-mode t)

;; use-package

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(if (not (package-installed-p 'use-package))
  (package-install 'use-package))

(bind-key "C-h" 'delete-backward-char)
(bind-key "C-M-h" 'backward-kill-word)

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

;; each packages

(use-package all-the-icons
  :ensure t)

(use-package beacon
  :ensure t
  :init
  (setq beacon-color "tomato"
        beacon-size 8
        beacon-blink-when-window-changes t)
  :config
  (beacon-mode t))

(use-package company
  :ensure t
  :diminish company-mode
  :hook (prog-mode . company-mode)
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 4
        company-selection-wrap-around t))

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode)
  :config
  (company-quickhelp-mode))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-startup-banner (cond ((eq window-system nil) 3)
                                       ((file-exists-p logo-file) logo-file)
                                       (t 'official))
        dashboard-footer-icon ">"
        dashboard-items '((recents  . 10)
                          (projects . 10)
                          (bookmarks . 10)
                          (registers . 10)))
  :config
  (dashboard-setup-startup-hook))

(use-package doom-themes
  :ensure t
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  :config
  (load-theme 'doom-sourcerer t))

(use-package doom-modeline
  :ensure t
  :init
  (setq doom-modeline-icon nil
        doom-modeline-height 1
        doom-modeline-minor-modes t)
  (set-face-attribute 'mode-line nil :family "Sarasa Mono J" :height os-font-height)
  (set-face-attribute 'mode-line-inactive nil :family "Sarasa Mono J" :height os-font-height)
  :config
  (doom-modeline-mode t))

(use-package dired
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             ("r" . dired-subtree-remove)))

(use-package dired-subtree
  :ensure t
  :commands (dired-subtree-insert dired-subtree-remove))

(use-package disable-mouse
  :ensure t
  :diminish disable-mouse-global-mode
  :config
  (global-disable-mouse-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package flymake
  :bind (("C-c n" . flymake-goto-next-error)
         ("C-c p" . flymake-goto-prev-error))
  :custom-face
  (flymake-note ((t (:underline "#87875f"))))
  (flymake-warning ((t (:underline "#ff9800"))))
  (flymake-error ((t (:underline "#aa4450")))))

(use-package magit
  :ensure t)

(use-package mozc
  :ensure t
  :init
  (setq mozc-candidate-style 'echo-area
        default-input-method "japanese-mozc-im"))

(use-package mozc-im
  :ensure t)

(use-package lsp-mode
  :ensure t
  :diminish lsp-mode
  :hook (enh-ruby-mode . lsp))

(use-package prettify-greek
  :ensure t
  :config
  (add-hook 'enh-ruby-mode-hook
            (lambda ()
              (setq prettify-symbols-alist
                    (append '(("!=" . 8800)
                              (">=" . 8805)
                              ("<=" . 8804))
                            prettify-greek-lower
                            prettify-greek-upper))
              (prettify-symbols-mode))))

(use-package projectile
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (require 'cl-lib)
  (require 'color)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
     (cl-callf color-saturate-name (face-foreground face) 30))))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :hook (after-init . which-key-mode))

(use-package whitespace
  :diminish whitespace-mode
  :hook (prog-mode . whitespace-mode)
  :init
  (setq whitespace-style '(face trailing spaces tab-mark)
        whitespace-action '(auto-cleanup)
        whitespace-space-regexp "\\(\u3000+\\)")
  :custom-face
  (whitespace-space ((t (:background "tomato")))))

(use-package xclip
  :if (eq system-type 'gnu/linux)
  :ensure t
  :config
  (xclip-mode 1))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :bind ("C-c y" . 'yas-describe-tables)
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :init
  (setq reject-snippets-list '("ruby-mode/pry" "ruby-mode/when"))
  :config
  (require 'cl-lib)
  (defun reject-snippets ()
    "Reject some yasnippet-snippets snnipets."
    (interactive)
    (cl-loop for e in reject-snippets-list
             do (delete-file (expand-file-name e yasnippet-snippets-dir)))
    (yas-reload-all)))

;; ruby

(use-package enh-ruby-mode
  :ensure t
  :mode (("\\.rb\\'" . enh-ruby-mode))
  :custom-face
  (erm-syn-warnline ((t (:underline "#ff9800"))))
  (erm-syn-errline ((t (:underline "#aa4450")))))

(use-package ruby-electric
  :ensure t
  :diminish ruby-electric-mode
  :hook (enh-ruby-mode . ruby-electric-mode))

(use-package inf-ruby
  :ensure t
  :hook (enh-ruby-mode . inf-ruby-minor-mode)
  :init
  (setq inf-ruby-default-implementation "pry"))

;; crystal

(use-package crystal-mode
  :ensure t
  :mode (("\\.cr\\'" . crystal-mode)))

;; lisp

(use-package slime
  :ensure t
  :commands slime
  :init
  (setq inferior-lisp-program "clisp"))

(use-package slime-company
  :ensure t)

;; web develop

(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.s?css\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.php\\'" . web-mode)
         ("\\.jsx?\\'" . web-mode))
  :init
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-markup-indent-offset 2
        web-mode-script-padding 2
        web-mode-style-padding 2))

(use-package slim-mode
  :ensure t
  :mode (("\\.slim\\'" . slim-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)))

;; document

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc"))

(use-package yatex
  :ensure t
  :mode (("\\.tex\\'" . yatex-mode)))

;; orgmode

(use-package org
  :bind (("C-c a" . 'org-agenda)
         ("C-c c" . 'org-capture)
         ("C-c l" . 'org-store-link))
  :init
  (setq org-directory (if (file-directory-p crowd-dir) crowd-dir "~/org")
        org-default-notes-file "inbox.org"
        org-agenda-files '("~/Dropbox/emacs/inbox.org")
        org-log-done 'time
        org-capture-templates
        '(("i" "Inbox" entry (file+headline "inbox.org" "Inbox") "* INBOX %? %U")
          ("w" "Wishlist" entry (file+headline "inbox.org" "Wishlist") "* INBOX %? %U")
          ("n" "Note" entry (file+headline "note.org" "Note") "* %? %U")
          ("e" "English" entry (file+headline "word.org" "Word") "* %?"))
        org-todo-keywords
        '((sequence "INBOX(i)" "TODO(t)" "SOMEDAY(s)" "HIGH(h)" "LOW(l)" "|" "CANCELLED(c)" "DONE(d)"))))

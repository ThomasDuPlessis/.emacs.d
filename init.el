;;; init.el ---  my emacs init config

;; Author: Thomas DuPlessis <thomasduplessis555@gmail.com>,

;;; Commentary:

;; This is an Emacs config that uses use-package for all packages.

;;; Code:


(defconst emacs-start-time (current-time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Package Manager ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(defvar prelude-packages
  '( use-package)
  "A list of packages to ensure are installed at launch.")

(require 'cl)
(defun prelude-packages-installed-p ()
  (loop for p in prelude-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (prelude-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs Prelude is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p prelude-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(require 'use-package)

(setq default-frame-alist '((font . "Roboto Mono-16")))

(use-package xresources-theme
  :config (load-theme 'xresources))

;; (use-package company
;;   :ensure t
;;   :defer t
;;   :bind
;;   (("<C-return>" . company-complete-common)
;;    ("M-RET" . company-complete-common) ; for terminals that don't rec c-return
;;    ("C-." . company-files))
;;   :init (global-company-mode))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x C-z" . magit-status))

(use-package eglot
  :defer t
  :ensure t)

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package ido :ensure t
  :defer nil
      :config
      (setq ido-enable-flex-matching t)
      (setq ido-everywhere t)
      (ido-mode 1))

(use-package ido-vertical-mode :ensure t
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package flx
  :ensure t
  :config
  (progn
    (use-package flx-ido :ensure t)
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-enable-flex-matching t)
    (setq ido-use-faces nil)))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))
(use-package expand-region :ensure t
  :bind ("C-=" . er/expand-region)
  ("C-c =" . er/expand-region))         ; for terminal
(use-package flyspell :ensure t)
;; highlight unmatched parens
(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :ensure t)

;;; elisp
(use-package paredit
  :ensure t
  :config (progn
            (autoload 'enable-paredit-mode "paredit"
              "Turn on pseudo-structural editing of Lisp code." t)
            (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
            (add-hook 'eval-expression-minibuffer-setup-hook
                      #'enable-paredit-mode)
            (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
            (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
            (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
            (add-hook 'scheme-mode-hook           #'enable-paredit-mode)))

(use-package clang-format
  :defer t
  :ensure t
  :bind (:map c++-mode-map
         ("C-M-q" . clang-format-region)
         ("M-q" . clang-format-region)
         :map c-mode-map
         ("C-M-q" . clang-format-region)
         ("M-q" . clang-format-region)))

(use-package ghc
  :defer t
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package latex
    :defer t
    :config
    (use-package preview)
    (add-hook 'LaTeX-mode-hook 'reftex-mode))

(use-package ace-window
  :ensure t
  :defer t
  :bind ("C-x o" . ace-window))

(use-package go-mode
  :defer t
  :ensure t
  :bind (:map go-mode-map ("M-." . godef-jump)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; costum commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<C-up>")    'shrink-window)
(global-set-key (kbd "<C-down>")  'enlarge-window)
(global-set-key (kbd "<C-left>")  'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

(define-key emacs-lisp-mode-map (kbd "C-c C-f") 'find-function)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-region)

(setq inhibit-startup-message t) ; get rid of the annoying start up page
(global-hl-line-mode +1)         ; highlight current line
(setq frame-title-format "emacs - %b")   ; always dispay filename as titlebar

(toggle-scroll-bar 1)
(size-indication-mode)
(unless (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(tool-bar-mode -1)

;; column length
(setq fill-column 80)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq column-number-mode 1)
(setq line-number-mode 1)

;;; Set the fill column (column number to wrap text after) to 80, not 70
(setq-default fill-column 80)

(setq confirm-kill-emacs 'y-or-n-p)     ; ask before qutting

;; recent file mode
(use-package recentf
  :bind ("\C-x\ \C-r" . recentf-open-files)
  :config 
  (recentf-mode 1)
  (setq recentf-max-menu-items 100)
  (setq recentf-max-saved-items 100))

(put 'erase-buffer 'disabled nil) ;; enable the erase-buffer function
(require 'server)
(if (not (server-running-p))
    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode 1) ;; turn on paren match highlighting
(setq show-paren-style 'mixed) ;; highlight entire bracket exp

(require 'linum)
(add-hook 'prog-mode-hook 'linum-on)

;; always add closing brackets and parens
(electric-pair-mode 1) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Spelling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))

(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Org-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org
  :ensure t
  :init (progn (setq org-completion-use-ido t)
               (setq org-catch-invisible-edits t)
               (setq org-src-fontify-natively t))
  :bind (("\C-cl" . org-store-link)
         ("\C-cc" . org-capture)
         ("\C-ca" . org-agenda)
         ("\C-cb" . org-iswitchb))
  :config
  (defun my-org-hook ()
    "Function for org hook."
    (interactive)
    (use-package yasnippet :ensure t)
    (use-package ido :ensure t)
    (org-indent-mode)
    (subword-mode))
  (add-hook 'org-mode-hook 'my-org-hook)
  (setq org-startup-with-inline-images t)

  (setq org-agenda-files '("~/org/"))

  (setq org-completion-use-ido t)
  (setq org-agenda-window-setup 'current-window)

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w)" "HOLD(h)" "|" "CANCELLED(c)")
                (sequence "PHONE" "MEETING" "|" "CANCELLED(c)"))))

  (setq org-todo-keyword-faces
        (quote (("TODO"           :weight bold)
                ("NEXT"        :weight bold)
                ("WAITING"    :weight bold)
                ("HOLD"         :weight bold)
                ("CANCELLED"   :weight bold)
                ("MEETING"      :weight bold)
                ("PHONE"        :weight bold)
                ("DONE"          :weight bold))))

  ;; highlight the cyrrent time in org agenda.
  (set-face-attribute 'org-agenda-current-time nil :foreground "purple")

 (setq org-use-fast-todo-selection t)
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org" "General")
           "* TODO %?\n  %i\n")
          ("r" "Read" entry (file+headline "~/org/todo.org" "Read")
           "* TODO %?\n:PROPERTIES:\n:Author:\n:Platform: Book\n:Bookmark:\n:END:")
          ("j" "Journal" entry (file+datetree "~/org/journal.org")
           "* %?\nEntered on %U\n  %i\n  %a")
          ("a" "Appointment" entry (file  "~/org/agenda.org")
           "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")))

  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-allow-creating-parent-nodes nil)
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

  (setq org-columns-default-format "%50ITEM(Task) %10CLOCKSUM %16TIMESTAMP_IA")
  ;; (setq org-columns-default-format "%50ITEM(Task) %3PRIORITY %TAGS
  ;; %10CLOCKSUM %16TIMESTAMP_IA")
  
  (defun org-columns-entire-file ()
    "turn on org-columns for the entire file."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "[[:space:]]*$")
          (org-columns)
        (progn
          (open-line 1)
          (org-columns)))))

  (setq org-clock-mode-line-total 'current)

  (define-key org-mode-map (kbd "M-p") 'flyspell-goto-previous-error)
  (add-hook 'org-mode-hook (lambda()
                             (turn-on-flyspell)
                             (abbrev-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dired Extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  :init (progn
          (setq dired-listing-switches "-alh")
          (put 'dired-find-alternate-file 'disabled nil))
  :bind (:map dired-mode-map
              ("C-x w" . wdired-change-to-wdired-mode)))

(if (file-exists-p "~/extras.el")
    (load-file "~/extras.el"))

;; print out loading time.
(when window-system
  (let ((elapsed (float-time (time-subtract (current-time)
                                            emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))

  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("86704574d397606ee1433af037c46611fb0a2787e8b6fd1d6c96361575be72d2" default)))
 '(package-selected-packages
   (quote
    (eglot xresources-theme xresource-theme yasnippet use-package solarized-theme smex rainbow-delimiters projectile popup pdf-tools paredit neotree multiple-cursors multi-term markdown-mode magit ledger-mode ido-vertical-mode god-mode go-mode ghc ggtags flycheck flx-ido expand-region emms doom company clang-format auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ---  my emacs init config

;; Author: Thomas DuPlessis <thomasduplessis555@gmail.com>,

;;; Commentary:

;; This is an Emacs config that uses el-get for all packages.

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

(use-package company
  :ensure t
  :defer t
  :bind
  (("<C-return>" . company-complete-common)
   ("M-RET" . company-complete-common) ; for terminals that don't rec c-return
   ("C-." . company-files))
  :init (global-company-mode))

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x C-z" . magit-status))

(use-package yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
              ("C-M-y" . yas-expand))
  :config (progn
            (yas-global-mode 1)
            (add-hook 'prog-mode-hook #'yas-minor-mode)
            (setq yas-snippet-dirs
                  (append yas-snippet-dirs
                          '("~/.emacs.d/my-snippets")))
            (yas-reload-all)))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package smex
  :ensure t
  :bind ("M-x" . smex))

(use-package multi-term ;; better version for running terminals in emacs
  :ensure t
  :bind (("<f10>" . multi-term)
         :map term-raw-map
         ("C-y" . term-paste) ;; better pasting in terminal
         ("C-c C-l" . erase-buffer)
         ("C-c M-o" . comint-clear-buffer)
         :map term-mode-map
         ("C-c M-o" . comint-clear-buffer))
  :config
  (progn
    (setq multi-term-program "/bin/bash")
    (setq explicit-shell-file-name "/bin/bash")
    (setq term-buffer-maximum-size 10000)
    (defun my-term-hook ()
      (setq yas-dont-activate t)
      (setq term-buffer-maximum-size 10000)
      (setq show-trailing-whitespace nil))
    (add-hook 'term-mode-hook 'my-term-hook)
    (add-to-list 'term-bind-key-alist '("M-d" . term-send-forward-kill-word))
    (add-to-list 'term-bind-key-alist '("<escape>" . term-send-esc))
    (add-to-list 'term-bind-key-alist '("<C-backspace>" . term-send-backward-kill-word))
    (add-to-list 'term-bind-key-alist '("<M-backspace>" . term-send-backward-kill-word))
    (add-to-list 'term-bind-key-alist '("M-[" . multi-term-prev))
    (add-to-list 'term-bind-key-alist '("M-]" . multi-term-next))))

(use-package projectile
  :ensure t
  :config
  (progn
    (projectile-global-mode)
    (setq projectile-enable-caching nil)
    (setq projectile-mode-line
          '(:eval
            (if (file-remote-p default-directory)
                " Projectile"
              (format " P[%s]"
                      (projectile-project-name)))))
    (setq projectile-switch-project-action 'projectile-dired)))

(use-package ido :ensure t
  :defer nil
      :config
      (setq ido-enable-flex-matching t)
      (setq ido-everywhere t)
      (ido-mode 1))

(use-package ido-ubiquitous :ensure t)

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

(use-package avy :ensure t
             :bind
             (("C-:" . avy-goto-char)
              ("C-'" . avy-goto-char-2)
              ("M-g f" . avy-goto-line)
              ("M-g e" . avy-goto-word-0))
             :config (avy-setup-default))
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

(use-package ibuffer
  :ensure t
  :init (setq ibuffer-show-empty-filter-groups nil)
  :bind ("C-x C-b" . ibuffer)
  :config (setq ibuffer-saved-filter-groups
                '(("default"
                   ("emacs-config" (or (filename . ".emacs.d")
                                       (filename . "emacs-config")))
                   ("dired" (mode . dired-mode))
                   ("Org" (or (mode . org-mode)
                              (filename . "OrgMode")))
                   ("code" (or (mode . prog-mode)
                               (filename . "Code")
                               (mode . cc-mode)))
                   ("Magit" (name . "\*magit"))
                   ("ERC" (mode . erc-mode))
                   ("Help" (or (name . "\*Help\*")
                               (name . "\*Apropos\*")
                               (name . "\*info\*"))))))
  (setq ibuffer-formats 
      '((mark modified read-only " "
              (name 30 30 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))

  (defun my-ibuffer-hook ()
    (ibuffer-auto-mode 1)
    (ibuffer-switch-to-saved-filter-groups "default"))
  (add-hook 'ibuffer-mode-hook 'my-ibuffer-hook))

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
(use-package dash :ensure t) ;; elisp library
(use-package popup :ensure t)

;;; java
;; (use-package eclim
;;   :ensure t
;;   :bind (:map java-mode-map
;;          ("C-c C-f" . eclim-java-find-declaration)
;;          ("C-c C-r" . eclim-java-find-references)
;;          :map scala-mode-map
;;          ("C-c C-f" . eclim-scala-find-declaration))
;;   :config (progn
;; 	    (use-package s :ensure t)
;;             (setq eclim-executable "/opt/eclipse/eclim")
;;             (setq eclim-eclipse-dirs "/opt/eclipse")
;;             (add-to-list 'load-path "~/.emacs.d/lisp/emacs-eclim")
;;             (require 'eclim)
;;             (require 'eclimd)
;;             (setq help-at-pt-display-when-idle t)
;;             (setq help-at-pt-timer-delay 0.1)
;;             (help-at-pt-set-timer)
;;             (require 'company-emacs-eclim)
;;             (company-emacs-eclim-setup)

;;             (add-hook 'java-mode-hook 'eclim-mode)
;;             (add-hook 'scala-mode-hook
;;                       (lambda ()
;;                         (scala-mode-feature-electric-mode)
;;                         (eclim-mode)))))
;;; c++ 
(use-package ggtags
  :ensure t
  :config
          (add-hook 'c-mode-common-hook
                    (lambda ()
                      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                        (ggtags-mode 1)))))
(use-package clang-format
  :defer t
  :ensure t
  :bind (:map c++-mode-map
         ("C-M-q" . clang-format-region)
         ("M-q" . clang-format-region)
         :map c-mode-map
         ("C-M-q" . clang-format-region)
         ("M-q" . clang-format-region)))

(use-package doxymacs
  :defer t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (doxymacs-mode t)
              (doxymacs-font-lock))))

;;; haskell
(use-package haskell-mode
  :defer t
  :ensure t
  :bind (:map haskell-mode-map
              ("C-c C-l" . haskell-process-load-file)
              ("C-c C-z" . haskell-interactive-switch)
              ("SPC" . haskell-mode-contextual-space)
              ("<f8>" . haskell-navigate-imports))
  :config
  (progn
    ;; Make Emacs look in Cabal directory for binaries
    (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
      (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
      (add-to-list 'exec-path my-cabal-path))

    ;; Use haskell-mode indentation
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
    (add-hook 'haskell-cabal-mode 'turn-on-haskell-indentation)))

(use-package ghc
  :defer t
  :ensure t)
(use-package ghc-mod
  :config
  (progn
    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t)
    (add-hook 'haskell-mode-hook 'ghc-init)
    (setq ghc-debug t)
    (use-package company-ghc
      :config (add-to-list 'company-backends 'company-ghc))))
;;; sml-mode
(use-package sml-mode)

;;; web mode
(use-package web-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.ssp$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.php$\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsp$" . web-mode))
    (setq web-mode-enable-current-element-highlight t)
    (setq javascript-i4de4t-level 4) ; javascript-mode
    (setq js-i4de4t-level 4) ; js-mode
    (setq web-mode-markup-i4de4t-offset 4)
    (setq web-mode-css-i4de4t-offset 4)
    (setq web-mode-code-i4de4t-offset 4)
    (setq css-i4de4t-offset 4)
    (use-package company-web
      :ensure t
      :config (add-to-list 'company-backends 'company-web-html))
    (use-package web-completion-data
      :ensure t)))

;;; other modes
(use-package ledger-mode
  :ensure t
  :bind (:map ledger-mode-map ("C-M-i" . complete-symbol)))
(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

(use-package markdown-mode
  :ensure t)

(use-package tex-site                   ; auctex
  :load-path "site-lisp/auctex/"
  :defines (latex-help-cmd-alist latex-help-file)
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :init
  :config
  (defun latex-help-get-cmd-alist ()    ;corrected version:
    "Scoop up the commands in the index of the latex info manual.
   The values are saved in `latex-help-cmd-alist' for speed."
    ;; mm, does it contain any cached entries
    (if (not (assoc "\\begin" latex-help-cmd-alist))
        (save-window-excursion
          (setq latex-help-cmd-alist nil)
          (Info-goto-node (concat latex-help-file "Command Index"))
          (goto-char (point-max))
          (while (re-search-backward "^\\* \\(.+\\): *\\(.+\\)\\." nil t)
            (let ((key (buffer-substring (match-beginning 1) (match-end 1)))
                  (value (buffer-substring (match-beginning 2)
                                           (match-end 2))))
              (add-to-list 'latex-help-cmd-alist (cons key value))))))
    latex-help-cmd-alist)

  (use-package latex
    :defer t
    :config
    (use-package preview)
    (add-hook 'LaTeX-mode-hook 'reftex-mode)))
(use-package tex-site
  :mode ("\\.tex\\'" . TeX-latex-mode))
(use-package auctex
  :defer t
  :ensure t
  :config (progn
            (require 'tex)
            (TeX-global-PDF-mode t)
            (setq TeX-auto-save t)
            (setq TeX-parse-self t)
            (setq TeX-save-query nil)
            (setq TeX-PDF-mode t)
            (use-package company-auctex
              :ensure t
              :config (company-auctex-init))))

(use-package neotree
  :init (setq neo-window-width 35)
  :ensure t
  :defer t)
(use-package solarized-theme
  :ensure t
  :defer t)
(use-package ace-window
  :ensure t
  :defer t
  :bind ("C-x o" . ace-window))
(use-package god-mode
  :ensure t
  :defer t
  :bind ("<escape>" . god-local-mode)   ;for terminal use
  ("C-c g" . god-local-mode)
  :config (progn
            (defun my-update-cursor ()
              (setq cursor-type (if (or god-local-mode buffer-read-only)
                                    'hollow
                                  'box)))
            (add-hook 'god-mode-enabled-hook 'my-update-cursor)
            (add-hook 'god-mode-disabled-hook 'my-update-cursor)))
(use-package pdf-tools
       :defer t
       :ensure t
       :config (pdf-tools-install))

(use-package emms
  :ensure t
  :defer t
  :config (progn
            (require 'emms-setup)
            (emms-all)
            (setq emms-setup-default-player-list
                  '(emms-player-vlc
                    emms-player-vlc-playlist
                    emms-player-mplayer-playlist
                    emms-player-mplayer))
            (setq emms-source-file-default-directory "~/Music/")
            (emms-add-directory-tree "~/Music/")))

(use-package doom :ensure t
  :config (defun load-doom-theme ()
            "Load doom-one theme and turn on all features."
            (interactive)
            (progn
              (require 'doom-themes)
              (require 'doom-neotree)
              (setq org-fontify-whole-heading-line t
                    org-fontify-done-headline t
                    org-fontify-quote-and-verse-blocks t
                    doom-one-brighter-modeline t
                    doom-one-brighter-comments t)
              (load-theme 'doom-one t))))

(use-package go-mode
  :ensure t
  :bind (:map go-mode-map ("M-." . godef-jump)))

;; extra lisp code should be in lisp directory of .emacs.d
(add-to-list 'load-path "~/.emacs.d/lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;; costum commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "<C-up>")    'shrink-window)
(global-set-key (kbd "<C-down>")  'enlarge-window)
(global-set-key (kbd "<C-left>")  'shrink-window-horizontally)
(global-set-key (kbd "<C-right>") 'enlarge-window-horizontally)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;elisp;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key emacs-lisp-mode-map (kbd "C-c C-f") 'find-function)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-region)

(defun repeat-string(n str)
  (apply 'concat (make-list n str)))

(defun str-divide(len str)
  (/ len (length str)))

;; lil function to make little section titles in any language with comment
;; character
(defun create-section-title(title)
  "creates a little title section with three lines, first line of
  all comment chars, the second is the title wrapped in comment
  chars and the third is all comment chars again"
  (interactive "sEnter the title:")
  (let* ((numReptimes
          (- (str-divide 80 comment-start)
             (length comment-end)))
         (fstAndThrdLn
          (concat
           (repeat-string numReptimes comment-start) comment-end "\n"))
         (titleLineCommentRepNum
          (/ (str-divide (- 80 (length title)) comment-start)
             2))
         (titleLine
          (concat (repeat-string titleLineCommentRepNum comment-start)
                  title
                  (repeat-string (- titleLineCommentRepNum
                                   (length comment-end))
                                comment-start)
                  comment-end
                  "\n")))
    (end-of-line)
    (newline)
    (insert (concat fstAndThrdLn
                    titleLine
                    fstAndThrdLn))))

(defun end-section-title()
  "ends a section created by 'mkSectionTitle"
  (interactive)
  (let* ((numReptimes
          (- (str-divide 80 comment-start) (length comment-end)))
         (ln (concat (repeat-string numReptimes comment-start)
                     comment-end
                     "\n")))
    (end-of-line)
    (newline)
    (insert (repeat-string 3 ln))))


(defun init ()
  "Go to the init file."
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "C-c i") 'init)

(defun my-insert-space-after-point ()
  (interactive)
  (save-excursion (insert " ")))
(global-set-key (kbd "C-;") 'my-insert-space-after-point)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; aesthetics/settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq inhibit-startup-message t) ; get rid of the annoying start up page
(global-hl-line-mode +1)         ; highlight current line
(setq frame-title-format "emacs - %b")   ; always dispay filename as titlebar

(toggle-scroll-bar 1)
(size-indication-mode)
(unless (display-graphic-p)
  (menu-bar-mode -1)
  (tool-bar-mode -1))

(tool-bar-mode -1)
;; (set-frame-font "Inconsolata-16" nil t)

;; use spaces instead of tabs
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

(setq-default tab-width 4)
(setq tab-width 4)
(setq c-default-style "bsd"
      c-basic-offset 2)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)   ; and how many of the old

;; column length
(setq fill-column 80)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;make emacs camel case sensitive in programming environments
(add-hook 'prog-mode-hook 'subword-mode)

;; winner mode, allows undoing and redoing window configurations
(when (fboundp 'winner-mode)
  (winner-mode 1))

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

;; (set-face-attribute 'region nil :background "yellow")

(put 'erase-buffer 'disabled nil) ;; enable the erase-buffer function
(require 'server)
(if (not (server-running-p))
    (server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Themes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(show-paren-mode 1) ;; turn on paren match highlighting
(setq show-paren-style 'mixed) ;; highlight entire bracket exp

;; (load-file "~/.emacs.d/lisp/gruvbox-emacs/gruvbox.el")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/lisp/gruvbox-emacs")
;; (load-theme 'gruvbox-light t)

(require 'linum)
(add-hook 'prog-mode-hook 'linum-on)

;; no line numbers for doc view,
(add-hook 'doc-view-mode-hook (lambda () (linum-mode -1)))

;; always add closing brackets and parens
(electric-pair-mode 1) 

(setq default-frame-alist '((font . "Inconsolata-16")))

(setq initial-frame-alist default-frame-alist)

;;; set up unicode
(prefer-coding-system                     'utf-8)
(set-default-coding-systems               'utf-8)
(set-terminal-coding-system               'utf-8)
(set-keyboard-coding-system               'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; pretty symbols
;; (global-prettify-symbols-mode -1)

(setq popup-use-optimized-column-computation nil)

;;add git to powerline
(vc-mode 1)

(defun my-term (buffer-name)
  "Start a terminal and rename buffer."
  (interactive "sbuffer name: ")
  (term "/bin/bash")
  (rename-buffer buffer-name t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Web ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path "~/.emacs.d/lisp/web-beautify")
;; (require 'web-beautify) ;; Not necessary if using ELPA package
(eval-after-load 'js-mode
  '(progn
     (define-key js-mode-map (kbd "C-c w b") 'web-beautify-js)
     (define-key js-mode-map (kbd "C-c C-i") 'nodejs-repl-load-file)))

(eval-after-load 'json-mode
  '(define-key json-mode-map (kbd "C-c w b") 'web-beautify-js))
(eval-after-load 'sgml-mode
  '(progn
     (define-key sgml-mode-map (kbd "C-c w b") 'web-beautify-html)
     (define-key sgml-mode-map (kbd "C-c w b") 'web-beautify-html)
     (define-key sgml-mode-map (kbd "C-c f") 'browse-url-of-buffer)))


(eval-after-load 'css-mode
  '(define-key css-mode-map (kbd "C-c w b") 'web-beautify-css))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; git ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'git)

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Latex;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun delete-latex-garbage()
  (interactive)
  (shell-command-ignore-buffer "rm *.aux")
  (shell-command-ignore-buffer "rm *.out")
  (shell-command-ignore-buffer "rm *.log"))

(defun shell-command-ignore-buffer (command)
  "Run COMMAND in shell while ignoring the buffer."
  (with-temp-buffer
    (shell-command command t)))

;; assumes you are calling this in a .tex file
(defun make-and-view-latex()
  (interactive)
  (save-excursion
    (save-buffer)
    (shell-command-ignore-buffer (concat "pdflatex " (buffer-name)))
    (delete-latex-garbage)
    (let ((pdfname
           (concat (substring (buffer-name) 0 -4) ".pdf")))
      (if (get-buffer pdfname)
          (progn
            (switch-to-buffer pdfname)
            (revert-buffer))
        (find-file pdfname)))))

(with-eval-after-load "latex"
  '(define-key LaTeX-mode-map [(f5)]
     'make-and-view-latex))

(add-hook 'LaTeX-mode-hook (lambda()
                             (turn-on-flyspell)
                             (abbrev-mode)))

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
        (quote (("TODO"      :foreground "red"     :weight bold)
                ("NEXT"      :foreground "yellow"  :weight bold)
                ("WAITING"   :foreground "magenta" :weight bold)
                ("HOLD"      :foreground "green"   :weight bold)
                ("CANCELLED" :foreground "purple"  :weight bold)
                ("MEETING"   :foreground "green"   :weight bold)
                ("PHONE"     :foreground "green"   :weight bold)
                ("DONE"      :foreground "blue"    :weight bold))))

  ;; highlight the cyrrent time in org agenda.
  (set-face-attribute 'org-agenda-current-time nil :foreground "purple")

  ;; move the habit graph further right
  (setq org-habit-graph-column 60)
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
                             (abbrev-mode)))
  (require 'org-habit)

;;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (haskell . nil)
     (latex . t)
     (ledger . t)
     (ocaml . nil)
     (octave . t)
     (org . t)  ;; VIP!
     (python . t)
     (ruby . t)
     (screen . nil)
     (sh . t)
     (sql . nil)
     (sqlite . t))))

(defun new-note (class-name)
  (interactive "sEnter name of Class: ")
  (let* ((file-name
          (concat (format-time-string "%Y-%m-%d")   ".org"))
         (file-full-name
          (concat "~/Documents/" class-name "/" file-name)))
    (find-file file-full-name)))



;; display links to images

(defun create-random-buffer ()
  "Creates an empty buffer with random name."
  (interactive)
  (switch-to-buffer
   (concat "*temp-" (concat (sha1 (number-to-string (random))) "*"))))

(defun kill-save ()
  "Kill the current buffer copying the contents to clipboard."
  (interactive)
  (kill-new (buffer-string))
  (kill-buffer (current-buffer)))

(defun email-writeup ()
  "Create a quick buffer to write an email."
  (interactive)
  (progn
    (create-random-buffer)
    (org-mode)
    (auto-fill-mode)
    (local-set-key (kbd "C-c C-c") 'kill-save)))

(global-set-key (kbd "C-c e") 'email-writeup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;; Dired Extensions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dired
  :init (progn
          (setq dired-listing-switches "-alh")
          (put 'dired-find-alternate-file 'disabled nil))
  :bind (:map dired-mode-map
              ("C-x w" . wdired-change-to-wdired-mode)))

;; (eval-after-load "dired"
;;   (define-key dired-mode-map (kbd "C-x w") 'wdired-change-to-wdired-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;C/C++;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make h files use c++ mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "C-x c") 'compile)))

(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "C-x c") 'compile)
            (setq flycheck-clang-language-standard nil)
            (setq flycheck-clang-args nil)))
(add-hook 'c++-mode-hook
          (lambda()
            (setq flycheck-clang-language-standard "c++14")
            (setq flycheck-clang-args "-std=c++14")))

;; Semantic
;; (semantic-mode t)
;; (global-semantic-idle-completions-mode t)
;; (global-semantic-decoration-mode nil)
;; (global-semantic-highlight-func-mode t)
;; (global-semantic-stickyfunc-mode t)    ; display function title on top of screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SQL stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on arch mysql is through mariadb which has an odd prompt-for-change-log-name
;; which won't show up  with current set up in sqli mode
(require 'sql)
(sql-set-product-feature 'mysql :prompt-regexp "^\\(?:mysql\\|mariadb\\).*> ")
(add-hook 'sql-interactive-mode-hook (lambda () (toggle-truncate-lines t)))
(with-eval-after-load "company"
  (add-to-list 'company-backends 'company-edbi)) ;; mysql auto complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gnus ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (setq gnus-use-full-window nil)

;; three pane gnus layout
;; (gnus-add-configuration
;;  '(article
;;    (horizontal 1.0
;;                (vertical 50
;;                          (group 1.0))
;;                (vertical 1.0
;;                          (summary 0.25 point)
;;                          (article 1.0)))))
;; (gnus-add-configuration
;;  '(summary
;;    (horizontal 1.0
;;                (vertical 50
;;                          (group 1.0))
;;                (vertical 1.0
;;                          (summary 1.0 point)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ERC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (load "~/.emacs.d/.ercrc.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (add-hook 'after-init-hook (load-file "~/.cache/wal/colors.el"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun swap-windows ()
  "If you have 2 windows, it swaps them." 
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p "~/extras.el")
    (load-file "~/extras.el"))


(org-agenda-list)

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
    ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(package-selected-packages
   (quote
    (ido-ubiquitous use-package solarized-theme smex rainbow-delimiters projectile pdf-tools paredit neotree multiple-cursors multi-term markdown-mode magit ledger-mode ido-vertical-mode highlight-indent-guides god-mode go-mode github-theme ghc ggtags flycheck flx-ido expand-region emms eclim ecb doom-themes doom company clang-format auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

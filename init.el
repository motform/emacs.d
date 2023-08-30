;;; init.el --- For fun and doubtful ergonomics -*- lexical-binding: t; -*-

;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/emacs.d
;; Package-Requires: ((emacs "29"))
;; Created: 2019-04-04

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This configuration is primarily interesting for me.
;; You might find a snippet or two useful, but as a whole
;; the keybinds are archaic and the settings highly personal.
;; Expects native-comp Emacs 28 on MacOS (Big Sur),
;; but should work on any other Unix.

;;; Code:


;;; Constants
(defconst mac-p (eq system-type 'darwin))


;;; Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setq exec-path (append exec-path '("~/.local/bin"))
      exec-path (append exec-path '("/usr/local/bin/")))
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/10:/usr/local/opt/libgccjit/lib/gcc/10:/usr/local/opt/gcc/lib/gcc/10/gcc/x86_64-apple-darwin20/10.2.0")

;;; Performance
(setq inhibit-compacting-font-caches t) ; PP is large font, this might help performance?
(setq native-comp-deferred-compilation-deny-list '()) ; emacs complains loudly if this is not set


(defadvice load-theme (before clear-previous-themes activate) ; Improve theme loading
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))


;;; Native-comp
(when (boundp 'comp-eln-load-path)
  (setcar comp-eln-load-path
          (expand-file-name "cache/eln-cache/" user-emacs-directory)))


;;; Unicode
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))    ; pretty
(set-language-environment      "UTF-8")
(prefer-coding-system          'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system   'utf-8)
(set-default-coding-systems    'utf-8)   ; pretty
(set-file-name-coding-system   'utf-8)
(set-keyboard-coding-system    'utf-8)
(set-selection-coding-system   'utf-8)
(set-terminal-coding-system    'utf-8)
(setq locale-coding-system     'utf-8)   ; please

;;; MacOS
(when mac-p
  (setq mac-option-modifier         nil ; alt-passthrough
        mac-command-modifier       'meta))


;;; Backups
(save-place-mode +1)

(setq create-lockfiles    nil
      make-backup-files   nil
      version-control     t
      backup-by-copying   nil
      delete-old-versions t
      kept-old-versions   5
      kept-new-versions   5
      auto-save-file-name-transforms `((".*" ,(concat user-emacs-directory "auto-save/") t))
      backup-directory-alist         '(("." . "~/.emacs.d/backup")))

;;; Start screen
(setq inhibit-startup-screen  t
      inhibit-startup-message t
      initial-major-mode     'fundamental-mode)

(setq visible-bell          nil
      ring-bell-function   'ignore
	  pixel-scroll-mode     1
      suggest-key-bindings  nil
      frame-title-format   '("%b"))

(fringe-mode 10)      ; set a 10 unit fringe, for flyspell and such
(blink-cursor-mode 0) ; No blinking cursor
(global-hl-line-mode) ; Global line hilight
(global-visual-line-mode 1)

(setq-default line-spacing 1) ; use patched fonts instead
(setq prettify-symbols-unprettify-at-point 'right-edge)


(add-to-list 'default-frame-alist  '(font . "MD IO 1.4"))
(set-face-attribute 'default        nil :family "MD IO 1.3" :height 120 :weight 'medium)
(set-face-attribute 'fixed-pitch    nil :family "MD IO 1.3" :height 120 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family "MD IO 1.3" :height 120 :weight 'medium)

(setq frame-resize-pixelwise t
      default-frame-alist    (append (list
                                      '(vertical-scroll-bars . nil)
                                      '(internal-border-width . 24)
                                      '(right-fringe   . 0)
                                      '(tool-bar-lines . 0))))

;; (defun simple-mode-line-render (left right)
;;   "Return a string of `window-width' length.
;; Containing LEFT, and RIGHT aligned respectively."
;;   (let ((available-width (- (window-total-width)
;; 							(+ (length (format-mode-line left))
;; 							   (length (format-mode-line right))))))
;;     (append left
;;             (list (format (format "%%%ds" available-width) ""))
;;             right)))

;; (setq-default
;;  column-number-mode t
;;  mode-line-format '((:eval (simple-mode-line-render
;; 							'("%e" ; left side
;;                               mode-line-front-space
;;                               mode-line-modified
;;                               mode-line-remote
;;                               mode-line-frame-identification
;;                               mode-line-buffer-identification
;; 							  "  "
;; 							  "%l:%c"
;;                               )
;;                             '("%"
;; 							  (vc-mode vc-mode)
;;                               mode-line-misc-info  ; right side
;;                               "  "
;;                               mode-line-process
;;                               mode-line-end-spaces
;;                               "  ")))))


;;; Misc
(fset 'yes-or-no-p 'y-or-n-p) ; Replace yes/no prompts with y/n
(setq save-interprogram-paste-before-kill  t
      help-window-select                   t
      require-final-newline                t
      load-prefer-newer                    t)


;;; Long Lines should wrap naturally
(setq bidi-paragraph-direction 'left-to-right)
(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t)
  (global-so-long-mode 1))


;;; Straight
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; (eval-when-compile ; NOTE
;;  (straight-use-package 'use-package))
(straight-use-package 'use-package)
(setq straight-use-package-by-default   t
      use-package-always-defer          t
	  comp-async-report-warnings-errors nil)


(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled.
NAME and ARGS are as in `use-package'.

SOURCE: https://github.com/raxod502/radian"
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))


(use-package git)


(use-package stimmung-themes
  :straight (stimmung-themes :local-repo "~/Developer/stimmung-themes")
  :demand   t
  :config   (stimmung-themes-load-light))

(setq window-divider-default-right-width  24
      window-divider-default-bottom-width 12
      window-divider-default-places       t)
(setq-default tab-width 4)
(window-divider-mode 1)


(use-package doom-modeline
  :demand t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 5)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-modification-icon nil)
  (doom-modeline-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-modal nil))


(use-package apheleia
  :demand t
  :init   (apheleia-global-mode +1))


(use-package flycheck
  :demand t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-fringe)
  (next-error-message-highlight t)
  ;; :config (setq-default flycheck-disabled-checkers
  ;; 						(append flycheck-disabled-checkers
  ;; 								'(javascript-jshint json-jsonlist)))

  :preface
  (defun mp-flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK."
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
				  (format "%s: %s"
						  (let ((level (flycheck-error-level err)))
							(pcase level
							  ('info (propertize "I" 'face 'flycheck-error-list-info))
							  ('error (propertize "E" 'face 'flycheck-error-list-error))
							  ('warning (propertize "W" 'face 'flycheck-error-list-warning))
							  (_ level)))
						  (flycheck-error-message err))
				  :thing (or (flycheck-error-id err)
							 (flycheck-error-group err))
				  :face 'font-lock-doc-face))
       flycheck-errors)))

  (defun mp-flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'mp-flycheck-eldoc nil t)
    (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly)
    (setq flycheck-display-errors-function nil)
    (setq flycheck-help-echo-function nil))
  :hook ((flycheck-mode . mp-flycheck-prefer-eldoc)))


(use-package flymake-stylelint
  :straight (flymake-stylelint :type git :host github :repo "orzechowskid/flymake-stylelint")
  :hook     (css-mode . flymake-stylelint-enable))


(use-feature flymake
  :config
  (define-key evil-normal-state-map (kbd "C-e") 'flymake-goto-next-error)
  (define-key evil-normal-state-map (kbd "C-M-e") 'flymake-show-buffer-diagnostics))


(use-feature flyspell
  :hook   (text-mode . flyspell-mode)
  :custom (ispell-program-name "aspell")
  :config (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))


(use-package flyspell-correct
  :bind   ("M-e" . flyspell-correct-wrapper)
  :custom (flyspell-define-abbrev))


(use-package exec-path-from-shell
  :demand t
  ;; :custom (exec-path-from-shell-arguments nil)
  :init   (when mac-p (exec-path-from-shell-initialize)))


;; (use-package undo-fu-session
;;   :demand t
;;   :init  (global-undo-fu-session-mode)
;;   :config
;;   (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))


(setq trash-directory "~/.Trash")
;; See `trash-directory' as it requires defining `system-move-file-to-trash'.
										; (defun system-move-file-to-trash (file)
										;  "Use \"trash\" to move FILE to the system trash."
										;   (cl-assert (executable-find "trash") nil "'trash' must be installed. Needs \"brew install trash\"")
										;    (call-process "trash" nil 0 nil "-F"  file))


(use-package evil
  :demand t
  :init   (setq evil-want-keybinding nil)
  :custom
  (evil-mode-line-format         nil)
  (evil-respect-visual-line-mode t)
  (evil-cross-lines              t)
  (evil-want-C-i-jump            nil) ; make C-i insert \t
  (evil-want-C-u-scroll          t)
  (evil-want-C-u-delete          t)
  (evil-want-C-d-scroll          t)
  (evil-show-paren-range         1)
  (evil-undo-system             'undo-redo)

  :config
  (evil-mode 1)

  (add-to-list 'evil-emacs-state-modes 'dired-mode)

  ;; add some emacs-like insert mode binds, for maximum confusion and heresy
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (global-set-key (kbd "M-v") 'clipboard-yank)

  ;; easier Emacs-driven macro definition
  (define-key evil-normal-state-map (kbd "q") 'kmacro-start-macro-or-insert-counter)
  (define-key evil-normal-state-map (kbd "Q") 'kmacro-end-or-call-macro)

  ;; scroll with C-u and bind the universal argument to M-u
  (define-key evil-normal-state-map (kbd "M-u") 'universal-argument)

  ;; move point to last change, not to next item in the jump list
  (define-key evil-normal-state-map (kbd "C-i") 'goto-last-change-reverse)
  (define-key evil-normal-state-map (kbd "C-o") 'goto-last-change))


(use-package evil-commentary
  :demand t
  :after  evil
  :config (evil-commentary-mode))


(use-package evil-collection
  :demand t
  :after  evil
  :config (evil-collection-init))


(use-feature align
  :demand t
  :bind (("M-l" . align-regexp))
  :config
  (define-key evil-normal-state-map (kbd "C-f") 'indent-buffer)
  (defun indent-buffer ()
	(interactive)
	(if (boundp 'cider-session-name)
		;;	(memq major-mode '(clojure-mode clojurec-mode clojurescript-mode))
		(cider-format-buffer)
	  (save-excursion
		(indent-region (point-min) (point-max) nil)))))


(use-package smartparens
  :demand t
  :init   (smartparens-global-mode 1))


(use-feature smartparens-config
  :demand t
  :after  smartparens
  :custom (sp-show-pair-from-inside t)
  :config
  (provide 'smartparens-setup)
  (require 'smartparens-clojure)
  (progn (show-smartparens-global-mode t))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

;;;; Allow ES6 arrow '=>' in web-mode
  (defun sp-after-equals-p (_id action _context)
	(when (memq action '(insert navigate))
      (sp--looking-back-p "=>" 2)))

  (defun sp-after-equals-skip (ms mb _me)
	(when (string= ms ">")
      (save-excursion
		(goto-char mb)
		(sp--looking-back-p "=" 1))))

  (sp-local-pair '(web-mode react-mode) "<" nil ; add web-mode
				 :unless '(:add sp-after-equals-p)
				 :skip-match 'sp-after-equals-skip))


(use-feature show-paren
  :init (show-paren-mode 1))


(use-package evil-smartparens
  :demand t
  :after  smartparens
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  (defun evil-sp--add-bindings ()
	(when smartparens-strict-mode
	  (evil-define-key 'normal evil-smartparens-mode-map
		(kbd "d") #'evil-sp-delete
		(kbd "c") #'evil-sp-change
		(kbd "y") #'evil-sp-yank
		(kbd "s") #'ctrlf-forward-fuzzy
		(kbd "S") #'search-everything-in-project
		(kbd "X") #'evil-sp-backward-delete-char
		(kbd "x") #'evil-sp-delete-char)
	  (add-to-list 'evil-change-commands #'evil-sp-change)
	  (evil-define-key 'visual evil-smartparens-mode-map
		(kbd "X") #'evil-sp-delete
		(kbd "x") #'evil-sp-delete))
	(evil-define-key 'normal evil-smartparens-mode-map
	  (kbd "D") #'evil-sp-delete-line
	  (kbd "Y") #'evil-sp-yank-line
	  (kbd "C") #'evil-sp-change-line)
	(when smartparens-strict-mode
	  (evil-define-key 'insert evil-smartparens-mode-map
		(kbd "DEL") 'sp-backward-delete-char))
	(evil-define-key 'visual evil-smartparens-mode-map
	  (kbd "o") #'evil-sp-override)
	(evil-normalize-keymaps)))


(use-package vertico
  :demand t
  :straight (:files (:defaults "extensions/*"))
  :hook (minibuffer-setup-hook . cursor-intangible-mode)
  :init (vertico-mode)
  :custom ; along with some unrelated settings
  (vertico-count 20)
  (vertico-cycle t)
  (vertico-count-format nil)
  (read-minibuffer-restore-windows t)
  (enable-recursive-minibuffers    t)
  (describe-bindings-outline       t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (read-extended-command-predicate #'command-completion-default-include-p)
  :bind
  (("M-q" . save-buffers-kill-terminal)
   ("M-p" . execute-extended-command)
   ("M-s" . save-buffer)
   :map minibuffer-local-map
   ("C-l" . 'vertico-directory-delete-word)))


(use-package prescient
  :demand t
  :config (prescient-persist-mode +1)
  :custom
  (prescient-filter-method          '(literal initialism prefix regexp))
  (prescient-use-char-folding        t)
  (prescient-use-case-folding       'smart)
  (prescient-sort-full-matches-first t)
  (prescient-sort-length-enable      t)
  (prescient-history-length          1000))


(use-package tree-sitter
  :demand t
  :hook ((typescript-mode     . tree-sitter-hl-mode)
	     (typescript-tsx-mode . tree-sitter-hl-mode))
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(use-package tree-sitter-langs
  :demand t
  :after tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))


(use-package typescript-mode
  :demand t
  :after tree-sitter
  :config
  (define-derived-mode typescript-tsx-mode typescript-mode "tsx")
  (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-mode)
  (add-hook 'typescript-mode #'subword-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-tsx-mode))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))


(use-package tsi
  :demand t
  :after tree-sitter
  :straight (tsi :type git :host github :repo "orzechowskid/tsi.el")
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook       (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook        (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook       (lambda () (tsi-scss-mode 1))))


(use-package lsp-tailwindcss
  :straight (lsp-tailwindcss
			 :type git
			 :host github
			 :repo "merrickluo/lsp-tailwindcss")
  :init (setq lsp-tailwindcss-add-on-mode t))

(use-feature eglot
  :custom
  (eglot-ignored-server-capabilites
   '(
	 ;; :codeLensProvider
	 ;; :documentHighlightProvider
	 :documentOnTypeFormattingProvider
	 :foldingRangeProvider))
  :bind (:map eglot-mode-map
			  ("M-r" . 'eglot-rename)))


(use-package eldoc-box
  :hook (prog-mode . eldoc-box-hover-mode))


(use-package orderless
  :demand t
  :config
  (defun orderless-fast-dispatch (word index total)
	(and (= index 0) (= total 1) (length< word 1)
		 `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
	(orderless-style-dispatchers '(orderless-fast-dispatch))
	(orderless-matching-styles '(orderless-literal orderless-regexp)))
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


(use-package visual-regexp)


(use-package visual-regexp-steroids
  :bind   (("M-i" . #'vr/query-replace)
		   ("M-I" . #'radian-query-replace-literal))
  :custom (vr/engine 'python)
  :config (defun radian-query-replace-literal ()
			"Do a literal query-replace using `visual-regexp'."
			(interactive)
			(let ((vr/engine 'emacs-plain))
			  (call-interactively #'vr/query-replace))))

(defun split-right-and-focus ()
  "Make a horizontal windows split and move there."
  (interactive)
  (split-window-right)
  (windmove-right))

(defun split-down-and-focus ()
  "Make a vertical windows split and move there."
  (interactive)
  (split-window-below)
  (windmove-down))

(defun kill-this-split-or-buffer ()
  "Kill split if active, else kill buffer."
  (interactive)
  (if (one-window-p) (kill-buffer)
	(delete-window)))


(use-feature windmove
  :config (define-key evil-normal-state-map (kbd "C-w") 'delete-window)
  :bind (("C-l"   . 'windmove-right)
		 ("C-h"   . 'windmove-left)
		 ("C-k"   . 'windmove-up)
		 ("C-j"   . 'windmove-down)
		 ("C-w"   . 'delete-window)
		 ("M-w"   . 'kill-this-split-or-buffer)
		 ("C-ä"   . 'split-right-and-focus)
		 ("C-å"   . 'split-down-and-focus)))


(use-package eyebrowse
  :defer  1
  :init   (global-unset-key (kbd "C-c C-w"))
  :custom
  (eyebrowse-mode-line-style 'hide)
  (eyebrowse-new-workspace   t)
  :config
  (progn
	(define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
	(define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
	(define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
	(define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
	(define-key eyebrowse-mode-map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
	(define-key eyebrowse-mode-map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
	(define-key eyebrowse-mode-map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
	(define-key eyebrowse-mode-map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
	(define-key eyebrowse-mode-map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)
	(define-key eyebrowse-mode-map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
	(eyebrowse-mode t)))

(use-feature minibuffer
  ;; use completion-at-point with selectrum+prescient
  :custom (tab-always-indent 'complete))


(use-package web-mode
  :custom (web-mode-markup-indentation-offset 2)
  :mode "\\.php\\'"
  :mode "\\.html?\\'")


(use-package consult
  :demand t
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config   ;; evil-integration
  (consult-customize consult-completion-in-region
					 :completion-styles (orderless-flex))
  :bind
  (("M-y"   . 'consult-yank-pop)
   ("M-a"   . 'consult-buffer)
   ("C-M-s" . 'consult-line-multi)
   (:map minibuffer-mode-map
		 ("C-k" . 'kill-line))))


(use-package rg
  :init (rg-enable-menu)
  :config
  (rg-define-search search-everything-in-project
    "Search files in the current `project'."
    :query ask
    :format literal
    :files "everything"
    :flags ("--hidden")
    :dir (car (last (project-current)))
    :menu ("Search" "p" "Project")))


(use-package ctrlf
  :demand t
  :bind (("C-s" . 'ctrlf-forward-fuzzy)))


(use-package consult-project-extra
  :straight t
  :bind (("M-t" . 'consult-project-extra-find)))


(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  :straight (:files (:defaults "extensions/*"))
  :demand t
  :config

  ;; Corfu-history
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; Eshell config
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (setq-local corfu-auto nil)
			  (corfu-mode)))

  (defun corfu-send-shell (&rest _)
	"Send completion candidate when inside comint/eshell."
	(cond
	 ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
	  (eshell-send-input))
	 ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
	  (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)

  ;; Move-to-minibuffer
  (defun corfu-move-to-minibuffer ()
	(interactive)
	(let ((completion-extra-properties corfu--extra)
		  completion-cycle-threshold completion-cycling)
	  (apply #'consult-completion-in-region completion-in-region--data)))

  :bind
  (:map corfu-map
		("M-p" . 'corfu-move-to-minibuffer)
		("C-ö" . 'corfu-popupinfo-location)
		("C-ä" . 'corfu-popupinfo-documentation))
  :custom
  (completion-cycle-threshold 3)
  (tab-always-indent 'complete)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-min-width 40)
  (corfu-auto-delay 0.10)
  (corfu-auto-prefix 3)
  (completion-styles '(orderless-fast))
  (corfu-separator ?\s)
  (corfu-scroll-margin 5)
  (corfu-popupinfo-delay nil))


(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard.
Source: https://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun insert-current-date ()
  "Insert current date in ISO format at current point in buffer."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date -I)")))


(use-package restclient
  :mode "\\.http\\’")


(use-feature python
  :custom (python-shell-interpreter "python3"))


(use-package arduino-cli-mode
  :mode "\\.ino\\'"
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify    t))


(use-package flycheck-clj-kondo)


(use-feature clojure-mode
  :config (require 'flycheck-clj-kondo))


(use-package cider
  :config
  (evil-make-intercept-map cider--debug-mode-map 'normal)
  :bind
  (:map cider-mode-map
		("C-<return>"   . 'cider-eval-defun-at-point)
		("C-S-<return>" . 'eval-last-sexp)
		("M-u"          . 'cider-format-buffer))
  :custom
  ;; (cider-eval-result-duration          nil)
  (cider-eval-result-prefix            "")
  (cider-clojure-cli-global-options    "-J-XX:-OmitStackTraceInFastThrow")
  (cider-repl-display-help-banner      nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-content-types        t)
  (cider-save-file-on-load             t)
  (cider-complete-at-point             t)
  (cider-eval-result-duration          'change)
  (cider-shadow-default-options        "app")
  (nrepl-hide-special-buffers          t)
  :config
  (defun cider-connect-default-unix-socket ()
	"Connect cider to the default unix socket in `PROJECT-CURRENT'."
	(interactive)
	(let ((socket-file (concat (car (last (project-current))) "nrepl-socket")))
	  (cider-connect-clj '(:host "local-unix-domain-socket" :socket-file socket-file)))))

(use-feature elisp-mode
  :config (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
  :custom (help-enable-symbol-autoload t))


(use-feature eglot)


(use-package eldoc-box
  :config (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))


(use-package package-lint)


(use-package tex
  :straight auctex
  :ensure   auctex
  :hook     ((tex-mode   . reftex-mode)
			 (latex-mode . reftex-mode)
			 (LaTeX-mode-hook . turn-on-auto-fill))
  :bind     (:map tex-mode-map
				  ("M-c" . 'reftex-citation))
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq-default TeX-engine   'xetex)
  (setq-default TeX-PDF-mode t)

  (eval-after-load 'reftex-vars ; SOURCE: https://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992
	'(progn
	   (setq reftex-cite-format
			 '((?c . "\\cite[]{%l}")
			   (?f . "\\footcite[][]{%l}")
			   (?t . "\\textcite[]{%l}")
			   (?p . "\\parencite[]{%l}")
			   (?o . "\\citepr[]{%l}")
			   (?n . "\\nocite{%l}")))))

  :custom
  (TeX-save-query          nil)
  (TeX-show-compilation    nil)
  (reftex-plug-into-AUCTeX t))


(use-package nov
  :mode "\\.epub\\'"
  :custom
  (visual-fill-column-center-text t)
  (nov-text-width                 80))


(use-package hydra
  :demand t
  :bind   (:map dired-mode-map
				;; ("ä"   . hydra-window/body)
				("SPC" . hydra-smartparens/body))
  :config
  (load "~/.emacs.d/hydras.el")
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-smartparens/body)
  ;; (define-key evil-normal-state-map (kbd "ä")   'hydra-window/body)
  )


(use-feature org
  :custom
  (calendar-date-style                'european)
  (calendar-week-start-day             1)
  (org-confirm-babel-evaluate          nil)
  (org-edit-src-content-indentation    0)
  (org-fontify-quote-and-verse-blocks  t)
  (org-fontify-whole-heading-line      t)
  (org-hide-emphasis-markers           t)
  (org-hide-leading-stars              nil)
  (org-src-fontify-natively            t)
  (org-src-tab-acts-natively           t)
  (org-startup-with-inline-images      t)
  (org-list-demote-modify-bullet      '(("+" . "-") ("-" . "+") ("*" . "+")))
  (calendar-date-display-form         '((if dayname (concat dayname ", ")) day " " monthname " " year)))


(use-package olivetti
  :custom (olivetti-set-width 100))


(use-package writegood-mode
  :after org)


;; (use-package dash-at-point
;;   :bind ("M-ö" . dash-at-point))


(use-feature eshell
  :custom
  (eshell-where-to-jump        'begin)
  (eshell-review-quick-commands nil)
  (eshell-glob-case-insensitive t)
  (eshell-cmpl-ignore-case      t)
  (eshell-banner-message        "")
  :hook   (eshell-mode-hook . (lambda () (define-key eshell-mode-map (kbd "<tab>")
													 (lambda () (interactive) (pcomplete-std-complete)))))

  :bind
  ("M-n" . (lambda ()
			 (interactive)
			 (if (project-current)
				 (project-eshell)
			   (eshell t))))
  :config
  (defun fish-path (path max-len)
	"Return a potentially trimmed-down version of the directory PATH, replacing
  parent directories with their initial characters to try to get the character
  length of PATH (sans directory slashes) down to MAX-LEN.
  Source: https://www.emacswiki.org/emacs/EshellPrompt"
	(let* ((components (split-string (abbreviate-file-name path) "/"))
		   (len (+ (1- (length components))
				   (cl-reduce '+ components :key 'length)))
		   (str ""))
	  (while (and (> len max-len) (cdr components))
		(setq str (concat str (cond ((= 0 (length (car components))) "/")
									((= 1 (length (car components)))
									 (concat (car components) "/"))
									(t (if (string= "." (string (elt (car components) 0)))
										   (concat (substring (car components) 0 2) "/")
										 (string (elt (car components) 0) ?/)))))
			  len (- len (1- (length (car components))))
			  components (cdr components)))
	  (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  (defun fishy-eshell-prompt-function ()
	(concat (fish-path (eshell/pwd) 40)
			(if (= (user-uid) 0) " # " " $ ")))

  (setq eshell-prompt-function 'fishy-eshell-prompt-function))


(use-package makefile-runner
  :bind ("M-m" . makefile-runner))


(use-package magit
  :bind (("C-x C-g" . magit-status)
		 ("M-g"     . magit-status)))


(use-package swift-mode)


(use-feature objc-mode
  :mode "\\.mm\\'"
  :mode "\\.m\\'")


(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
			  ("M-n" . (lambda ()
						 (interactive)
						 (if (project-current)
							 (project-eshell)
						   (eshell t))))
			  ("M-p" . 'execute-extended-command)))


(use-package toml-mode)


(use-package csv-mode)


(use-package yaml-mode)


(use-package dockerfile-mode)


(use-feature dired
  :bind ("C-x C-d" . 'dired)
  :custom
  (dired-recursive-deletes  'always)
  (dired-recursive-copies   'always)
  ;; (delete-by-moving-to-trash t)
  (dired-dwim-target         t)  ; big norton commander energy
  :config
  (when mac-p
	(setq dired-use-ls-dired t
		  insert-directory-program "/opt/homebrew/bin/gls"
		  dired-listing-switches "-aBhl --group-directories-first")))

(defun open-init ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(use-feature emacs
  :bind (("M-," . open-init)))


(use-feature autorevert
  :defer 2
  :custom
  (auto-revert-interval                1)
  (global-auto-revert-non-file-buffers t)
  (revert-without-query               '(" . *"))
  :config
  (global-auto-revert-mode +1))

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((eval define-key evil-normal-state-map
		   (kbd "ö")
		   '(lambda nil
			  (interactive)
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
			  (cider-eval-file "/Users/aj6531/Documents/motform/src/motform/portfolio/template.clj")
			  (cider-interactive-eval "(motform.portfolio.core/-main)")))))
 '(safe-local-variable-values
   '((eval define-key evil-normal-state-map
		   (kbd "ä")
		   'clerk-show)
	 (eval define-key evil-normal-state-map
		   (kbd "ö")
		   'clerk-show)
	 (eval define-key evil-normal-state-map
		   (kbd "ö")
		   'processing-3-run)
	 (eval define-key evil-normal-state-map
		   (kbd "ö")
		   '(lambda nil
			  (interactive)
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/templates/multiverse.clj")
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/templates/front_page.clj")
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/template.clj")
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
			  (cider-interactive-eval "(motform.portfolio.core/-main)")))
	 (eval define-key evil-normal-state-map
		   (kbd "ö")
		   '(lambda nil
			  (interactive)
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/template.clj")
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
			  (cider-interactive-eval "(motform.portfolio.core/-main)")))
	 (cider-shadow-default-options . "app")
	 (cider-default-cljs-repl . shadow)
	 (eval define-key evil-normal-state-map
		   (kbd "ö")
		   '(lambda nil
			  (interactive)
			  (cider-load-file "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
			  (cider-interactive-eval "(motform.portfolio.core/-main)")))
	 (eval define-key evil-normal-state-map
		   (kbd "ö")
		   '(lambda nil
			  (interactive)
			  (cider-load-file "/Users/lla/Documents/motform/src/motform/portfolio/core.clj")
			  (cider-interactive-eval "(motform.portfolio.core/-main)")))
	 (eval define-key evil-normal-state-map
		   (kbd "ö")
		   '(lambda nil
			  (interactive)
			  (cider-load-file "/Users/lla/Projects/motform/src/motform/portfolio/core.clj")
			  (cider-interactive-eval "(motform.portfolio.core/-main)"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;;; GUI
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

(add-to-list 'default-frame-alist  '(font . "PragmataPro Liga 1.4"))
(set-face-attribute 'default        nil :family "PragmataPro Liga 1.4" :height 140)
(set-face-attribute 'fixed-pitch    nil :family "PragmataPro Liga 1.4" :height 140)
(set-face-attribute 'variable-pitch nil :family "PragmataPro Liga 1.4" :height 140)

(setq frame-resize-pixelwise t
      default-frame-alist    (append (list
                                      '(vertical-scroll-bars . nil)
                                      '(internal-border-width . 24)
                                      '(right-fringe   . 0)
                                      '(tool-bar-lines . 0))))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length.
Containing LEFT, and RIGHT aligned respectively."
  (let ((available-width
         (- (window-total-width)
            (+ (length (format-mode-line left))
               (length (format-mode-line right))))))
    (append left
            (list (format (format "%%%ds" available-width) ""))
            right)))

;; Display to mode-line buffer name relative to current-project.
;; SOURCE: https://www.reddit.com/r/emacs/comments/8xobt3/tip_in_modeline_show_buffer_file_path_relative_to/
(with-eval-after-load 'subr-x
  (setq-default mode-line-buffer-identification
                '(:eval
                  (format-mode-line
                   (propertized-buffer-identification
                    (or (when-let* ((buffer-file-truename buffer-file-truename)
                                    (prj                  (cdr-safe (project-current)))
                                    (prj-parent           (file-name-directory (directory-file-name (expand-file-name prj)))))
                          (concat (file-relative-name (file-name-directory buffer-file-truename) prj-parent) (file-name-nondirectory buffer-file-truename)))
                        "%b"))))))

(setq-default column-number-mode t
              mode-line-format   '((:eval (simple-mode-line-render '("%e" ; left side
                                                                     mode-line-front-space
                                                                     mode-line-modified
                                                                     mode-line-remote
                                                                     mode-line-frame-identification
                                                                     mode-line-buffer-identification
                                                                     "   "
                                                                     "%l:%c")
                                                                   '("%"
                                                                     mode-line-misc-info  ; right side
                                                                     "  "
                                                                     mode-line-process
                                                                     mode-line-end-spaces
                                                                     "  ")))))


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
(setq straight-use-package-by-default t
      use-package-always-defer        t)


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
  :straight (stimmung-themes :local-repo "~/Documents/stimmung-themes")
  :demand   t
  :config   (stimmung-themes-load-light))

(setq window-divider-default-right-width  24
      window-divider-default-bottom-width 12
      window-divider-default-places       t)
(setq-default tab-width 4)
(window-divider-mode 1)

(use-package exec-path-from-shell
  :demand t
  :custom (setq exec-path-from-shell-arguments nil)
  :init   (when mac-p (exec-path-from-shell-initialize)))

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
  :bind (("M-l" . align-regexp)
         ("M-o" . indent-buffer))
  :config  (defun indent-buffer ()
			 (interactive)
			 (save-excursion
			   (indent-region (point-min) (point-max) nil))))


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
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))


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
		(kbd "s") #'ctrlf-forward-fuzzy-regexp
		(kbd "S") #'ctrlf-backward-fuzzy-regexp
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


(use-package selectrum
  :init (selectrum-mode +1)
  :custom ; along with some general things
  (selectrum-max-window-height 20)
  (read-minibuffer-restore-windows t)
  (describe-bindings-outline t)
  :config
  (defun zap-to-path ()
	"Zaps up to the root of the current path."
	(interactive)
	(zap-up-to-char -1 ?\/))
  (defun open-init ()
	(interactive)
	(find-file "~/.emacs.d/init.el"))
  :bind
  (("M-y" . yank-pop)
   ("M-a" . switch-to-buffer)
   ("M-w" . kill-this-buffer)
   ("M-q" . save-buffers-kill-terminal)
   ("M-p" . execute-extended-command)
   ("M-s" . save-buffer)
   ("M-," . open-init)
   :map minibuffer-local-map
   ("C-l" . zap-to-path)))


(use-package prescient
  :config (prescient-persist-mode +1)
  :custom (prescient-history-length 1000))


(use-package selectrum-prescient
  :demand t
  :after selectrum
  :config (selectrum-prescient-mode +1))


(use-package ctrlf
  :init (ctrlf-mode +1)
  :bind (("M-f" . 'ctrlf-forward-fuzzy-regexp)
		 ("M-S-f" . 'ctrlf-backward-fuzzy-regexp))
  :config
  (define-key evil-normal-state-map (kbd "s") 'ctrlf-forward-fuzzy-regexp)
  (define-key evil-normal-state-map (kbd "S") 'ctrlf-backward-fuzzy-regexp))


(use-package rg
  :init (rg-enable-menu)
  :bind (("M-C-f" . rg)
		 (:map rg-mode-map ("M-n" . rg-menu))))


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


(use-feature project
  :bind (("M-t" . 'project-find-file)
		 ("M-r" . 'project-switch-project)))


(use-package flycheck
  :hook   (prog-mode . flycheck-mode)
  :custom
  (flycheck-indication-mode 'left-fringe)
  (next-error-message-highlight t)
  :config (setq-default flycheck-disabled-checkers
						(append flycheck-disabled-checkers
								'(javascript-jshint json-jsonlist))))


(use-feature flyspell
  :hook   (text-mode . flyspell-mode)
  :custom (ispell-program-name "aspell")
  :config
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))


(use-package flyspell-correct
  :bind   ("M-e" . flyspell-correct-wrapper)
  :custom (flyspell-define-abbrev))


(use-feature minibuffer
  ;; use completion-at-point with selectrum+prescient
  :custom (tab-always-indent 'complete))


(use-package web-mode
  :custom (web-mode-markup-indentation-offset 2)
  :mode "\\.php\\'"
  :mode "\\.tsx\\'"
  :mode "\\.html?\\'")


(use-package tide
  :after  (typescript-mode flycheck)
  :hook   ((typescript-mode . tide-setup)
		   (typescript-mode . tide-hl-identifier-mode)
		   (before-save . tide-format-before-save))
  :config (add-hook 'web-mode-hook
					(lambda ()
					  (when (string-equal "tsx" (file-name-extension buffer-file-name))
						(setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))


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
		("M-o"          . 'cider-format-buffer))
  :custom
  ;; (cider-eval-result-duration          nil)
  (cider-eval-result-prefix            "")
  (cider-clojure-cli-global-options    "-J-XX:-OmitStackTraceInFastThrow")
  (cider-repl-display-help-banner      nil)
  (cider-repl-pop-to-buffer-on-connect nil)
  (cider-repl-use-content-types        t)
  (cider-save-file-on-load             t)
  (cider-eval-result-duration          'change)
  (cider-shadow-default-options        "app")
  (nrepl-hide-special-buffers          t))


(use-feature elisp-mode
  :config (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)
  :custom (help-enable-symbol-autoload t))


(use-package package-lint)


(use-package tex
  :straight auctex
  :ensure   auctex
  :hook     (tex-mode   . reftex-mode)
  :hook     (latex-mode . reftex-mode)
  :bind     (:map tex-mode-map
				  ("M-c" . 'reftex-citation))
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
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
				("ä"   . hydra-window/body)
				("SPC" . hydra-smartparens/body))
  :config
  (load "~/.emacs.d/hydras.el")
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-smartparens/body)
  (define-key evil-normal-state-map (kbd "ä")   'hydra-window/body))


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

(use-package synosaurus
  :bind   ("M-u" . 'synosaurus-choose-and-replace)
  :custom (synosaurus-choose-method 'completing-read))


(use-package olivetti
  :custom (olivetti-set-width 100))


(use-package writegood-mode
  :after org)


(use-package dash-at-point
  :bind ("M-ö" . dash-at-point))


(use-feature eshell
  :custom
  (eshell-where-to-jump        'begin)
  (eshell-review-quick-commands nil)
  (eshell-glob-case-insensitive t)
  (eshell-cmpl-ignore-case      t)
  (eshell-banner-message        "")
  :bind
  ("M-n" . (lambda ()
			 (interactive)
			 (if (project-current)
				 (project-eshell)
			   (eshell t))))
  :config
  (add-hook 'eshell-mode-hook
			(lambda ()
			  (define-key eshell-mode-map (kbd "<tab>")
						  (lambda () (interactive) (pcomplete-std-complete)))))

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
  :init (setq markdown-command "multimarkdown"))


(use-package toml-mode)


(use-package yaml-mode)


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


(use-feature autorevert
  :defer 2
  :custom
  (auto-revert-interval                1)
  (global-auto-revert-non-file-buffers t)
  (revert-without-query               '(" . *"))
  :config
  (global-auto-revert-mode +1))

;;; init.el ends here

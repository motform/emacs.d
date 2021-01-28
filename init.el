;;; init.el --- For fun and doubtful ergonomics -*- lexical-binding: t; -*-

;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/emacs.d
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

;;; Code:


;;; Constants
(defconst IS-MAC (eq system-type 'darwin))


;;; Path
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin/"))
(setq exec-path (append exec-path '("~/.local/bin"))
      exec-path (append exec-path '("/usr/local/bin/")))


;;; Performance
(setq inhibit-compacting-font-caches t) ; pp is large font, this might help performance?

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
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)   ; pretty
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system  'utf-8)   ; please


;;; MacOS
(when IS-MAC
  (setq mac-right-command-modifier 'meta
        mac-option-modifier         nil ; alt-passthrough
        mac-command-modifier       'super))

(defun pasteboard-paste ()
  "Paste from OS X system pasteboard via `pbpaste' to point.
         By 4ae1e1 at https://stackoverflow.com/a/24249229"
  (interactive)
  (shell-command-on-region
   (point) (if mark-active (mark) (point)) "pbpaste" nil t))

(global-set-key (kbd "s-v") 'pasteboard-paste)


;;; Long Lines should wrap naturally
(setq bidi-paragraph-direction 'left-to-right)
(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t)
  (global-so-long-mode 1))


;;; Backups
(save-place-mode +1)

(setq create-lockfiles    nil
      make-backup-files   nil
      version-control     t
      backup-by-copying   nil
      delete-old-versions t
      kept-old-versions   5
      kept-new-versions   5
      backup-directory-alist '(("." . "~/.emacs.d/backup"))
      tramp-backup-directory-alist backup-directory-alist)


;;; Start screen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-major-mode 'fundamental-mode
      inhibit-startup-echo-area-message "nanospasm")

;;; GUI
(setq ns-use-proxy-icon    nil
      visible-bell         nil
      ring-bell-function   'ignore
      suggest-key-bindings nil
      frame-title-format   '("%b"))

(fringe-mode 10)      ; set a 10 unit fringe, for flyspell and such
(blink-cursor-mode 0) ; No blinking cursor
(global-hl-line-mode) ; Global line hilight
(global-visual-line-mode 1)


(setq-default line-spacing 0.2)
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-to-list 'default-frame-alist '(font . "PragmataPro Liga"))
(set-face-attribute 'default        nil :family "PragmataPro Liga" :height 120)
(set-face-attribute 'fixed-pitch    nil :family "PragmataPro Liga" :height 120)
(set-face-attribute 'variable-pitch nil :family "PragmataPro Liga" :height 120)


;; (load "~/.emacs.d/liga.el")
;; (add-hook 'prog-mode-hook 'prettify-hook)
;; (add-hook 'text-mode-hook 'prettify-hook)
;; (global-prettify-symbols-mode t)


;;; Misc
(fset 'yes-or-no-p 'y-or-n-p) ; Replace yes/no prompts with y/n
(setq save-interprogram-paste-before-kill  t
      apropos-do-all                       t
      help-window-select                   t
      require-final-newline                t
      load-prefer-newer                    t)


;;; Long Lines should wrap naturally
(setq bidi-paragraph-direction 'left-to-right)
(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t)
  (global-so-long-mode 1))


;;; Straight
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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(setq use-package-always-defer t)
(use-package git)


(use-package exec-path-from-shell
  :demand t
  :init (exec-path-from-shell-initialize))


(straight-use-package
 '(stimmung :host github :repo "motform/stimmung"))
(load-theme 'stimmung t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))


(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init (unless after-init-time (setq-default mode-line-format nil)) ; prevent flash of unstyled modeline at startup
  :custom
  (column-number-mode               t)
  (doom-modeline-buffer-encoding    nil)
  (doom-modeline-modal-icon         nil)
  (doom-modeline-buffer-state-icon  nil)
  (doom-modeline-icon               nil)
  (doom-modeline-enable-word-count  nil)
  (doom-modeline-github             nil)
  (doom-modeline-mu4e               nil)
  (doom-modeline-persp-name         nil)
  (doom-modeline-minor-modes        nil)
  (doom-modeline-major-mode-icon    nil))


(use-package evil
  :demand t
  :init (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (setq-default evil-cross-lines t)
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  
  ;; add some emacs-like insert mode binds, for maximum confusion and heresy
  (define-key evil-insert-state-map (kbd "C-a") 'move-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-p") 'previous-line)
  (define-key evil-insert-state-map (kbd "C-n") 'next-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

  ;; scroll with C-u and bind the universal argument to M-u
  (define-key evil-normal-state-map (kbd "M-u") 'universal-argument)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-insert-state-map (kbd "C-u")
    (lambda ()
      (interactive)
      (evil-delete (point-at-bol) (point))))

  ;; This makes evil work betther with visual-line-mode
  (define-key evil-normal-state-map (kbd "<remap> <evil-next-line>")     'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-next-line>")     'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line))


(use-package evil-commentary
  :demand t
  :after evil
  :config (evil-commentary-mode))


(use-package evil-collection
  :demand t
  :after evil
  :config (evil-collection-init))


(use-package align
  :straight nil
  :after evil
  :bind ("s-l" . align-regexp))


(use-package undo-fu
  :after evil
  :demand t
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))


;; (use-package undo-fu-session
;;   :after undo-fu
;;   :demand t
;;   :config (global-undo-fu-session-mode))


(use-package smartparens
  :demand t
  :init (smartparens-global-mode 1))


(use-package smartparens-config
  :demand t
  :after smartparens
  :straight nil
  :custom
  (blink-matching-paren nil)
  (sp-show-pair-from-inside t)
  :config
  (show-paren-mode 1)
  (provide 'smartparens-setup)
  (progn (show-smartparens-global-mode t))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (require 'smartparens-clojure)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))


(use-package evil-smartparens
  :demand t
  :after smartparens
  :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))


(use-package highlight-parentheses
  :config (global-highlight-parentheses-mode t)
  :custom (highlight-parentheses-colors '("red")))


(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode)
  :config
  (setq-default indent-tabs-mode nil)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'elpy-mode))

(use-package adaptive-wrap
  :after aggressive-indent
  :demand t
  :config
  (add-hook 'prog-mode-hook #'adaptive-wrap-prefix-mode)
  (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode))


(use-package selectrum
  :init (selectrum-mode +1)
  :custom
  (suggeest-key-bindings t)
  (selectrum-num-candidates-displayed 10)
  :config
  (defun zap-to-path ()
    "Zaps up to the root of the current path."
    (interactive)
    (zap-up-to-char -1 ?\/))
  :bind
  (("s-y"     . yank-pop)
   ("s-a"     . switch-to-buffer)
   ("M-r"     . selectrum-repeat)
   ("C-x C-b" . switch-to-buffer)
   :map minibuffer-local-map
   ("C-l"     . zap-to-path)))


(use-package prescient
  :config (prescient-persist-mode +1)
  :custom (prescient-history-length 1000))


(use-package selectrum-prescient
  :demand t
  :after selectrum
  :config (selectrum-prescient-mode +1))


(use-package ctrlf
  :init (ctrlf-mode +1)
  :bind
  (("C-s" . ctrlf-forward-fuzzy)
   ("C-S" . ctrlf-backward-fuzzy))
  :config
  (evil-define-key 'normal evil-normal-state-map
    "s" 'ctrlf-forward-fuzzy
    "S" 'ctrlf-backward-fuzzy))


(use-package rg
  :bind (("s-s" . rg)
         (:map rg-mode-map ("M-n" . rg-menu))))


(use-package browse-kill-ring
  :bind ("s-y" . browse-kill-ring)
  :custom (browse-kill-ring-highlight-current-entry t))


(use-package visual-regexp
  :bind (("s-i" . #'vr/query-replace)))


(use-package visual-regexp-steroids
  :demand t
  :after visual-regexp
  :bind (("s-I" . #'radian-query-replace-literal))
  :custom (vr/engine 'python)
  :config
  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace))))


(use-package eyebrowse
  :defer 1
  :init (global-unset-key (kbd "C-c C-w"))
  :custom
  (eyebrowse-mode-line-style 'hide)
  (eyebrowse-new-workspace   t)
  :config
  (progn
    (define-key eyebrowse-mode-map (kbd "s-1") 'eyebrowse-switch-to-window-config-1)
    (define-key eyebrowse-mode-map (kbd "s-2") 'eyebrowse-switch-to-window-config-2)
    (define-key eyebrowse-mode-map (kbd "s-3") 'eyebrowse-switch-to-window-config-3)
    (define-key eyebrowse-mode-map (kbd "s-4") 'eyebrowse-switch-to-window-config-4)
    (define-key eyebrowse-mode-map (kbd "s-5") 'eyebrowse-switch-to-window-config-5)
    (define-key eyebrowse-mode-map (kbd "s-6") 'eyebrowse-switch-to-window-config-6)
    (define-key eyebrowse-mode-map (kbd "s-7") 'eyebrowse-switch-to-window-config-7)
    (define-key eyebrowse-mode-map (kbd "s-8") 'eyebrowse-switch-to-window-config-8)
    (define-key eyebrowse-mode-map (kbd "s-9") 'eyebrowse-switch-to-window-config-9)
    (define-key eyebrowse-mode-map (kbd "s-0") 'eyebrowse-switch-to-window-config-0)
    (eyebrowse-mode t)))


(use-package projectile
  :custom (projectile-enable-caching t)
  :bind
  (("C-c p" . projectile-command-map)
   ("s-t"   . projectile-find-file)
   ("s-p"   . projectile-switch-project))
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".node_modules")
  (add-to-list 'projectile-globally-ignored-directories "shadow-cljs")
  (add-to-list 'projectile-globally-ignored-directories ".shadow-cljs")
  (projectile-mode +1))


(use-package flycheck
  :init (global-flycheck-mode)
  :config (setq-default flycheck-disabled-checkers
                        (append flycheck-disabled-checkers
                                '(javascript-jshint json-jsonlist))))

(use-package flycheck-pos-tip
  :after flycheck
  :config
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))


(use-package flyspell
  :straight nil
  :hook (text-mode . flyspell-mode)
  :custom (ispell-program-name "aspell")
  :bind
  (("s-E" . save-word)
   ("s-d" . 'ispell-change-dictionary))
  :config
  (defun save-word ()
    "Saves word under point to current dict."
    (interactive)
    (let ((current-location (point))
          (word (flyspell-get-word)))
      (when (consp word)
        (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))


(use-package flyspell-correct
  :bind ("s-e" . flyspell-correct-wrapper)
  :custom (flyspell-define-abbrev))


(use-package minibuffer
  ;; use completion-at-point with selectrum/marginalia/prescient instead of company
  :demand t
  :straight nil
  :custom (tab-always-indent 'complete))


(use-package rainbow-mode
  :after css)


(use-package web-mode
  :mode "\\.php\\'"
  :mode "\\.html?\\'")


(use-package restclient
  :mode "\\.http\\’")


(use-package python
  :straight nil
  :custom (python-shell-interpreter "python3"))


(use-package arduino-cli-mode
  :mode "\\.ino\\'"
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t))


(use-package cider
  :config (evil-make-intercept-map cider--debug-mode-map 'normal)
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-use-content-types  t)
  (cider-save-file-on-load       t))


(use-package flycheck-clj-kondo)


(use-package clojure-mode
  :straight nil
  :config (require 'flycheck-clj-kondo))


;;; Elisp
(use-package elisp-mode
  :straight nil
  :config
  (define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer))


(use-package tex
  :straight auctex
  :ensure auctex
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  :custom
  (TeX-save-query          nil)
  (TeX-show-compilation    t)
  (reftex-plug-into-AUCTeX t))


(use-package nov
  :mode "\\.epub\\'"
  :custom
  (visual-fill-column-center-text t)
  (nov-text-width                 80))


(use-package hydra
  :demand t
  :bind
  (:map dired-mode-map
        ("ä" . hydra-window/body)
        ("SPC" . hydra-smartparens/body))
  :config
  (load "~/.emacs.d/hydras.el")
  (define-key evil-normal-state-map (kbd "ä")   'hydra-window/body)
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-smartparens/body)
  (define-key evil-visual-state-map (kbd "SPC") 'hydra-smartparens/body))


(use-package org
  :custom
  (org-src-fontify-natively           t)
  (org-src-tab-acts-natively          t)
  (org-hide-leading-stars             nil)
  (org-edit-src-content-indentation   0)
  (org-fontify-quote-and-verse-blocks t)
  (org-confirm-babel-evaluate         nil)
  (org-hide-emphasis-markers          t)
  (org-fontify-whole-heading-line     t)
  (org-startup-with-inline-images     t)

  (calendar-week-start-day            1)
  (calendar-date-style                'european)
  (calendar-date-display-form         '((if dayname (concat dayname ", ")) day " " monthname " " year))
  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-hook 'org-mode-hook (lambda ()
                             (setq paragraph-start "\\|[  ]*$"
                                   paragraph-separate "[  ]*$"))))


(use-package synosaurus
  :bind ("s-u" . 'synosaurus-choose-and-replace)
  :custom (synosaurus-choose-method 'selectrum-completing-read))


(use-package olivetti
  :custom (olivetti-set-width 100))


(use-package writegood-mode
  :after org)


(use-package eshell
  :demand t
  :straight nil
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-glob-case-insensitive t)
  (eshell-cmpl-ignore-case t)
  (eshell-banner-message "")
  :config
  (define-key global-map (kbd "M-t") 'eshell-new)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "<tab>")
                (lambda () (interactive) (pcomplete-std-complete)))))

  (defun eshell-new () ;; Make a new eshell buffer
    "Open eshell buffer relative to buffer or project (if applicable)"
    (interactive)
    (if (projectile-project-p)
        (projectile-run-eshell nil)
      (eshell t)))

  (defun fish-path (path max-len)
    "Return a potentially trimmed-down version of the directory PATH, replacing
  parent directories with their initial characters to try to get the character
  length of PATH (sans directory slashes) down to MAX-LEN.
  Source: https://www.emacswiki.org/emacs/EshellPrompt"
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (cl-reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len)
                  (cdr components))
        (setq str (concat str
                          (cond ((= 0 (length (car components))) "/")
                                ((= 1 (length (car components)))
                                 (concat (car components) "/"))
                                (t
                                 (if (string= "."
                                              (string (elt (car components) 0)))
                                     (concat (substring (car components) 0 2)
                                             "/")
                                   (string (elt (car components) 0) ?/)))))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  (defun fishy-eshell-prompt-function ()
    (concat (fish-path (eshell/pwd) 40)
            (if (= (user-uid) 0) " # " " $ ")))

  (setq eshell-prompt-function 'fishy-eshell-prompt-function))


(use-package magit
  :demand t
  :bind
  (("C-x C-g" . magit-status)
   ("M-f" . 'magit-find-file)))


(use-package gitignore-mode)


(use-package dockerfile-mode)


(use-package toml-mode)


(use-package yaml-mode)


(use-package dired
  :straight nil
  :bind ("C-x C-d" . 'dired)
  :custom
  (dired-dwim-target t)  ; big norton commander energy
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (delete-by-moving-to-trash t)
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired t
          insert-directory-program "/usr/local/bin/gls"
          dired-listing-switches "-aBhl --group-directories-first")))


(use-package autorevert
  :straight nil
  :defer 2
  :config
  (setq auto-revert-interval 1)
  (global-auto-revert-mode +1)
  (setq global-auto-revert-non-file-buffers t)
  (setq revert-without-query '(".*")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1e1e1e" "#dddddd" "#dddddd" "#dddddd" "#dddddd" "#dddddd" "#dddddd" "#dddddd"])
 '(custom-safe-themes
   '("25a53137220785d57df68eff4e948217f68bb91b6bbeb63dad6bacd1c3366b89" default))
 '(safe-local-variable-values
   '((cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here

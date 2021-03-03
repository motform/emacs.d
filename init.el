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
;;
;; Expects native-comp Emacs 28 on MacOS (Big Sur),
;; but should work on any other Unix.

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


(setq-default line-spacing 0.0) ; use patched fonts instead
(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-to-list 'default-frame-alist '(font . "PragmataPro Liga 1.4"))
(set-face-attribute 'default        nil :family "PragmataPro Liga 1.4" :height 130)
(set-face-attribute 'fixed-pitch    nil :family "PragmataPro Liga 1.4" :height 130)
(set-face-attribute 'variable-pitch nil :family "PragmataPro Liga 1.4" :height 130)

(setq default-frame-alist
      (append (list
               '(vertical-scroll-bars . nil)
               '(internal-border-width . 24)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0))))

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


(use-package stimmung
  :straight (stimmung :local-repo "/Users/lla/Projects/stimmung"))

;; NOTE
(use-package auto-dark-emacs
  :straight (auto-dark-emacs :type git :host github :repo "LionyxML/auto-dark-emacs")
  :demand t
  :custom
  (auto-dark-emacs/polling-interval-seconds 10) ; does this cause any discernible slowdown?
  (auto-dark-emacs/dark-theme 'stimmung)
  (auto-dark-emacs/light-theme 'mixtur))

(load-theme 'mixtur t)
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq window-divider-default-right-width 24)
(setq window-divider-default-places 'right-only)
(window-divider-mode 1)


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


(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode)
  :custom
  (c-basic-offset 4)
  :config
  (setq-default indent-tabs-mode nil)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode))


(use-package adaptive-wrap
  :hook ((prog-mode . adaptive-wrap-prefix-mode)
         (text-mode . adaptive-wrap-prefix-mode)))


(use-package selectrum
  :init (selectrum-mode +1)
  :custom
  (selectrum-max-window-height 20)
  (suggeest-key-bindings t)
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
  :config
  (evil-define-key 'normal evil-normal-state-map
    "s" 'ctrlf-forward-fuzzy
    "S" 'ctrlf-backward-fuzzy))


(use-package rg
  :init (rg-enable-menu)
  :bind (("s-s" . rg)
         (:map rg-mode-map ("S-n" . rg-menu))))


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


(use-package project
  :straight nil
  :bind
  (("s-t" . 'project-find-file)
   ("s-p"   . 'project-switch-project)))


(use-package flycheck
  :init (global-flycheck-mode)
  :config (setq-default flycheck-disabled-checkers
                        (append flycheck-disabled-checkers
                                '(javascript-jshint json-jsonlist))))

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
  :hook (tex-mode . reftex-mode)
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  :custom
  (TeX-save-query          nil)
  (reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
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
  (define-key evil-normal-state-map (kbd "å")   'hydra-roam/body)
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

  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (calendar-date-display-form         '((if dayname (concat dayname ", ")) day " " monthname " " year))

  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-hook 'org-mode-hook (lambda ()
                             (setq paragraph-start "\\|[  ]*$"
                                   paragraph-separate "[  ]*$"))))


(use-package synosaurus
  :bind ("s-u" . 'synosaurus-choose-and-replace)
  :custom (synosaurus-choose-method 'completing-read))


(use-package olivetti
  :custom (olivetti-set-width 100))


(use-package writegood-mode
  :after org)


(use-package org-roam
  :custom
  (org-roam-db-update-method 'immediate)
  (org-roam-directory "~/Documents/org_roam")
  (org-roam-index-file "~/Documents/org_roam/index.org")
  :bind (:map org-roam-mode-map
              (("C-c C-n C-r" . org-roam)
               ("C-c C-n C-f" . org-roam-find-file)
               ("C-c C-n C-g" . org-roam-graph))
              :map org-mode-map
              (("C-c C-n C-n" . org-roam-insert))
              (("C-c C-n C-m" . org-roam-insert-immediate))))


;; (use-package org-roam-bibtex
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode))


(use-package org-roam-server
  :custom
  (org-roam-server-host                          "127.0.0.1")
  (org-roam-server-port                          8080)
  (org-roam-server-authenticate                  nil)
  (org-roam-server-export-inline-images          t)
  (org-roam-server-serve-files                   nil)
  (org-roam-server-served-file-extensions        '("pdf" "mp4" "ogv"))
  (org-roam-server-network-poll                  t)
  (org-roam-server-network-arrows                nil)
  (org-roam-server-network-label-truncate        t)
  (org-roam-server-network-label-truncate-length 60)
  (org-roam-server-network-label-wrap-length     20))


(use-package org-ref)


(use-package org-appear
  :straight (org-appear :type git :host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))


(use-package dash-at-point
  :bind ("M-d" . dash-at-point))


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
    (if (project-current)
        (project-eshell)
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


(use-package makefile-runner
  :bind ("s-m" . makefile-runner))


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
   '("3815d96c3b0b08690858557b044db25fb1c88d88e1edf51f61395b8a890575c5" "396e8871440b2c8fafe3fa0ff7110e8bde6b403ccb3750c07a72f5c80a6316c3" "eca06538016bffd7dfbf9f47a67a8039a4c9a4e70b34136b441159dabd35e97d" "7bbe2dc40016f78f50ac4658a899c943533078a54021ab158084df081d31b982" "9ce06291e88508abe560fc75611d14da58a1d020ebfe11a5296dfae8bbd56634" "6f69dcb1ad05693a31efa4c76ab7516d8f4c5e9e41fedf9aa27698c58ea87b56" "210e21fbdd9e0b80bd3f47a2b1d887a58af881dc6daef26c52804a9ee879e6be" "4de30906920607f83f64fcbd70353b4f2f950e3056523b0dcc276ad442493341" "f12a599e3b808e1a799dae3f9f739c66a95f876a9f9b753037ba4cbf541cf4a7" "9da6a5dbbf584c92be4ca63ab6beaaac5022921c704560d20cb7eacb408dbdba" "5a15749dc26fde8b1eac89f9a3994e847a6afed27dcfacdf20e1dccd4950e050" "b85d7a3d030ef7e26b7f6f01ac75230258794e3c113d8dd58f7d14ff1b69709d" "33e9c3380c2d9b57025ebc736166b86109af012345cb007e3bae21f44ff09c5f" "e5c42359a7767839fb9dd09c467bd460e77b9836472112bbf3c97bd70fef0bc6" "1206198742e745d87d5fef57cb15be100c7a806ab90410e1ada625ab338b5083" "4a775c47f0f19af232d8ddcdcb888120180a1274a77ca37776a98b28ce73cc9b" "65bc53508798a384ab2b1257bbf5ca0c26ac71571d55ff5e317ef52bcc4d521d" "769a78a6bc60dd9d7ce589ede7637a479cdf95d7d034b03b7bb5acc5af3b33d5" "4743ce4635875e83c90f6530eab4e5eb537a32862ec4903de9a75878d7614a42" "3c3108cc9771de1d4421e9af0ed2dbc48f160c42d7ddc8e94f13ae0b301a5f2a" "aabb6c55ac37e3cab92403ed94fed7ba89f524f8622b65108f4dedac0190a240" "2af3ea2dbce65f45265bf6fa168921d7ba8abc1ede7da3c8bdc5d7c401510e79" "82a4ffacf3c10828e8940c467adc41efafdc8419afe5eec552f61bd43be8106e" "e2291b33e6a8dac235275412bfca279a9ca956e567cb37e9b151340ef149eb60" "1d8548a9c2460b0496279c6dcde3813beb88c3478fb9ace7cf369c1f9cf2115d" "acf57c83b818f81e0be64af6c1eae292c93af210c69c012a8edbf6c06ce7e134" "e83d4bfc428e8e4f2a9cff8fd9b45f1d94a7a4cb59c5f952c502d243ee90ddd5" "f3b07bf333c0c9a21f02eb29680ba5a4bff3ef248c206bab50617c358363cbba" "072c2820931157003be42c99d7742fb550551efb0c22c990b0335b845ec1ac88" "6e812c082efb392764123c58ba54cdfd1b09c153443f3dcf94684044eab7430f" "79d0f22827c9edd14e5337366d59a094f86817d7fd0d8e14296e4537eaa9dfc7" "eaa922bc1bf650b7043efeb97338cfd75a8c75619510933cd005cb7a102cb4cb" "66807181db0be36f721c4c10f9ef106fb431345136c53d7b01726a2e8bf22164" "0faafecd6827c2f42de1bad134cacca403e572ab767c1be6d76a7058e6eb4a28" "2c3df17b287aacaacd04ce51e4d46dc3ee2a07e06aaca3e001cf2ca74dd62bd4" "519fd8e869d672359a6ceb2f345a7aea0751be2329cbca372e20c5dacda6d327" "1a6b437f7eb737c924e24bb63c2a37d9a6ac15fa1827eb9cc99cd438e4c4c188" "73615e541c6ccd1f0c457f1a2324c0f00472349d57775eacf02b403548843801" "34a0003c758d9b76c647e9e8189a339874f13264465a19bbe1b5a1d35a835ed5" "a266137f450c08188eb65fbcb57f87300af73a6a061b1030455bd1b34f752a2f" "5cd9ee9ba3fc38dd83d73f06ed73a98fbb7b0c23fbb9c0875153fc88a1ebccd8" "730927d4742864223fa08522424f1524164c11bb83498336e4c69ef113211b4a" "0fb08f64882305154063642ad01a861bdab8f809de3e7b100b8d1a22662bbc3a" "6ab603be350bc629a7308c7be90cfec92e5d41170cfc3246313a24228f9b9bc0" "a9413bb90ba79c4d22047ddba02805139181cda58dac0ab2010062e6ee1c5c88" "3e2b2f4c16c8c483d93d1def8577c81cb8ae6d9fadc10289772efc44a38d21ae" "830eb5b0b3e3daee9073b7fa1744885c1f89d66918db985a867b47635a7de122" "3569581533b6a7a3c239807f1a6baec3e7a100a92dc4e3b7aeb8112a7d88da84" "c1b8e92d9a83f83a618de0c3ba0243a0991f0354520029ecc8296751b2b3ec97" "d50ce146f71d1c5a866612a490c8c8faf84bab0462f3c7d4c7fb39b295ed8c2b" "45349c0d06701bbc2f55f3ca5144b75d5b24aa6d90e726db45fbc1d86efce9f1" "6a474e80ec147d243a1d16bc73ae869d656da139438a289254de5d973e6e4333" "fdb0d86c3911fbdde64ddfbe2402884ea7690db36b81ba1358e8a52f72702b9b" "e6de3b35aa05a5dcd6730ace1de2397f4a5388ee6d6ac59736f34730602f6591" "aa9e8c4248394b400f1ccbe04261123728bb63576ff152abedb8538ef14ff812" "fe7820584ef35a85e9a508da4dcd8585e50eb423d0cd040a81d3e6cda4f6a3ec" "6f0763cee50f0d5db83b6237013dd110264fda58b89b5a3db5aaed570723cb32" "086d8fdf53d7d6ccdf03b3d751348be7dcd78b4af82d86fa4a45c4ca3ace78d9" "e1fbee1c7268b68796ac9f59eb70483ed74dab6bcb297937a55a7ab2f3ef095b" "6c70bc49575da6fbf104724fca19df08ef42383658ae38371847f10c6089ed14" "d0b49ee0d16d11fdf0cd0d93d8241018aa115b539aae6caacc6f04e7a9a9b71e" "deda26e8d66adbc42735fa165592fd7e2cf01511fe29bb3adb53c865e4059da1" "c2d7219bb060b8e032a4c1ba1849c78b2cd69f74eb9b27ded39d26597c3dd548" "42c9177d7fde0016845c8ae43d9a39679fd32728948438d6a412c3e1f474d87e" "15e5844fd1ed2806451865d4daa56bf5aeb2b0db9a5e81f869aab6515e7e5a5e" "c283cfacf88bdde080e7b495e7b5fe109becf64040d837aa1947deb89438f1a5" "def1e53c83b501ab2a7b5a5af273168d9aca9288be50481dbcd9ef0c3c5e5842" "b2f14cfb8d5e4a6db97623d392fd068d423d010a9dd566010190637dcac97d0a" "af996e6e2335cc38ac29781b91b8adf2fb2b5663ca8591fa9dd9469ce99a64db" "5a2dea80a4cb336f4eff1cc88c032f51f819d592cd6b6ead563bed19ba15b7a6" "fda662c8d1370d6073434600ac4de5baa8834be91b8787925f380a82d732bd63" "9e0334988b10435174d17ff49f3839075dce1ad7ee79e91078830025ed55ddc2" "715262b4f82fc794bc38da2c1e553ae7f654335a24001b916498dece5e0518b5" "bf89d724a22db3c9861b3d735f90cb26bf4679c3df4f0e67686e511d9c8b4c03" "70d44a98161bded4a92134977ca02363414af005db5b5f429db027cc946a6bfd" "676dbc42fd152461406f7e772ffd76c3e33d50d7be8768cad573c34297a2fc83" "28c5efb5853ac9dde500f1a2d9ec29d5db3420a8ee624eb2e7931f946134abe5" "4f969405482bdc9ef28d2ab4a7ff98a0b85ed74e83599edff0577344ac6c6baa" "d409eacf95c392294ccdaa5e1549bf58b065bfca04fde7352f875d56d399b3bb" "d1ef71e2b85c895036e98106d377a17ec25cb20ec7b926285600e509ef72d1fd" "70c8c0b30514af459c90bf6653bc5736c60f3d45e8f3d15824689bcc55308cfe" "2acf5a6136dbe12e156906e83c5d8f0bfe4dd714e203067e77d747485ed187a4" "e2f9e90adf6d6b8174fa68a9ec9e527e952d156a68cf65e6abe0e7f913922a06" "f877f98bf13a835a3e145a6ce2172a3d446c798f851ff9cd2b99f66b73d2ec1f" "d4512c65279604f8898ac0908170d4bcacf24becb8ce83ce237c22e583f83ac7" "177c98f1c1a56361259be3fa9dfd09294a80b5af591ca329d567d03278648b15" "59ba69365ec20cba016a07c01b9e1cc5112c9ea199357c13f192c5a01443d5a0" "1553261f443857ec70ee2e454a94a73d0b725a3d40348ebae1a832bb2e68503f" "f33e27d959168456099f912df22a6ea0a817adf6b2325279497fe00020e5847d" "9d0d71878b2f6ac672c3b4033059955de9e7c352df044c4e44f2ae91e7fcf043" "c0741145346ed76ced583086daaa3537c30eab7d57f7102903a7d731b47b38c8" "86fb7cffaf11f4578b91ba504faca3e297eee8d5519da5e998a3de7ed125732a" "95e31cd3b0a394a21dcb7b5f5732a61cfd774f29e073b0e913ef63396058bb79" "8e33b4e4c12ba64478801f663a6d576870da52292615840139ba412607a67315" default))
 '(helm-minibuffer-history-key "M-p")
 '(safe-local-variable-values
   '((reftex-default-bibliography . "/Users/lla/Projects/IDM-19/design-based-research/essay/bibliography.bib")
     (reftex-default-bibliography . "bibliography.bib")
     (define-key evil-normal-state-map
       (kbd "ö")
       (cider-read-and-eval "(motform.portfolio.core/build-it!)"))
     (cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here

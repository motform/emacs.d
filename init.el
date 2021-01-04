;;; init.el -*- lexical-binding: t; -*-

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
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please


;;; MacOS
(when IS-MAC
  (setq mac-right-command-modifier 'meta
        mac-command-modifier       'super))


;;; Long Lines should wrap naturally
(setq bidi-paragraph-direction 'left-to-right)
(when (version<= "27.1" emacs-version)
  (setq bidi-inhibit-bpa t)
  (global-so-long-mode 1))


;;; Backups
(save-place-mode 1)
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
(setq inhibit-startup-screen t)
;; (setf initial-buffer-choice "~/.emacs.d/numogram.txt")


;; TODO move to straight
;;; Theme
(load "~/Projects/stimmung/stimmung-theme.el")
(add-to-list 'custom-theme-load-path "~/Projects/stimmung/")
(load-theme 'stimmung t)


;;; GUI
(setq ns-use-proxy-icon    nil
      frame-title-format   nil
      visible-bell         nil
      ring-bell-function   'ignore
      suggest-key-bindings nil)

(fringe-mode 10)      ; set a 10 unit fringe, for flyspell and such
(blink-cursor-mode 0) ; No blinking cursor
(global-hl-line-mode) ; Global line hilight
(global-visual-line-mode 1)


;;; Scrath buffer
(setq initial-scratch-message ";; Nanospasm")


;;; Typography, NOTE that the default font is set in `early-init.el'
(setq line-spacing 0.1
      prettify-symbols-unprettify-at-point 'right-edge)
(add-to-list 'default-frame-alist '(font . "PragmataPro Liga"))
(set-face-attribute 'default nil :family "PragmataPro Liga" :height 130)
(set-face-attribute 'fixed-pitch    nil :family "PragmataPro Liga" :height 130)
(set-face-attribute 'variable-pitch nil :family "PragmataPro Liga" :height 130)
;; (mac-auto-operator-composition-mode t) ; if using the macport
(global-prettify-symbols-mode t)


;;; Misc
(fset 'yes-or-no-p 'y-or-n-p) ; Replace yes/no prompts with y/n
(setq save-interprogram-paste-before-kill t
      apropos-do-all t
      require-final-newline t
      load-prefer-newer t)


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
(use-package git)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))


;;; Modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init (unless after-init-time (setq-default mode-line-format nil)) ; prevent flash of unstyled modeline at startup
  :custom
  (column-number-mode               t)
  (doom-modeline-buffer-encoding    nil)
  (doom-modeline-icon               nil)
  (doom-modeline-enable-word-count  nil)
  (doom-modeline-github             nil)
  (doom-modeline-mu4e               nil)
  (doom-modeline-persp-name         nil)
  (doom-modeline-minor-modes        nil)
  (doom-modeline-major-mode-icon    nil))


;;; Evil
(use-package evil
  :init
  (setq evil-want-keybinding nil)
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
  :after evil
  :config (evil-commentary-mode))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))


;;; Undo
(use-package undo-fu
  :defer t
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :after undo-fu
  :config (global-undo-fu-session-mode))


;;; Smartparens
(use-package smartparens
  :defer t)

(use-package smartparens-config
  :straight nil
  :defer t
  :after smartparens
  :config
  (show-paren-mode 1)
  (provide 'smartparens-setup)
  (progn (show-smartparens-global-mode t))
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (require 'smartparens-clojure)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  :custom
  (sp-show-pair-from-inside t))

(use-package evil-smartparens
  :defer t
  :config (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package highlight-parentheses
  :config (global-highlight-parentheses-mode t)
  :custom (highlight-parentheses-colors '("red")))


;;; Indentation
(use-package aggressive-indent
  :defer t
  :config
  (setq-default indent-tabs-mode nil)
  (add-hook 'prog-mode-hook #'aggressive-indent-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'cider-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'python-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'elpy-mode))

(use-package adaptive-wrap
  :defer t
  :config
  (add-hook 'prog-mode-hook #'adaptive-wrap-prefix-mode)
  (add-hook 'text-mode-hook #'adaptive-wrap-prefix-mode))


;;; avy
(use-package avy
  :defer t
  :custom
  (avy-background t)
  (avy-all-windows nil)
  (avy-keys '(?a ?r ?s ?t ?d ?h ?n ?e ?e ?i ?o))
  :config
  (evil-define-key 'normal global-map (kbd "s") #'evil-avy-goto-char-timer)
  (evil-define-key 'normal evil-smartparens-mode-map (kbd "s") #'evil-avy-goto-char-timer))


;;; ivy-swiper-counsel
(use-package counsel
  :defer t
  :bind
  (("s-y" . counsel-yank-pop)
   ("s-u" . counsel-unicode-char)
   ("s-g" . counsel-org-goto-all)
   ("M-x" . counsel-M-x)
   ("s-d" . counsel-dired-jump)
   ("C-h a" . counsel-apropos)
   ("C-x C-f" . counsel-find-file)
   :map ivy-minibuffer-map
   ("s-y" . ivy-next-line)))

(use-package counsel-projectile
  :after counsel)

(use-package ivy
  :defer t
  :diminish (ivy-mode . "")
  :custom
  (ivy-height 20) ;; number of result lines to display
  (ivy-use-virtual-buffers t)
  (ivy-count-format "") ;; does not count candidates
  (ivy-initial-inputs-alist nil) ;; no regexp by default
  (ivy-re-builders-alist '((t   . ivy--regex-ignore-order)))
  :config
  (ivy-mode 1)
  (global-set-key (kbd "C-s") 'swiper-isearch)
  (global-set-key (kbd "C-x b") 'ivy--buffer-list))


;;; eyebrowse
(use-package eyebrowse
  :defer 1
  :init (global-unset-key (kbd "C-c C-w"))
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
    (eyebrowse-mode t)
    (setq eyebrowse-mode-line-style 'hide)
    (setq eyebrowse-new-workspace t)))


;;; Projectile
(use-package projectile
  :defer t
  :custom
  (projectile-enable-caching t)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "s-t") 'counsel-projectile)
  (define-key projectile-mode-map (kbd "s-p") 'counsel-projectile-switch-project)
  (define-key projectile-mode-map (kbd "s-s") 'counsel-projectile-rg)
  (define-key projectile-mode-map (kbd "s-a") 'ivy-switch-buffer)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (add-to-list 'projectile-globally-ignored-directories ".node_modules")
  (add-to-list 'projectile-globally-ignored-directories "shadow-cljs")
  (add-to-list 'projectile-globally-ignored-directories ".shadow-cljs")
  (projectile-mode +1))



;;; Flycheck
(use-package flycheck
  :defer t
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


;;; Flyspell
(use-package flyspell
  :defer t
  :config
  (setenv "DICPATH" (concat (getenv "HOME") "/Library/Spelling")) ; Set $DICPATH to "$HOME/Library/Spelling" for hunspell.
  (setq
   ispell-dictionary "british"
   ispell-program-name "hunspell"
   flyspell-issue-message-flag nil
   ispell-hunspell-dict-paths-alist
   '(("en_GB" "~/Library/Spelling/en_GB.aff")
     ("en_US" "~/Library/Spelling/en_US.aff")
     ("american" "~/Library/Spelling/en_US.aff")
     ("british" "~/Library/Spelling/en_US.aff")
     ("swedish" "~/Library/Spelling/sv_SE.aff")
     ("sv_SE" "~/Library/Spelling/sv_SE.aff")))

  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

  (add-to-list 'ispell-dictionary-alist '("british" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en_GB") nil utf-8))
  (add-to-list 'ispell-dictionary-alist '("swedish" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "sv_SE") nil utf-8)))

(use-package flyspell-correct-ivy
  :after flyspell
  :bind ("s-e" . flyspell-correct-wrapper)
  :init (setq flyspell-correct-interface #'flyspell-correct-ivy))

(use-package writegood-mode
  :defer t)


;;; Company
(use-package company
  :defer t
  :init (company-tng-mode)
  :config
  (define-key evil-insert-state-map (kbd "TAB") 'company-manual-begin)
  (global-set-key [backtab] 'tab-indent-or-complete)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay            nil
        company-dabbrev-downcase      nil
        company-dabbrev-other-buffers t
        company-frontends             '(company-tng-frontend
                                        company-pseudo-tooltip-frontend company-echo-metadata-frontend)))



;;; Web
(use-package rainbow-mode
  :defer t)

(use-package web-mode
  :defer t
  :mode "\\.php\\'"
  :mode "\\.html?\\'")

(use-package restclient
  :defer t
  :mode "\\.http\\’")


;;; Python
(use-package elpy
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config
  (define-key elpy-mode-map (kbd "C-c C-k") 'elpy-shell-send-region-or-buffer)
  (define-key elpy-mode-map (kbd "C-c C-c") 'elpy-shell-send-statement-and-step)
  :custom
  (elpy-rpc-virtualenv-path 'global)
  (python-shell-interpreter "python3")
  (elpy-rpc-python-command  "python3"))


;;; Clojure
(use-package cider
  :defer t
  :config (evil-make-intercept-map cider--debug-mode-map 'normal)
  :custom
  (cider-repl-display-help-banner nil)
  (cider-repl-use-content-types t)
  (cider-save-file-on-load t))

(use-package clj-refactor
  :defer t
  :config
  (defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

(use-package flycheck-clj-kondo
  :defer t)

;; ????
(use-package clojure-mode
  :straight nil
  :config
  (require 'flycheck-clj-kondo))


;;; Elisp
(define-key emacs-lisp-mode-map (kbd "C-c C-k") 'eval-buffer)


;;; TeX
(use-package tex
  :defer t
  :straight auctex
  :ensure auctex
  :config
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
  (setq-default TeX-engine 'xetex)
  (setq-default TeX-PDF-mode t)
  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
  :custom
  (TeX-save-query nil)
  (TeX-show-compilation t)
  (reftex-plug-into-AUCTeX t))


;;; ebooks
(use-package nov
  :mode "\\.epub\\'"
  :config
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  :custom
  (nov-text-width most-positive-fixnum)
  (visual-fill-column-center-text t)
  (nov-text-width 80))


;;; Hydra
(use-package hydra
  :config

  ;; Window Management
  (defhydra hydra-window ()
    ("p" counsel-projectile-switch-project)
    ("h" windmove-left)
    ("j" windmove-down)
    ("k" windmove-up)
    ("l" windmove-right)
    ("=" balance-windows)
    ("a" ivy-switch-buffer)
    ("t" counsel-projectile)
    ("f" counsel-find-file)
    ("F" follow-mode)
    ("v" (lambda ()
           (interactive)
           (split-window-right)
           (windmove-right)))
    ("x" (lambda ()
           (interactive)
           (split-window-below)
           (windmove-down)))
    ("s" save-buffer)
    ("d" delete-window)
    ("D" delete-other-windows))
  (define-key evil-normal-state-map (kbd "ä") 'hydra-window/body)

  ;; Smartparens
  (defhydra hydra-smartparens (:hint nil)
    "
    Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
   ------------------------------------------------------------------------------------------------------------------------
    [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
    [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
    [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
    [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
    ;; Moving
    ("a" sp-beginning-of-sexp)
    ("e" sp-end-of-sexp)
    ("f" sp-forward-sexp)
    ("b" sp-backward-sexp)
    ("n" sp-down-sexp)
    ("N" sp-backward-down-sexp)
    ("p" sp-up-sexp)
    ("P" sp-backward-up-sexp)

    ;; Slurping & barfing
    ("h" sp-backward-slurp-sexp)
    ("H" sp-backward-barf-sexp)
    ("l" sp-forward-slurp-sexp)
    ("L" sp-forward-barf-sexp)

    ;; Wrapping
    ("R" sp-rewrap-sexp)
    ("u" sp-unwrap-sexp)
    ("U" sp-backward-unwrap-sexp)
    ("(" sp-wrap-round)
    ("{" sp-wrap-curly)
    ("[" sp-wrap-square)

    ;; Sexp juggling
    ("S" sp-split-sexp)
    ("s" sp-splice-sexp)
    ("r" sp-raise-sexp)
    ("j" sp-join-sexp)
    ("t" sp-transpose-sexp)
    ("A" sp-absorb-sexp)
    ("E" sp-emit-sexp)
    ("o" sp-convolute-sexp)

    ;; Destructive editing
    ("c" sp-change-inner :exit t)
    ("C" sp-change-enclosing :exit t)
    ("k" sp-kill-sexp)
    ("K" sp-backward-kill-sexp)
    ("w" sp-copy-sexp)

    ("q" nil)
    ("g" nil))
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-smartparens/body)
  (define-key evil-visual-state-map (kbd "SPC") 'hydra-smartparens/body))


;;; Org
(use-package org
  :defer t
  :custom
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-hide-leading-stars nil)
  (org-edit-src-content-indentation 0)
  (org-fontify-quote-and-verse-blocks t)
  (org-confirm-babel-evaluate nil)
  (org-hide-emphasis-markers t)
  (org-fontify-whole-heading-line t)
  (org-startup-with-inline-images t)
  (calendar-week-start-day 1)
  (calendar-date-style 'european)
  (calendar-date-display-form '((if dayname (concat dayname ", ")) day " " monthname " " year))

  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . org-pdfview-open))
  (add-hook 'org-mode-hook (lambda ()
                             (setq paragraph-start "\\|[  ]*$"
                                   paragraph-separate "[  ]*$"))))
(use-package typo
  :defer t
  :config
  (typo-global-mode 1)
  (add-hook 'text-mode-hook 'typo-mode))


;;; Eshell
(use-package eshell
  :straight nil
  :custom
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-glob-case-insensitive t)
  (eshell-cmpl-ignore-case t)
  (eshell-banner-message "")
  :config
  (define-key global-map (kbd "M-s") 'eshell-new)
  (define-key global-map (kbd "M-q") 'counsel-esh-history)

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


;;; Git
(use-package magit
  :defer t)

(use-package forge
  :defer t)

(use-package transient
  :defer t)

(use-package evil-magit
  :after magit)

(use-package diff-hl
  :defer t
  :config
  (global-diff-hl-mode)
  (diff-hl-margin-mode t)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))


;;; Dired
(use-package dired
  :straight nil
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

;;; init.el ends here

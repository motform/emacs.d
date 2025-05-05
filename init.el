;;; init.el --- For fun and doubtful ergonomics -*- lexical-binding: t; -*-

;; Copyright © 2019

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/emacs.d
;; Package-Requires: ((emacs "29.1"))
;; Version: Perpetual Alpha
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
(setenv "PATH" (concat (getenv "PATH")
                       ":/usr/local/bin/"
                       ":~/.pyenv/shims"
                       ":/opt/homebrew/bin/"
                       ":~/.local/bin"))
(add-to-list 'exec-path "/usr/local/bin/")
(add-to-list 'exec-path "/Users/lovelagerkvist/.local/bin")
(add-to-list 'exec-path "/Users/lovelagerkvist/.pyenv/shims/")
(add-to-list 'exec-path "/opt/homebrew/bin/")

;;; Performance
(setq inhibit-compacting-font-caches t) ; PP is large font, this might help performance?
(setq native-comp-deferred-compilation-deny-list '()) ; emacs complains loudly if this is not set
(setq native-comp-async-report-warnings-errors nil)


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

;;; Scroll bar
(scroll-bar-mode -1)


;; Perf
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(setq gc-cons-threshold        (* 1024 1024 1024)
      gcmh-high-cons-threshold (* 1024 1024 1024)
      ;; idle-update-delay        1.0
      ;; gcmh-idle-delay-factor   20
      ;; jit-lock-defer-time      0.05
      read-process-output-max (* 1024 1024))


;; Use two space indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)


(setq create-lockfiles      nil
      make-backup-files     nil
      version-control       t
      backup-by-copying     nil
      byte-compile-warnings nil
      delete-old-versions   t

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

(fringe-mode 20)      ; set a 10 unit fringe, for flyspell and such
(blink-cursor-mode 0) ; No blinking cursor
(global-hl-line-mode) ; Global line hilight
(global-visual-line-mode 1)

(setq-default line-spacing 1) ; use patched fonts instead
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defvar font-mono "MD IO 1.2")
(defvar font-sans "MD System")

(add-to-list 'default-frame-alist  '(font . font-mono))
(set-face-attribute 'default        nil :family font-mono :height 120 :weight 'medium)
(set-face-attribute 'fixed-pitch    nil :family font-mono :height 120 :weight 'medium)
(set-face-attribute 'variable-pitch nil :family font-mono :height 120 :weight 'medium)

(setq frame-resize-pixelwise t
      default-frame-alist    (append (list
                                      '(vertical-scroll-bars . 0)
                                      '(internal-border-width . 0)
                                      '(right-fringe   . 0)
                                      '(tool-bar-lines . 0))))

;;; Misc
(fset 'yes-or-no-p 'y-or-n-p) ; Replace yes/no prompts with y/n
(setq save-interprogram-paste-before-kill  t
      help-window-select      t
      require-final-newline   t
      load-prefer-newer       t
      initial-scratch-message "")


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
  :custom
  (stimmung-themes-constant 'none)
  (stimmung-themes-type 'none :italic? t)
  (stimmung-themes-comment 'background :italic? nil)
  :config   (stimmung-themes-load-light))

(use-package exec-path-from-shell
  :demand t
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package hide-mode-line
  :demand t
  :hook (eshell-mode      . hide-mode-line-mode)
  :hook (treemacs-mode    . hide-mode-line-mode)
  :hook (rg-mode          . hide-mode-line-mode)
  :hook (magit-mode       . hide-mode-line-mode)
  :hook (nodejs-repl-mode . hide-mode-line-mode)
  :custom
  (column-number-mode t)
  (mode-line-percent-position nil)
  :config
  (setq-default mode-line-format (delq 'mode-line-modes mode-line-format))
  (advice-add 'vc-git-mode-line-string :override (lambda (file) "")))


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

  (evil-define-key 'insert 'global
    (kbd "C-a") 'move-beginning-of-line
    (kbd "C-e") 'move-end-of-line
    (kbd "C-p") 'previous-line
    (kbd "C-n") 'next-line
    (kbd "C-k") 'kill-line)

  (evil-define-key 'normal 'global
    (kbd "C-i") 'goto-last-change-reverse
    (kbd "C-o") 'goto-last-change
    (kbd "M-u") 'universal-argument
    "m" 'universal-argument
    "q" 'kmacro-start-macro-or-insert-counter
    "Q" 'kmacro-end-or-call-macro)

  (global-set-key (kbd "M-v") 'clipboard-yank))


(use-package evil-commentary
  :demand t
  :after  evil
  :config (evil-commentary-mode))


(use-package evil-collection
  :demand t
  :after  evil
  :config (evil-collection-init))


(use-feature pixel-scroll-precision-mode
  :hook (prog-mode . pixel-scroll-precision-mode))


(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       bold)
          ("FUTURE"     bold)
          ("FIXME"      bold)
          ("PERF"      bold)
          ("FIX"        bold)
          ("NOTE"       bold)
          ("HACK"       bold)
          ("XXX"        bold)
          ("REVIEW"     bold)
          ("NOTE"       bold)
          ("DEPRECATED" bold))))


(use-package mini-frame
  :demand t
  :hook (after-init . mini-frame-mode)
  :config
  (custom-set-variables
   '(mini-frame-show-parameters
	   '((top . 200)
	     (width . 0.55)
	     (left . 0.5)))))


(use-package solaire-mode
  :demand t
  :init (solaire-global-mode +1))


(use-package treemacs
  :defer 2
  :bind
  ((:map treemacs-mode-map
         ("M-p" . 'execute-extended-command)))
  :custom
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  ;; (treemacs-no-png-images t)
  (treemacs-indentation 2)
  (treemacs-space-between-root-nodes nil)
  (treemacs-width 50)
  :config
  (treemacs-resize-icons 10)
  (evil-define-key 'normal treemacs-mode-map
    "r" 'treemacs-rename-file
    "x" 'treemacs-delete-file
    "a" 'treemacs-create-file
    "d" 'treemacs-create-dir
    "m" 'treemacs-move-file
    "q" 'treemacs-set-width
    "c" 'treemacs-copy-file
    "y" 'treemacs-copy-file
    "b" 'treemacs-bookmark)
  
  (evil-define-key 'normal 'global
    (kbd "C-ö") 'treemacs-select-window
    (kbd "M-ö") 'treemacs)
  (push '(treemacs-window-background-face . solaire-default-face) solaire-mode-remap-alist)
  (push '(treemacs-hl-line-face . solaire-hl-line-face) solaire-mode-remap-alist))


(use-package apheleia
  :demand t
  :init   (apheleia-global-mode +1)
  :config
  (add-to-list 'apheleia-formatters
               '(cljstyle "cljstyle" "pipe"))
  (add-to-list 'apheleia-mode-alist '(clojure-mode . cljstyle))
  (add-to-list 'apheleia-mode-alist '(clojure-ts-mode . cljstyle))

  (add-to-list 'apheleia-formatters
               '(swiftformat "swiftformat" "pipe"))
  (add-to-list 'apheleia-mode-alist '(swift-mode . swiftformat))

  (setf (alist-get 'eslint-fix apheleia-formatters)
        '("eslint" "--fix" "--stdin" "--stdin-filename" filepath))

  ;; Associate eslint-fix with JS/TS modes
  (dolist (mode '(js-mode js2-mode js-ts-mode typescript-mode typescript-tsx-mode web-mode))
    (setf (alist-get mode apheleia-mode-alist) 'eslint-fix))

  
  (add-to-list 'apheleia-formatters
               '(prettier-json "prettier" "--parser" "json" "--tab-width" "4" "--print-width" "100"))
  (add-to-list 'apheleia-mode-alist '(json-mode . prettier-json))
  (add-to-list 'apheleia-mode-alist '(json-ts-mode . prettier-json))
  
  (setf (alist-get 'python-mode apheleia-mode-alist) 'black)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'black)
  :custom
  (css-indent-offset 2)
  (js-indent-level   2))


(use-package markdown-mode
  :demand t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("M-n" . start-project-eshell)
              ("M-p" . 'execute-extended-command))
  :custom (markdown-list-item-bullets '("•" "•" "•" "•" "•" "•")))


(use-package yasnippet
  :demand t
  :after markdown-mode
  :init (yas-global-mode 1)
  :bind (:map yas-minor-mode-map  ("C-ö" . 'yas-expand))
  :config
  (add-hook 'tsx-ts-mode-hook
            (lambda ()
              (setq-local yas-extra-modes '(typescript-tsx-mode)))))


(use-package lsp-bridge
  :demand t
  :after yasnippet
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
                         :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                         :build (:not compile))
  :init (global-lsp-bridge-mode)
  :bind
  (:map acm-mode-map
        ("<ret>" . 'acm-complete)
        ("<tab>" . nil))
  :custom
  (acm-enable-icon nil)
  (acm-enable-copilot nil)
  (lsp-bridge-enable-inlay-hint nil)
  (lsp-bridge-enable-auto-format-code nil)
  (lsp-bridge-python-lsp-server "pyright")
  ;; (lsp-bridge-python-multi-lsp-server "pyright_ruff")
  :config
  (eval-after-load 'acm
    (define-key acm-mode-map (kbd "<tab>") nil))

  (eval-after-load 'lsp-bridge
    (define-key lsp-bridge-mode-map (kbd "<tab>") nil))

  (setq mode-line-misc-info (cdr mode-line-misc-info))
  (defun lsp-bridge-find-def-other-window-and-balance ()
    "Make a horizontal windows split and move there."
    (interactive)
    (lsp-bridge-find-def-other-window)
    (balance-windows))

  ;; the above but in evil-define-key form
  (evil-define-key 'normal lsp-bridge-mode-map
    (kbd "ä")     'lsp-bridge-popup-documentation
    (kbd "K")     'lsp-bridge-popup-documentation
    (kbd "C-e")   'lsp-bridge-diagnostic-jump-next
    (kbd "C-n")   'lsp-bridge-diagnostic-jump-prev
    (kbd "C-f")   'lsp-bridge-find-references
    (kbd "M-e")   'lsp-bridge-find-def
    (kbd "g d")   'lsp-bridge-find-def
    (kbd "M-E")   'lsp-bridge-find-def-other-window-and-balance
    (kbd "M-E")   'lsp-bridge-find-def-other-window-and-balance
    (kbd "M-c")   'lsp-bridge-code-action
    (kbd "M-C-e") 'lsp-bridge-find-type-def
    (kbd "M-r")   'lsp-bridge-rename)

  (evil-set-initial-state 'lsp-bridge-ref-mode 'emacs)


  (defun local/lsp-bridge-get-single-lang-server-by-project (project-path filepath)
    (let* ((json-object-type 'plist)
           (custom-dir (expand-file-name ".cache/lsp-bridge/pyright" user-emacs-directory))
           (custom-config (expand-file-name "pyright.json" custom-dir))
           (default-config (json-read-file (expand-file-name "~/Developer/lsp-bridge/langserver/pyright.json")))
           (settings (plist-get default-config :settings)))
      (plist-put settings :pythonPath (executable-find "python"))
      (make-directory (file-name-directory custom-config) t)
      (with-temp-file custom-config
        (insert (json-encode default-config)))
      custom-config))

  (add-hook 'python-mode-hook
            (lambda ()
              (setq-local lsp-bridge-get-single-lang-server-by-project
                          'local/lsp-bridge-get-single-lang-server-by-project)))

  (add-hook 'pyvenv-post-activate-hooks
            (lambda ()
              (lsp-bridge-restart-process))))


(use-feature flymake
  :config
  (evil-define-key 'normal flymake-mode-map
    "C-e" 'flymake-goto-next-error
    "C-M-e" 'flymake-show-buffer-diagnostics))


(use-feature flyspell
  :custom (ispell-program-name "aspell")
  :config (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell))


(use-package flyspell-correct
  :bind   ("M-e" . flyspell-correct-wrapper)
  :custom (flyspell-define-abbrev))



(use-feature align
  :demand t
  :bind (("M-l" . align-regexp)))


(use-package smartparens
  :demand t
  :init   (smartparens-global-mode 1)
  :config
  (defun sp-wrap-double-quote ()
    "Wrap the next expression with double quotes."
    (interactive)
    (sp-wrap-with-pair "\""))

  (defun sp-wrap-angle ()
    "Wrap the next expression with angle brackets."
    (interactive)
    (sp-wrap-with-pair "<"))
  
  (defun sp-wrap-backtick ()
    "Wrap the next expression with backticks."
    (interactive)
    (sp-wrap-with-pair "`")))


(use-feature smartparens-config
  :demand t
  :after  smartparens
  :custom (sp-show-pair-from-inside t)
  :config
  (provide 'smartparens-setup)
  (require 'smartparens-clojure)
  (progn (show-smartparens-global-mode t))
  (add-hook 'clojure-mode 'turn-on-smartparens-strict-mode)
  (add-hook 'elisp-mode 'turn-on-smartparens-strict-mode)
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil))


(use-feature show-paren
  :init (show-paren-mode 1))


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
  (enable-recursive-minibuffers    nil)
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


(customize-set-variable 'treesit-font-lock-level 4)
(use-feature tree-sitter
  :custom
  (major-mode-remap-alist
   '((yaml-mode          . yaml-ts-mode)
     (bash-mode          . bash-ts-mode)
     (js2-mode           . js-ts-mode)
     (html-mode          . html-ts-mode)
     (js-mode            . js-ts-mode)
     (typescript-mode    . typescript-ts-mode)
     (json-mode          . json-ts-mode)
     (css-mode           . css-ts-mode)
     (clojure-mode       . clojure-ts-mode)
     (python-mode        . python-ts-mode)))

  (treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (clojure "https://github.com/sogaiu/tree-sitter-clojure")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (nix "https://github.com/nix-community/tree-sitter-nix")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     (graphql "https://github.com/bkegley/tree-sitter-graphql"))))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\Dockerfile\\'" . dockerfile-ts-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

;; ;;; .tsx + css.modules utils

(defvar css-ish-regex "\\(.css$\\|.scss$\\)")

(defun file-window (file)
  (get-buffer-window (get-buffer file)))

(defun first-file-matching-regex (regex)
  (car (directory-files default-directory nil regex)))

(defun open-file-to-right (file)
  (split-window-right)
  (other-window 1) 
  (find-file file)
  (balance-windows))

(defun open-index-in-dir ()
  (interactive)
  (if-let ((file (first-file-matching-regex "\\(^index.tsx$\\|^index.jsx$\\)")))
      (let* ((buf (get-buffer file))
             (window (get-buffer-window buf)))
        (if (and buf window) (select-window window)
          (select-window window)
          (open-file-to-right file)))
    (message "Index file not found in the directory")))

(defun open-css-in-dir ()
  (interactive)
  (if-let ((file (first-file-matching-regex css-ish-regex)))
      (let* ((buf (get-buffer file))
             (window (get-buffer-window buf)))
        (if (and buf window) (select-window window)
          (open-file-to-right file)))
    (let ((file-type (completing-read "No css or scss file found. Create ('css' or 'scss'): " '("css" "scss"))))
      (if (member file-type '("css" "scss"))
          (open-file-to-right
           (concat (file-name-as-directory default-directory) 
                   (file-name-nondirectory (directory-file-name default-directory))
                   ".module."
                   file-type))
        (message "Invalid file type")))))

(defun open-complementary-file ()
  (interactive)
  (if (string-match css-ish-regex (buffer-file-name))
      (open-index-in-dir)
    (open-css-in-dir)))

(use-package npm-mode
  :hook (typescript-ts-mode  . npm-mode)
  :hook (tsx-ts-mode . npm-mode)
  :hook (js-ts-mode . npm-mode)
  :bind (("M-o" . 'open-complementary-file))
  :config
  (evil-define-key 'normal 'npm-mode-keymap
    "ö" 'npm-mode-npm-run))


(use-package nodejs-repl)


(use-package ansi-color
  :after npm-mode
  :hook (compilation-filter . ansi-color-compilation-filter))


(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :demand t
  :hook   (prog-mode . copilot-mode)
  :custom (copilot-indent-offset-warning-disable t)
  :bind
  (:map copilot-mode-map
        ("<tab>" . 'copilot-accept-completion)
        ("C-f" . 'copilot-accept-completion)
        ("C-TAB" . 'copilot-accept-completion-by-word)
        ("C-<tab>" . 'copilot-accept-completion-by-word)))


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

(setq kill-transform-function
      (lambda (string)
        (and (not (string-blank-p string))
             string)))

(defun split-right-and-focus ()
  "Make a horizontal windows split and move there."
  (interactive)
  (split-window-right)
  (balance-windows)
  (windmove-right))

(defun split-down-and-focus ()
  "Make a vertical windows split and move there."
  (interactive)
  (split-window-below)
  (balance-windows)
  (windmove-down))

(defun kill-this-split-or-buffer ()
  "Kill split if active, else kill buffer."
  (interactive)
  (if (one-window-p) (kill-buffer)
	  (delete-window))
  (balance-windows))


(use-feature windmove
  :config
  (evil-define-key 'normal 'global
    (kbd "C-w") 'delete-window
    (kbd "M-k") 'kill-buffer)
  :bind (("C-l"   . 'windmove-right)
		     ("C-h"   . 'windmove-left)
		     ("C-k"   . 'windmove-up)
		     ("C-j"   . 'windmove-down)
		     ("C-w"   . 'delete-window)
		     ("M-w"   . 'kill-this-split-or-buffer)
		     ("M-k"   . 'kill-buffer)
		     ("C-ä"   . 'split-right-and-focus)
		     ("C-å"   . 'split-down-and-focus)))


(use-package perspective
  :defer  1 
  :custom
  (persp-suppress-no-prefix-key-warning t)
  (persp-sort 'created)
  :config
  (global-set-key (kbd "M-1") (kbd "M-u 1 M-S-p"))
  (global-set-key (kbd "M-2") (kbd "M-u 2 M-S-p"))
  (global-set-key (kbd "M-3") (kbd "M-u 3 M-S-p"))
  (global-set-key (kbd "M-4") (kbd "M-u 4 M-S-p"))
  :bind (:map persp-mode-map
              ("M-S-p" . persp-switch-by-number)
              ("M-C-p" . persp-switch)
              ("M-C-r" . persp-rename)
              ("M-C-k" . persp-kill))
  :init   (persp-mode))


(use-feature minibuffer
  ;; use completion-at-point with selectrum+prescient
  :custom (tab-always-indent 'complete))


(use-package consult
  :demand t
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config   
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source)
  (consult-customize consult-completion-in-region ;; evil-integration
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
  :config
  (define-key evil-normal-state-map (kbd "s") 'ctrlf-forward-fuzzy)
  (define-key evil-normal-state-map (kbd "S") #'search-everything-in-project)
  :bind (("C-s" . 'ctrlf-forward-fuzzy)))


(use-package consult-project-extra
  :straight t
  :bind (("M-t" . 'consult-project-extra-find)))

(use-package corfu
  :init
  (corfu-popupinfo-mode)
  (corfu-echo-mode)
  :straight (:files (:defaults "extensions/*"))
  :demand t
  :config

  ;; Corfu-history
  (corfu-history-mode 1)
  (savehist-mode 1)
  (add-to-list 'savehist-additional-variables 'corfu-history)
  (add-hook 'nodejs-repl-mode 'corfu-mode)

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

  :custom
  (completion-cycle-threshold 1)
  (tab-always-indent 'complete)
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-min-width 40)
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (completion-styles '(orderless-fast))
  (corfu-separator ?\s)
  (corfu-scroll-margin 5)
  (corfu-popupinfo-delay nil))


(defun copy-path ()
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


(use-package aidermacs
  :demand t
  :config
  (define-key evil-normal-state-map (kbd "å") 'aidermacs-transient-menu)
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-auto-accept-architect nil)
  (aidermacs-default-model "sonnet")
  (aidermacs-show-diff-after-change nil)
  (aidermacs-weak-model "haiku"))


(use-feature python
  :custom (python-shell-interpreter "python3"))


(use-package flymake-ruff
  :hook (python-mode . flymake-ruff-load))


(use-package arduino-cli-mode
  :mode "\\.ino\\'"
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify    t))

(use-package flycheck-clj-kondo
  :demand t)

(use-package clojure-ts-mode
  :custom
  (clojure-ts-indent-style 'semantic)
  (clojure-ts-toplevel-inside-comment-form t)
  :config
  (require 'flycheck-clj-kondo)
  :bind
  (:map clojure-ts-mode-map
		    ("C-<return>" . 'cider-eval-defun-at-point)
		    ("C-x C-e"    . 'cider-eval-last-sexp)
		    ("C-c C-k"    . 'cider-eval-buffer)
		    ("M-u"        . 'cider-format-buffer)))


(use-package cider
  :config
  (evil-make-intercept-map cider--debug-mode-map 'normal)
  :bind
  (:map cider-mode-map
		    ("C-<return>"   . 'cider-eval-defun-at-point)
		    ("C-S-<return>" . 'eval-last-sexp)
		    ("C-x C-e"    . 'cider-eval-last-sexp)
		    ("C-c C-k"    . 'cider-eval-buffer)
		    ("M-u"          . 'cider-format-buffer))
  :custom
  (cider-eval-result-duration          'change)
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

(use-package nameless)

(use-feature elisp-mode
  :config (define-key emacs-lisp-mode-map (kbd "C-c C-k")  'eval-buffer)
  ;; add prettify symbols mode
  :hook ((emacs-lisp-mode . prettify-symbols-mode)
         (emacs-lisp-mode . nameless-mode))
  :custom (help-enable-symbol-autoload t))


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
  :config
  (load "~/.emacs.d/hydras.el")
  (define-key evil-normal-state-map (kbd "SPC") 'hydra-smartparens/body)
  (define-key evil-visual-state-map (kbd "SPC") 'hydra-smartparens/body)
  (define-key evil-normal-state-map (kbd "C-SPC") 'hydra-treemacs/body)
  (define-key evil-visual-state-map (kbd "C-SPC") 'hydra-treemacs/body)
  (define-key evil-normal-state-map (kbd "C-m") 'hydra-turbo-log/body)
  (define-key evil-visual-state-map (kbd "C-m") 'hydra-turbo-log/body))


(use-feature org
  :hook (org-mode . org-indent-mode)
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
  (("M-n" . start-project-eshell)
   (:map eshell-mode-map
		     ("M-p" . 'execute-extended-command)))
  :config
  (defun start-project-eshell ()
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
		     ("M-g"     . magit-status)
		     (:map magit-mode-map
			         ("M-p" . execute-extended-command)
			         ("M-n" . start-project-eshell))))

(defun magit-checkout-main-and-pull ()
  "Checkout main branch and pull from remote."
  (interactive)
  (magit-checkout "main")
  (magit-pull-from-upstream nil))


(use-package swift-mode
  :mode "\\.swift\\'"
  :config

  :custom
  (swift-mode:basic-offset 4))

(use-feature objc-mode
  :mode "\\.mm\\'"
  :mode "\\.m\\'")


(use-package csv-mode)


(use-package just-mode)


(use-package justl
  :config
  (evil-define-key 'normal 'global
    "Ö" 'justl))



(use-package dotenv-mode
  :mode ("\\.env\\'"))


(use-package graphql-ts-mode
  :ensure nil
  :load-path "lisp/graphql-ts-mode/"
  :mode ("\\.graphql\\'" "\\.gql\\'"))


(use-package web-mode)


(use-package json-navigator)


(use-package tree-mode)


(use-feature dired
  :bind ("C-x C-d" . 'dired)
  :custom
  (dired-recursive-deletes  'always)
  (dired-recursive-copies   'always))


(defun replace-backslash-n-with-newline (start end)
  "Replace '\\n' with actual newlines in the selected region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (search-forward "\\n" nil t)
        (replace-match "\n" nil t)))))


(defun open-init ()
  "Open the Emacs configuration file."
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
 '(custom-safe-themes
   '("1b4c6180160023cdc749ecfb4a6abd2b9d143cabd44b69019b438513fa679cbb"
     "87c8b7f36581dd52adf2a850d7fa24ec2e8f2184119c836cde6de391871fb8ec"
     "85c13cb313c11e5c3cf541468dac6d24345039bce893590694b6036b98089b91"
     "82fbf2cae07b4796112e7f28ec488ed67adddfc35dad6634036ad726d15ba952"
     "7892a8ddcb09e7204923ded11c3485ec9eae0a857847654e7c0ae688a449d8c1"
     "8c718215c26ae262a8d91b77c8171e8328f29f06496a637a8cc6f3ef006d8d15"
     "b96fc4debf4c7fffd8adfa0deeefd725db399623d023088270364a024154f5e0"
     "c2cd117832ad873c9888a47541d8e00cbe9821951c015627c75d1f102cc6432d"
     "dfa2d9d49760d36e0620e795f5a7fe0447f6dd3fe4f87b453607fea731fb1617"
     "637495c558372c9317d3f3bc17df366855654238de3463b14fddd90ad730767f"
     "fff2f0aabedef08f6aa56bd5e294e839efc05df43857f27f018092d5a84d7a4e"
     "8ba79230b393f2d615cfeb1683b9f84ef5f702af2f4025717a43185ff4109dee"
     "2c6e10f6a0831c7f3514844a826a6f3091d41abbc75597e34b35622b1de47888"
     "31b239d841c5a5f5842d785aaff225bc504da413e99b7b64202f64b5caf5578e"
     "4e4800f348715e90c30861fcc283c4555587cea40747694a60d6498cdfd413f2"
     "0991cf12cb33238a35bed67bb5788df437710e996f700c3c63e6d3e48b3d8fbf"
     "ec5778e315af725c6b2473e99c6f8b7c88c7900be9ba348e3e77fac6cb1a263f"
     "c1795afae7ff894317dd9bdbd201936e12cbf60c157f03e5a64253c38da00a58"
     "fe6b6b41b768b7816641f5fcc4b99ddf68ca23c5995832aed9af3c16208ab2ec"
     "0190cb63eef9fb4e53c97b612cc9d3ef65bda54b422b495573c1e4effd7e2a91"
     "b3e06b4fd0ca9b3ae0a4fb1b95b7c2279b5612bff2c59f1ee5429082646b0d12"
     "97e07cdd0b2303e93fd6406fe34d3ab91bee82db2aafdbb4a266acf696375262"
     "80548fdb97c88f9cd2802243237a7bd770dce156236af01e4bdc94eb18d652bd"
     "df2dfe8a2500addeb5b96170679172714b684f6c25e7c7afba266d01dfd03cc4"
     "21156194d40decdc17a7a55b2b9a1c6ca5f3c4a4e7ecb6de80c1a54792a69c5a"
     "d5412d9460299e33f6d83e67e484a8690783669c7596b046472b157870cc1a9f"
     "300bf867156a1b020db7b71b1d566661ff9d552f0d4a9d258884d381f969cc92"
     "4ceb112651887da68ef79c1560432e1f5458a12d8b0b3c6df35f48e43cd8808c"
     "f7ab83a2e57457090c4bc979d0220189df62f131a123e6574c948c5cc236da46"
     "8dcb2490e383506653f10ebb50cc2fec1c29c850070e277525f89e6a4f1f14d8"
     "0985533c02b5aaa46e00808f6f7f9d464dbc27ffdc19982c3f1965c7b3660979"
     default))
 '(ignored-local-variable-values
   '((eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
              (cider-eval-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/template.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)")))))
 '(mini-frame-show-parameters '((top . 200) (width . 0.55) (left . 0.5)))
 '(safe-local-variable-values
   '((eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/lla/Developer/portfolio/src/motform/portfolio/templates/mau.clj")
              (cider-load-file
               "/Users/lla/Developer/portfolio/src/motform/portfolio/templates/multiverse.clj")
              (cider-load-file
               "/Users/lla/Developer/portfolio/src/motform/portfolio/templates/strange_materials.clj")
              (cider-load-file
               "/Users/lla/Developer/portfolio/src/motform/portfolio/templates/misc.clj")
              (cider-load-file
               "/Users/lla/Developer/portfolio/src/motform/portfolio/templates/front_page.clj")
              (cider-load-file
               "/Users/lla/Developer/portfolio/src/motform/portfolio/template.clj")
              (cider-load-file
               "/Users/lla/Developer/portfolio/src/motform/portfolio/core.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)")))
     (eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/aj6531/Documents/Portfolio/src/motform/portfolio/templates/mau.clj")
              (cider-load-file
               "/Users/aj6531/Documents/Portfolio/src/motform/portfolio/templates/multiverse.clj")
              (cider-load-file
               "/Users/aj6531/Documents/Portfolio/src/motform/portfolio/templates/strange_materials.clj")
              (cider-load-file
               "/Users/aj6531/Documents/Portfolio/src/motform/portfolio/templates/misc.clj")
              (cider-load-file
               "/Users/aj6531/Documents/Portfolio/src/motform/portfolio/templates/front_page.clj")
              (cider-load-file
               "/Users/aj6531/Documents/Portfolio/src/motform/portfolio/template.clj")
              (cider-load-file
               "/Users/aj6531/Documents/Portfolio/src/motform/portfolio/core.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)")))
     (eval define-key evil-normal-state-map (kbd "ä") 'clerk-show)
     (eval define-key evil-normal-state-map (kbd "ö") 'clerk-show)
     (eval define-key evil-normal-state-map (kbd "ö")
           'processing-3-run)
     (eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/templates/multiverse.clj")
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/templates/front_page.clj")
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/template.clj")
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)")))
     (eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/template.clj")
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)")))
     (cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/aj6531/Documents/motform/src/motform/portfolio/core.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)")))
     (eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/lla/Documents/motform/src/motform/portfolio/core.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)")))
     (eval define-key evil-normal-state-map (kbd "ö")
           '(lambda nil (interactive)
              (cider-load-file
               "/Users/lla/Projects/motform/src/motform/portfolio/core.clj")
              (cider-interactive-eval "(motform.portfolio.core/-main)"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

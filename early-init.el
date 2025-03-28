;;; early-init.el -*- lexical-binding: t; -*-
;;; forked from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

;;; Code:
(setq gc-cons-threshold most-positive-fixnum) ; Defer garbage collection further back in the startup process

(when (getenv-internal "DEBUG")
  (setq init-file-debug t
        debug-on-error t))

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)
(setq comp-deferred-compilation t)
(set-face-attribute 'default nil :family "MD IO 1.2" :height 120 :weight 'medium)

(setenv "LIBRARY_PATH"
        (mapconcat
         'identity
         '("/usr/local/Cellar/gcc/13.1.0/lib/gcc/13/"
           "/usr/local/Cellar/libgccjit/13.1.0/lib/gcc/13/"
           "/usr/local/Cellar/gcc/13.1.0/lib/gcc/13/gcc/x86_64-apple-darwin22/13/"
           "/opt/homebrew/opt/gcc/lib/gcc/13"
           "/opt/homebrew/opt/libgccjit/lib/gcc/13"
           "/opt/omebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
         ":"))
;;; early-init.el ends here

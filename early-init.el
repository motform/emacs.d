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

;;; early-init.el ends here

;;; early-init.el -*- lexical-binding: t; -*-
;;; forked from https://github.com/hlissner/doom-emacs/blob/develop/early-init.el

;;; Code:
(setq gc-cons-threshold most-positive-fixnum) ; Defer garbage collection further back in the startup process
;; (setq comp-speed 2)

(setq package-enable-at-startup nil)

(menu-bar-mode -1)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)
;; (setq comp-deferred-compilation nil)
(set-face-attribute 'default nil :family "PragmataPro Liga" :height 130)

;;; early-init.el ends here

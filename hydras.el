;;; hydras.el --- Hydras that don't take all the space in your init.el -*- lexical-binding: t; -*-

;; Copyright © 2021

;; Author: Love Lagerkvist
;; URL: https://github.com/motform/emacs.d
;; Created: 2021-01-04

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

;; This file contains the hydras that I use, as they take up a lot of 
;; space in `init.el'. 

;;; Code:

;; Window Management
(defhydra hydra-window (:hint nil)
  "
  Moving^^    Navigation^^            Window^^            
  ----------------------------------------------------------
  [_l_] left  [_p_] switch project    [_=_] balance windows    
  [_j_] down  [_t_] project find file [_d_] delete window      
  [_k_] up    [_p_] find file         [_x_] vertical split
  [_l_] right [_a_] switch to buffer  [_v_] horizontal split"

  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)

  ("p" project-switch-project)
  ("t" 'project-find-file)
  ("a" switch-to-buffer)
  ("f" find-file)

  ("=" balance-windows)
  ("d" delete-window)
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down)))
  ("v" 'split-right-and-focus))

;; Smartparens
(defhydra hydra-smartparens (:hint nil)
  "
    Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
   ------------------------------------------------------------------------------------------------------------------------
    [_n_] beginning  [_e_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split    [_t_] transpose   [_c_] change inner  [_w_] copy
    [_o_] end        [_E_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice   [_A_] absorb      [_C_] change outer
    [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise    [_E_] emit        [_k_] kill          [_g_] quit
    [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[__\"__`__<_] wrap (){}[]\"`< [_j_] join    [_O_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("n" sp-beginning-of-sexp)
  ("o" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("e" sp-down-sexp)
  ("E" sp-backward-down-sexp)
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
  ("\"" sp-wrap-double-quote)
  ("`" sp-wrap-backtick)
  ("<" sp-wrap-angle)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("O" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))

;; Smartparens
(defhydra hydra-combobulate (:hint nil)
  "
    Moving^^^^                       Marking^^        Splice^^           Destructive^^^^
   ------------------------------------------------------------------------------------------------------------------------
    [_b_] down       [_n_] backward  [_m_] mark       [_s_] splice-down  [_c_] edit-node  [_v_] envelop
    [_B_] up         [_N_] log-back  [_i_] highlight  [_S_] splice-up    [_C_] edit
    [_f_] down-list  [_e_] forward   [_I_] hl-clear   [_r_] vanish       [_k_] kill
    [_F_] up-list    [_E_] log-forw                 [_t_] transpose    [_y_] clone "
  ;; Moving
  ("f" combobulate-navigate-down-list-maybe)
  ("F" combobulate-navigate-up-list-maybe)
  ("b" combobulate-navigate-down)
  ("B" combobulate-navigate-up)
  ("n" combobulate-navigate-backward)
  ("N" combobulate-navigate-logical-previous)
  ("e" combobulate-navigate-forward)
  ("E" combobulate-navigate-logical-next)
  ("p" combobulate-navigate-beginning-of-defun)
  ("P" combobulate-navigate-end-of-defun)

  ("m" combobulate-mark-node-dwim)
  ("i" combobulate-highlight)
  ("I" combobulate-highlight-clear)

  ;; Sexp juggling
  ("S" combobulate-splice-up)
  ("s" combobulate-splice-down)
  ("r" combobulate-vanish-node)
  ("t" combobulate-transpose-sexps)

  ;; Destructive editing
  ("c" combobulate-edit-node-type-dwim :exit t)
  ("C" combobulate-edit :exit t)
  ("k" combobulate-kill-node-dwim)
  ("y" combobulate-clone-node-dwim)
  ("v" combobulate-envelop)

  ("q" nil)
  ("g" nil))


(defhydra hydra-smerge (:hint nil)
  "
^Move^          ^Keep^               ^Diff^                 ^Other^
^----^          ^----^               ^----^                 ^-----^
_n_ext          _b_ase               _<_: upper/base        _c_ombine
_p_rev          _u_pper              _=_: upper/lower       _r_esolve
^^              _l_ower              _>_: base/lower        _k_ill current
^^              _a_ll                _R_efine
^^              _RET_: current       _E_diff
"
  ("n" smerge-next)
  ("p" smerge-prev)
  ("b" smerge-keep-base)
  ("u" smerge-keep-upper)
  ("l" smerge-keep-lower)
  ("a" smerge-keep-all)
  ("RET" smerge-keep-current)
  ("<" smerge-diff-base-upper)
  ("=" smerge-diff-upper-lower)
  (">" smerge-diff-base-lower)
  ("R" smerge-refine)
  ("E" smerge-ediff)
  ("c" smerge-combine-with-next)
  ("r" smerge-resolve)
  ("k" smerge-kill-current)
  ("q" nil "cancel" :color pink))

(defhydra hydra-treemacs (:hint nil)
  "
  Treemacs^^
  ^Navigation^    ^Actions^           ^Other^
  ^----------^    ^-------^           ^-----^
  [_j_] next      [_r_] rename        [_m_] peek mode
  [_k_] previous  [_d_] delete        [_w_] set width
  [_p_] root up   [_a_] add file      [_b_] bookmark
  [_e_] root down [_A_] add directory
  [_q_] quit      
  "
  ("j" treemacs-next-line)
  ("k" treemacs-previous-line)
  ("s" treemacs-select-window)
  ("p" treemacs-root-up)
  ("e" treemacs-root-down)

  ("r" treemacs-rename-file)
  ("d" treemacs-delete-file)
  ("a" treemacs-create-file)
  ("A" treemacs-create-dir)

  ("m" treemacs-peek)
  ("w" treemacs-set-width)
  ("b" treemacs-bookmark)

  ("q" nil))

(defhydra hydra-turbo-log (:color blue :hint nil)
  "
Turbo Log Commands:
  ^Insert^              ^Comment^            ^Paste^                 ^Delete^
--------------------------------------------------------------------------------
  _l_: Insert log       _h_: Comment logs    _p_: Paste logger       _d_: Delete logs
  _i_: Insert & exec    _s_: Uncomment logs  _t_: Paste & exec
"
  ("l" turbo-log-print)
  ("i" turbo-log-print-immediately)
  ("h" turbo-log-comment-all-logs)
  ("s" turbo-log-uncomment-all-logs)
  ("p" turbo-log-paste-as-logger)
  ("t" turbo-log-paste-as-logger-immediately)
  ("d" turbo-log-delete-all-logs)
  ("q" nil "quit"))

;;; hydras.el ends here

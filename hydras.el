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
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))))

;; Smartparens
(defhydra hydra-smartparens (:hint nil)
  "
    Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
   ------------------------------------------------------------------------------------------------------------------------
    [_n_] beginning  [_e_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
    [_o_] end        [_E_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
    [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
    [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_O_] convolute   [_K_] bw kill       [_q_] quit"
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

;;; hydras.el ends here

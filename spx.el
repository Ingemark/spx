;;; spx.el --- Spx: Lispy limited to SmartParens -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Josip Gracin <josip.gracin@ingemark.com>

;; Author: Josip Gracin <josip.gracin@ingemark.com>
;; Maintainer: Josip Gracin <josip.gracin@ingemark.com>
;; Version: 1.0
;; Keywords: smartparens, lispy, speed, convenience
;; URL: http://github.com/Ingemark/spx
;; Package-Requires: (smartparens)

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Lispy (https://github.com/abo-abo/lispy) is great but it seems to do too many
;; things (for my taste) and it conflicted with some other packages as well. Spx
;; is a minor mode which only takes the idea of "automatic mode switching" from
;; Lispy and restricts it to simple wrappers around SmartParens commands.
;; Automatic mode switching means that when the point is at
;; parenthesis/brackets/braces, your regular keys activate commands instead of
;; self-inserting themselves.
;;
;; Usage:
;;
;; Enable the mode with spx-mode, place the point before the left parenthesis
;; (or brace or bracket) of an sexp and use the action keys. Note that for the
;; left parenthesis you have to place the point before the parenthesis, and for
;; the right parenthesis you have to place the point after the parenthesis in
;; order to activate the bindings.
;;
;; Example 1:
;; (the point is marked with |)
;;
;; (1 2 |(3 4) 5 6)
;; pressing k produces
;; (1 2 5 6)
;;
;; Example 2:
;; (1 2 (3 4)| 5 6)
;; pressing k produces
;; (1 2 5 6)

;;; Code:

(require 'smartparens)

(defvar spx--left-parenthesis  40)
(defvar spx--left-bracket      91)
(defvar spx--left-brace        123)
(defvar spx--right-parenthesis 41)
(defvar spx--right-bracket     93)
(defvar spx--right-brace       125)

(defun spx--at-beginning-of-sexp-p ()
  (let ((c (char-after)))
    (or (eql c spx--left-parenthesis)
        (eql c spx--left-brace)
        (eql c spx--left-bracket))))

(defun spx--at-end-of-sexp-p ()
  (let ((c (char-before)))
    (or (eql c spx--right-parenthesis)
        (eql c spx--right-brace)
        (eql c spx--right-bracket))))

(defun spx--at-either-side-of-sexp-p ()
  (or (spx--at-beginning-of-sexp-p)
      (spx--at-end-of-sexp-p)))

(defun spx--kill-whitespace (direction)
  "Kills all whitespace from point :forward or :backward,
depending on the provided direction."
  (let ((non-whitespace-regex "[^ \n]"))
    (if (equal direction :backward)
        (let ((end (point))
              (start (1+ (search-backward-regexp non-whitespace-regex))))
          (delete-region start end)
          (forward-char))
      (let ((start (point))
            (end (1- (search-forward-regexp non-whitespace-regex))))
        (delete-region start end)
        (backward-char)))))

(defun spx-action (condition fn)
  (list condition fn))

(defun spx--find-first-eligible-action (actions)
  (if actions
      (let ((action (first actions)))
        (if (funcall (first action))
            action
          (spx--find-first-eligible-action (rest actions))))))

(defun spx-handler (&rest actions)
  (lambda ()
    (interactive)
    (let ((c (spx--find-first-eligible-action actions)))
      (if c
          (funcall (second c))
        (self-insert-command 1)))))

(defvar spx--kill-sexp-forward
  (spx-action 'spx--at-beginning-of-sexp-p
               (lambda ()
                 (sp-kill-sexp)
                 (spx--kill-whitespace :forward))))

(defvar spx--kill-sexp-backward
  (spx-action 'spx--at-end-of-sexp-p
               (lambda ()
                 (sp-backward-kill-sexp)
                 (spx--kill-whitespace :backward))))

(defvar spx--wrap-sexp
  (spx-action 'spx--at-either-side-of-sexp-p
              (lambda ()
                (let* ((c (read-char))
                       (wrap-with (if (eql c ?w)
                                      spx--left-parenthesis
                                    c)))
                  (sp-wrap-with-pair (char-to-string wrap-with))
                  (insert-char 32)
                  (backward-char)))))

(defvar spx--unwrap-sexp (spx-action 'spx--at-either-side-of-sexp-p 'sp-unwrap-sexp))

(defvar spx--raise-sexp (spx-action 'spx--at-beginning-of-sexp-p 'sp-raise-sexp))

(defvar spx--copy-sexp (spx-action 'spx--at-either-side-of-sexp-p 'sp-copy-sexp))

(defvar spx--forward-sexp (spx-action 'spx--at-either-side-of-sexp-p 'sp-forward-sexp))

(defvar spx--backward-sexp (spx-action 'spx--at-either-side-of-sexp-p 'sp-backward-sexp))

(defvar spx--transpose-sexp (spx-action 'spx--at-either-side-of-sexp-p 'sp-transpose-sexp))

(defvar spx--backward-slurp-sexp
  (spx-action 'spx--at-beginning-of-sexp-p
              (lambda ()
                (forward-char)
                (sp-backward-slurp-sexp)
                (backward-up-list))))

(defvar spx--forward-barf-sexp
  (spx-action 'spx--at-end-of-sexp-p
              (lambda ()
                (backward-char)
                (sp-forward-barf-sexp))))

(defvar spx--forward-slurp-sexp
  (spx-action 'spx--at-end-of-sexp-p
              (lambda ()
                (backward-char)
                (sp-forward-slurp-sexp)
                (sp-end-of-sexp)
                (forward-char))))

(defvar spx--backward-barf-sexp
  (spx-action 'spx--at-beginning-of-sexp-p
              (lambda ()
                (forward-char)
                (sp-backward-barf-sexp))))

(defvar spx--indent-sexp (spx-action 'spx--at-beginning-of-sexp-p 'indent-pp-sexp))

(defvar spx-mode-map (make-sparse-keymap))

(let ((m spx-mode-map))
  (define-key m (kbd "r") (spx-handler spx--raise-sexp))
  (define-key m (kbd "k") (spx-handler spx--kill-sexp-forward spx--kill-sexp-backward))
  (define-key m (kbd "w") (spx-handler spx--wrap-sexp))
  (define-key m (kbd "W") (spx-handler spx--unwrap-sexp))
  (define-key m (kbd "y") (spx-handler spx--copy-sexp))
  (define-key m (kbd "f") (spx-handler spx--forward-sexp))
  (define-key m (kbd "b") (spx-handler spx--backward-sexp))
  (define-key m (kbd "t") (spx-handler spx--transpose-sexp))
  (define-key m (kbd "<") (spx-handler spx--backward-slurp-sexp spx--forward-barf-sexp))
  (define-key m (kbd ">") (spx-handler spx--forward-slurp-sexp spx--backward-barf-sexp))
  (define-key m (kbd "i") (spx-handler spx--indent-sexp)))

(define-minor-mode spx-mode
  "Minor mode for efficient keyboard bindings when standing on parenthesis/brackets/braces in Lisp editing modes.
Idea copied from project Lispy."
  :keymap spx-mode-map
  :group 'spx
  :lighter "∇") ; this is Unicode character 0x2207

(provide 'spx)

;;; spx.el ends here

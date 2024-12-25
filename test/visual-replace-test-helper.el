;;; test-helper.el --- Helpers for visual-replace-test.el -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Stephane Zermatten

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

(require 'visual-replace)
(require 'ert)
(require 'ert-x)

(defconst test-visual-replace-faces
  '((visual-replace-match "[]")
    (visual-replace-match-highlight "[]*")
    (visual-replace-delete-match "[]")
    (visual-replace-delete-match-highlight "[]*")
    (visual-replace-replacement "{}")
    (visual-replace-replacement-highlight "{}*"))
  "Convenient settings grab-face argument of `turtles-with-grab-buffer'.")

(defmacro test-visual-replace-env (&rest body)
  "Run BODY within a test buffer and environment."
  `(save-window-excursion
     (let ((transient-mark-mode t)
           (kill-ring nil)
           (inhibit-message t)
           (case-fold-search t)
           (replace-lax-whitespace nil)
           (replace-regexp-lax-whitespace nil)
           (visual-replace-read-history nil)
           (visual-replace-preview t)
           (visual-replace-first-match t)
           (visual-replace-display-total nil)
           (visual-replace-keep-incomplete t)
           (visual-replace-min-length 3)
           (visual-replace-functions nil)
           (visual-replace-initial-scope nil)
           (visual-replace-default-to-full-scope nil)
           (visual-replace-highlight-match-at-point t)
           (visual-replace-minibuffer-mode-hook nil)
           (visual-replace-defaults-hook nil)
           (visual-replace-keep-initial-position nil))
       (cl-letf (((symbol-function 'sit-for) (lambda (_))))
         (ert-with-test-buffer nil
           ,@body)))))

(defun test-visual-run-idle-search-timers ()
  "Run idle search timers, as long as there is one."
  (while visual-replace--idle-search-timer
    (cl-assert (memq visual-replace--idle-search-timer timer-idle-list))
    (ert-run-idle-timers)))

(defun test-visual-replace-highlight-property (text property &rest values)
  "Return a copy of TEXT with PROPERTY set to VALUES highlighted.

The matching regions of text are surrounded with []."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((active nil))
      (while (< (point) (point-max))
        (cond
         ((and (not active)
               (memq (get-text-property (point) property) values))
          (insert "[")
          (goto-char (1+ (point)))
          (setq active t))
         ((and active
               (not (memq (get-text-property (point) property) values)))
          (insert "]")
          (goto-char (1+ (point)))
          (setq active nil)))
        (goto-char (next-property-change (point) nil (point-max))))
      (when active (insert "]")))
    (buffer-substring (point-min) (point-max))))

(defun visual-replace-ert-explain-string-match (a b)
  `(string-match ,a ,b))
(put 'equal 'ert-explainer 'visual-replace-ert-explain-string-match)

(provide 'visual-replace-test-helper)

;;; visual-replace-test-helper.el ends here

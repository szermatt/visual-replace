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
(require 'seq)

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

(defun test-visual-replace-highlight-face (text &rest faces)
  "Return a copy of TEXT with FACES highlighted.

FACES should be one or more face to highlight.

The region of text with FACES are surrounded with []."
  (apply #'test-visual-replace-highlight-property text 'face faces))

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

(defun test-visual-replace-content ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun visual-replace-ert-explain-string-match (a b)
  `(string-match ,a ,b))
(put 'equal 'ert-explainer 'visual-replace-ert-explain-string-match)

(defun visual-replace-test-window-content (&optional win)
  "Return the visible portion of WIN.

If WIN is unspecified or nil, return the content of the selected
window. See `visual-replace-content' for details on the captured
text.

This requires `window-end' to be up-to-date. See
`test-visual-window-end'."
  (let ((win (or win (selected-window))))
    (with-current-buffer (window-buffer win)
      (save-restriction
        (narrow-to-region (window-start win) (window-end win))
        (visual-replace-test-content)))))

(defun visual-replace-test-content ()
  "Return the content of the current buffer.

Sections that are invisible are not included into the text.

Section with a \\='display overlay property are included instead
of the text, with the display text put within curly braces.

Other overlay properties are stored into the returned text as
text properties."
  (save-excursion
    (let ((last (point-min))
          (sections)
          (ov-props)
          (ov-lists (overlay-lists)))
      (dolist (ov (append (car ov-lists) (cdr ov-lists)))
        (let ((props (overlay-properties ov)))
          (while props
            (cl-pushnew (car props) ov-props)
            (setq props (cdr (cdr props))))))
      (setq ov-props (delq 'invisible ov-props))
      (setq ov-props (delq 'display ov-props))
      (setq ov-props (delq 'before-string ov-props))
      (setq ov-props (delq 'after-string ov-props))

      (goto-char (point-min))
      (while (not (eobp))
        (setq last (point))
        (goto-char (next-char-property-change (point) (point-max)))
        (let* ((invisible (invisible-p last))
               (display (get-char-property last 'display))
               (before-string (get-char-property last 'before-string))
               (after-string (get-char-property last 'after-string)))
          (dolist (ov (overlays-in last last))
            (when (= (overlay-start ov) (overlay-end ov))
              (when-let ((str (overlay-get ov 'before-string)))
                (push str sections))
              (when-let ((str (overlay-get ov 'after-string)))
                (push str sections))))
          (when before-string
            (push before-string sections))
          (push (cond
                 (invisible "")
                 (display (concat display))
                 (t
                  (let ((text (buffer-substring-no-properties (point) last)))
                    (dolist (ov-prop ov-props)
                      (let ((val (get-char-property last ov-prop)))
                        (put-text-property 0 (length text) ov-prop val text)))
                    text)))
                sections)
          (when after-string
            (push after-string sections))))
      (apply 'concat (nreverse sections)))))

(provide 'visual-replace-test-helper)

;;; visual-replace-test-helper.el ends here

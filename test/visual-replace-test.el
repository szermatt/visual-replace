;;; visual-replace-test.el --- Tests for (visual-replace) -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ert-x)

(require 'visual-replace)
(require 'visual-replace-test-helper)

;; compatibility: ert-replace-simulate-keys is only available since
;; Emacs 28. It is provided here to make the tests pass under 26 and 27.
(defmacro visual-replace-ert-simulate-keys (keys &rest body)
  "Execute BODY with KEYS as pseudo-interactive input."
  (declare (debug t) (indent 1))
  `(let ((unread-command-events
          ;; Add some C-g to try and make sure we still exit
          ;; in case something goes wrong.
          (append ,keys '(?\C-g ?\C-g ?\C-g)))
         ;; Tell `read-from-minibuffer' not to read from stdin when in
         ;; batch mode.
         (executing-kbd-macro t))
     ,@body))

(ert-deftest test-visual-replace-from-point ()
  (test-visual-replace-env
   (insert "hello 1\n")
   (save-excursion ;; point is here
     (insert "hello 2\n"))
   (test-visual-replace-run "hello RET hullo RET"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content)
                   "hello 1\nhullo 2\n"))))

(ert-deftest test-visual-replace-whole-buffer ()
  (test-visual-replace-env
   (insert "hello 1\n")
   (save-excursion ;; point is here
     (insert "hello 2\n"))
   (test-visual-replace-run "hello RET hullo <F1> s RET"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content)
                   "hullo 1\nhullo 2\n"))))

(ert-deftest test-visual-replace-in-region ()
  (test-visual-replace-env
   (let ((mark-1) (mark-2))
     (insert "hello 1\n")
     (setq mark-1 (point))
     (insert "hello 2\n")
     (setq mark-2 (point))
     (insert "hello 3\n")
     (goto-char mark-1)
     (set-mark mark-2))
   (test-visual-replace-run "hello RET hullo RET"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content)
                   "hello 1\nhullo 2\nhello 3\n"))))

(ert-deftest test-visual-replace-in-region-keep-initial-position ()
  (test-visual-replace-env
   (let ((visual-replace-keep-initial-position t))
     (let ((mark-1) (mark-2))
       (insert "hello 1\n")
       (setq mark-1 (point))
       (insert "hello 2\n")
       (setq mark-2 (point))
       (insert "hello 3\n")
       (goto-char mark-1)
       (set-mark mark-2))
     (test-visual-replace-run "hello RET hullo RET"
                              (call-interactively 'visual-replace))
     (should (equal (test-visual-replace-content)
                    "hello 1\nhullo 2\nhello 3\n")))))

(ert-deftest test-visual-replace-regexp ()
  (test-visual-replace-env
   (insert "hello, world")
   (goto-char (point-min))
   (test-visual-replace-run "h\\(.*?\\)o RET H\\1O <F1> r RET"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content)
                   "HellO, world"))))

(ert-deftest test-visual-replace-regexp-eval ()
  (test-visual-replace-env
   (insert "hello, hello")
   (goto-char (point-min))
   (test-visual-replace-run
    "h\\(.*?\\)o RET \\#\\,(upcase SPC \\1)e <F1> r RET"
    (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content)
                   "0ELLe, 1ELLe"))))

(ert-deftest test-visual-replace-query ()
  (test-visual-replace-env
   (insert "hello 1\n")
   (insert "hello 2\n")
   (insert "hello 3\n")
   (goto-char (point-min))
   (test-visual-replace-run "hello RET hullo <F1> q RET n y n"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content)
                   "hello 1\nhullo 2\nhello 3\n"))))

(ert-deftest test-visual-replace-lax-ws ()
  (test-visual-replace-env
   (insert "hello   world.")
   (goto-char (point-min))
   (test-visual-replace-run "hello SPC world TAB foobar <F1> l RET"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content) "foobar."))))

(ert-deftest test-visual-replace-lax-ws-regexp ()
  (test-visual-replace-env
   (insert "hello world.")
   (goto-char (point-min))
   (test-visual-replace-run "hello SPC w.*d TAB foobar <F1> r <F1> l RET"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-content) "foobar."))))


(ert-deftest test-visual-replace-preprocess ()
  (test-visual-replace-env
   (let ((visual-replace-functions
          (list (lambda (args)
                  (setf (visual-replace-args-from args)
                        (concat "^" (visual-replace-args-from args)))))))
     (insert "hello, hello")
     (goto-char (point-min))
     (test-visual-replace-run "hello RET hullo <F1> r RET"
                              (call-interactively 'visual-replace))
     (should (equal (test-visual-replace-content)
                    "hullo, hello")))))

(ert-deftest test-visual-replace-read-only-buffer ()
  (test-visual-replace-env
   (insert "foo")
   (read-only-mode)
   (should-error
    (visual-replace (visual-replace-make-args :from "foo" :to "bar")))))

(ert-deftest test-visual-replace-thing-at-point ()
  (test-visual-replace-env
   (emacs-lisp-mode)
   (dotimes (i 6)
     (insert (format "this is thing-at-point %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward "-at-point 3")
     (goto-char (match-beginning 0))
     (visual-replace-ert-simulate-keys (kbd "r e p l RET")
       (visual-replace-thing-at-point))
     (should (equal (concat "this is thing-at-point 0\n"
                            "this is thing-at-point 1\n"
                            "this is thing-at-point 2\n"
                            "this is repl 3\n"
                            "this is repl 4\n"
                            "this is repl 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-word-at-point ()
  (test-visual-replace-env
   (emacs-lisp-mode)
   (dotimes (i 6)
     (insert (format "this is thing-at-point %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward "-at-point 3")
     (goto-char (match-beginning 0))
     (visual-replace-ert-simulate-keys (kbd "r e p l RET")
       (visual-replace-thing-at-point 'word))
     (should (equal (concat "this is thing-at-point 0\n"
                            "this is thing-at-point 1\n"
                            "this is thing-at-point 2\n"
                            "this is repl-at-point 3\n"
                            "this is repl-at-point 4\n"
                            "this is repl-at-point 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-thing-at-point-full-scope ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward-regexp "text 3")
     (goto-char (match-beginning 0))
     (let ((visual-replace-default-to-full-scope 'full))
       (visual-replace-ert-simulate-keys (kbd "r e p l a c e d RET")
         (visual-replace-selected)))
     (should (equal (concat "this is replaced 0\n"
                            "this is replaced 1\n"
                            "this is replaced 2\n"
                            "this is replaced 3\n"
                            "this is replaced 4\n"
                            "this is replaced 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-selected ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is some text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward-regexp "\\(some text\\) 3")
     (set-mark (match-beginning 1))
     (goto-char (match-end 1))
     (visual-replace-ert-simulate-keys (kbd "r e p l a c e d RET")
       (visual-replace-selected))
     (should (equal (concat "this is some text 0\n"
                            "this is some text 1\n"
                            "this is some text 2\n"
                            "this is replaced 3\n"
                            "this is replaced 4\n"
                            "this is replaced 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-selected-fallback ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is some text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward "some text 3")
     (goto-char (match-beginning 0))
     (visual-replace-ert-simulate-keys (kbd "r e p l a c e d RET")
       (visual-replace-selected))
     (should (equal (concat "this is some text 0\n"
                            "this is some text 1\n"
                            "this is some text 2\n"
                            "this is replaced text 3\n"
                            "this is replaced text 4\n"
                            "this is replaced text 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-selected-full-scope ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is some text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward-regexp "\\(some text\\) 3")
     (set-mark (match-beginning 1))
     (goto-char (match-end 1))
     (let ((visual-replace-default-to-full-scope 'full))
       (visual-replace-ert-simulate-keys (kbd "r e p l a c e d RET")
         (visual-replace-selected)))
     (should (equal (concat "this is replaced 0\n"
                            "this is replaced 1\n"
                            "this is replaced 2\n"
                            "this is replaced 3\n"
                            "this is replaced 4\n"
                            "this is replaced 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-scope-to-region-if-active ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward-regexp "text 2")
     (set-mark (point))
     (search-forward-regexp "text 4")

     (visual-replace-ert-simulate-keys (kbd "text TAB r e p l a c e d RET")
       (call-interactively 'visual-replace))
     (should (equal (concat "this is text 0\n"
                            "this is text 1\n"
                            "this is text 2\n"
                            "this is replaced 3\n"
                            "this is replaced 4\n"
                            "this is text 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-override-initial-scope ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward-regexp "text 2")
     (set-mark (point))
     (search-forward-regexp "text 4")

     ;; region is active, yet use the full scope anyway
     (let ((visual-replace-initial-scope 'full))
       (visual-replace-ert-simulate-keys (kbd "text TAB r e p l a c e d RET")
         (call-interactively 'visual-replace)))
     (should (equal (concat "this is replaced 0\n"
                            "this is replaced 1\n"
                            "this is replaced 2\n"
                            "this is replaced 3\n"
                            "this is replaced 4\n"
                            "this is replaced 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-scope-default-to-from-point ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward-regexp "text 2")
     (visual-replace-ert-simulate-keys (kbd "text TAB r e p l a c e d RET")
       (call-interactively 'visual-replace))
     (should (equal (concat "this is text 0\n"
                            "this is text 1\n"
                            "this is text 2\n"
                            "this is replaced 3\n"
                            "this is replaced 4\n"
                            "this is replaced 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-default-to-full-scope ()
  (test-visual-replace-env
   (dotimes (i 6)
     (insert (format "this is text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (search-forward-regexp "text 2")
     (let ((visual-replace-default-to-full-scope 'full))
       (visual-replace-ert-simulate-keys (kbd "text TAB r e p l a c e d RET")
         (call-interactively 'visual-replace)))
     (should (equal (concat "this is replaced 0\n"
                            "this is replaced 1\n"
                            "this is replaced 2\n"
                            "this is replaced 3\n"
                            "this is replaced 4\n"
                            "this is replaced 5\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-jump-forward-to-first-match ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (let* ((snapshots)
            (win (selected-window))
            (height (window-height win))
            (to-replace (* 2 height)))
       (dotimes (i (* 3 height))
         (insert (format "this is text %d.\n" i)))
       (goto-char (point-min))
       (forward-line 3)
       (recenter)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (visual-replace--update-preview)
          (while visual-replace--idle-search-timer
            (cl-assert (memq visual-replace--idle-search-timer timer-idle-list))
            (ert-run-idle-timers))
          (push (visual-replace-test-window-content win) snapshots)))
       (visual-replace-ert-simulate-keys (kbd (format "t e x t SPC %d . TAB r e p l C-c t RET" to-replace))
         (call-interactively 'visual-replace))

       ;; C-c t was called once to capture window content.
       (should (equal 1 (length snapshots)))

       ;; The replacement was highlighted, even though it required scrolling the window.
       (should (string-match
                (regexp-quote (format "[text %d.]repl" to-replace))
                (test-visual-replace-highlight-face
                 (car snapshots) 'visual-replace-delete-match)))

       ;; The point is now at the first match.
       (should (equal "this is repl" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))

       ;; The mark is at the original position, but inactive.
       (should-not (region-active-p))
       (should (equal "this is text 3."
                      (save-excursion
                        (goto-char (mark))
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))))))

(ert-deftest test-visual-replace-jump-backward-to-first-match ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (let* ((snapshots)
            (win (selected-window))
            (visual-replace-default-to-full-scope t)
            (height (window-height win))
            (start-line))
       (dotimes (i (* 3 height))
         (insert (format "this is text %d.\n" i)))
       (goto-char (point-max))
       (forward-line -3)
       (setq start-line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
       (recenter)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (visual-replace--update-preview)
          (while visual-replace--idle-search-timer
            (cl-assert (memq visual-replace--idle-search-timer timer-idle-list))
            (ert-run-idle-timers))
          (push (visual-replace-test-window-content win) snapshots)))
       (visual-replace-ert-simulate-keys (kbd "t e x t SPC 5 . TAB repl C-c t RET")
         (call-interactively 'visual-replace))

       ;; C-c t was called once to capture window content.
       (should (equal 1 (length snapshots)))

       ;; The replacement was highlighted, even though it required scrolling the window.
       (should (string-match (regexp-quote "[text 5.]repl")
                             (test-visual-replace-highlight-face
                              (car snapshots) 'visual-replace-delete-match)))

       ;; The point is now at the first match.
       (should (equal "this is repl" (buffer-substring-no-properties
                                      (line-beginning-position)
                                      (line-end-position))))

       ;; The mark is at the original position, but inactive.
       (should-not (region-active-p))
       (should (equal start-line
                      (save-excursion
                        (goto-char (mark))
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))))))))

(ert-deftest test-visual-replace-restore-position-after-jump ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (let* ((win (selected-window))
            (visual-replace-keep-initial-position t)
            (height (window-height win))
            (to-replace (* 2 height)))
       (dotimes (i (* 3 height))
         (insert (format "this is text %d.\n" i)))
       (goto-char (point-min))
       (forward-line 3)
       (recenter)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (visual-replace--update-preview)
          (while visual-replace--idle-search-timer
            (cl-assert (memq visual-replace--idle-search-timer timer-idle-list))
            (ert-run-idle-timers))))
       (visual-replace-ert-simulate-keys (kbd (format "t e x t SPC %d . TAB r e p l C-c t RET" to-replace))
         (call-interactively 'visual-replace))

       (should (equal "this is text 3."
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))))))

(ert-deftest test-visual-replace-stay-in-position-after-jump ()
  (test-visual-replace-env
     (with-selected-window (display-buffer (current-buffer))
       (let* ((win (selected-window))
              (visual-replace-keep-initial-position nil)
              (height (window-height win))
              (to-replace (* 2 height)))
         (dotimes (i (* 3 height))
           (insert (format "this is text %d.\n" i)))
         (goto-char (point-min))
         (forward-line 3)
         (recenter)
         (define-key
          visual-replace-mode-map
          (kbd "C-c t")
          (lambda ()
            (interactive)
            (visual-replace--update-preview)
            (while visual-replace--idle-search-timer
              (cl-assert (memq visual-replace--idle-search-timer timer-idle-list))
              (ert-run-idle-timers))))
         (visual-replace-ert-simulate-keys (kbd (format "t e x t SPC %d . TAB r e p l C-c t RET" to-replace))
                                           (call-interactively 'visual-replace))
         ;; The point should be at the first match.
         (should (equal "this is repl"
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))))))

(ert-deftest test-visual-replace-give-up-looking-for-first-match ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (let* ((snapshots)
            (win (selected-window))
            (visual-replace-max-size-for-search 1024))
       (dotimes (_ 1024)
         (insert "some text.\n"))
       (insert "the end.")
       (goto-char (point-min))
       (recenter)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (visual-replace--update-preview)
          (while visual-replace--idle-search-timer
            (cl-assert (memq visual-replace--idle-search-timer timer-idle-list))
            (ert-run-idle-timers))
          (push (visual-replace-test-window-content win) snapshots)))
       (visual-replace-ert-simulate-keys (kbd "the SPC end TAB fin C-c t RET")
         (call-interactively 'visual-replace))

       ;; The text was replaced
       (goto-char (point-max))
       (should (equal "fin." (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))

       ;; C-c t was called once to capture window content.
       (should (equal 1 (length snapshots)))

       ;; The replacement was NOT highlighted.
       (should (string-match
                "the end.$"
                (test-visual-replace-highlight-face
                 (car snapshots) 'visual-replace-delete-match)))))))

(ert-deftest test-visual-replace-small-ranges ()
  (ert-with-test-buffer nil
    (dotimes (i 300)
      (insert (format "line %d.\n" i)))
    (goto-char (point-min))

    (should (equal nil (visual-replace--small-ranges nil)))

    (should (equal nil (visual-replace--small-ranges '((10 . 10)))))

    (should (equal `((1 . ,(line-beginning-position 20))
                     (,(line-beginning-position 100) . ,(line-beginning-position 120)))
                   (visual-replace--small-ranges
                    `((1 . ,(line-beginning-position 20))
                      (,(line-beginning-position 100) . ,(line-beginning-position 120))))))

    (should (equal `((1 . ,(line-beginning-position 81))
                     (,(line-beginning-position 81) . ,(line-beginning-position 161))
                     (,(line-beginning-position 161) . ,(line-beginning-position 200)))
                   (visual-replace--small-ranges `((1 . ,(line-beginning-position 200))))))))

(ert-deftest test-visual-replace-highlight-scope-from-point ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (delete-other-windows)
     (should (>= (window-height) 6))
     (let* ((snapshots)
            (win (selected-window)))
       (dotimes (i 6)
         (insert (format "line %d.\n" i)))
       (goto-char (point-min))
       (forward-line 3)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (push
           (cons (visual-replace-test-window-content) ;; minibuffer
                 (test-visual-replace-highlight-face
                  (visual-replace-test-window-content win)
                  'visual-replace-region))
           snapshots)))
       (define-key
        visual-replace-mode-map
        (kbd "C-c s")
        #'visual-replace-toggle-scope)
       (should-not visual-replace-default-to-full-scope)
       (visual-replace-ert-simulate-keys (kbd "foo C-c t C-c s C-c t TAB bar RET")
         (call-interactively 'visual-replace))

       (should (equal
                (list
                 ;; 'from-point is highlighted
                 (cons
                  "Replace from point: foo"
                  (concat "line 0.\n"
                          "line 1.\n"
                          "line 2.\n"
                          "[line 3.\n"
                          "line 4.\n"
                          "line 5.\n"
                          "]"))
                 ;; 'full is not highlighted
                 (cons
                  "Replace in buffer: foo"
                  (concat "line 0.\n"
                          "line 1.\n"
                          "line 2.\n"
                          "line 3.\n"
                          "line 4.\n"
                          "line 5.\n")))
                (nreverse snapshots)))))))

(ert-deftest test-visual-replace-highlight-scope-region ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (delete-other-windows)
     (should (>= (window-height) 6))
     (let* ((snapshots)
            (win (selected-window)))
       (dotimes (i 6)
         (insert (format "line %d.\n" i)))
       (goto-char (point-min))
       (forward-line 2)
       (set-mark (line-end-position 2))
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (push
           (cons (visual-replace-test-window-content) ;; minibuffer
                 (test-visual-replace-highlight-face
                  (visual-replace-test-window-content win)
                  'visual-replace-region))
           snapshots)))
       (define-key
        visual-replace-mode-map
        (kbd "C-c s")
        #'visual-replace-toggle-scope)
       (should-not visual-replace-default-to-full-scope)
       (should (region-active-p))
       (visual-replace-ert-simulate-keys (kbd "foo C-c t C-c s C-c t TAB bar RET")
         (call-interactively 'visual-replace))

       (should (equal
                (list
                 ;; 'region is highlighted
                 (cons
                  "Replace in region (2L): foo"
                  (concat "line 0.\n"
                          "line 1.\n"
                          "[line 2.\n"
                          "line 3.]\n"
                          "line 4.\n"
                          "line 5.\n"))
                 ;; 'full is not highlighted
                 (cons
                  "Replace in buffer: foo"
                  (concat "line 0.\n"
                          "line 1.\n"
                          "line 2.\n"
                          "line 3.\n"
                          "line 4.\n"
                          "line 5.\n")))
                (nreverse snapshots)))))))

(ert-deftest test-visual-replace-highlight-scope-rect-region ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (delete-other-windows)
     (should (>= (window-height) 6))
     (let* ((snapshots)
            (win (selected-window)))
       (dotimes (i 6)
         (insert (format "line %d.\n" i)))
       (goto-char (point-min))
       (forward-line 2)
       (rectangle-mark-mode)
       (forward-line 2)
       (goto-char (1- (line-end-position)))
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (push
           (cons (visual-replace-test-window-content) ;; minibuffer
                 (test-visual-replace-highlight-face
                  (visual-replace-test-window-content win)
                  'visual-replace-region))
           snapshots)))
       (define-key
        visual-replace-mode-map
        (kbd "C-c s")
        #'visual-replace-toggle-scope)
       (should-not visual-replace-default-to-full-scope)
       (should (region-active-p))
       (visual-replace-ert-simulate-keys (kbd "foo C-c t C-c s C-c t TAB bar RET")
         (call-interactively 'visual-replace))

       (should (equal
                (list
                 ;; 'region is highlighted
                 (cons
                  "Replace in region (3L): foo"
                  (concat "line 0.\n"
                          "line 1.\n"
                          "[line 2].\n"
                          "[line 3].\n"
                          "[line 4].\n"
                          "line 5.\n"))
                 ;; 'full is not highlighted
                 (cons
                  "Replace in buffer: foo"
                  (concat "line 0.\n"
                          "line 1.\n"
                          "line 2.\n"
                          "line 3.\n"
                          "line 4.\n"
                          "line 5.\n")))
                (nreverse snapshots)))))))

(ert-deftest test-visual-replace-highlight-scope-rect-region-with-gaps ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (delete-other-windows)
     (should (>= (window-height) 6))
     (let* ((snapshots)
            (win (selected-window)))
       (insert "line 1.\n")
       (insert "     line 2.\n")
       (insert "3.\n")
       (insert "     line 4.\n")
       (insert "line 5.\n")
       (insert "line 6.\n")
       (goto-char (point-min))
       (search-forward "line 2.")
       (goto-char (match-beginning 0))
       (rectangle-mark-mode)
       (let ((col (current-column)))
         (forward-line 3)
         (goto-char (+ (point) col)))
       (rectangle-right-char 4)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (push
           (test-visual-replace-highlight-face
            (visual-replace-test-window-content win)
            'visual-replace-region)
           snapshots)))
       (should-not visual-replace-default-to-full-scope)
       (should (region-active-p))
       (visual-replace-ert-simulate-keys (kbd "foo C-c t TAB bar RET")
         (call-interactively 'visual-replace))

       (should (equal
                (list
                 (concat "line 1.\n"
                         "     [line] 2.\n"
                         "3.   [    ]\n"
                         "     [line] 4.\n"
                         "line [5.  ]\n"
                         "line 6.\n"))
                snapshots))))))

(ert-deftest test-visual-replace-prev-next-match ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (let* ((snapshots))
       (dotimes (i 6)
         (insert (format "line %d.\n" i)))
       (goto-char (point-min))
       (forward-line 3)
       (define-key visual-replace-mode-map (kbd "<up>") #'visual-replace-prev-match)
       (define-key visual-replace-mode-map (kbd "<down>") #'visual-replace-next-match)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (push
           (with-current-buffer visual-replace--calling-buffer
             (let ((text (buffer-substring-no-properties
                          (point-min) (point-max))))
               (concat
                (substring text 0 (1- (point)))
                "<>"
                (substring text (1- (point))))))
           snapshots)))
       (should-not visual-replace-default-to-full-scope)
       (visual-replace-ert-simulate-keys
        (kbd "line C-c t <down> C-c t <down> C-c t <up> C-c t <up> C-c t TAB bar RET")
         (call-interactively 'visual-replace))

       (should (equal
                (list
                 (concat "line 0.\n"
                         "line 1.\n"
                         "line 2.\n"
                         "<>line 3.\n"
                         "line 4.\n"
                         "line 5.\n")
                 (concat "line 0.\n"
                         "line 1.\n"
                         "line 2.\n"
                         "line 3.\n"
                         "<>line 4.\n"
                         "line 5.\n")
                 (concat "line 0.\n"
                         "line 1.\n"
                         "line 2.\n"
                         "line 3.\n"
                         "line 4.\n"
                         "<>line 5.\n")
                 (concat "line 0.\n"
                         "line 1.\n"
                         "line 2.\n"
                         "line 3.\n"
                         "<>line 4.\n"
                         "line 5.\n")
                 (concat "line 0.\n"
                         "line 1.\n"
                         "line 2.\n"
                         "<>line 3.\n"
                         "line 4.\n"
                         "line 5.\n"))
                (nreverse snapshots)))))))

(ert-deftest test-visual-replace-rect-replace ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (dotimes (_ 6)
       (insert "foo foo foo.\n"))
     (goto-char (point-min))
     (forward-line 2)
     (right-char 4) ;; point at beginning of 2nd "foo"
     (rectangle-mark-mode)
     (let ((col (current-column)))
       (forward-line 2)
       (goto-char (+ (point) col)))
     (rectangle-right-char 4)
     (define-key
      visual-replace-mode-map
      (kbd "C-c r")
      #'visual-replace-toggle-regexp)
     (should-not visual-replace-default-to-full-scope)
     (should (region-active-p))
     (visual-replace-ert-simulate-keys
         (kbd "foo TAB C-c r \\ # . bar RET")
       (call-interactively 'visual-replace))
     (should (equal (concat "foo foo foo.\n"
                            "foo foo foo.\n"
                            "foo 0.bar foo.\n"
                            "foo 1.bar foo.\n"
                            "foo 2.bar foo.\n"
                            "foo foo foo.\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(when (eval-when-compile (>= emacs-major-version 29))
(ert-deftest test-visual-replace-apply-one-repeat ()
  (test-visual-replace-env
   (dotimes (i 4)
     (insert (format "this is text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (define-key visual-replace-mode-map (kbd "<F1> a") #'visual-replace-apply-one-repeat)
     (visual-replace-ert-simulate-keys (kbd "text TAB r e p l <F1> a a a C-g")
       (condition-case _
           (call-interactively 'visual-replace)
         (minibuffer-quit)))
     (should (equal (concat "this is repl 0\n"
                            "this is repl 1\n"
                            "this is repl 2\n"
                            "this is text 3\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

(ert-deftest test-visual-replace-apply-one-repeat-up-down ()
  (test-visual-replace-env
   (dotimes (i 4)
     (insert (format "this is text %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))
     (define-key visual-replace-mode-map (kbd "<F1> a") #'visual-replace-apply-one-repeat)
     (visual-replace-ert-simulate-keys (kbd "text TAB r e p l <F1> a <down> <down> <up> a C-g")
       (condition-case _
           (call-interactively 'visual-replace)
         (minibuffer-quit)))
     (should (equal (concat "this is repl 0\n"
                            "this is text 1\n"
                            "this is repl 2\n"
                            "this is text 3\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

  (ert-deftest test-visual-replace-apply-one-repeat-continue ()
    (test-visual-replace-env
     (dotimes (i 4)
       (insert (format "this is text %d\n" i)))
     (with-selected-window (display-buffer (current-buffer))
       (goto-char (point-min))
       (define-key visual-replace-mode-map (kbd "<F1> a") #'visual-replace-apply-one-repeat)
       ;; Anything but a/<up>/<down> ends the transient map, so just
       ;; typing "lace" leaves the mode.
       (visual-replace-ert-simulate-keys (kbd "text TAB r e p <F1> a a l a c e RET")
                                         (condition-case _
                                             (call-interactively 'visual-replace)
                                           (minibuffer-quit)))
       (should (equal (concat "this is rep 0\n"
                              "this is rep 1\n"
                              "this is replace 2\n"
                              "this is replace 3\n")
                      (buffer-substring-no-properties
                       (point-min) (point-max))))))))

(ert-deftest test-visual-replace-from-isearch ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (with-selected-window (display-buffer (current-buffer))
     ;; visual-replace-from-isearch must start at the beginning of the current match,
     ;; so that it's covered with "replace from point".
     (goto-char (point-min))
     (execute-kbd-macro (kbd "C-s hell C-s M-x visual-replace-from-isearch RET hull"))
     (should (equal "hello, world, hullo, hullo!"
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))


;;; visual-replace-test.el ends here

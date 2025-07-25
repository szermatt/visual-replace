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
(require 'turtles)
(require 'hideshow)
(require 'elisp-mode)
(require 'which-key nil 'noerror) ;; optional

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

(ert-deftest visual-replace-from-point ()
  (test-visual-replace-env
   (insert "hello 1\n")
   (save-excursion ;; point is here
     (insert "hello 2\n"))
   (visual-replace (visual-replace-make-args :from "hello" :to "hullo")
                     `((,(point) . ,(point-max))))
   (should (equal (buffer-string)
                   "hello 1\nhullo 2\n"))))

(ert-deftest visual-replace-whole-buffer ()
  (test-visual-replace-env
   (insert "hello 1\n")
   (save-excursion ;; point is here
     (insert "hello 2\n"))
   (visual-replace (visual-replace-make-args :from "hello" :to "hullo")
                     `((,(point-min) . ,(point-max))))
   (should (equal (buffer-string)
                   "hullo 1\nhullo 2\n"))))

(ert-deftest visual-replace-in-region ()
  (test-visual-replace-env
   (let ((mark-1) (mark-2))
     (insert "hello 1\n")
     (setq mark-1 (point))
     (insert "hello 2\n")
     (setq mark-2 (point))
     (insert "hello 3\n")

     (visual-replace (visual-replace-make-args :from "hello" :to "hullo")
                     `((,mark-1 . ,mark-2)))
   (should (equal (buffer-string)
                   "hello 1\nhullo 2\nhello 3\n")))))

(ert-deftest visual-replace-in-region-keep-initial-position ()
  (test-visual-replace-env
   (let ((visual-replace-keep-initial-position t))
     (let ((mark-1) (mark-2))
       (insert "hello 1\n")
       (setq mark-1 (point))
       (insert "hello 2\n")
       (setq mark-2 (point))
       (insert "hello 3\n")
       (goto-char mark-2)

       (visual-replace (visual-replace-make-args :from "hello" :to "hullo")
                       `((,mark-1 . ,mark-2)))
       (should (equal "hello 1\nhullo 2\nhello 3\n"
                      (buffer-string)))

       (should (equal mark-2 (point)))))))

(ert-deftest visual-replace-regexp ()
  (test-visual-replace-env
   (insert "hello, world")
   (goto-char (point-min))
   (visual-replace (visual-replace-make-args
                    :from "h\\(.*?\\)o"
                    :to "H\\1O"
                    :regexp t)
                   `((,(point-min) . ,(point-max))))
   (should (equal (buffer-string)
                   "HellO, world"))))

(ert-deftest visual-replace-regexp-eval ()
  (test-visual-replace-env
   (insert "hello, hello")
   (goto-char (point-min))
   (visual-replace (visual-replace-make-args
                    :from "h\\(.*?\\)o" :to "\\#\\,(upcase \\1)e" :regexp t)
                   `((,(point-min) . ,(point-max))))
   (should (equal (buffer-string)
                   "0ELLe, 1ELLe"))))

(ert-deftest visual-replace-query ()
  (test-visual-replace-env
   (insert "hello 1\n")
   (insert "hello 2\n")
   (insert "hello 3\n")
   (goto-char (point-min))

   (visual-replace-ert-simulate-keys (kbd "n y n")
    (visual-replace (visual-replace-make-args :from "hello" :to "hullo" :query t)
                    `((,(point-min) . ,(point-max)))))

   (should (equal "hello 1\nhullo 2\nhello 3\n"
                  (buffer-string)))))

(ert-deftest visual-replace-lax-ws ()
  (test-visual-replace-env
   (insert "hello   world.")
   (goto-char (point-min))
   (visual-replace (visual-replace-make-args
                    :from "hello world" :to "foobar" :lax-ws-non-regexp t)
                   `((,(point-min) . ,(point-max))))
   (should (equal (buffer-string) "foobar."))))

(ert-deftest visual-replace-lax-ws-regexp ()
  (test-visual-replace-env
   (insert "hello world.")
   (goto-char (point-min))
   (visual-replace (visual-replace-make-args
                    :from "hello w.*d" :to "foobar" :lax-ws-regexp t :regexp t)
                   `((,(point-min) . ,(point-max))))
   (should (equal (buffer-string) "foobar."))))

(ert-deftest visual-replace-forward ()
  (test-visual-replace-env
   (insert "foo bar foo foo")
   (goto-char (point-min))
   (visual-replace (visual-replace-make-args
                    :from "foo" :to "(num \\#)" :regexp t)
                   `((,(point-min) . ,(point-max))))
   (should (equal (buffer-string)
                   "(num 0) bar (num 1) (num 2)"))))

(ert-deftest visual-replace-backwards ()
  (test-visual-replace-env
   (insert "foo bar foo foo")
   (goto-char (point-min))
   (visual-replace (visual-replace-make-args
                    :from "foo" :to "(num \\#)" :regexp t :backwards t)
                   `((,(point-min) . ,(point-max))))
   (should (equal (buffer-string)
                   "(num 2) bar (num 1) (num 0)"))))


(ert-deftest visual-replace-preprocess ()
  (test-visual-replace-env
   (let ((visual-replace-functions
          (list (lambda (args)
                  (setf (visual-replace-args-from args)
                        (concat "^" (visual-replace-args-from args)))))))
     (insert "hello, hello")
     (goto-char (point-min))

     (visual-replace (visual-replace-make-args
                      :from "hello" :to "hullo" :regexp t)
                     `((,(point-min) . ,(point-max))))

     (should (equal (buffer-string)
                    "hullo, hello")))))

(ert-deftest visual-replace-read-only-buffer ()
  (test-visual-replace-env
   (insert "foo")
   (read-only-mode)
   (should-error
    (visual-replace (visual-replace-make-args :from "foo" :to "bar")))))

(ert-deftest visual-replace-thing-at-point ()
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

(ert-deftest visual-replace-word-at-point ()
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

(ert-deftest visual-replace-thing-at-point-full-scope ()
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

(ert-deftest visual-replace-selected ()
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

(ert-deftest visual-replace-selected-fallback ()
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

(ert-deftest visual-replace-selected-full-scope ()
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

(ert-deftest visual-replace-scope-to-region-if-active ()
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

(ert-deftest visual-replace-override-initial-scope ()
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

(ert-deftest visual-replace-scope-default-to-from-point ()
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

(ert-deftest visual-replace-default-to-full-scope ()
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

(turtles-ert-deftest visual-replace-jump-forward-to-first-match ()
  (test-visual-replace-env
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (let ((testbuf (current-buffer)))
         (dotimes (i 100)
           (insert (format "this is text %d.\n" i)))
         (goto-char (point-min))
         (forward-line 3)
         (recenter)

         (turtles-with-minibuffer
             (call-interactively 'visual-replace)

           :keys "text SPC 40 TAB repl"
           (visual-replace--update-preview)
           (test-visual-run-idle-search-timers)

           (turtles-with-grab-buffer (:buf testbuf :faces test-visual-replace-faces)
             (should (equal "this is [text 40]*{repl}*."
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))))

       ;; The point is now at the first match.
       (should (equal "this is repl."
                      (buffer-substring-no-properties
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

(turtles-ert-deftest visual-replace-jump-backward-to-first-match ()
  (test-visual-replace-env
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (let ((testbuf (current-buffer)))
         (dotimes (i 100)
           (insert (format "this is text %d.\n" i)))
         (goto-char (point-max))
         (forward-line -3)
         (setq start-line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position)))
         (recenter)

         (turtles-with-minibuffer
             (call-interactively 'visual-replace)

           :command #'visual-replace-toggle-scope
           :keys "text SPC 5. TAB repl"
           (visual-replace--update-preview)
           (test-visual-run-idle-search-timers)

           (turtles-with-grab-buffer (:buf testbuf :faces test-visual-replace-faces)
             (should (equal "this is [text 5.]*{repl}*"
                            (buffer-substring (line-beginning-position)
                                              (line-end-position))))))

       ;; The point is now at the first match.
       (should (equal "this is repl"
                      (buffer-substring-no-properties
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

(turtles-ert-deftest visual-replace-restore-position-after-jump ()
  (test-visual-replace-env
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (let ((testbuf (current-buffer))
             (visual-replace-keep-initial-position t))
         (dotimes (i 100)
           (insert (format "this is text %d.\n" i)))
         (goto-char (point-min))
         (forward-line 3)
         (recenter)

         (turtles-with-minibuffer
             (call-interactively 'visual-replace)

           :keys "text SPC 40 TAB repl"
           (visual-replace--update-preview)
           (test-visual-run-idle-search-timers))

       ;; The point is at the original position
       (should (equal "this is text 3."
                      (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))))))

(turtles-ert-deftest visual-replace-give-up-looking-for-first-match ()
  (test-visual-replace-env
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (let ((testbuf (current-buffer))
             (visual-replace-max-size-for-search 1024))
         (dotimes (_ 1024)
           (insert "some text.\n"))
         (insert "the end.")
         (goto-char (point-min))
         (recenter)

         (turtles-with-minibuffer
             (call-interactively 'visual-replace)

           :keys "the SPC end TAB fin"
           (visual-replace--update-preview)
           (test-visual-run-idle-search-timers)

           ;; The match isn't visible, because it's too far according
           ;; to visual-replace-max-size-for-search.
           (turtles-with-grab-buffer (:buf testbuf)
             (goto-char (point-min))
             (should-not (search-forward "the end." nil 'noerror))))

       ;; The text was replaced
       (goto-char (point-max))
       (should (equal "fin." (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position))))))))

(ert-deftest visual-replace-small-ranges ()
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

(turtles-ert-deftest visual-replace-highlight-scope-from-point ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (dotimes (i 6)
         (insert (format "line %d.\n" i)))
       (goto-char (point-min))
       (forward-line 3)

       (should-not visual-replace-default-to-full-scope)
       (turtles-with-minibuffer
           (call-interactively 'visual-replace)

         :keys "foo TAB bar"
         (turtles-with-grab-buffer (:name "from-point")
           (should (equal "Replace from point: foo → bar" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "from-point is highlighted" :buf testbuf :faces '((visual-replace-region . "[]")))
           (should (equal (concat "line 0.\n"
                                  "line 1.\n"
                                  "line 2.\n"
                                  "[line 3.\n"
                                  "line 4.\n"
                                  "line 5.]")
                          (buffer-string))))

         :command #'visual-replace-toggle-scope
         (turtles-with-grab-buffer (:name "full")
           (should (equal "Replace in buffer: foo → bar" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "full is not highlighted" :buf testbuf :faces '((visual-replace-region . "[]")))
           (should (equal (concat "line 0.\n"
                                  "line 1.\n"
                                  "line 2.\n"
                                  "line 3.\n"
                                  "line 4.\n"
                                  "line 5.")
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-highlight-scope-region ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (dotimes (i 6)
         (insert (format "line %d.\n" i)))
       (goto-char (point-min))
       (forward-line 2)
       (set-mark (line-end-position 2))
       (should (region-active-p))
       (should-not visual-replace-default-to-full-scope)
       (turtles-with-minibuffer
           (call-interactively 'visual-replace)

         :keys "foo TAB bar"
         (turtles-with-grab-buffer (:name "in-region")
           (should (equal "Replace in region (2L): foo → bar" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "in-region is highlighted" :buf testbuf :faces '((visual-replace-region . "[]")))
           (should (equal (concat "line 0.\n"
                                  "line 1.\n"
                                  "[line 2.\n"
                                  "line 3.]\n"
                                  "line 4.\n"
                                  "line 5.")
                          (buffer-string))))

         :command #'visual-replace-toggle-scope
         (turtles-with-grab-buffer (:name "full")
           (should (equal "Replace in buffer: foo → bar" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "full is not highlighted" :buf testbuf :faces '((visual-replace-region . "[]")))
           (should (equal (concat "line 0.\n"
                                  "line 1.\n"
                                  "line 2.\n"
                                  "line 3.\n"
                                  "line 4.\n"
                                  "line 5.")
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-highlight-scope-rect-region ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (dotimes (i 6)
         (insert (format "line %d.\n" i)))
       (goto-char (point-min))
       (forward-line 2)
       (rectangle-mark-mode)
       (forward-line 2)
       (goto-char (1- (line-end-position)))

       (should (region-active-p))
       (should-not visual-replace-default-to-full-scope)
       (turtles-with-minibuffer
           (call-interactively 'visual-replace)

         :keys "foo TAB bar"
         (turtles-with-grab-buffer (:name "in-region")
           (should (equal "Replace in region (3L): foo → bar" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "in-region is highlighted" :buf testbuf :faces '((visual-replace-region . "[]")))
           (should (equal (concat "line 0.\n"
                                  "line 1.\n"
                                  "[line 2].\n"
                                  "[line 3].\n"
                                  "[line 4].\n"
                                  "line 5.")
                          (buffer-string))))

         :command #'visual-replace-toggle-scope
         (turtles-with-grab-buffer (:name "full")
           (should (equal "Replace in buffer: foo → bar" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "full is not highlighted" :buf testbuf :faces '((visual-replace-region . "[]")))
           (should (equal (concat "line 0.\n"
                                  "line 1.\n"
                                  "line 2.\n"
                                  "line 3.\n"
                                  "line 4.\n"
                                  "line 5.")
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-highlight-scope-rect-region-with-gaps ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

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

       (should (region-active-p))
       (should-not visual-replace-default-to-full-scope)
       (turtles-with-minibuffer
           (call-interactively 'visual-replace)

         :keys "foo TAB bar"
         (turtles-with-grab-buffer (:name "in-region")
           (should (equal "Replace in region (4L): foo → bar" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "in-region is highlighted" :buf testbuf :faces '((visual-replace-region . "[]")))
           (should (equal (concat "line 1.\n"
                         "     [line] 2.\n"
                         "3.   [    ]\n"
                         "     [line] 4.\n"
                         "line [5.  ]\n"
                         "line 6.")
                          (buffer-string)))))))))

(ert-deftest visual-replace-prev-next-match ()
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

(ert-deftest visual-replace-rect-replace ()
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
(ert-deftest visual-replace-apply-one-repeat ()
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

(ert-deftest visual-replace-apply-one-repeat-up-down ()
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

  (ert-deftest visual-replace-apply-one-repeat-continue ()
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

(ert-deftest visual-replace-from-isearch ()
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

(turtles-ert-deftest visual-replace-read-RET-twice ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       :keys "hello RET world RET"))))

(turtles-ert-deftest visual-replace-exit-immediately ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args)
                        (car (visual-replace-read))))))))

(turtles-ert-deftest visual-replace-exit-before-tab ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args)
                        (car (visual-replace-read))))
       :keys "hello"))))

(turtles-ert-deftest visual-replace-read-TAB-then-RET ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       :keys "hello TAB world RET"))))

(turtles-ert-deftest visual-replace-read-TAB-navigation ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello"
       (turtles-with-grab-buffer (:name "before TAB" :point "<>")
         (should (equal "Replace from point: hello<>" (buffer-string))))

       :keys "TAB"
       (turtles-with-grab-buffer (:name "1st TAB" :point "<>")
         (should (equal "Replace from point: hello → <>" (buffer-string))))

       :keys "world TAB"
       (turtles-with-grab-buffer (:name "2nd TAB" :point "<>")
         (should (equal "Replace from point: hello<> → world" (buffer-string))))

       :keys "TAB"
       (turtles-with-grab-buffer (:name "3rd TAB" :point "<>")
         (should (equal "Replace from point: hello → world<>" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-TAB-remember-pos ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello <left> TAB world <left>"
       (turtles-with-grab-buffer (:name "before TAB" :point "<>")
         (should (equal "Replace from point: hello → worl<>d" (buffer-string))))

       :keys "TAB"
       (turtles-with-grab-buffer (:name "1st TAB" :point "<>")
         (should (equal "Replace from point: hell<>o → world" (buffer-string))))

       :keys "TAB"
       (turtles-with-grab-buffer (:name "2nd TAB" :point "<>")
         (should (equal "Replace from point: hello → worl<>d" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-TAB-default-to-end ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world C-u 8 <left> TAB"
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: hello → world<>" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-regexp ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :regexp t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-regexp
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello →.* world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-regexp-with-case-default ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; If the default value is not handled properly, case-fold could
     ;; end up being nil when toggling another flag.
     (let ((case-fold-search t))
       (turtles-with-minibuffer
           (should (equal (visual-replace-make-args :from "hello" :to "world" :regexp t :case-fold t)
                          (car (visual-replace-read))))
         :keys "hello TAB world"
         :command #'visual-replace-toggle-regexp
         (turtles-with-grab-buffer ()
           (should (equal "Replace from point: hello →.* world" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-toggle-query ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :query t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-query
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello →? world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-regexp-query ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :query t :regexp t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-query
       :command #'visual-replace-toggle-regexp
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello →?.* world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-word ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :word t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-word
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello →w world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-substring-match-from-word ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read (visual-replace-make-args :word t)))))
       :keys "hello TAB world"
       :command #'visual-replace-substring-match
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello → world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-substring-match-from-regexp ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read (visual-replace-make-args :regexp t)))))
       :keys "hello TAB world"
       :command #'visual-replace-substring-match
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello → world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-word-and-regexp ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :word t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-word
       (turtles-with-grab-buffer (:name "word")
         (should (equal "Replace from point: hello →w world" (buffer-string))))

       ;; Turning on regexp turns off word
       :command #'visual-replace-toggle-regexp
       (turtles-with-grab-buffer (:name "word regexp")
         (should (equal "Replace from point: hello →.* world" (buffer-string))))

       ;; Turning off regexp again does not recover word
       :command #'visual-replace-toggle-regexp
       (turtles-with-grab-buffer (:name "neither word nor regexp")
         (should (equal "Replace from point: hello → world" (buffer-string))))

       :command #'visual-replace-toggle-word
       (turtles-with-grab-buffer (:name "word again")
         (should (equal "Replace from point: hello →w world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-args-describe ()
  (should (equal "Match Substrings, Case Fold: Enabled, Lax Whitespaces: Disabled"
                 (visual-replace-args--describe (visual-replace-make-args))))
  (should (equal "Match Substrings, Case Fold: Disabled, Lax Whitespaces: Disabled"
                 (visual-replace-args--describe (visual-replace-make-args :case-fold nil))))
  (should (equal "Match Substrings, Case Fold: Enabled, Lax Whitespaces: Enabled"
                 (visual-replace-args--describe (visual-replace-make-args :lax-ws-non-regexp t))))

  (should (equal "Match Words, Case Fold: Enabled, Lax Whitespaces: Disabled"
                 (visual-replace-args--describe (visual-replace-make-args :word t))))
  (should (equal "Match Words, Case Fold: Enabled, Lax Whitespaces: Enabled"
                 (visual-replace-args--describe (visual-replace-make-args :word t :lax-ws-non-regexp t))))

  (should (equal "Match Regexps, Case Fold: Enabled, Lax Whitespaces: Disabled"
                 (visual-replace-args--describe (visual-replace-make-args :regexp t))))
  (should (equal "Match Regexps, Case Fold: Enabled, Lax Whitespaces: Enabled"
                 (visual-replace-args--describe (visual-replace-make-args :regexp t :lax-ws-regexp t)))))

(turtles-ert-deftest visual-replace-read-default-case-fold ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :case-fold t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"))))

(turtles-ert-deftest visual-replace-read-toggle-case-fold ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :case-fold nil)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-case-fold
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello →c world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-case-fold-off-by-default ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (let ((case-fold-search nil))
       (turtles-with-minibuffer
           (should (equal (visual-replace-make-args :from "hello" :to "world" :case-fold t)
                          (car (visual-replace-read))))
         :keys "hello TAB world"
         :command #'visual-replace-toggle-case-fold
         (turtles-with-grab-buffer ()
           (should (equal "Replace from point: hello →i world" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-toggle-lax-ws ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args
                         :from "hello" :to "world" :lax-ws-non-regexp t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-lax-ws
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello →(lax ws) world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-lax-ws-and-regexp ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args
                         :from "hello" :to "world" :regexp t)
                        (car (visual-replace-read))))
       :keys "hello TAB world"
       :command #'visual-replace-toggle-regexp
       :command #'visual-replace-toggle-lax-ws
       (turtles-with-grab-buffer (:name "lax ws and regexp")
         (should (equal "Replace from point: hello →(lax ws).* world" (buffer-string))))

       :command #'visual-replace-toggle-lax-ws
       (turtles-with-grab-buffer (:name "regexp only")
         (should (equal "Replace from point: hello →.* world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-backwards ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args
                         :from "foo" :to "num\\#" :regexp t :backwards t)
                        (car (visual-replace-read))))
       :keys "foo TAB num\\#"
       :command #'visual-replace-toggle-regexp
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: foo →.* num\\#" (buffer-string))))

       :command #'visual-replace-toggle-backwards
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: foo →.*↩ num\\#" (buffer-string))))

       :command #'visual-replace-toggle-backwards
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: foo →.* num\\#" (buffer-string))))

       :command #'visual-replace-toggle-backwards
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: foo →.*↩ num\\#" (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-scope ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (insert "some content")
     (goto-char (point-min))

     (let (ranges)
       (turtles-with-minibuffer
           (setq ranges (nth 1 (visual-replace-read)))
         :keys "hello TAB world"
         :command #'visual-replace-toggle-scope)
       (should (equal ranges (list (cons (point-min) (point-max)))))))))

(turtles-ert-deftest visual-replace-read-toggle-scope-keeps-flags ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world"
       :command #'visual-replace-toggle-regexp
       :command #'visual-replace-toggle-query
       :command #'visual-replace-toggle-scope
       (turtles-with-grab-buffer ()
         (should (equal "Replace in buffer: hello →?.* world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-tab-keeps-flags ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world"
       :command #'visual-replace-toggle-regexp
       :command #'visual-replace-toggle-query
       :keys "TAB"
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello →?.* world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-toggle-scope-minibuffer-and-preview ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows)

       (insert "hello, world, hello, hello!")
       (goto-char (point-min))
       (search-forward "world")

       (turtles-with-minibuffer
           (visual-replace-read)
         :keys "hello"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "from point minibuffer")
           (should (equal "Replace from point: hello" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "from point" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "hello, world, [hello]*, [hello]!" (buffer-string))))

         :command #'visual-replace-toggle-scope
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "full buffer minibuffer")
           (should (equal "Replace in buffer: hello" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "full buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hello], world, [hello]*, [hello]!" (buffer-string))))

         :command #'visual-replace-toggle-scope
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "from point again minibuffer")
           (should (equal "Replace from point: hello" (buffer-string))))
         (turtles-with-grab-buffer
             (:name "from point again" :buf testbuf :faces test-visual-replace-faces)
           ;; This makes sure the extra matches are deleted
           (should (equal "hello, world, [hello]*, [hello]!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-switch-to-scope ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (dotimes (i 6)
       (insert (format "line %d\n" i)))
     (goto-char (point-min))
     (search-forward "line 2")
     (set-mark (match-beginning 0))
     (search-forward "line 5")
     (goto-char (point-min))

     (turtles-with-minibuffer
         (visual-replace-read)
       (turtles-with-grab-buffer ()
           (should (equal "Replace in region (2L):"
                          (buffer-string))))
       :command #'visual-replace-switch-to-full-scope
       (turtles-with-grab-buffer ()
           (should (equal "Replace in buffer:"
                          (buffer-string))))
       :command #'visual-replace-switch-to-from-point-scope
       (turtles-with-grab-buffer ()
           (should (equal "Replace from point:"
                          (buffer-string))))
       :command #'visual-replace-switch-to-full-scope
       (turtles-with-grab-buffer ()
           (should (equal "Replace in buffer:"
                          (buffer-string))))
       :command #'visual-replace-switch-to-region-scope
       (turtles-with-grab-buffer ()
           (should (equal "Replace in region (2L):"
                          (buffer-string))))))))

(turtles-ert-deftest visual-replace-forgets-setting ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world"
       :command #'visual-replace-toggle-regexp
       :command #'visual-replace-toggle-query
       :command #'visual-replace-toggle-scope)

     ;; settings from the first call should not reappear as default in
     ;; the second call
     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "foo" :to "bar")
                        (car (visual-replace-read))))
       :keys "foo TAB bar"))))

(defun test-visual-replace-setup-region ()
  "Define a region in the current test buffer."
  (insert "hello\nworld\n")
  (goto-char 1)
  (set-mark (point))
  (forward-line)
  (should (region-active-p)))

(turtles-ert-deftest visual-replace-read-toggle-scope-with-region-display ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (test-visual-replace-setup-region)
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world"
       (turtles-with-grab-buffer (:name "region")
         (should (equal "Replace in region (1L): hello → world"
                        (buffer-string))))

       :command #'visual-replace-toggle-scope
       (turtles-with-grab-buffer (:name "buffer")
         (should (equal "Replace in buffer: hello → world"
                        (buffer-string))))

       :command #'visual-replace-toggle-scope
       (turtles-with-grab-buffer (:name "region again")
         (should (equal "Replace in region (1L): hello → world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-read-toggle-scope-with-region ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (test-visual-replace-setup-region)
     (let (ranges)
       (turtles-with-minibuffer
           (setq ranges (nth 1 (visual-replace-read)))
         :keys "hello TAB world")
       (should (equal ranges (list (cons 1 7))))))))

(turtles-ert-deftest visual-replace-read-noncontiguous-region ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (insert "hello\nworld\nhello\nworld\n")
     (goto-char (point-min))
     (move-to-column 2)
     (rectangle-mark-mode 1)
     (goto-char (line-beginning-position 2))
     (move-to-column 4)

     (let (ranges)
       (turtles-with-minibuffer
           (setq ranges (nth 1 (visual-replace-read)))

         (turtles-with-grab-buffer ()
           (should (equal "Replace in region (2L):" (buffer-string))))
         :keys "he TAB ho")

     (should (equal ranges '((3 . 5) (9 . 11))))))))

(turtles-ert-deftest visual-replace-read-toggle-scope-with-region-then-buffer ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (test-visual-replace-setup-region)

     (let (ranges)
       (turtles-with-minibuffer
           (setq ranges (nth 1 (visual-replace-read)))
         :keys "hello TAB world"
         :command #'visual-replace-toggle-scope)

       (should (equal ranges (list (cons (point-min) (point-max)))))))))

(turtles-ert-deftest visual-replace-fields ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world"

       (should (equal (test-visual-replace-highlight-property
                       (minibuffer-contents) 'field 'search)
                      "[hello] world"))

       (should (equal (test-visual-replace-highlight-property
                       (minibuffer-contents) 'field 'replace)
                      "hello [world]"))))))

(turtles-ert-deftest visual-replace-fields-in-history-entry ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Fill in history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")

     ;; Recall history entry.
     (turtles-with-minibuffer
         (visual-replace-read)
       :command #'previous-history-element

       (should (equal (test-visual-replace-highlight-property
                       (minibuffer-contents) 'field 'search)
                      "[hello] world"))

       (should (equal (test-visual-replace-highlight-property
                       (minibuffer-contents) 'field 'replace)
                      "hello [world]"))))))

(turtles-ert-deftest visual-replace-kill ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world TAB C-a <right>"
       :command #'kill-line
       (turtles-with-grab-buffer (:name "1st kill")
         (should (equal "Replace from point: h → world"
                        (buffer-string))))

       :keys "TAB C-a <right>"
       :command #'kill-line
       (turtles-with-grab-buffer (:name "2nd kill")
         (should (equal "Replace from point: h → w"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-kill-no-separator ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello <left>"
       :command #'kill-line
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: hell<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-kill-whole-line ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (save-excursion (insert "from buffer"))

     (turtles-with-minibuffer
         (visual-replace-read)

       :keys "hello TAB world"
       (turtles-with-grab-buffer (:name "before kill" :point "<>")
         (should (equal "Replace from point: hello → world<>"
                        (buffer-string))))

       :command #'kill-whole-line
       (turtles-with-grab-buffer (:name "after 1st kill" :point "<>")
         (should (equal "Replace from point: hello → <>"
                        (buffer-string))))

       :keys "TAB"
       :command #'kill-whole-line
       (turtles-with-grab-buffer (:name "after 2nd kill" :point "<>")
         (should (equal "Replace from point: <>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-in-match-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (save-excursion (insert "from buffer"))

     (turtles-with-minibuffer
         (visual-replace-read)
       :command #'visual-replace-yank
       :keys "TAB"
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: from →"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-symbol-in-match-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (save-excursion (insert "from-a buffer"))

     (turtles-with-minibuffer
         (visual-replace-read)
       :command #'visual-replace-yank
       :keys "TAB"
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: from-a →"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-multiple-yank-in-match-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (save-excursion (insert "from current buffer"))

     (turtles-with-minibuffer
         (visual-replace-read)
       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:name "1st yank" :point "<>")
         (should (equal "Replace from point: from<>"
                        (buffer-string))))

       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:name "2nd yank" :point "<>")
         (should (equal "Replace from point: from current<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-with-symbols ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (insert "(progn (when some-test some-value))")
     (goto-char (point-min))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "TAB world TAB"

       ;; After typing "(when", the pointer goes to the beginning of
       ;; the first match, then moves as more and more text is added
       ;; by visual-replace-yank.
       :keys "(when"
       (visual-replace--update-preview)
       (test-visual-run-idle-search-timers)
       (turtles-with-grab-buffer (:name "after when")
         (should (equal "Replace from point: (when → world"
                        (buffer-string))))

       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:name "1st yank")
         (should (equal "Replace from point: (when some-test → world"
                        (buffer-string))))

       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:name "2nd yank")
         (should (equal "Replace from point: (when some-test some-value → world"
                        (buffer-string))))

       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:name "3rd yank")
         (should (equal "Replace from point: (when some-test some-value) → world"
                        (buffer-string))))

       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:name "4th yank")
         (should (equal "Replace from point: (when some-test some-value)) → world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-in-match-field-with-prompt ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (save-excursion (insert "from buffer"))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "TAB world TAB"
       :command #'visual-replace-yank
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: from → world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-in-match-field-after-next-match ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (save-excursion (insert "from-buffer\nfrom-region\nfrom-point\n"))

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "from"
       :command #'visual-replace-next-match
       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: from-region<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-in-replacement-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (kill-new "from kill-ring")

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB"
       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: hello → hello<>"
                        (buffer-string))))))))


(turtles-ert-deftest visual-replace-multiple-yank-in-replacement-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (kill-new "from kill-ring")

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB"
       :command #'visual-replace-yank
       :keys "SPC"
       :command #'visual-replace-yank
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: hello → hello hello<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-pop-in-match-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (kill-new "prev3")
     (kill-new "prev2")
     (kill-new "prev1")

     (turtles-with-minibuffer
         (visual-replace-read)
       :command #'visual-replace-yank-pop
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: prev1<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-multiple-yank-pop-in-match-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (kill-new "prev3")
     (kill-new "prev2")
     (kill-new "prev1")

     (turtles-with-minibuffer
         (visual-replace-read)
       :command #'visual-replace-yank-pop
       :command #'visual-replace-yank-pop
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: prev2<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-yank-then-yank-pop-in-replacement-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (kill-new "prev3")
     (kill-new "prev2")
     (kill-new "prev1")

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB"
       :command #'visual-replace-yank
       :keys "SPC"
       :command #'visual-replace-yank-pop
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point: hello → hello prev1<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-multiple-yank-pop-in-replacement-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (kill-new "prev3")
     (kill-new "prev2")
     (kill-new "prev1")

     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB"
       :command #'visual-replace-yank-pop
       :command #'visual-replace-yank-pop
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: hello → prev2"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-history-by-default ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       :keys "RET"))))

(turtles-ert-deftest visual-replace-default-but-no-history ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (should-error (visual-replace-read))
       :keys "RET"))))

(turtles-ert-deftest visual-replace-history-by-default-despite-separator ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       :keys "TAB RET"))))

(turtles-ert-deftest visual-replace-history-by-default-despite-toggle ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       :command #'visual-replace-toggle-regexp
       :keys "RET"))))

(turtles-ert-deftest visual-replace-history ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello → world]:" (buffer-string))))
       :command #'previous-history-element
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point [hello → world]: <>hello → world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-complete-from-history ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "foo TAB bar RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [foo → bar]:" (buffer-string))))
       :keys "h"
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point [foo → bar]: h<>" (buffer-string))))
       :command #'previous-complete-history-element
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point [foo → bar]: h<>ello → world" (buffer-string))))))))

(turtles-ert-deftest visual-replace-history-regex ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       (visual-replace-toggle-regexp)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world" :regexp t)
                        (car (visual-replace-read))))
       :command #'previous-complete-history-element
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →.* world]: hello →.* world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-history-regex-edit ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       (visual-replace-toggle-regexp)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "worldx" :regexp t)
                        (car (visual-replace-read))))
       :command #'previous-complete-history-element
       :keys "TAB x"
       (turtles-with-grab-buffer (:point "<>")
         (should (equal "Replace from point [hello →.* world]: hello →.* worldx<>"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-history-regex-toggle ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       (visual-replace-toggle-regexp)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args :from "hello" :to "world")
                        (car (visual-replace-read))))
       :command #'previous-complete-history-element
       :command #'visual-replace-toggle-regexp
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →.* world]: hello → world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-history-regex-query ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       (visual-replace-toggle-regexp)
       (visual-replace-toggle-query)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args
                         :from "hello" :to "world" :regexp t :query t)
                        (car (visual-replace-read))))
       :command #'previous-complete-history-element
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →?.* world]: hello →?.* world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-history-regex-toggle-2 ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       (visual-replace-toggle-regexp)
       (visual-replace-toggle-query)
       :keys "hello TAB world RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args
                         :from "hello" :to "world" :regexp t)
                        (car (visual-replace-read))))
       :command #'previous-complete-history-element
       :command #'visual-replace-toggle-query
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →?.* world]: hello →.* world"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-history-multiple ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       (visual-replace-toggle-regexp)
       :keys "hello TAB foo RET")
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB bar RET")
     (turtles-with-minibuffer
         (visual-replace-read)
       (visual-replace-toggle-query)
       :keys "hello TAB query RET")

     (turtles-with-minibuffer
         (should (equal (visual-replace-make-args
                         :from "hello" :to "foo" :regexp t)
                        (car (visual-replace-read))))
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →? query]:"
                        (buffer-string))))

       :command #'previous-complete-history-element
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →? query]: hello →? query"
                        (buffer-string))))

       :command #'previous-complete-history-element
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →? query]: hello → bar"
                        (buffer-string))))

       :command #'previous-complete-history-element
       (turtles-with-grab-buffer ()
         (should (equal "Replace from point [hello →? query]: hello →.* foo"
                        (buffer-string))))))))

(turtles-ert-deftest visual-replace-keep-incomplete-in-history ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     ;; Incomplete
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "foo TAB bar"
       (setq quit-flag t))
     (setq quit-flag nil)

     ;; Continue from incomplete
     (should
      (equal
       (visual-replace-make-args :from "faafoo" :to "bar")
       (car (turtles-with-minibuffer
                (visual-replace-read)
              (turtles-with-grab-buffer ()
                (should (equal "Replace from point:" (buffer-string))))
              :command #'previous-history-element
              (turtles-with-grab-buffer (:point "<>")
                (should (equal "Replace from point: <>foo → bar" (buffer-string))))
              :keys "faa"
              (turtles-with-grab-buffer (:point "<>")
                (should (equal "Replace from point: faa<>foo → bar" (buffer-string)))))))))))

(turtles-ert-deftest visual-replace-keep-incomplete-with-only-match-field ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     ;; Incomplete
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "foo"
       (setq quit-flag t))
     (setq quit-flag nil)

     ;; Continue from incomplete
     (should
      (equal
       (visual-replace-make-args :from "foo" :to "bar")
       (car (turtles-with-minibuffer
                (visual-replace-read)
              (turtles-with-grab-buffer ()
                (should (equal "Replace from point:" (buffer-string))))
              :command #'previous-history-element
              (turtles-with-grab-buffer (:point "<>")
                (should (equal "Replace from point: <>foo" (buffer-string))))
              :keys "TAB bar"
              (turtles-with-grab-buffer ()
                (should (equal "Replace from point: foo → bar" (buffer-string)))))))))))

(turtles-ert-deftest visual-replace-incomplete-then-history ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")

     ;; Incomplete
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "foo TAB bar"
       (setq quit-flag t))
     (setq quit-flag nil)

     ;; Access real history
     (should
      (equal
       (visual-replace-make-args :from "hello" :to "world")
       (car (turtles-with-minibuffer
                (visual-replace-read)
              (turtles-with-grab-buffer ()
                (should (equal "Replace from point [hello → world]:" (buffer-string))))
              :command #'previous-history-element
              (turtles-with-grab-buffer ()
                (should (equal "Replace from point [hello → world]: foo → bar" (buffer-string))))
              :command #'previous-history-element
              (turtles-with-grab-buffer ()
                (should (equal "Replace from point [hello → world]: hello → world" (buffer-string)))))))))))

(turtles-ert-deftest visual-replace-incomplete-but-default-from-history ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     ;; Added to history
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "hello TAB world RET")

     ;; Incomplete
     (turtles-with-minibuffer
         (visual-replace-read)
       :keys "foo TAB bar"
       (setq quit-flag t))
     (setq quit-flag nil)

     ;; Accept default, which should be from history
     (should
      (equal
       (visual-replace-make-args :from "hello" :to "world")
       (car (turtles-with-minibuffer
                (visual-replace-read)
              (turtles-with-grab-buffer ()
                (should (equal "Replace from point [hello → world]:" (buffer-string))))
              :keys "RET")))))))

(turtles-ert-deftest visual-replace-warn-newline ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (ert-with-message-capture captured-message
           (visual-replace-read)
           (should (member
                    "Note: ‘\\n’ here doesn’t match a newline; to do that, type C-q C-j instead"
                    (string-split captured-message "\n"))))

       :keys "a\\n RET b"
       (visual-replace-toggle-regexp)))))

(turtles-ert-deftest visual-replace-warn-tab ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (ert-with-message-capture captured-message
           (visual-replace-read)
           (should (member "Note: ‘\\t’ here doesn’t match a tab; to do that, just type TAB"
                           (string-split captured-message "\n"))))

       :keys "a\\t RET b"
       (visual-replace-toggle-regexp)))))

(turtles-ert-deftest visual-replace-warn-only-for-regexp ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (ert-with-message-capture captured-message
           (visual-replace-read)
           (should-not (delq nil
                             (mapcar (lambda (msg)
                                       (string-prefix-p "Note: " msg))
                                     (string-split captured-message "\n")))))

       :keys "a\\n RET b"))))

(turtles-ert-deftest visual-replace-warn-the-first-time ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (ert-with-message-capture captured-message
           (visual-replace-read)
           (should (member
                    "Note: ‘\\n’ here doesn’t match a newline; to do that, type C-q C-j instead"
                    (string-split captured-message "\n"))))

       :keys "a\\n RET b"
       (visual-replace-toggle-regexp))

     (turtles-with-minibuffer
         (ert-with-message-capture captured-message
           (visual-replace-read)
           (should-not (delq nil
                             (mapcar (lambda (msg)
                                       (string-prefix-p "Note: " msg))
                                     (string-split captured-message "\n")))))

       :keys "RET"
       (visual-replace-toggle-regexp)))))

(turtles-ert-deftest visual-replace-kill-and-yank-separator ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (visual-replace-read)

       :keys "hello TAB"
       (turtles-with-grab-buffer (:name "initial")
         (should (equal "Replace from point: hello →" (buffer-string))))

       :keys "TAB C-a" ;; go to start of 1st field
       (kill-line)
       (turtles-with-grab-buffer (:name "kill-line in 1st field")
         (should (equal "Replace from point:  →" (buffer-string))))

       (yank)
       (turtles-with-grab-buffer (:name "after yank")
         (should (equal "Replace from point: hello →" (buffer-string))))

       ;; Don't complain about "nothing to replace"
       (setq quit-flag t)))))

(turtles-ert-deftest visual-replace-initial-input ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (visual-replace-read (visual-replace-make-args :from "initial"))

       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: initial" (buffer-string))))

       ;; Don't complain about "nothing to replace"
       (setq quit-flag t)))))

(turtles-ert-deftest visual-replace-initial-input-complete ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (visual-replace-read (visual-replace-make-args :from "foo" :to "bar"))

       (turtles-with-grab-buffer ()
         (should (equal "Replace from point: foo → bar" (buffer-string))))

       ;; Don't complain about "nothing to replace"
       (setq quit-flag t)))))

(turtles-ert-deftest visual-replace-initial-word ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (visual-replace-read (visual-replace-make-args :word t))

       (turtles-with-grab-buffer ()
         (should (equal "Replace from point:  →w" (buffer-string))))

       ;; Don't complain about "nothing to replace"
       (setq quit-flag t)))))

(turtles-ert-deftest visual-replace-initial-regexp ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (visual-replace-read (visual-replace-make-args :regexp t))

       (turtles-with-grab-buffer ()
         (should (equal "Replace from point:  →.*" (buffer-string))))

       ;; Don't complain about "nothing to replace"
       (setq quit-flag t)))))

(turtles-ert-deftest visual-replace-initial-query ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (turtles-with-minibuffer
         (visual-replace-read (visual-replace-make-args :query t))

       (turtles-with-grab-buffer ()
         (should (equal "Replace from point:  →?" (buffer-string))))

       ;; Don't complain about "nothing to replace"
       (setq quit-flag t)))))

(turtles-ert-deftest visual-replace-initial-case-fold-enable ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (let ((case-fold-search nil))
       (turtles-with-minibuffer
           (visual-replace-read (visual-replace-make-args :case-fold t))

         (turtles-with-grab-buffer ()
           (should (equal "Replace from point:  →i" (buffer-string))))

         ;; Don't complain about "nothing to replace"
         (setq quit-flag t))))))

(turtles-ert-deftest visual-replace-initial-case-fold-disable ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (let ((case-fold-search t))
       (turtles-with-minibuffer
           (visual-replace-read (visual-replace-make-args :case-fold nil))

         (turtles-with-grab-buffer ()
           (should (equal "Replace from point:  →c" (buffer-string))))

         ;; Don't complain about "nothing to replace"
         (setq quit-flag t))))))

(turtles-ert-deftest visual-replace-initial-case-fold-default ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))

     (turtles-with-minibuffer
         (visual-replace-read)

       (turtles-with-grab-buffer ()
         (should (equal "Replace from point:" (buffer-string))))

       ;; Don't complain about "nothing to replace"
       (setq quit-flag t)))))

(turtles-ert-deftest visual-replace-preview ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hel"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer
             (:name "hel" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hel]*lo, world, [hel]lo, [hel]lo!" (buffer-string))))

         :keys "l"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer
             (:name "hell" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hell]*o, world, [hell]o, [hell]o!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-case-fold ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Hello, world, hello, heLLO!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hello"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer
             (:name "on" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[Hello]*, world, [hello], [heLLO]!" (buffer-string))))

         (visual-replace-toggle-case-fold)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer
             (:name "off" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "Hello, world, [hello]*, heLLO!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-case-fold-uppercase ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Hello, world, hello, HELLO!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "HELLO"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer
             (:name "on" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "Hello, world, hello, [HELLO]*!" (buffer-string))))

         (visual-replace-toggle-case-fold)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer
             (:name "off" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "Hello, world, hello, [HELLO]*!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-delete-replace ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hell TAB hul"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer
             (:buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hell]*{hul}*o, world, [hell]{hul}o, [hell]{hul}o!"
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-regex ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hel+"
         (visual-replace-toggle-regexp)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "Replace from point: hel+ →.*" (buffer-string))))
         (turtles-with-grab-buffer (:name "buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hell]*o, world, [hell]o, [hell]o!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-lax-ws ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello   world!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hello SPC world"
         (visual-replace-toggle-lax-ws)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "Replace from point: hello world →(lax ws)" (buffer-string))))
         (turtles-with-grab-buffer (:name "buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hello   world]*!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-bad-regex ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (call-interactively 'visual-replace)

         ;; This is just a smoke test. \b\b\b matches empty strings, which
         ;; cannot be displayed and might cause some implementations to
         ;; enter an infinite loop.
         (visual-replace-toggle-regexp)
         :keys "\\b\\b\\b"
         (visual-replace--update-preview)

         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "Replace from point: \\b\\b\\b →.*" (buffer-string))))
         (turtles-with-grab-buffer (:name "buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "hello, world, hello" (buffer-string)))))
       (should (equal "hello, world, hello" (buffer-string)))))))

(turtles-ert-deftest visual-replace-preview-regex-eval ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "h\\(el+\\) TAB \\#\\,(upcase SPC \\1)"
         (visual-replace-toggle-regexp)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "Replace from point: h\\(el+\\) →.* \\#\\,(upcase \\1)"
                          (buffer-string))))
         (turtles-with-grab-buffer (:name "buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hell]*{0ELL}*o, world, [hell]{1ELL}o"
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-skip-readonly ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, ")
     (let ((start-read-only (point))
           (end-read-only nil))
       (insert "hello")
       (setq end-read-only (point))
       (insert ", hello!")
       (set-text-properties start-read-only end-read-only '(read-only t)))
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (call-interactively #'visual-replace)

         :keys "hel TAB hol"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hel]*{hol}*lo, world, hello, [hel]{hol}lo!"
                          (buffer-string))))
         (exit-minibuffer))

       (should (equal
                "hollo, world, hello, hollo!"
                (buffer-string)))))))

(turtles-ert-deftest visual-replace-disable-preview-if-too-short ()
  (test-visual-replace-env
   (let ((visual-replace-min-length 3)
         (testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hel"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "3 chars" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hel]*lo, world, [hel]lo, [hel]lo!"
                          (buffer-string))))

         :keys "DEL"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "2 chars" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "hello, world, hello, hello!"
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-customize-min-length ()
  (test-visual-replace-env
   (let ((visual-replace-min-length 0)
         (testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "h"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "1 char" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[h]*ello, world, [h]ello, [h]ello!"
                          (buffer-string))))

         :keys "e"
         (visual-replace--update-preview)

         (turtles-with-grab-buffer (:name "2 chars" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[he]*llo, world, [he]llo, [he]llo!"
                          (buffer-string)))))))))

(ert-deftest visual-replace-read-read-only-buffer ()
  (test-visual-replace-env
   (insert "foo")
   (read-only-mode)
   (should-error (visual-replace-read))))

(turtles-ert-deftest visual-replace-preview-highlight-match-at-point ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hel TAB hu"
         (visual-replace--update-preview)

         (turtles-with-grab-buffer
             (:name "preview 1" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hel]*{hu}*lo, world, [hel]{hu}lo, [hel]{hu}lo!"
                          (buffer-string))))

         (visual-replace-next-match)
         (visual-replace--update-preview)

         (turtles-with-grab-buffer
             (:name "preview 2" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hel]{hu}lo, world, [hel]*{hu}*lo, [hel]{hu}lo!"
                          (buffer-string))))

         (visual-replace-prev-match)
         (visual-replace--update-preview)

         (turtles-with-grab-buffer
             (:name "preview 3" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "[hel]*{hu}*lo, world, [hel]{hu}lo, [hel]{hu}lo!"
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-apply-one ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

           :keys "f TAB l"
           :command #'visual-replace-apply-one
           (turtles-with-grab-buffer (:name "1st apply" :buf testbuf)
             (should (equal "Lee fi fo fum!" (buffer-string))))

           :command #'visual-replace-next-match
           :command #'visual-replace-apply-one
           :command #'visual-replace-apply-one
           (turtles-with-grab-buffer (:name "at end" :buf testbuf)
             (should (equal "Lee fi lo lum!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-apply-multiple ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

           :keys "f TAB l ESC 3"
           :command #'visual-replace-apply-one
           (turtles-with-grab-buffer (:buf testbuf)
             (should (equal "Lee li lo fum!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-undo ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "f TAB l"
         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "before undo" :buf testbuf)
           (should (equal "Lee li fo fum!" (buffer-string))))

         :command #'visual-replace-undo
         (turtles-with-grab-buffer (:name "after undo 1" :buf testbuf)
           (should (equal "Lee fi fo fum!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-undo-further ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "f TAB l"
         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "before undo" :buf testbuf)
           (should (equal "Lee li fo fum!" (buffer-string))))

         :command #'visual-replace-undo
         :command #'visual-replace-undo
         (turtles-with-grab-buffer (:name "after undo 2" :buf testbuf)
           (should (equal "Fee fi fo fum!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-undo-multiple ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "f TAB l"
         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "before undo" :buf testbuf)
           (should (equal "Lee li lo fum!" (buffer-string))))

         :command #'visual-replace-undo
         :command #'visual-replace-undo
         (turtles-with-grab-buffer (:name "after undo" :buf testbuf)
           (should (equal "Lee fi fo fum!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-undo-everything ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "f TAB l"
         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "before undo" :buf testbuf)
           (should (equal "Lee li lo fum!" (buffer-string))))

         :keys "ESC 1000"
         :command #'visual-replace-undo

         ;; Undo should not have reverted past visual-replace; the
         ;; text should still be there.
         (turtles-with-grab-buffer (:name "after undo" :buf testbuf)
           (should (equal "Fee fi fo fum!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-apply-multiple-undo-once ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "f TAB l"
         :command #'visual-replace-apply-one
         :keys "ESC 2"
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "before undo" :buf testbuf)
           (should (equal "Lee li lo fum!" (buffer-string))))

         :command #'visual-replace-undo
         ;; Undo undoes 2 of the 3 replacements executed, because they
         ;; were executed in one command.
         (turtles-with-grab-buffer (:name "after undo" :buf testbuf)
           (should (equal "Lee fi fo fum!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-apply-undo-everything-then-redo ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "foo1 foo2 foo3 foo4")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "foo TAB bar"
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "after apply" :buf testbuf :point "<>" :faces test-visual-replace-faces)
           (should (equal "bar1 <>[foo]*{bar}*2 [foo]{bar}3 [foo]{bar}4" (buffer-string))))

         :command #'visual-replace-undo
         (turtles-with-grab-buffer (:name "after undo" :buf testbuf :point "<>" :faces test-visual-replace-faces)
           (should (equal "<>[foo]*{bar}*1 [foo]{bar}2 [foo]{bar}3 [foo]{bar}4" (buffer-string))))

         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         :command #'visual-replace-undo
         ;; Executing commands using M-x guarantees that undo breaks
         ;; are applied as they would normally be.
         (turtles-with-grab-buffer (:name "after second undo" :buf testbuf :point "<>" :faces test-visual-replace-faces)
           (should (equal "bar1 <>[foo]*{bar}*2 [foo]{bar}3 [foo]{bar}4" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-apply-one-start-recursive ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "foo bar foo foo")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

           :keys "foo TAB foor"
           (visual-replace--update-preview)
           :command #'visual-replace-apply-one
           (visual-replace--update-preview)
           (turtles-with-grab-buffer (:name "1st" :buf testbuf :faces test-visual-replace-faces :point "<>")
             (should (equal "[foo]{foor}r bar <>[foo]*{foor}* [foo]{foor}" (buffer-string))))

           :command #'visual-replace-apply-one
           (visual-replace--update-preview)
           (turtles-with-grab-buffer (:name "2nd" :buf testbuf :faces test-visual-replace-faces :point "<>")
             (should (equal "[foo]{foor}r bar [foo]{foor}r <>[foo]*{foor}*" (buffer-string))))

           :command #'visual-replace-apply-one
           (visual-replace--update-preview)
           (turtles-with-grab-buffer
               (:name "final"
                      :buf testbuf
                      ;; Emacs 26 doesn't highlight the match, but
                      ;; later versions do. The difference doesn't
                      ;; matter, so leave it out from the test.
                      :faces test-visual-replace-faces-no-highlight
                      :point "<>")
             (should (equal "[foo]{foor}r bar [foo]{foor}r [foo]{foor}r<>" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-apply-one-end-recursive ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "foo bar foo foo")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

           :keys "foo TAB rfoo"
           (visual-replace--update-preview)
           :command #'visual-replace-apply-one
           (visual-replace--update-preview)
           (turtles-with-grab-buffer (:name "1st" :buf testbuf :faces test-visual-replace-faces :point "<>")
             (should (equal "r[foo]{rfoo} bar <>[foo]*{rfoo}* [foo]{rfoo}" (buffer-string))))

           :command #'visual-replace-apply-one
           (visual-replace--update-preview)
           (turtles-with-grab-buffer (:name "2nd" :buf testbuf :faces test-visual-replace-faces :point "<>")
             (should (equal "r[foo]{rfoo} bar r[foo]{rfoo} <>[foo]*{rfoo}*" (buffer-string))))

           :command #'visual-replace-apply-one
           (visual-replace--update-preview)
           (turtles-with-grab-buffer
               (:name "final"
                      :buf testbuf
                      ;; Emacs 26 doesn't highlight the match, but other
                      ;; version do. The difference doesn't matter, so leave it out from the test.
                      :faces test-visual-replace-faces-no-highlight
                      :point "<>")
             (should (equal "r[foo]{rfoo} bar r[foo]{rfoo} r[foo]{rfoo}<>" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-apply-one-last-match ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "foo foo foo foo")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

           :keys "foo TAB bar"
           (visual-replace--update-preview)
           :command #'visual-replace-apply-one
           :command #'visual-replace-next-match
           :command #'visual-replace-next-match
           :command #'visual-replace-apply-one
           (visual-replace--update-preview)
           (turtles-with-grab-buffer
            (:buf testbuf :faces test-visual-replace-faces-no-highlight :point "<>")
            (should (equal "bar [foo]{bar} [foo]{bar} bar<>" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-display-total ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t))
     (insert "I say, hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "he"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "not enough chars")
           (should (equal "Replace from point: he"
                          (buffer-string))))

         :keys "ll"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "hell")
           (should (equal "[1/3] Replace from point: hell"
                          (buffer-string))))
         (turtles-with-grab-buffer
             (:name "hell preview" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "I say, [hell]*o, world, [hell]o, [hell]o!"
                          (buffer-string))))

         )))))

(turtles-ert-deftest visual-replace-read-display-index ()
  (test-visual-replace-env
   (let ((visual-replace-display-total t))
     (insert "I say, hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hello"
         (visual-replace-next-match)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "1/3")
           (should (equal "[1/3] Replace from point: hello"
                          (buffer-string))))

         (visual-replace-next-match)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "2/3")
           (should (equal "[2/3] Replace from point: hello"
                          (buffer-string))))

         (visual-replace-next-match)
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "3/3")
           (should (equal "[3/3] Replace from point: hello"
                          (buffer-string))))

         (should-error (visual-replace-next-match)))))))

(turtles-ert-deftest visual-replace-read-display-total-update ()
  (test-visual-replace-env
   (let ((visual-replace-display-total t))
     (insert "I say, hello, world, hellow, hellow!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hello"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "hello")
           (should (equal "[1/3] Replace from point: hello"
                          (buffer-string))))

         :keys "w"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "hellow")
           (should (equal "[1/2] Replace from point: hellow"
                          (buffer-string))))


         :keys "DEL ,"
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "hello,")
           (should (equal "[1/1] Replace from point: hello,"
                          (buffer-string))))

         :keys "DEL ."
         (visual-replace--update-preview)
         (turtles-with-grab-buffer (:name "hello.")
           (should (equal "[0] Replace from point: hello."
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-display-total-too-short ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t))
     (insert "I say, hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hell"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "minibuffer with total")
           (should (equal "[1/3] Replace from point: hell" (buffer-string))))

         (turtles-with-grab-buffer (:name "buffer with matches" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "I say, [hell]*o, world, [hell]o, [hell]o!" (buffer-string))))

         ;; "he" is too short, it won't be previewed or counted
         :keys "DEL DEL"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "minibuffer too short")
           (should (equal "Replace from point: he" (buffer-string))))

         (turtles-with-grab-buffer (:name "buffer too short" :buf testbuf :faces test-visual-replace-faces)
           (should (equal "I say, hello, world, hello, hello!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-read-display-total-large-buffer ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t)
         (visual-replace-max-matches-for-total 1000))
     (dotimes (i 900)
       (insert (format "some text%d\n" i)))
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "text"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "[1/900] Replace from point: text" (buffer-string))))

         ;; All matches must be highlighted
         (with-current-buffer testbuf
           (save-excursion
             (goto-char (point-min))
             (while (search-forward "text" nil 'noerror)
               (unless (memq
                        (get-char-property (match-beginning 0) 'face) '(visual-replace-match-highlight
                                                                        visual-replace-match))
                 (error "Should have been highlighted: %s"
                        (buffer-substring (line-beginning-position)
                                          (line-end-position))))))))))))


(turtles-ert-deftest visual-replace-read-display-total-too-many-matches ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t)
         (visual-replace-max-matches-for-total 100))
     (dotimes (i 300)
       (insert (format "some text%d\n" i)))
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "text"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "Replace from point: text" (buffer-string))))

         ;; All visible text must be highlighted
         (turtles-with-grab-buffer (:name "buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal (concat "some [text]*0\n"
                                  "some [text]1\n"
                                  "some [text]2\n"
                                  "some [text]3\n"
                                  "some [text]4\n"
                                  "some [text]5\n"
                                  "some [text]6\n"
                                  "some [text]7\n"
                                  "some [text]8\n"
                                  "some [text]9\n"
                                  "some [text]10\n"
                                  "some [text]11\n"
                                  "some [text]12\n"
                                  "some [text]13\n"
                                  "some [text]14\n"
                                  "some [text]15\n"
                                  "some [text]16\n"
                                  "some [text]17\n"
                                  "some [text]18\n"
                                  "some [text]19\n"
                                  "some [text]20\n"
                                  "some [text]21")
                          (buffer-string))))

         ;; Not all matches should have been highlighted.
         (with-current-buffer testbuf
           (save-excursion
             (goto-char (point-min))
             (search-forward "some text299")
             (should-not (memq (get-char-property (match-beginning 0) 'face)
                               '(visual-replace-match visual-replace-match-highlight))))))))))

(turtles-ert-deftest visual-replace-read-display-total-too-large ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t)
         (visual-replace-max-matches-for-total 1000)
         (visual-replace-max-size-for-search 1024))
     (dotimes (i 300)
       (insert (format "some text%d\n" i)))
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "text"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "Replace from point: text" (buffer-string))))

         ;; All visible text must be highlighted
         (turtles-with-grab-buffer (:name "buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal (concat "some [text]*0\n"
                                  "some [text]1\n"
                                  "some [text]2\n"
                                  "some [text]3\n"
                                  "some [text]4\n"
                                  "some [text]5\n"
                                  "some [text]6\n"
                                  "some [text]7\n"
                                  "some [text]8\n"
                                  "some [text]9\n"
                                  "some [text]10\n"
                                  "some [text]11\n"
                                  "some [text]12\n"
                                  "some [text]13\n"
                                  "some [text]14\n"
                                  "some [text]15\n"
                                  "some [text]16\n"
                                  "some [text]17\n"
                                  "some [text]18\n"
                                  "some [text]19\n"
                                  "some [text]20\n"
                                  "some [text]21")
                          (buffer-string))))

         ;; Not all matches should have been highlighted.
         (with-current-buffer testbuf
           (save-excursion
             (goto-char (point-min))
             (search-forward "some text299")
             (should-not (memq (get-char-property (match-beginning 0) 'face)
                               '(visual-replace-match visual-replace-match-highlight))))))))))

(turtles-ert-deftest visual-replace-read-display-total-in-region-buffer-too-large ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t)
         (visual-replace-max-matches-for-total 1000)
         (visual-replace-max-size-for-search 1024))
     (dotimes (i 300)
       (insert (format "some text%d\n" i)))
     (goto-char (point-min))
     (forward-line 10)
     (set-mark (point))
     (forward-line 5)

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "text"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "minibuffer")
           (should (equal "[5/5] Replace in region (5L): text" (buffer-string))))

         (turtles-with-grab-buffer (:name "buffer" :buf testbuf :faces test-visual-replace-faces)
           (should (equal (concat "some text0\n"
                                  "some text1\n"
                                  "some text2\n"
                                  "some text3\n"
                                  "some text4\n"
                                  "some text5\n"
                                  "some text6\n"
                                  "some text7\n"
                                  "some text8\n"
                                  "some text9\n"
                                  "some [text]10\n"
                                  "some [text]11\n"
                                  "some [text]12\n"
                                  "some [text]13\n"
                                  "some [text]*14\n"
                                  "some text15\n"
                                  "some text16\n"
                                  "some text17\n"
                                  "some text18\n"
                                  "some text19\n"
                                  "some text20\n"
                                  "some text21")
                          (buffer-string)))))))))

(turtles-ert-deftest visual-replace-preview-display-window ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hello"

         ;; Hide the test buffer
         (select-window (display-buffer (get-scratch-buffer-create)))
         (delete-other-windows)

         ;; This should redisplay the test buffer
         (visual-replace-next-match)

         (should (get-buffer-window testbuf)))))))

(turtles-ert-deftest visual-replace-read-toggle-query-from-hook ()
  (test-visual-replace-env
   (add-hook 'visual-replace-defaults-hook #'visual-replace-toggle-query)

   (turtles-with-minibuffer
       (should (equal
                (visual-replace-make-args :from "hello" :to "world" :query t)
                (car (visual-replace-read))))

     :keys "hello TAB world")))

(turtles-ert-deftest visual-replace-read-toggle-word-from-hook ()
  (test-visual-replace-env
   (add-hook 'visual-replace-defaults-hook #'visual-replace-toggle-word)

   (turtles-with-minibuffer
       (should (equal
                (visual-replace-make-args :from "hello" :to "world" :word t)
                (car (visual-replace-read))))

     :keys "hello TAB world")))

(turtles-ert-deftest visual-replace-read-toggle-word-from-hook-not-default ()
  (test-visual-replace-env
   ;; The hook below doesn't apply if a visual-replace-args struct is
   ;; passed to visual-replace-read.
   (add-hook 'visual-replace-defaults-hook #'visual-replace-toggle-word)

   (turtles-with-minibuffer
       (should (equal
                (visual-replace-make-args :from "hello" :to "world" :regexp t)
                (car (visual-replace-read
                      (let ((args (visual-replace-make-args)))
                        (setf (visual-replace-args-regexp args) t)
                        args)))))

     :keys "hello TAB world")))

(turtles-ert-deftest visual-replace-goto-closest-match ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))
     (search-forward "world")

     (with-selected-window (display-buffer testbuf)
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "hello"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "closest match" :buf testbuf :faces test-visual-replace-faces)
           (turtles-mark-point "<>")

           (should (equal "hello, world, <>[hello]*, [hello]!" (buffer-string)))))))))

(turtles-ert-deftest visual-replace-open-hideshow-blocks ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "(defun test-1 ()\n")
     (insert " \"This is my first test function.\"\n")
     (insert " (message \"test, the first\"))\n")
     (insert "\n")
     (insert "(defun test-2 ()\n")
     (insert " \"This is another test function.\"\n")
     (insert " (message \"test, the second\"))\n")
     (insert "(defun test-3 ()\n")
     (insert " \"This is another test function.\"\n")
     (insert " (message \"test, the third\"))\n")
     (goto-char (point-min))

     (emacs-lisp-mode)
     (hs-minor-mode)

     (goto-char (point-min))
     (search-forward "test-1")
     (hs-hide-block)

     (search-forward "test-2")
     (hs-hide-block)

     (search-forward "test-3")
     (hs-hide-block)

     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         (visual-replace--update-preview t)
         (turtles-with-grab-buffer (:name "folded block" :buf testbuf :faces test-visual-replace-faces)
           (goto-char (point-min))
           (search-forward "(defun test-1 ()...)")
           (search-forward "(defun test-2 ()...)")
           (search-forward "(defun test-3 ()...)"))

         :keys "function"
         (visual-replace--update-preview t)

         (turtles-with-grab-buffer (:name "block 1 unfolded" :buf testbuf :faces test-visual-replace-faces)
           (should (equal
                    (concat "(defun test-1 ()\n"
                            " \"This is my first test [function]*.\"\n"
                            " (message \"test, the first\"))\n"
                            "\n"
                            "(defun test-2 ()...)\n"
                            "(defun test-3 ()...)")
                    (buffer-string))))
         (visual-replace-next-match)
         (visual-replace--update-preview t)

         (turtles-with-grab-buffer (:name "block 2 unfolded" :buf testbuf :faces test-visual-replace-faces)
           (should (equal
                    (concat "(defun test-1 ()...)\n"
                            "\n"
                            "(defun test-2 ()\n"
                            " \"This is another test [function]*.\"\n"
                            " (message \"test, the second\"))\n"
                            "(defun test-3 ()...)")
                    (buffer-string))))

         (visual-replace-next-match)
         (visual-replace--update-preview t)

         (turtles-with-grab-buffer (:name "block 3 unfolded" :buf testbuf :faces test-visual-replace-faces)
           (should (equal
                    (concat "(defun test-1 ()...)\n"
                            "\n"
                            "(defun test-2 ()...)\n"
                            "(defun test-3 ()\n"
                            " \"This is another test [function]*.\"\n"
                            " (message \"test, the third\"))")
                    (buffer-string))))

         (should-error (visual-replace-next-match)))

       (turtles-with-grab-buffer (:name "end with block 3 unfolded" :buf testbuf :faces test-visual-replace-faces)
         (turtles-mark-point "<>")

         (should (equal
                  (concat "(defun test-1 ()...)\n"
                          "\n"
                          "(defun test-2 ()...)\n"
                          "(defun test-3 ()\n"
                          " \"This is another test <>function.\"\n"
                          " (message \"test, the third\"))")
                  (buffer-string))))))))

(turtles-ert-deftest visual-replace-scroll-then-open-hideshow-block ()
  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "(defun test-1 ()\n")
     (dotimes (i (frame-height))
       (insert (format " (message \"line %d\"))\n" i)))
     (insert "\n")
     ;; This is out of the default turtles screen (80x24) if test-1
     ;; is unfolded.
     (insert "(defun test-2 ()\n")
     (insert " \"This is another test function.\"\n")
     (insert " (message \"test, the second\"))\n")
     (insert "(defun test-3 ()\n")
     (insert " \"This is another test function.\"\n")
     (insert " (message \"test, the third\"))\n")
     (goto-char (point-min))

     (emacs-lisp-mode)
     (hs-minor-mode)

     (goto-char (point-min))

     (search-forward "test-2")
     (hs-hide-block)

     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-with-minibuffer
           (visual-replace-read)

         :keys "function"
         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "scroll and unfold test-2" :buf testbuf :faces test-visual-replace-faces)
           (goto-char (point-min))
           (search-forward "test-2")
           (delete-region (point-min) (line-beginning-position))


           (should (equal
                    (concat "(defun test-2 ()\n"
                            " \"This is another test [function]*.\"\n"
                            " (message \"test, the second\"))\n"
                            "(defun test-3 ()\n"
                            " \"This is another test [function].\"\n"
                            " (message \"test, the third\"))")
                    (buffer-string))))

         (visual-replace-next-match)
         (visual-replace--update-preview t)

         (turtles-with-grab-buffer (:name "re-fold test-2" :buf testbuf :faces test-visual-replace-faces)
           (goto-char (point-min))
           (search-forward "test-2")
           (delete-region (point-min) (line-beginning-position))

           (should (equal
                    (concat "(defun test-2 ()...)\n"
                            "(defun test-3 ()\n"
                            " \"This is another test [function]*.\"\n"
                            " (message \"test, the third\"))")
                    (buffer-string))))

         (should-error (visual-replace-next-match)))))))

(turtles-ert-deftest visual-replace-show-keymap-help ()
  (skip-unless (and (featurep 'which-key)
                    (functionp 'set-transient-map)))
  (let ((visual-replace-show-mode-map-help t)
        (which-key-popup-type 'side-window))
    (should (maybe-show-keymap))))

(turtles-ert-deftest visual-replace-show-keymap-help-type-immediately ()
  (skip-unless (and (featurep 'which-key)
                    (functionp 'set-transient-map)))
  (let ((visual-replace-show-mode-map-help t)
        (which-key-popup-type 'side-window))
    (should-not (maybe-show-keymap 0.0))))

(turtles-ert-deftest visual-replace-show-keymap-help-disabled ()
  (skip-unless (and (featurep 'which-key)
                    (functionp 'set-transient-map)))
  (let ((visual-replace-show-mode-map-help nil)
        (which-key-popup-type 'side-window))
    (should-not (maybe-show-keymap))))

(turtles-ert-deftest visual-replace-show-keymap-help-minibuffer-popup-type ()
  (skip-unless (and (featurep 'which-key)
                    (functionp 'set-transient-map)))
  (let ((visual-replace-show-mode-map-help nil)
        (which-key-popup-type 'minibuffer))
    (should-not (maybe-show-keymap))))

(defun maybe-show-keymap (&optional delay)
  (cl-letf* ((testbuf (current-buffer))
             (which-key-idle-delay 0.01)
             (shown-keymap nil)
             ((symbol-function 'which-key-show-keymap)
              (lambda (keymap no-paging)
                (push keymap shown-keymap)
                (should no-paging))))
    (insert "foo, foo foo.")
    (goto-char (point-min))

    (with-selected-window (display-buffer (current-buffer))
      (delete-other-windows (selected-window))
      (which-key-mode 1)
      (turtles-with-minibuffer
          (call-interactively #'visual-replace)
        (sleep-for (or delay 0.02))
        :keys "foo TAB bar RET")
      (should (equal "bar, bar bar." (buffer-string)))

      shown-keymap)))

(ert-deftest visual-replace-default-scope-no-region ()
  (ert-with-test-buffer ()
    (insert "foobar")
    (goto-char 3)

    (let ((visual-replace-initial-scope nil)
          (visual-replace-default-to-full-scope nil))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point 3)
                     (visual-replace--default-scope nil)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point 3)
                     (visual-replace--default-scope 'full)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point 3)
                     (visual-replace--default-scope 'from-point)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point 3)
                     (visual-replace--default-scope 'region)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point 10)
                     (visual-replace--default-scope 10))))))

(ert-deftest visual-replace-default-scope-no-region-default-to-full-scope ()
  (ert-with-test-buffer ()
    (insert "foobar")
    (goto-char 3)

    (let ((visual-replace-initial-scope nil)
          (visual-replace-default-to-full-scope t))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point 3)
                     (visual-replace--default-scope nil)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point 3)
                     (visual-replace--default-scope 'full)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point 3)
                     (visual-replace--default-scope 'from-point)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point 3)
                     (visual-replace--default-scope 'region)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point 10)
                     (visual-replace--default-scope 10))))))

(ert-deftest visual-replace-default-scope-no-region-visual-replace-initial-scope ()
  (ert-with-test-buffer ()
    (insert "foobar")
    (goto-char 3)

    (let ((visual-replace-initial-scope 'full))
      (dolist (def '(t nil))
        (let ((visual-replace-default-to-full-scope def))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point 3)
                         (visual-replace--default-scope nil)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point 3)
                         (visual-replace--default-scope 'full)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point 3)
                         (visual-replace--default-scope 'from-point)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point 3)
                         (visual-replace--default-scope 'region)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point 10)
                         (visual-replace--default-scope 10))))))

    (let ((visual-replace-initial-scope 'from-point))
      (dolist (def '(t nil))
        (let ((visual-replace-default-to-full-scope def))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point 3)
                         (visual-replace--default-scope nil)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point 3)
                         (visual-replace--default-scope 'full)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point 3)
                         (visual-replace--default-scope 'from-point)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point 3)
                         (visual-replace--default-scope 'region)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point 10)
                         (visual-replace--default-scope 10))))))))

(ert-deftest visual-replace-default-scope-with-region ()
  (ert-with-test-buffer ()
    (dotimes (i 6)
      (insert (format "line %d\n" i)))
    (goto-char (point-min))
    (search-forward "line 2")
    (set-mark (match-beginning 0))
    (search-forward "line 4")
    (goto-char (match-end 0))
    (activate-mark)
    (should (region-active-p))

    (let ((visual-replace-initial-scope nil)
          (visual-replace-default-to-full-scope nil))
      (should (equal (visual-replace--make-scope-internal
                      :type 'region
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope nil)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 'full)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 'from-point)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'region
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 'region)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point 10
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 10))))))

(ert-deftest visual-replace-default-scope-with-region-default-to-full-scope ()
  (ert-with-test-buffer ()
    (dotimes (i 6)
      (insert (format "line %d\n" i)))
    (goto-char (point-min))
    (search-forward "line 2")
    (set-mark (match-beginning 0))
    (search-forward "line 4")
    (goto-char (match-end 0))
    (activate-mark)
    (should (region-active-p))

    (let ((visual-replace-initial-scope nil)
          (visual-replace-default-to-full-scope t))
      (should (equal (visual-replace--make-scope-internal
                      :type 'region
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope nil)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 'full)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'from-point
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 'from-point)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'region
                      :point (point)
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 'region)))
      (should (equal (visual-replace--make-scope-internal
                      :type 'full
                      :point 10
                      :bounds `((,(mark) . ,(point)))
                      :topleft-edge (mark)
                      :line-count 3)
                     (visual-replace--default-scope 10))))))

(ert-deftest visual-replace-default-scope-with-region-visual-replace-initial-scope ()
  (ert-with-test-buffer ()
    (dotimes (i 6)
      (insert (format "line %d\n" i)))
    (goto-char (point-min))
    (search-forward "line 2")
    (set-mark (match-beginning 0))
    (search-forward "line 4")
    (goto-char (match-end 0))
    (activate-mark)
    (should (region-active-p))

    (let ((visual-replace-initial-scope 'full))
      (dolist (def '(t nil))
        (let ((visual-replace-default-to-full-scope def))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope nil)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 'full)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 'from-point)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 'region)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'full
                          :point 10
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 10))))))

    (let ((visual-replace-initial-scope 'from-point))
      (dolist (def '(t nil))
        (let ((visual-replace-default-to-full-scope def))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope nil)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 'full)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 'from-point)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point (point)
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 'region)))
          (should (equal (visual-replace--make-scope-internal
                          :type 'from-point
                          :point 10
                          :bounds `((,(mark) . ,(point)))
                          :topleft-edge (mark)
                          :line-count 3)
                         (visual-replace--default-scope 10))))))))

(ert-deftest visual-replace-regexp-command ()
  (test-visual-replace-env
   (dotimes (i 3)
     (insert (format "line %d\n" i)))
   (with-selected-window (display-buffer (current-buffer))
     (goto-char (point-min))

     (visual-replace-ert-simulate-keys (kbd "[0-9]+ TAB REDACTED RET")
       (call-interactively 'visual-replace-regexp))
     (should (equal (concat "line REDACTED\n"
                            "line REDACTED\n"
                            "line REDACTED\n")
                    (buffer-substring-no-properties
                     (point-min) (point-max)))))))

;;; visual-replace-test.el ends here

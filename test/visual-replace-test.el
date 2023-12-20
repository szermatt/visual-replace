;;; visual-replace-test.el --- Tests for (visual-replace) -*- lexical-binding: t -*-

(require 'ert)
(require 'ert-x)

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
     (visual-replace-ert-simulate-keys (kbd "TAB r e p l RET")
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
     (visual-replace-ert-simulate-keys (kbd "TAB r e p l RET")
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
       (visual-replace-ert-simulate-keys (kbd "TAB r e p l a c e d RET")
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
     (visual-replace-ert-simulate-keys (kbd "TAB r e p l a c e d RET")
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
     (visual-replace-ert-simulate-keys (kbd "TAB r e p l a c e d RET")
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
       (visual-replace-ert-simulate-keys (kbd "TAB r e p l a c e d RET")
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
       (recenter 3 t)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (visual-replace--update-preview)
          (while visual-replace--first-match-timer
            (ert-run-idle-timers))
          (push (visual-replace-test-window-content win) snapshots)))
       (visual-replace-ert-simulate-keys (kbd (format "t e x t SPC %d . TAB r e p l C-c t RET" to-replace))
         (call-interactively 'visual-replace))

       ;; C-c t was called once to capture window content.
       (should (equal 1 (length snapshots)))

       ;; The replacement was highlighted, even though it required scrolling the window.
       (should (string-match (regexp-quote (format "[text %d.repl]" to-replace)) (car snapshots)))

       ;; We're now back at the original position.
       (should (equal 4 (line-number-at-pos (point))))))))

(ert-deftest test-visual-replace-jump-backward-to-first-match ()
  (test-visual-replace-env
   (with-selected-window (display-buffer (current-buffer))
     (let* ((snapshots)
            (win (selected-window))
            (height (window-height win)))
       (dotimes (i (* 3 height))
         (insert (format "this is text %d.\n" i)))
       (goto-char (point-max))
       (forward-line -3)
       (recenter 3 t)
       (define-key
        visual-replace-mode-map
        (kbd "C-c t")
        (lambda ()
          (interactive)
          (visual-replace--update-preview)
          (while visual-replace--first-match-timer
            (ert-run-idle-timers))
          (push (visual-replace-test-window-content win) snapshots)))
       (visual-replace-ert-simulate-keys (kbd "t e x t SPC 5 . C-c t TAB repl RET")
         (call-interactively 'visual-replace))

       ;; C-c t was called once to capture window content.
       (should (equal 1 (length snapshots)))

       ;; The replacement was highlighted, even though it required scrolling the window.
       (should (string-match "[text 5.]" (car snapshots)))

       ;; We're now back at the original position.
       (should (equal (- (line-number-at-pos (point-max)) 3) (line-number-at-pos (point))))))))

;;; visual-replace-test.el ends here

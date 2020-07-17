;;; visual-replace-test.el --- Tests for (visual-replace) -*- lexical-binding: t -*-

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
  :expected-result :failed
  (test-visual-replace-env
   (insert "foo")
   (read-only-mode)
   (visual-replace (visual-replace-make-args :from "foo" :to "bar"))))

;;; visual-replace-test.el ends here

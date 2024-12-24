;;; visual-replace-test.el --- Tests for (visual-replace-read) -*- lexical-binding: t -*-

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

(require 'visual-replace)
(require 'visual-replace-test-helper)

(ert-deftest test-visual-replace-read-RET-twice ()
  (test-visual-replace-env
   (let ((args (car (test-visual-replace-run "hello RET world RET" (visual-replace-read)))))
     (should (equal args (visual-replace-make-args :from "hello" :to "world"))))))

(ert-deftest test-visual-replace-exit-immediately ()
  (test-visual-replace-env
   (let ((args (car (test-visual-replace-run "<F1> x" (visual-replace-read)))))
     (should (equal args (visual-replace-make-args))))))

(ert-deftest test-visual-replace-exit-before-tab ()
  (test-visual-replace-env
   (let ((args (car (test-visual-replace-run "hello <F1> x" (visual-replace-read)))))
     (should (equal args (visual-replace-make-args))))))

(ert-deftest test-visual-replace-read-TAB-then-RET ()
  (test-visual-replace-env
   (let ((args (car (test-visual-replace-run "hello TAB world RET" (visual-replace-read)))))
     (should (equal args (visual-replace-make-args :from "hello" :to "world"))))))

(ert-deftest test-visual-replace-read-TAB-navigation ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB <F1> ! world TAB <F1> ! TAB <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → []"
                    "Replace from point: hello[] → world"
                    "Replace from point: hello → world[]")))))

(ert-deftest test-visual-replace-read-TAB-remember-pos ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <left> TAB <left> TAB <F1> ! TAB <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → worl[]d"
                    "Replace from point: hell[]o → world")))))

(ert-deftest test-visual-replace-read-TAB-default-to-end ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world C-u 8 <left> TAB <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → world[]")))))

(ert-deftest test-visual-replace-read-toggle-regexp ()
  (test-visual-replace-env
   (let ((args (car (test-visual-replace-run "hello TAB world <F1> r <F1> ! RET" (visual-replace-read)))))
     (should (equal args (visual-replace-make-args :from "hello" :to "world" :regexp t))))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →.* world[]")))))

(ert-deftest test-visual-replace-read-toggle-regexp-with-case-default ()
  (test-visual-replace-env
   (let ((case-fold-search t))
     ;; If the default value is not handled properly, case-fold could
     ;; end up being nil when toggling another flag.
     (should
      (equal (car (test-visual-replace-run
                   "hello TAB world <F1> r <F1> ! RET" (visual-replace-read)))
             (visual-replace-make-args :from "hello" :to "world" :regexp t :case-fold t)))
     (should (equal test-visual-replace-snapshot
                    '("Replace from point: hello →.* world[]"))))))

(ert-deftest test-visual-replace-read-toggle-query ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> q <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :query t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →? world[]")))))

(ert-deftest test-visual-replace-read-toggle-regexp-query ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> r <F1> q <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :regexp t :query t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →?.* world[]")))))

(ert-deftest test-visual-replace-read-toggle-word ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> w <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :word t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →w world[]")))))

(ert-deftest test-visual-replace-read-toggle-word-and-regexp ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> w <F1> ! <F1> r <F1> ! <F1> w <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :word t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →w world[]"
                    "Replace from point: hello →.* world[]"
                    "Replace from point: hello →w world[]")))))

(ert-deftest test-visual-replace-read-default-case-fold ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :case-fold t)))))

(ert-deftest test-visual-replace-read-toggle-case-fold ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> c <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :case-fold nil)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →c world[]")))))

(ert-deftest test-visual-replace-read-toggle-case-fold-off-by-default ()
  (test-visual-replace-env
   (let ((case-fold-search nil))
     (should (equal (car (test-visual-replace-run
                  "hello TAB world <F1> c <F1> ! RET" (visual-replace-read)))
                    (visual-replace-make-args :from "hello" :to "world" :case-fold t))))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →i world[]")))))

(ert-deftest test-visual-replace-read-toggle-lax-ws ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> l <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world"
                                 :lax-ws-regexp t :lax-ws-non-regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →(lax ws) world[]")))))

(ert-deftest test-visual-replace-read-toggle-lax-ws-and-regexp ()
  (test-visual-replace-env
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world <F1> l <F1> r <F1> ! <F1> l <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello →(lax ws).* world[]"
                    "Replace from point: hello →.* world[]")))))

(ert-deftest test-visual-replace-read-toggle-scope ()
  (test-visual-replace-env
   (let ((ranges (nth 1 (test-visual-replace-run
                        "hello TAB world <F1> s RET" (visual-replace-read)))))
     (should (equal ranges (list (cons (point-min) (point-max))))))))

(ert-deftest test-visual-replace-read-toggle-scope-keeps-flags ()
  (test-visual-replace-env
   (test-visual-replace-run
    "hello TAB world <F1> r <F1> q <F1> s <F1> ! RET" (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace in buffer: hello →?.* world[]")))))

(ert-deftest test-visual-replace-tab-keeps-flags ()
  (test-visual-replace-env
   (test-visual-replace-run
    "hello TAB world <F1> r <F1> q TAB <F1> ! RET" (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello[] →?.* world")))))

(ert-deftest test-visual-replace-read-toggle-scope-display ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> ! <F1> s <F1> ! <F1> s <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → world[]"
                    "Replace in buffer: hello → world[]"
                    "Replace from point: hello → world[]")))))

(ert-deftest test-visual-replace-preview-toggle-scope ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (search-forward "world")
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "hello <F1> _ <F1> s <F1> _ <F1> s <F1> _ <F1> x" (visual-replace-read))
   (should (equal
            '( ;; from point
              "hello, world, [hello], [hello]!"
              ;; full buffer
              "[hello], world, [hello], [hello]!"
              ;; from point again, to make sure the extra matches are
              ;; deleted
              "hello, world, [hello], [hello]!")
            (mapcar (lambda (snapshot)
                      (test-visual-replace-highlight-face
                       snapshot 'visual-replace-match 'visual-replace-match-highlight))
                    test-visual-replace-snapshot)))))

(ert-deftest test-visual-replace-forgets-setting ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> r <F1> q <F1> s RET" (visual-replace-read))
   ;; settings from the first call should not reappear as default in
   ;; the second call
   (should
    (equal (car (test-visual-replace-run "foo TAB bar RET" (visual-replace-read)))
           (visual-replace-make-args :from "foo" :to "bar")))))

(defun test-visual-replace-setup-region ()
  "Define a region in the current test buffer."
  (insert "hello\nworld\n")
  (goto-char 1)
  (set-mark (point))
  (forward-line)
  (should (region-active-p)))

(ert-deftest test-visual-replace-read-toggle-scope-with-region-display ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> ! <F1> s <F1> ! <F1> s <F1> ! RET"
                        (test-visual-replace-setup-region)
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace in region (1L): hello → world[]"
                    "Replace in buffer: hello → world[]"
                    "Replace in region (1L): hello → world[]")))))

(ert-deftest test-visual-replace-read-toggle-scope-with-region ()
  (test-visual-replace-env
   (let ((ranges (nth 1 (test-visual-replace-run "hello TAB world RET"
                                             (test-visual-replace-setup-region)
                                             (visual-replace-read)))))
     (should (equal ranges (list (cons 1 7)))))))

(ert-deftest test-visual-replace-read-noncontiguous-region ()
  (test-visual-replace-env
   (insert "hello\nworld\nhello\nworld\n")
   (goto-char (point-min))
   (move-to-column 2)
   (rectangle-mark-mode 1)
   (goto-char (line-beginning-position 2))
   (move-to-column 4)
   (let ((ranges (nth 1 (test-visual-replace-run "<F1> ! he TAB ho RET"
                                             (visual-replace-read)))))
     (should (equal test-visual-replace-snapshot '("Replace in region (2L): []")))
     (should (equal ranges '((3 . 5) (9 . 11)))))))

(ert-deftest test-visual-replace-read-toggle-scope-with-region-then-buffer ()
  (test-visual-replace-env
   (let ((ranges (nth 1 (test-visual-replace-run "hello TAB world <F1> s RET"
                                             (test-visual-replace-setup-region)
                                             (visual-replace-read)))))
     (should (equal ranges (list (cons (point-min) (point-max))))))))

(ert-deftest test-visual-replace-fields ()
  (test-visual-replace-env
   (test-visual-replace-run
    "hello TAB world <F1> ! RET"
    (define-key visual-replace-mode-map (kbd "<F1> !")
                (lambda ()
                  (interactive)
                  (push (minibuffer-contents)
                        test-visual-replace-snapshot)))
    (visual-replace-read))
   (should (equal (test-visual-replace-highlight-property
                   (car test-visual-replace-snapshot)
                   'field 'search)
                  "[hello] world"))
   (should (equal (test-visual-replace-highlight-property
                   (car test-visual-replace-snapshot)
                   'field 'replace)
                  "hello [world]"))))

(ert-deftest test-visual-replace-fields-in-history-entry ()
  (test-visual-replace-env
   ;; fill history
   (test-visual-replace-run
    "hello TAB world RET"
    (visual-replace-read))
   ;; recall history entry.
   (test-visual-replace-run
    "<F1> h <F1> ! RET"
    (define-key visual-replace-mode-map (kbd "<F1> !")
                (lambda ()
                  (interactive)
                  (push (minibuffer-contents)
                        test-visual-replace-snapshot)))
    (visual-replace-read))
   (should (equal (test-visual-replace-highlight-property
                   (car test-visual-replace-snapshot)
                   'field 'search)
                  "[hello] world"))
   (should (equal (test-visual-replace-highlight-property
                   (car test-visual-replace-snapshot)
                   'field 'replace)
                  "hello [world]"))))

(ert-deftest test-visual-replace-kill ()
  (test-visual-replace-env
   (test-visual-replace-run
    "hello TAB world TAB C-a <right> C-k <F1> ! TAB C-a <right> C-k <F1> ! RET"
    (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: h[] → world"
                    "Replace from point: h → w[]")))))

(ert-deftest test-visual-replace-kill-no-separator ()
  (test-visual-replace-env
   (test-visual-replace-run "hello <left> C-k <F1> ! RET" (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hell[]")))))

(ert-deftest test-visual-replace-kill-whole-line ()
  (test-visual-replace-env
   (test-visual-replace-run
    "hello TAB world <F1> k <F1> ! TAB <F1> k <F1> ! <F1> g"
    (define-key visual-replace-mode-map (kbd "<F1> k") 'kill-whole-line)
    (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → []"
                    "Replace from point: []")))))

(ert-deftest test-visual-replace-yank-in-from ()
  (test-visual-replace-env
   (save-excursion (insert "from buffer"))
   (test-visual-replace-run "<F1> y TAB <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from → []")))))

(ert-deftest test-visual-replace-yank-symbol-in-from- ()
  (test-visual-replace-env
   (save-excursion (insert "from-a buffer"))
   (test-visual-replace-run "<F1> y TAB <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from-a → []")))))

(ert-deftest test-visual-replace-multiple-yank-in-from ()
  (test-visual-replace-env
   (save-excursion (insert "from current buffer"))
   (test-visual-replace-run "<F1> y <F1> ! <F1> y <F1> ! TAB RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from[]"
                    "Replace from point: from current[]")))))

(ert-deftest test-visual-replace-yank-with-symbols ()
  (test-visual-replace-env
   (save-excursion (insert "(progn (when some-test some-value))"))
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))

   ;; After typing "(when", the pointer goes to the beginning of the
   ;; first match, then moves as more and more text is added.
   (test-visual-replace-run
    (concat
     "<F1> s (when <F1> ! "
     (mapconcat #'identity (make-list 4 "<F1> y <F1> !") " ")
     " RET")
    (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace in buffer: (when[]"
                    "Replace in buffer: (when some-test[]"
                    "Replace in buffer: (when some-test some-value[]"
                    "Replace in buffer: (when some-test some-value)[]"
                    "Replace in buffer: (when some-test some-value))[]")))))

(ert-deftest test-visual-replace-yank-in-from-with-prompt ()
  (test-visual-replace-env
   (save-excursion (insert "from buffer"))
   (test-visual-replace-run "TAB world TAB <F1> y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from[] → world")))))

(ert-deftest test-visual-replace-yank-in-from-after-next-match ()
  (test-visual-replace-env
   (save-excursion (insert "from-buffer\nfrom-region\nfrom-point\n"))
   (test-visual-replace-run "from <down> <F1> y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from-region[]")))))

(ert-deftest test-visual-replace-yank-in-to ()
  (test-visual-replace-env
   (kill-new "from kill-ring")
   (test-visual-replace-run "hello TAB <F1> y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → hello[]")))))

(ert-deftest test-visual-replace-multiple-yank-in-to ()
  (test-visual-replace-env
   (kill-new "from kill-ring")
   (test-visual-replace-run "hello TAB <F1> y <F1> y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → hellohello[]")))))

(ert-deftest test-visual-replace-yank-pop-in-from ()
  (test-visual-replace-env
   (kill-new "prev3")
   (kill-new "prev2")
   (kill-new "prev1")
   (test-visual-replace-run "<F1> Y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: prev1[]")))))

(ert-deftest test-visual-replace-multiple-yank-pop-in-from ()
  (test-visual-replace-env
   (kill-new "prev3")
   (kill-new "prev2")
   (kill-new "prev1")
   (test-visual-replace-run "<F1> Y <F1> Y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: prev2[]")))))

(ert-deftest test-visual-replace-yank-then-yank-pop-in-to ()
  (test-visual-replace-env
   (kill-new "prev3")
   (kill-new "prev2")
   (kill-new "prev1")
   (test-visual-replace-run "hello TAB <F1> y <F1> Y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → helloprev1[]")))))

(ert-deftest test-visual-replace-multiple-yank-pop-in-to ()
  (test-visual-replace-env
   (kill-new "prev3")
   (kill-new "prev2")
   (kill-new "prev1")
   (test-visual-replace-run "hello TAB <F1> Y <F1> Y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → prev2[]")))))

(ert-deftest test-visual-replace-history-by-default ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world RET" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run "RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world")))))

(ert-deftest test-visual-replace-default-but-no-history ()
  (test-visual-replace-env
   (should-error (test-visual-replace-run "TAB RET" (visual-replace-read)))))

(ert-deftest test-visual-replace-history-by-default-despite-separator ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world RET" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run "TAB RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world")))))

(ert-deftest test-visual-replace-history-by-default-despite-toggle ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world RET" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run "<F1> r RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world")))))

(ert-deftest test-visual-replace-history ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world RET"
                        (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world")))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello → world]: []hello → world")))))

(ert-deftest test-visual-replace-history-regex ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> r RET"
                        (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →.* world]: []hello →.* world")))))

(ert-deftest test-visual-replace-history-regex-edit ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> r RET"
                        (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h TAB x <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "worldx" :regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →.* world]: hello →.* worldx[]")))))

(ert-deftest test-visual-replace-history-regex-toggle ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> r RET"
                        (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h <F1> r RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world")))))

(ert-deftest test-visual-replace-history-regex-query ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> r <F1> q RET"
                        (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :regexp t :query t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →?.* world]: []hello →?.* world")))))

(ert-deftest test-visual-replace-history-regex-toggle-2 ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> r <F1> q RET" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h <F1> q <F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →?.* world]: []hello →.* world")))))

(ert-deftest test-visual-replace-history-multiple-1 ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB query <F1> q RET"
                        (visual-replace-read))
   (test-visual-replace-run "<F1> h <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →? query]: []hello →? query")))))

(ert-deftest test-visual-replace-history-multiple-2 ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB bar RET" (visual-replace-read))
   (test-visual-replace-run "hello TAB query <F1> q RET" (visual-replace-read))
   (test-visual-replace-run "<F1> h <F1> h <F1> ! RET" (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →? query]: []hello → bar")))))

(ert-deftest test-visual-replace-history-multiple-3 ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB foo <F1> r RET" (visual-replace-read))
   (test-visual-replace-run "hello TAB bar RET" (visual-replace-read))
   (test-visual-replace-run "hello TAB query <F1> q RET" (visual-replace-read))
   (test-visual-replace-run "<F1> h <F1> h <F1> h <F1> ! RET" (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →? query]: []hello →.* foo")))))

(ert-deftest test-visual-replace-keep-incomplete-in-history ()
  (test-visual-replace-env
   (test-visual-replace-run "foo TAB bar <F1> g" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h f a a RET" (visual-replace-read)))
           (visual-replace-make-args :from "faafoo" :to "bar")))))

(ert-deftest test-visual-replace-keep-incomplete-only-from ()
  (test-visual-replace-env
   (test-visual-replace-run "foo <F1> g" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h TAB bar RET" (visual-replace-read)))
           (visual-replace-make-args :from "foo" :to "bar")))))

(ert-deftest test-visual-replace-incomplete-then-history ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world RET" (visual-replace-read))
   (test-visual-replace-run "foo TAB bar <F1> g" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run
                 "<F1> h <F1> h RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world")))))

(ert-deftest test-visual-replace-incomplete-but-default-from-history ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world RET" (visual-replace-read))
   (test-visual-replace-run "foo TAB bar <F1> g" (visual-replace-read))
   (should
    (equal (car (test-visual-replace-run "<F1> ! RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world")))))

(ert-deftest test-visual-replace-warn-newline ()
  (test-visual-replace-env
   (ert-with-message-capture captured-message
     (test-visual-replace-run "a\\n RET b <F1> r RET" (visual-replace-read))
     (should (string-match-p "^Note.*" captured-message)))))

(ert-deftest test-visual-replace-warn-tab ()
  (test-visual-replace-env
   (ert-with-message-capture captured-message
     (test-visual-replace-run "a\\t RET b <F1> r RET" (visual-replace-read))
     (should (string-match-p "^Note.*" captured-message)))))

(ert-deftest test-visual-replace-warn-only-for-regexp ()
  (test-visual-replace-env
   (ert-with-message-capture captured-message
     (test-visual-replace-run "a\\n RET b RET" (visual-replace-read))
     (should (equal "" captured-message)))))

(ert-deftest test-visual-replace-warn-only-the-first-time ()
  (test-visual-replace-env
   (ert-with-message-capture captured-message
     (test-visual-replace-run "a\\n RET b <F1> r RET" (visual-replace-read))
     (should-not (equal "" captured-message)))
   (ert-with-message-capture captured-message
     (test-visual-replace-run "RET" (visual-replace-read))
     (should (equal "" captured-message)))))

(ert-deftest test-visual-replace-kill-and-yank-separator ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB TAB C-a <F1> q <F1> ! <F1> u <F1> ! TAB <F1> ! RET"
                        (define-key visual-replace-mode-map (kbd "<F1> q")
                          (lambda () (interactive) (call-interactively 'kill-line)))
                        (define-key visual-replace-mode-map (kbd "<F1> u")
                          (lambda () (interactive) (call-interactively 'yank)))
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] → "
                    "Replace from point: hello[] → "
                    "Replace from point: hello → []")))))

(ert-deftest test-visual-replace-initial-input ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> g" (visual-replace-read (visual-replace-make-args :from "initial")))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: initial[]")))))

(ert-deftest test-visual-replace-initial-input-complete ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> g"
    (visual-replace-read (visual-replace-make-args :from "foo" :to "bar")))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: foo → bar[]")))))

(ert-deftest test-visual-replace-initial-word ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> g"
    (visual-replace-read (visual-replace-make-args :word t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →w ")))))

(ert-deftest test-visual-replace-initial-regexp ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> g"
    (visual-replace-read (visual-replace-make-args :regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →.* ")))))

(ert-deftest test-visual-replace-initial-query ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> g"
    (visual-replace-read (visual-replace-make-args :query t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →? ")))))

(ert-deftest test-visual-replace-initial-case-fold-enable ()
  (test-visual-replace-env
   (let ((case-fold-search nil))
     (test-visual-replace-run
      "<F1> ! <F1> g"
      (visual-replace-read (visual-replace-make-args :case-fold t)))
     (should (equal test-visual-replace-snapshot
                    '("Replace from point: [] →i "))))))

(ert-deftest test-visual-replace-initial-case-fold-disable ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> g"
    (visual-replace-read (visual-replace-make-args :case-fold nil)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →c ")))))

(ert-deftest test-visual-replace-initial-case-fold-matches-default ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> g"
    (visual-replace-read (visual-replace-make-args :case-fold t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: []")))))

(ert-deftest test-visual-replace-preview ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "hel <F1> _ l <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (nth 0 test-visual-replace-snapshot)
                   'visual-replace-match 'visual-replace-match-highlight)
                  "[hel]lo, world, [hel]lo, [hel]lo!"))
   (should (equal (test-visual-replace-highlight-face
                   (nth 1 test-visual-replace-snapshot)
                   'visual-replace-match 'visual-replace-match-highlight)
                  "[hell]o, world, [hell]o, [hell]o!"))))

(ert-deftest test-visual-replace-preview-case-fold ()
  (test-visual-replace-env
   (insert "Hello, world, hello, heLLO!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "hello <F1> _ <F1> c <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (nth 0 test-visual-replace-snapshot)
                   'visual-replace-match 'visual-replace-match-highlight)
                  "[Hello], world, [hello], [heLLO]!"))
   (should (equal (test-visual-replace-highlight-face
                   (nth 1 test-visual-replace-snapshot)
                   'visual-replace-delete-match-highlight)
                  "Hello, world, [hello], heLLO!"))))

(ert-deftest test-visual-replace-preview-case-fold-uppercase ()
  (test-visual-replace-env
   (insert "Hello, world, hello, HELLO!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "HELLO <F1> _ <F1> c <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (nth 0 test-visual-replace-snapshot)
                   'visual-replace-match
                   'visual-replace-match-highlight)
                  "Hello, world, hello, [HELLO]!"))
   (should (equal (test-visual-replace-highlight-face
                   (nth 1 test-visual-replace-snapshot)
                   'visual-replace-delete-match
                   'visual-replace-delete-match-highlight)
                  "Hello, world, hello, [HELLO]!"))))

(ert-deftest test-visual-replace-preview-delete ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "hell TAB <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot)
                   'visual-replace-delete-match
                   'visual-replace-delete-match-highlight)
                  "[hell]o, world, [hell]o, [hell]o!"))))

(ert-deftest test-visual-replace-preview-replace ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "hell TAB hul <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot)
                   'visual-replace-delete-match
                   'visual-replace-delete-match-highlight)
                  "[hell]hulo, world, [hell]hulo, [hell]hulo!"))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot)
                   'visual-replace-replacement
                   'visual-replace-replacement-highlight)
                  "hell[hul]o, world, hell[hul]o, hell[hul]o!"))))

(ert-deftest test-visual-replace-preview-regex ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "hel+ <F1> r <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot)
                   'visual-replace-delete-match
                   'visual-replace-delete-match-highlight)
                  "[hell]o, world, [hell]o, [hell]o!"))))

(ert-deftest test-visual-replace-preview-lax-ws ()
  (test-visual-replace-env
   (insert "hello   world!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "hello SPC world <F1> l <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot)
                   'visual-replace-delete-match 'visual-replace-delete-match-highlight)
                  "[hello   world]!"))))

(ert-deftest test-visual-replace-preview-bad-regex ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   ;; This is just a smoke test. \b\b\b matches empty strings, which
   ;; cannot be displayed and might cause some implementations to
   ;; enter an infinite loop.
   (test-visual-replace-run "<F1> r \\b\\b\\b <F1> _ <F1> x" (visual-replace-read))))

(ert-deftest test-visual-replace-preview-regex-eval ()
  (test-visual-replace-env
   (insert "hello, world, hello")
   (goto-char (point-min))
   (set-window-buffer (selected-window) (current-buffer))
   (test-visual-replace-run "h\\(el+\\) TAB \\#\\,(upcase SPC \\1) <F1> r <F1> _ <F1> x"
                            (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot)
                   'visual-replace-replacement
                   'visual-replace-replacement-highlight)
                  "hell[0ELL]o, world, hell[1ELL]o"))))

(ert-deftest test-visual-replace-preview-skip-readonly ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((visual-replace-min-length 3)
         (testbuf (current-buffer)))
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

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-disable-preview-if-too-short ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((visual-replace-min-length 3)
         (testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-customize-min-length ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((visual-replace-min-length 0)
         (testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-read-only-buffer ()
  (test-visual-replace-env
   (insert "foo")
   (read-only-mode)
   (should-error (visual-replace-read))))

(ert-deftest test-visual-replace-preview-highlight-match-at-point ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-apply-one ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-apply-multiple ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
           (visual-replace-read)

           :keys "f TAB l ESC 3"
           :command #'visual-replace-apply-one
           (turtles-with-grab-buffer (:buf testbuf)
             (should (equal "Lee li lo fum!" (buffer-string)))))))))

(ert-deftest test-visual-replace-read-undo ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
           (visual-replace-read)

         :keys "f TAB l"
         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "before undo" :buf testbuf)
           (should (equal "Lee li fo fum!" (buffer-string))))

         :command #'visual-replace-undo
         (turtles-with-grab-buffer (:name "after undo 1" :buf testbuf)
           (should (equal "Lee fi fo fum!" (buffer-string)))))))))

(ert-deftest test-visual-replace-read-undo-further ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-undo-multiple ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-undo-everything ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-apply-multiple-undo-once ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-apply-undo-everything-then-redo ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "Fee fi fo fum!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
           (visual-replace-read)

         :keys "f TAB l"
         :command #'visual-replace-apply-one
         (turtles-with-grab-buffer (:name "after apply" :buf testbuf)
           (should (equal "Lee fi fo fum!" (buffer-string))))

         :command #'visual-replace-undo
         (turtles-with-grab-buffer (:name "after undo" :buf testbuf)
           (should (equal "Fee fi fo fum!" (buffer-string))))

         :command #'visual-replace-apply-one
         :command #'visual-replace-apply-one
         :command #'visual-replace-undo
         ;; Executing commands using M-x guarantees that undo breaks
         ;; are applied as they would normally be.
         (turtles-with-grab-buffer (:name "after second undo" :buf testbuf)
           (should (equal "Lee fi fo fum!" (buffer-string)))))))))

(ert-deftest test-visual-replace-read-display-total ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t))
     (insert "I say, hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-display-index ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((visual-replace-display-total t))
     (insert "I say, hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-display-total-update ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((visual-replace-display-total t))
     (insert "I say, hello, world, hellow, hellow!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-display-total-too-short ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t))
     (insert "I say, hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-read-display-total-large-buffer ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t)
         (visual-replace-max-matches-for-total 1000))
     (dotimes (i 900)
       (insert (format "some text%d\n" i)))
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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


(ert-deftest test-visual-replace-read-display-total-too-many-matches ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer))
         (visual-replace-display-total t)
         (visual-replace-max-matches-for-total 100))
     (dotimes (i 300)
       (insert (format "some text%d\n" i)))
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
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
                                  "some [text]17")
                          (buffer-string))))

         ;; Not all matches should have been highlighted.
         (with-current-buffer testbuf
           (save-excursion
             (goto-char (point-min))
             (search-forward "some text299")
             (should-not (memq (get-char-property (match-beginning 0) 'face)
                               '(visual-replace-match visual-replace-match-highlight))))))))))

(ert-deftest test-visual-replace-read-display-total-too-large ()
  (turtles-ert-test)

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

       (turtles-read-from-minibuffer
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
                                  "some [text]17")
                          (buffer-string))))

         ;; Not all matches should have been highlighted.
         (with-current-buffer testbuf
           (save-excursion
             (goto-char (point-min))
             (search-forward "some text299")
             (should-not (memq (get-char-property (match-beginning 0) 'face)
                               '(visual-replace-match visual-replace-match-highlight))))))))))

(ert-deftest test-visual-replace-read-display-total-in-region-buffer-too-large ()
  (turtles-ert-test)

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

       (turtles-read-from-minibuffer
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
                                  "some text17")
                          (buffer-string)))))))))

(ert-deftest test-visual-replace-preview-display-window ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))

     (with-selected-window (display-buffer (current-buffer))
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
           (visual-replace-read)

         :keys "hello"

         ;; Hide the test buffer
         (select-window (display-buffer (get-scratch-buffer-create)))
         (delete-other-windows)

         ;; This should redisplay the test buffer
         (visual-replace-next-match)

         (should (get-buffer-window testbuf)))))))

(ert-deftest test-visual-replace-read-toggle-query-from-hook ()
  (test-visual-replace-env
   (add-hook 'visual-replace-minibuffer-mode-hook #'visual-replace-toggle-query)
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :query t)))))

(ert-deftest test-visual-replace-read-toggle-word-from-hook ()
  (test-visual-replace-env
   (add-hook 'visual-replace-defaults-hook #'visual-replace-toggle-word)
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world RET" (visual-replace-read)))
           (visual-replace-make-args :from "hello" :to "world" :word t)))))

(ert-deftest test-visual-replace-read-toggle-word-from-hook-not-default ()
  (test-visual-replace-env
   ;; The hook below doesn't apply if a visual-replace-args struct is
   ;; passed to visual-replace-read.
   (add-hook 'visual-replace-defaults-hook #'visual-replace-toggle-word)
   (should
    (equal (car (test-visual-replace-run
                 "hello TAB world RET" (visual-replace-read
                                        (let ((args (visual-replace-make-args)))
                                          (setf (visual-replace-args-regexp args) t)
                                          args))))
           (visual-replace-make-args :from "hello" :to "world" :regexp t)))))

(ert-deftest test-visual-replace-goto-closest-match ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "hello, world, hello, hello!")
     (goto-char (point-min))
     (search-forward "world")

     (with-selected-window (display-buffer testbuf)
       (delete-other-windows (selected-window))

       (turtles-read-from-minibuffer
           (visual-replace-read)

         :keys "hello"

         (visual-replace--update-preview)
         (test-visual-run-idle-search-timers)

         (turtles-with-grab-buffer (:name "closest match" :buf testbuf :faces test-visual-replace-faces)
           (turtles-mark-point "<>")

           (should (equal "hello, world, <>[hello]*, [hello]!" (buffer-string)))))))))

(ert-deftest test-visual-replace-open-hideshow-blocks ()
  (turtles-ert-test)

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

       (turtles-read-from-minibuffer
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

(ert-deftest test-visual-replace-scroll-then-open-hideshow-block ()
  (turtles-ert-test)

  (test-visual-replace-env
   (let ((testbuf (current-buffer)))
     (insert "(defun test-1 ()\n")
     (dotimes (i 20)
       (insert (format " (message \"line %d\"))\n" i)))
     (insert "\n")
     ;; This is out of the default turtles screen (80x20) if test-1
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

       (turtles-read-from-minibuffer
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

;;; visual-replace-test.el ends here

;;; tests visual-replace.el -*- lexical-binding: t -*-

(require 'visual-replace)
(require 'ert)
(require 'ert-x)
(require 'seq)

(defvar test-visual-replace-snapshot nil
  "Variable filled with snapshots of the minibuffer. Snapshots
are triggered by the key F1 ! when running test-visual-replace-run.")

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
           (visual-replace-preview nil)
           (visual-replace-keep-incomplete t)
           (visual-replace-functions nil)
           (test-visual-replace-snapshot nil))
       (cl-letf (((symbol-function 'sit-for) (lambda (_))))
         (ert-with-test-buffer nil
           ,@body)))))

(defmacro test-visual-replace-run (macro &rest body)
  "Run BODY and execute MACRO.

MACRO should limit itself to keybindings starting with F1, TAB
and RET. To have access to additional key bindings than those
setup by this function in MACRO, bind them to a shortcut starting
with F1 in visual-replace-mode-map.

This is meant to be called within `test-visual-replace-env`."
  `(test-visual-replace-run-1 ,macro (lambda () ,@body)))

(defun test-visual-replace-run-1 (macro func)
  (save-window-excursion
    (let ((start-buffer (current-buffer))
          (result nil)
          (test-map (make-sparse-keymap))
          (launch-map (make-sparse-keymap))
          (f1-binding (lookup-key visual-replace-mode-map (kbd "<F1>")))
          (tab-binding (lookup-key visual-replace-mode-map (kbd "TAB")))
          (ret-binding (lookup-key visual-replace-mode-map (kbd "RET"))))
      (define-key launch-map (kbd "<F1>")
        (lambda ()
          (interactive)
          (setq result
                (with-current-buffer start-buffer
                  (let ((inhibit-quit t))
                    (prog1
                        (funcall func)
                      (setq quit-flag nil)))))))
      (set-transient-map launch-map)
      ;; Temporarily bind some keys in visual-replace-mode-map to have access
      ;; to the different commands no matter what visual-replace-mode-map
      ;; contains.
      (define-key visual-replace-mode-map (kbd "TAB") 'visual-replace-tab)
      (define-key visual-replace-mode-map (kbd "RET") 'visual-replace-enter)
      (define-key visual-replace-mode-map (kbd "<F1>") test-map)
      (unwind-protect
          (progn
            (define-key visual-replace-mode-map (kbd "<F1> !") 'test-visual-replace-snapshot)
            (define-key visual-replace-mode-map (kbd "<F1> _")
              (lambda () (interactive)
                (test-visual-replace-preview start-buffer)))
            (define-key visual-replace-mode-map (kbd "<F1> r") 'visual-replace-toggle-regexp)
            (define-key visual-replace-mode-map (kbd "<F1> q") 'visual-replace-toggle-query)
            (define-key visual-replace-mode-map (kbd "<F1> w") 'visual-replace-toggle-word)
            (define-key visual-replace-mode-map (kbd "<F1> s") 'visual-replace-toggle-scope)
            (define-key visual-replace-mode-map (kbd "<F1> c") 'visual-replace-toggle-case-fold)
            (define-key visual-replace-mode-map (kbd "<F1> l") 'visual-replace-toggle-lax-ws)
            (define-key visual-replace-mode-map (kbd "<F1> k") 'visual-replace-kill)
            (define-key visual-replace-mode-map (kbd "<F1> K") 'visual-replace-kill-whole-line)
            (define-key visual-replace-mode-map (kbd "<F1> y") 'visual-replace-yank)
            (define-key visual-replace-mode-map (kbd "<F1> Y") 'visual-replace-yank-pop)
            (define-key visual-replace-mode-map (kbd "<F1> h") 'previous-history-element)
            (define-key visual-replace-mode-map (kbd "<F1> x") 'exit-minibuffer)
            (define-key visual-replace-mode-map (kbd "<F1> g") ;; simulates a quit
              (lambda () 
                (interactive)
                (setq quit-flag t)
                (exit-minibuffer)))
            
            ;; The binding for the first <F1> is taken from launch-map and
            ;; starts visual-replace-read, after, when running visual-replace-read,
            ;; visual-replace-mode-map becomes active.
            (execute-kbd-macro (vconcat (kbd "<F1>") (kbd macro))))
        (define-key visual-replace-mode-map (kbd "<F1>") f1-binding)
        (define-key visual-replace-mode-map (kbd "TAB") tab-binding)
        (define-key visual-replace-mode-map (kbd "RET") ret-binding))
      result)))

(defun test-visual-replace-snapshot ()
  "Make a snapshot of the minibuffer and append it to `test-visual-replace-snapshot`."
  (interactive)
  (let ((display) (lastpos) (lastdisplay) (results))
    (with-current-buffer (window-buffer (minibuffer-window))
      (unwind-protect
          (save-excursion
            (insert "[]")
            (goto-char (point-min))
            (setq test-visual-replace-snapshot
                  (append test-visual-replace-snapshot
                          (list (test-visual-replace-buffer-substring
                                 (point-min) (point-max))))))
        (delete-region (point) (+ 2 (point)))))))

(defun test-visual-replace-buffer-substring (start end)
  "Returns the content of the buffer between START and END.

The returned string takes text properties 'display and 'invisible
into account, to better match what's displayed."
  (let ((display) (lastpos) (lastdisplay) (results))
    (save-excursion
      (goto-char start)
      (dolist (part (test-visual-replace-split-by-properties
                     (point-min) (point-max)))
        (let ((invisible (get-text-property 0 'invisible part))
              (display (get-text-property 0 'display part)))
          (when (or (null invisible)
                    (not (memq invisible buffer-invisibility-spec)))
            (push (or display part) results))))
      (apply 'concat (nreverse results)))))

(defun test-visual-replace-split-by-properties (start end)
  "Split the text between START and END by properties 'invisible and 'display."
  (save-excursion
    (goto-char start)
    (let ((parts) (next-point))
      (while (progn
               (setq next-point (test-visual-replace-next-properties-change (point) end))
               (push (buffer-substring (point) next-point) parts)
               (goto-char next-point)
               (< (point) end)))
      (nreverse parts))))

(defun test-visual-replace-next-properties-change (start end)
  "Find next position between START and END with a change in property 'invisible
and 'display."
  (let ((last-invisible (get-text-property start 'invisible))
        (last-display (get-text-property start 'display))
        (pos start))
    (while (progn
             (setq pos (next-property-change pos nil end))
             (and (< pos end)
                  (and (eq last-invisible (get-text-property pos 'invisible))
                       (eq last-display (get-text-property pos 'display))))))
    (or pos end)))

(defun test-visual-replace-overlay-to-properties ()
  "Converts overlays in the current buffer to properties."
  (save-excursion
    (let ((ovlists (overlay-lists)))
      (dolist (ov (append (car ovlists) (cdr ovlists)))
        (let ((start (overlay-start ov))
              (end (overlay-end ov))
              (props (overlay-properties ov)))
          (add-text-properties start end props))))))

(defun test-visual-replace-preview (buf)
  "Compute a preview for the current state.

The preview is put into test-visual-replace-snapshot, after
interpreting 'display and 'invisible. The face properties of the
overlays are kept as text properties."
  (with-current-buffer buf
    (visual-replace--update-preview)
    (let ((inhibit-read-only t))
      (test-visual-replace-overlay-to-properties)
      (setq test-visual-replace-snapshot
            (append test-visual-replace-snapshot
                    (list (test-visual-replace-buffer-substring
                           (point-min) (point-max))))))))

(defun test-visual-replace-highlight-face (text face)
  "Return a copy of TEXT with FACE highlighted.

The region of text with FACE are surrounded with []."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((active nil))
      (while (< (point) (point-max))
        (cond
         ((and (not active)
               (eq face (get-text-property (point) 'face)))
          (insert "[")
          (goto-char (1+ (point)))
          (setq active t))
         ((and active
               (not (eq face (get-text-property (point) 'face))))
          (insert "]")
          (goto-char (1+ (point)))
          (setq active nil)))
        (goto-char (next-property-change (point) nil (point-max)))))
    (buffer-substring (point-min) (point-max))))

(defun test-visual-replace-content ()
  (buffer-substring-no-properties (point-min) (point-max)))

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
                    "Replace from point: []hello → world"
                    "Replace from point: hello → []world")))))

(ert-deftest test-visual-replace-read-TAB-navigation-while-whitin-section ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <left> TAB <F1> ! <right> TAB <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: []hello → world"
                    "Replace from point: hello → []world")))))

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
     (equal (car (test-visual-replace-run
                  "hello TAB world <F1> c <F1> ! RET" (visual-replace-read)))
            (visual-replace-make-args :from "hello" :to "world" :case-fold t)))
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
                  '("Replace from point: []hello →?.* world")))))

(ert-deftest test-visual-replace-read-toggle-scope-display ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB world <F1> ! <F1> s <F1> ! <F1> s <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hello → world[]"
                    "Replace in buffer: hello → world[]"
                    "Replace from point: hello → world[]")))))

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
                  '("Replace in region (2L): hello → world[]"
                    "Replace in buffer: hello → world[]"
                    "Replace in region (2L): hello → world[]")))))

(ert-deftest test-visual-replace-read-toggle-scope-with-region ()
  (test-visual-replace-env
   (let ((ranges (nth 1 (test-visual-replace-run "hello TAB world RET"
                                             (test-visual-replace-setup-region)
                                             (visual-replace-read)))))
     (should (equal ranges (list (cons (mark) (point))))))))

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

(ert-deftest test-visual-replace-kill ()
  (test-visual-replace-env
   (test-visual-replace-run
    "hello TAB world TAB <right> <F1> k <F1> ! TAB <right> <F1> k <F1> ! RET"
    (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: h[] → world"
                    "Replace from point: h → w[]")))))

(ert-deftest test-visual-replace-kill-no-separator ()
  (test-visual-replace-env
   (test-visual-replace-run "hello <left> <F1> k <F1> ! RET" (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: hell[]")))))

(ert-deftest test-visual-replace-kill-whole-line ()
  (test-visual-replace-env
   (test-visual-replace-run
    "hello TAB world TAB <right> <F1> K <F1> ! TAB <right> <F1> K <F1> ! RET"
    (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] → world"
                    "Replace from point:  → []")))))

(ert-deftest test-visual-replace-kill-whole-line-no-separator ()
  (test-visual-replace-env
   (test-visual-replace-run "hello <F1> K <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot '("Replace from point: []")))))

(ert-deftest test-visual-replace-yank-in-from ()
  (test-visual-replace-env
   (save-excursion (insert "from buffer"))
   (test-visual-replace-run "<F1> y TAB <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from → []")))))

(ert-deftest test-visual-replace-multiple-yank-in-from ()
  (test-visual-replace-env
   (save-excursion (insert "from current buffer"))
   (test-visual-replace-run "<F1> y <F1> ! <F1> y <F1> ! TAB RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from[]"
                    "Replace from point: from current[]")))))

(ert-deftest test-visual-replace-yank-in-from-with-prompt ()
  (test-visual-replace-env
   (save-excursion (insert "from buffer"))
   (test-visual-replace-run "TAB world TAB <F1> y <F1> ! RET"
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: from[] → world")))))

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
   (should-error (test-visual-replace-run "TAB RET" (visual-replace)))))

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
           (visual-replace-make-args :from "hello" :to "xworld" :regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point [hello →.* world]: hello →.* x[]world")))))

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

(ert-deftest test-visual-replace-history-regex-toggle ()
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

(ert-deftest test-visual-replace-kill-and-yank-separator ()
  (test-visual-replace-env
   (test-visual-replace-run "hello TAB TAB <F1> q <F1> ! <F1> u <F1> ! TAB <F1> ! RET"
                        (define-key visual-replace-mode-map (kbd "<F1> q")
                          (lambda () (interactive) (call-interactively 'kill-line)))
                        (define-key visual-replace-mode-map (kbd "<F1> u")
                          (lambda () (interactive) (call-interactively 'yank)))
                        (visual-replace-read))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: []"
                    "Replace from point: hello []"
                    "Replace from point: hello  → []")))))

(ert-deftest test-visual-replace-initial-input ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> x" (visual-replace-read (visual-replace-make-args :from "initial")))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: initial[]")))))

(ert-deftest test-visual-replace-initial-input-complete ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> x"
    (visual-replace-read (visual-replace-make-args :from "foo" :to "bar")))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: foo → bar[]")))))

(ert-deftest test-visual-replace-initial-word ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> x"
    (visual-replace-read (visual-replace-make-args :word t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →w ")))))

(ert-deftest test-visual-replace-initial-regexp ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> x"
    (visual-replace-read (visual-replace-make-args :regexp t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →.* ")))))

(ert-deftest test-visual-replace-initial-query ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> x"
    (visual-replace-read (visual-replace-make-args :query t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →? ")))))

(ert-deftest test-visual-replace-initial-case-fold-enable ()
  (test-visual-replace-env
   (let ((case-fold-search nil))
     (test-visual-replace-run
      "<F1> ! <F1> x"
      (visual-replace-read (visual-replace-make-args :case-fold t)))
     (should (equal test-visual-replace-snapshot
                    '("Replace from point: [] →i "))))))

(ert-deftest test-visual-replace-initial-case-fold-disable ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> x"
    (visual-replace-read (visual-replace-make-args :case-fold nil)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: [] →c ")))))

(ert-deftest test-visual-replace-initial-case-fold-matches-default ()
  (test-visual-replace-env
   (test-visual-replace-run
    "<F1> ! <F1> x"
    (visual-replace-read (visual-replace-make-args :case-fold t)))
   (should (equal test-visual-replace-snapshot
                  '("Replace from point: []")))))

(ert-deftest test-visual-replace-preview ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (display-buffer (current-buffer))
   (test-visual-replace-run "hel <F1> _ l <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (nth 0 test-visual-replace-snapshot) 'visual-replace-match)
                  "[hel]lo, world, [hel]lo, [hel]lo!"))
   (should (equal (test-visual-replace-highlight-face
                   (nth 1 test-visual-replace-snapshot) 'visual-replace-match)
                  "[hell]o, world, [hell]o, [hell]o!"))))

(ert-deftest test-visual-replace-preview-delete ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (display-buffer (current-buffer))
   (test-visual-replace-run "hell TAB <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot) 'visual-replace-delete-match)
                  "[hell]o, world, [hell]o, [hell]o!")))) 

(ert-deftest test-visual-replace-preview-replace ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (display-buffer (current-buffer))
   (test-visual-replace-run "hell TAB hul <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot) 'visual-replace-delete-match)
                  "[hell]hulo, world, [hell]hulo, [hell]hulo!"))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot) 'visual-replace-replacement)
                  "hell[hul]o, world, hell[hul]o, hell[hul]o!"))))

(ert-deftest test-visual-replace-preview-regex ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (display-buffer (current-buffer))
   (test-visual-replace-run "hel+ <F1> r <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot) 'visual-replace-delete-match)
                  "[hell]o, world, [hell]o, [hell]o!"))))

(ert-deftest test-visual-replace-preview-lax-ws ()
  (test-visual-replace-env
   (insert "hello   world!")
   (goto-char (point-min))
   (display-buffer (current-buffer))
   (test-visual-replace-run "hello SPC world <F1> l <F1> _ <F1> x" (visual-replace-read))
   (should (equal (test-visual-replace-highlight-face
                   (car test-visual-replace-snapshot) 'visual-replace-delete-match)
                  "[hello   world]!"))))

(ert-deftest test-visual-replace-preview-bad-regex ()
  (test-visual-replace-env
   (insert "hello, world, hello, hello!")
   (goto-char (point-min))
   (display-buffer (current-buffer))
   ;; This is just a smoke test. \b\b\b matches empty strings, which
   ;; cannot be displayed and might cause some implementations to
   ;; enter an infinite loop.
   (test-visual-replace-run "<F1> r \\b\\b\\b <F1> _ <F1> x" (visual-replace-read))))

(ert-deftest test-visual-replace-preview-skip-readonly ()
  (test-visual-replace-env
   (insert "hello, world, ")
   (let ((start-read-only (point))
         (end-read-only nil))
     (insert "hello")
     (setq end-read-only (point))
     (insert ", hello!")
     (set-text-properties start-read-only end-read-only '(read-only t)))
   (goto-char (point-min))
   (display-buffer (current-buffer))
   (test-visual-replace-run "hel TAB hol <F1> _ RET"
                        (call-interactively 'visual-replace))
   (should (equal (test-visual-replace-highlight-face
                   (nth 0 test-visual-replace-snapshot) 'visual-replace-replacement)
                  "hel[hol]lo, world, hello, hel[hol]lo!"))
   (should (equal
            "hollo, world, hello, hollo!"
            (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-visual-replace-read-only-buffer ()
  :expected-result :failed
  (test-visual-replace-env
   (insert "foo")
   (read-only-mode)
   (visual-replace (visual-replace-make-args :from "foo" :to "bar"))))

(ert-deftest test-visual-replace-read-read-only-buffer ()
  :expected-result :failed
  (test-visual-replace-env
   (insert "foo")
   (read-only-mode)
   (visual-replace-read)))

(ert-deftest test-visual-replace-merge-range ()
  (should (equal
           '((1 . 10) (20 . 30))
           (visual-replace--ranges-nmerge '((20 . 30) (1 . 10)))))
  (should (equal
           '((1 . 15))
           (visual-replace--ranges-nmerge '((1 . 10) (5 . 15)))))
  (should (equal
           '((1 . 15))
           (visual-replace--ranges-nmerge '((1 . 10) (1 . 15)))))
  (should (equal
           '((0 . 100))
           (visual-replace--ranges-nmerge '((1 . 10) (0 . 100)))))
  (should (equal
           '((1 . 10) (20 . 30))
           (visual-replace--ranges-nmerge '((20 . 25) (25 . 30) (1 . 5) (5 . 10))))))

(ert-deftest test-visual-replace-intersect-sorted ()
  (should (equal
           '((10 . 20) (30 . 40))
           (visual-replace--range-intersect-sorted '((0 . 100))
                                               '((10 . 20) (30 . 40) (200 . 210)))))
  (should (equal
           '((10 . 15) (30 . 35))
           (visual-replace--range-intersect-sorted '((0 . 15) (30 . 40))
                                               '((10 . 20) (25 . 35)))))
  (should (equal
           '((10 . 15) (30 . 35))
           (visual-replace--range-intersect-sorted '((10 . 15) (30 . 35))
                                               '((10 . 15) (30 . 35)))))
  (should (equal
           nil
           (visual-replace--range-intersect-sorted nil '((10 . 15) (30 . 35)))))
  (should (equal
           nil
           (visual-replace--range-intersect-sorted nil nil))))


(ert-deftest test-visual-replace-range-fix ()
  (should (equal nil (visual-replace--ranges-fix nil)))
  (should (equal '((10 . 100)) (visual-replace--ranges-fix '((100 . 10)))))
  (should (equal '((10 . 20) (30 . 40)) (visual-replace--ranges-fix '((10 . 20) (30 . 35) (32 . 40))))))

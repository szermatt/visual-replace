;;; test-helper.el --- Helpers for visual-replace-test.el -*- lexical-binding: t -*-

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
           (visual-replace-initial-scope nil)
           (visual-replace-default-to-full-scope nil)
           (test-visual-replace-snapshot nil))
       (cl-letf (((symbol-function 'sit-for) (lambda (_)))
                 ((symbol-function 'window-end) #'test-visual-window-end))
         (ert-with-test-buffer nil
           ,@body)))))

(defun test-visual-window-end (win)
  "Compute window-end on demand for WIN.

In normal situations, `window-end' is computed at the end of a
redisplay. This is efficient, but causes problems during tests."
  (with-current-buffer (window-buffer win)
    (save-excursion
      (goto-char (window-start win))
      (forward-line (window-height win))
      (point))))

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

(defun test-visual-replace-preview (buf)
  "Compute a preview for the current state.

The preview is put into test-visual-replace-snapshot, after
interpreting 'display and 'invisible. The face properties of the
overlays are kept as text properties."
  (with-current-buffer buf
    (visual-replace--update-preview)
    (setq test-visual-replace-snapshot
          (append test-visual-replace-snapshot
                  (list (visual-replace-test-content))))))

(defun test-visual-replace-highlight-property (text property mark)
  "Add MARK to TEXT where PROPERTY first turns non-nil."
  (let ((pos (if (get-text-property 0 property text)
                 0
               (next-single-property-change 0 property text ))))
    (if pos
        (concat (substring text 0 pos) mark (substring text pos (length text)))
      text)))

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
  (let ((win (or win (selected-window)))
        (text))
    (with-current-buffer (window-buffer win)
      (save-restriction
        (narrow-to-region (window-start win) (window-end win))
        (visual-replace-test-content)))))

(defun visual-replace-test-content ()
  "Return the content of the current buffer.

Sections that are invisible are not included into the text.

Section with a 'display overlay property are included instead of
the text, with the display text put within curly braces.

Other overlay properties are stored into the returned text as
text properties."
  (save-excursion
    (let ((last (point-min))
          (sections)
          (ov-props))
      (dolist (ov (car (overlay-lists)))
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
               (text (buffer-substring-no-properties (point) last))
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
                 (display (concat "{" display "}"))
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

;;; test-helper.el ends here

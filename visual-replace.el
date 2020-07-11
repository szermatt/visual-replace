;;; visual-replace.el --- A prompt for replace-string and query-replace -*- lexical-binding: t -*-

;; Copyright (C) 2020 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmail.com>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 0.1
;; Keywords: replace query-replace
;; URL: http://github.com/szermatt/visual-replace
;; Package-Requires: ((emacs "26.1"))


;;; Commentary:
;;
;; This file provides the command `visual-replace', which provides a nicer
;; frontend for the commands `replace-string', `replace-regexp',
;; `query-replace' and `query-replace-regexp'.
;;
;; `visual-replace` allows editing both the text to replace and its
;; replacement at the same time and provide a preview of what the
;; replacement would look like in the current buffer.
;;
;; It's possible to toggle options on an off while editing a
;; query/replace query, including:
;;
;; - whether the search is a regexp, with `visual-replace-toggle-regexp',
;; bound by default to M-% r or, more precisely, <visual-replace shortcut> r
;
;; - whether to ask for confirmation like `query-replace' does, with
;; `visual-replace-toggle-query', bound by default to M-% q or, more
;; precisely, <visual-replace shortcut> q
;
;; - whether to search for a word, with `visual-replace-toggle-word',
;; bound by default to M-% w or, more precisely, <visual-replace shortcut>
;; w
;
;; - whether to apply the replacement to the whole buffer with
;; `visual-replace-toggle-scope', bound by default to M-% SPC or, more
;; precisely, <visual-replace shortcut> SPC
;;
;; Notable differences between `query-replace' and `visual-replace':
;;  - `visual-replace' keeps the point where it was before the replacement
;;  - `query-replace-skip-read-only' is always set
;;
;; This package also defines the command `visual-replace-from-isearch',
;; which allows switching from isearch to `visual-replace', while keeping
;; as much of the current isearch flags as possible.

;;; Installation
;;
;; To replace `query-replace' with `visual-replace' globally, do:
;;
;;   (require 'visual-replace)
;;   (visual-replace-global-mode 1)
;;
;; With this setup, `visual-replace' replaces `query-replace', bound by
;; default to M-%. While on the minibuffer M-% is used as prefix for
;; toggling options, defined in `visual-replace-secondary-mode-map', so to
;; toggle regexp on and off, type "M-% r".
;;
;; Here's an alternative example that uses
;; https://github.com/jwiegley/use-package to initialize visual-replace
;; and bound it to custom shortcuts:
;;
;; (use-package visual-replace
;;   :defer t
;;   :bind (("C-c l" . visual-replace)
;;          :map isearch-mode-map
;;          ("C-c l" . visual-replace-from-isearch)))
;;
;; With this setup, `visual-replace' is bound to "C-c l". While on the
;; minibuffer "C-c l" is used as prefix for toggling options, defined
;; in `visual-replace-secondary-mode-map', so to toggle regexp on and off,
;; type "C-c l r".
;;

(require 'seq)
(eval-when-compile (require 'subr-x)) ;; if-let

;;; Code:

(defcustom visual-replace-keep-incomplete t
  "Make value from interrupted session available.

When this is on, the first element of the history might contain
incomplete value from the last minibuffer session that was
interrupted."
  :type 'boolean
  :group 'visual-replace)
  
(defcustom visual-replace-preview t
  "If true, highlight the matches while typing."
  :type 'boolean
  :group 'visual-replace)
  
(defcustom visual-replace-preview-delay 0.1
  "Highlight matchs after that many seconds of inactivity.

When `visual-replace-preview' is enabled, only refresh the preview
after the user stopped typing for that long. Increasing this
value on slow machines or connection is a good idea. Decreasing
this value lower than 0.1s might cause issues."
  :type 'number
  :group 'visual-replace)

(defcustom visual-replace-preview-max-duration 0.1
  "How much time to spend computing the preview.

Allow that much time to compute the preview. If computing the
preview takes longer than that, give up. This avoids allowing
Emacs freezing because of an overly complex query."
  :type 'number
  :group 'visual-replace)

(defface visual-replace-match
  '((t :inherit query-replace))
  "How to display the string that was matched.

This is the face that's used to highlight matches, before a
replacement has been defined."
  :group 'visual-replace)

(defface visual-replace-delete-match
  '((((class color)) :strike-through t :background "red" :foreground "black")
    (t :inverse-video t))
  "How to display the string to be replaced.

This is the face that's used to show the replacement string, once a replacement
has been defined."
  :group 'visual-replace)

(defface visual-replace-replacement
  '((t :inherit match))
  "How to display the replacement string.

This is the face that's used to show the replacement string, once a replacement
has been defined."
  :group 'visual-replace)

(defvar visual-replace-secondary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") 'visual-replace-toggle-regexp)
    (define-key map (kbd "SPC") 'visual-replace-toggle-scope)
    (define-key map (kbd "q") 'visual-replace-toggle-query)
    (define-key map (kbd "w") 'visual-replace-toggle-word)
    (define-key map (kbd "c") 'visual-replace-toggle-case-fold)
    (define-key map (kbd "s") 'visual-replace-toggle-lax-ws)
    map)
  "Keyboard shortcuts specific to `visual-replace'.

This map is, by default, bound to the prefix that corresponds to
the shortcut that was used to trigger `visual-replace'.")
  
(defvar visual-replace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap isearch-toggle-regexp] 'visual-replace-toggle-regexp)
    (define-key map [remap isearch-toggle-word] 'visual-replace-toggle-word)
    (define-key map [remap isearch-toggle-case-fold] 'visual-replace-toggle-case-fold)
    (define-key map [remap isearch-toggle-lax-whitespace] 'visual-replace-toggle-lax-ws)
    
    (define-key map (kbd "RET") 'visual-replace-enter)
    (define-key map (kbd "<return>") 'visual-replace-enter)
    
    (define-key map (kbd "TAB") 'visual-replace-tab)
    (define-key map (kbd "<tab>") 'visual-replace-tab)
    
    (define-key map [remap yank] 'visual-replace-yank)
    (define-key map [remap yank-pop] 'visual-replace-yank-pop)
    (define-key map [remap kill] 'visual-replace-kill)
    (define-key map [remap kill-whole-line] 'visual-replace-kill-whole-line)
    map)
"Map of minibuffer keyboard shortcuts available when editing a query.

Note also the shortcuts bound to a prefix key that correspond to
the shortcut used to start `visual-replace'. If, for example, you
start `visual-replace' with \"C-c r\", then you'll be able to toggle
regexp support with \"C-c r r\". See `visual-replace-secondary-mode-map'.

Inherits from `minibuffer-mode-map'.")

(define-minor-mode visual-replace-minibuffer-mode
  "Local minibuffer mode for `visual-replace'.

Not normally turned on manually."
  :keymap visual-replace-mode-map)

(defvar visual-replace-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap query-replace] 'visual-replace)
    (define-key map [remap replace-string] 'visual-replace)
    (define-key map [remap isearch-query-replace] 'visual-replace-from-isearch)
    (define-key map [remap isearch-query-replace-regexp] 'visual-replace-from-isearch)
    map))

(define-minor-mode visual-replace-global-mode
  "Global mode for remapping `query-replace' to `visual-replace.'"
  :keymap visual-replace-global-mode-map
  :global t)

(defvar visual-replace-functions nil
  "Hooks that modify a `visual-replace-args' instance, just before execution.

The hooks are called in order, with one argument, the
`visual-replace-args' instance to modify.")

(cl-defstruct (visual-replace-args (:constructor visual-replace-make-args)
                               (:copier visual-replace-copy-args)
                               (:type vector))
  "Query/replace arguments.

This structure collects arguments to pass to `visual-replace'.
`visual-replace-read` builds such a structure, but also accepts
one, as initial value.

`visual-replace-make-args' creates new instances.
`visual-replace-copy-args' to make copies of existing instances.

Slots:

from               Text to modify. Might be a regexp if regexp is t.
to                 Replacement string.
regexp             if t, from is a regexp and to might include back-references,
                   such as `\\&' and `\\N'.
query              if t, replacement behaves as `query-replace'.
word               if t, from is a word
case-fold          overrides `case-fold-search` for the current query
lax-ws-non-regexp  overrides `replace-lax-whitespace` for the current query
lax-ws-regexp      overrides `replace-regexp-lax-whitespace` for the current query

To read or edit the lax-ws value that's appropriate to the
current value of regexp, call `visual-replace-args-lax-ws'.
"
  from to
  regexp query word
  (case-fold case-fold-search)
  (lax-ws-non-regexp replace-lax-whitespace)
  (lax-ws-regexp replace-regexp-lax-whitespace))

(defun visual-replace-args-lax-ws (args)
  "Return the appropriate lax whitespace setting for ARGS.

Returns either lax-ws-non-regexp or lax-ws-regexp, depending on
the value of the regexp slot."
  (if (visual-replace-args-regexp args)
      (visual-replace-args-lax-ws-regexp args)
    (visual-replace-args-lax-ws-non-regexp args)))

(defun visual-replace-args-lax-ws-default (args)
  "Return the appropriate default for lax whitespace for ARGS.

Returns either `replace-lax-whitespace' or
`replace-lax-whitespace', depending on the value of the regexp
slot."
  (if (visual-replace-args-regexp args)
      replace-regexp-lax-whitespace
    replace-lax-whitespace))

(defvar visual-replace-read-history nil
  "History of `visual-replace-read`.

Each entry is a struct `visual-replace-args'.")

(defvar visual-replace--scope nil
  "What replace applies to: 'region 'from-point or 'full.")

(defvar visual-replace--calling-buffer nil
  "Buffer from which `visual-replace' was called.")

(defvar visual-replace--calling-point nil
  "Position of the point in the buffer when `visual-replace' was called.")

(defvar visual-replace--calling-region nil
  "The region originally active in the calling buffer.

Expressed as a list of (start . end), returned by `region-bounds'.")

(defvar visual-replace--overlays nil
  "Overlays added for the preview in the calling buffer.")

(defvar visual-replace--incomplete nil
  "Replacement text entered, but not confirmed.")

(defun visual-replace-enter ()
  "Confirm the current text to replace.

If both the text to replace and its replacement have been
defined, execute the replacement. If only the text to replace
has been defined, create a new field to fill in the replacement.

See also `visual-replace-tab'."
  (interactive)
  (visual-replace--update-separator (visual-replace-args--from-minibuffer))
  (let ((separator-start (visual-replace--separator-start))
        (separator-end (visual-replace--separator-end)))
    (cond
     ((and (= (point) (minibuffer-prompt-end))
           (= (point) separator-start))
      (exit-minibuffer))
     ((and (<= (point) separator-start)
           (= (point-max) separator-end))
      (goto-char (point-max)))
     (t (exit-minibuffer)))))

(defun visual-replace-tab ()
  "Replacement for TAB while building args for `visual-replace'.

Introduce a separator or navigate between fields.

See also `visual-replace-enter'."
  (interactive)
  (visual-replace--update-separator (visual-replace-args--from-minibuffer))
  (let ((separator-start (visual-replace--separator-start))
        (separator-end (visual-replace--separator-end)))
    (if (<= (point) separator-start)
        (goto-char separator-end)
     (goto-char (minibuffer-prompt-end)))))

(defun visual-replace-yank ()
  "Replacement for `yank' while building args for `visual-replace'.

When editing the text to be replaced, insert the text at point.
Multiple calls to `visual-replace-yank` put more and more of the text
at point into the field.

When editing the replacement text, insert the original text.

See also `visual-replace-yank-pop'."
  (interactive)
  (let ((separator-start (visual-replace--separator-start))
        (separator-end (visual-replace--separator-end)))
    (cond
     ;; in the modification section
     ((and separator-start (>= (point) separator-end))
      (insert (buffer-substring-no-properties (minibuffer-prompt-end)
                                              separator-start)))
     ;; in the original section
     (t (insert (with-current-buffer visual-replace--calling-buffer
                  (let ((start (point)))
                    (forward-word)
                    (buffer-substring-no-properties start (point)))))))))

(defun visual-replace-yank-pop ()
  "Replacement for `yank-pop' while building args for `visual-replace'.

The first time it's called, executes a `yank', then a `yank-pop'."
  (interactive)
  (if (memq last-command '(yank yank-pop))
      (progn  (setq this-command 'yank-pop)
              (call-interactively 'yank-pop))
    ;; If previous command was not a yank, call yank. This gives
    ;; access to yank for the modified test.
    (setq this-command 'yank)
    (yank)))

(defun visual-replace-kill ()
  "Replacement for command`kill' for `visual-replace'.

This kills to separator or end of line."
  (interactive)
  (let ((separator-start (visual-replace--separator-start)))
    (if (and separator-start (< (point) separator-start))
        (kill-region (point) separator-start)
      (kill-line))))

(defun visual-replace-kill-whole-line ()
  "Replacement for command `kill-whole-line' for `visual-replace'.

This kills the whole section."
  (interactive)
  (let ((separator-start (visual-replace--separator-start)))
    (cond
     ((and separator-start (< (point) separator-start))
      (kill-region (minibuffer-prompt-end) separator-start))
     (separator-start
      (kill-region (visual-replace--separator-end) (line-end-position)))
     (t (kill-region (minibuffer-prompt-end) (line-end-position))))))

(defun visual-replace-toggle-regexp ()
  "Toggle the regexp flag while building arguments for `visual-replace'."
  (interactive)
  (let ((args (visual-replace-args--from-minibuffer)))
    (if (visual-replace-args-regexp args)
        (setf (visual-replace-args-regexp args) nil)
      (setf (visual-replace-args-regexp args) t)
      (setf (visual-replace-args-word args) nil))
    (visual-replace--update-separator args 'forced)))

(defun visual-replace-toggle-query ()
  "Toggle the query flag while building arguments for `visual-replace'."
  (interactive)
  (let ((args (visual-replace-args--from-minibuffer)))
    (setf (visual-replace-args-query args)
          (not (visual-replace-args-query args)))
    (visual-replace--update-separator args 'forced)))

(defun visual-replace-toggle-word ()
  "Toggle the word-delimited flag while building arguments for `visual-replace'."
  (interactive)
  (let ((args (visual-replace-args--from-minibuffer)))
    (if (visual-replace-args-word args)
        (setf (visual-replace-args-word args) nil)
      (setf (visual-replace-args-word args) t)
      (setf (visual-replace-args-regexp args) nil))
    (visual-replace--update-separator args 'forced)))

(defun visual-replace-toggle-case-fold ()
  "Toggle the case-fold flag while building arguments for `visual-replace'."
  (interactive)
  (let ((args (visual-replace-args--from-minibuffer)))
    (setf (visual-replace-args-case-fold args)
          (not (visual-replace-args-case-fold args)))
    (visual-replace--update-separator args 'forced)))

(defun visual-replace-toggle-lax-ws ()
  "Toggle the lax-ws flag while building arguments for `visual-replace'."
  (interactive)
  (let* ((args (visual-replace-args--from-minibuffer))
         (newval (not (visual-replace-args-lax-ws args))))
    (setf (visual-replace-args-lax-ws-regexp args) newval)
    (setf (visual-replace-args-lax-ws-non-regexp args) newval)
    (visual-replace--update-separator args 'forced)))

(defun visual-replace-toggle-scope ()
    "Toggle the scope while building arguments for `visual-replace'."
  (interactive)
  (setq visual-replace--scope
        (if visual-replace--calling-region
            (pcase visual-replace--scope
              ('region 'full)
              (_ 'region))
          (pcase visual-replace--scope
            ('from-point 'full)
            (_ 'from-point))))
  (visual-replace--setup-invisibility-spec))

(defun visual-replace-read (&optional initial-args)
  "Read arguments for `query-replace'.

INITIAL-ARGS is used to set the prompt's initial state, if
specified. It must be a `visual-replace-args' struct."
  (barf-if-buffer-read-only)
  (let ((history-add-new-input nil)
        (visual-replace--calling-buffer (current-buffer))
        (visual-replace--calling-point (point))
        (visual-replace--calling-region (when (region-active-p) (region-bounds)))
        (visual-replace--scope (if (region-active-p) 'region 'from-point))
        (minibuffer-allow-text-properties t) ; separator uses text-properties
        (minibuffer-history (mapcar 'visual-replace-args--text visual-replace-read-history))
        (initial-input (let* ((args (or initial-args (visual-replace-make-args)))
                              (text (visual-replace-args--text args))
                              (from (visual-replace-args-from args)))
                         (cons text (if from (1+ (length text)) 0))))
        (trigger (this-command-keys-vector))
        (default-value)
        (text)
        (timer))
    (setq default-value (car minibuffer-history))
    (when visual-replace--incomplete
      (push visual-replace--incomplete minibuffer-history))
    (save-excursion
      (unwind-protect
          (progn
            (when visual-replace-preview
              (setq timer (run-with-idle-timer
                           visual-replace-preview-delay
                           'repeat 'visual-replace--update-preview)))
            (minibuffer-with-setup-hook
                (lambda ()
                  (when visual-replace-keep-incomplete
                    (add-hook 'after-change-functions 'visual-replace--after-change 0 'local))
                  (when trigger
                    (local-set-key trigger visual-replace-secondary-mode-map))
                  (visual-replace-minibuffer-mode t)
                  (visual-replace--setup-invisibility-spec)
                  (setq-local yank-excluded-properties (append '(separator display face) yank-excluded-properties))
                  (setq-local text-property-default-nonsticky
                              (append '((separator . t) (face . t))
                                      text-property-default-nonsticky)))
              (setq text (read-from-minibuffer
                          (concat "Replace "
                                  (visual-replace--scope-text)
                                  (if default-value (format " [%s]" default-value) "")
                                ": ")
                          initial-input nil nil nil (car search-ring) t))))
        ;; unwind
        (when timer (cancel-timer timer))
        (visual-replace--clear-preview)))
    (unless quit-flag (setq visual-replace--incomplete nil))
    (let* ((final-args (visual-replace-args--from-text text))
           (from (visual-replace-args-from final-args))
           (to (visual-replace-args-to final-args)))
      (cond
       ((or quit-flag (null to) nil)
        (setq final-args (visual-replace-make-args)))
       ((and (zerop (length from)) (zerop (length to)))
        (setq final-args (car visual-replace-read-history)))
       (t
        (when (visual-replace-args-regexp final-args)
          (visual-replace--warn from))
        (add-to-history query-replace-from-history-variable from nil t)
        (add-to-history query-replace-to-history-variable to nil t)
        (add-to-history 'visual-replace-read-history final-args nil t)))
      ;; visual-replace argument list
      (list final-args (visual-replace--scope-ranges)))))

;;;###autoload
(defun visual-replace (args ranges)
  "Replace text.

ARGS specifies the text to replace, the replacement and any
flags. It is a `visual-replace-args' struct, usually one created by
`visual-replace-read'.

Replacement applies in the current buffer on RANGES, a list
of (start . end) as returned by `region-bounds'."
  (interactive (visual-replace-read (visual-replace-make-args
                                 :word (and current-prefix-arg (not (eq current-prefix-arg '-))))))
  (barf-if-buffer-read-only)
  (let* ((origin (make-marker))
         (args (visual-replace-preprocess args))
         (from (visual-replace-args-from args)))
    (unwind-protect
        (progn
          (set-marker origin (point))
          (unless (and (stringp from) (not (zerop (length from))))
            (error "Nothing to replace"))
          (let ((case-fold-search (visual-replace-args-case-fold args))
                (replace-lax-whitespace
                 (visual-replace-args-lax-ws-non-regexp args))
                (replace-regexp-lax-whitespace
                 (visual-replace-args-lax-ws-regexp args))
                (query-replace-skip-read-only t))
            (dolist (range (visual-replace--ranges-fix ranges))
              (perform-replace
               from
               (visual-replace-args-to args)
               (visual-replace-args-query args)
               (visual-replace-args-regexp args)
               (visual-replace-args-word args)
               nil nil (car range) (cdr range))))
          (goto-char origin))
      (set-marker origin nil))))

;;;###autoload
(defun visual-replace-from-isearch ()
    "Switch from isearch to `visual-replace'.

This function attempts to copy as much of the current state of
isearch as possible, with the text being searched set as query
for `visual-replace'. Replacement starts at the current point."
  (interactive)
  (let ((args
         (visual-replace-make-args
          :from isearch-string
          :regexp isearch-regexp
          :word isearch-regexp-function
          :case-fold isearch-case-fold-search)))
    (when (seq-position isearch-string ?\ )
      (if isearch-regexp
          (setf (visual-replace-args-lax-ws-regexp args)
                isearch-regexp-lax-whitespace)
        (setf (visual-replace-args-lax-ws-non-regexp args)
              isearch-lax-whitespace)))
    (isearch-exit)
    (apply 'visual-replace (visual-replace-read args))))

(defun visual-replace-args--text (args &optional force-separator)
  "Build the text representation of ARGS, a `visual-replace-args' struct.

The text representation is the content of minibuffer that would result
in such a struct being returned by `visual-replace-read'.

Unless FORCE-SEPARATOR is non-nil, only add a separator if
necessary, to capture flags defined in ARGS."
  (let ((flags-text
         (concat
          (if (visual-replace-args-query args) "?" "")
          (cond ((eq (visual-replace-args-lax-ws args)
                     (visual-replace-args-lax-ws-default args))
                 "")
                ((visual-replace-args-lax-ws args) "(lax ws)")
                (t "(strict ws)"))
          (cond ((eq (visual-replace-args-case-fold args) case-fold-search) "")
                ((visual-replace-args-case-fold args) "i")
                (t "c"))
          (if (visual-replace-args-regexp args) ".*" "")
          (if (visual-replace-args-word args) "w" "")))
        (from-text (or (visual-replace-args-from args) "")))
    (if (and (not force-separator)
             (null (visual-replace-args-to args))
             (equal "" flags-text))
        from-text
      (let ((stored-args (visual-replace-copy-args args)))
        (setf (visual-replace-args-from stored-args) nil)
        (setf (visual-replace-args-to stored-args) nil)
        (concat
         from-text
         (propertize " "
                     'display (concat " →" flags-text " ")
                     'visual-replace-args stored-args
                     'face 'minibuffer-prompt
                     'separator t)
         (or (visual-replace-args-to args) ""))))))

(defun visual-replace-args--separator (args)
  "Return the separator for ARGS, a `visual-replace-args'."
  (let ((flag-args (visual-replace-copy-args args)))
    (setf (visual-replace-args-from flag-args) nil)
    (setf (visual-replace-args-to flag-args) nil)
    (visual-replace-args--text flag-args 'forced)))

(defun visual-replace-args--from-text (text)
  "Build a `visual-replace-args' that corresponds to TEXT.

TEXT is the textual content of the minibuffer, with properties."
  (if-let ((start (text-property-any 0 (length text) 'separator t text)))
      (let ((end (or (text-property-not-all start (length text) 'separator t text)
                     (length text)))
            (args (visual-replace-copy-args
                   (get-text-property start 'visual-replace-args text))))
        (setf (visual-replace-args-from args)
              (substring-no-properties text 0 start))
        (setf (visual-replace-args-to args)
              (substring-no-properties text end))
        args)
    (visual-replace-make-args :from text)))

(defun visual-replace-args--from-minibuffer ()
  "Build a `visual-replace-args' from the minibuffer content."
  (visual-replace-args--from-text
   (with-selected-window
       (or (active-minibuffer-window)
           (minibuffer-window))
     (minibuffer-contents))))

(defun visual-replace--scope-text ()
  "Build prompt text that reflects the current scope.

This returns text for all prompt, with different visibility
spec. `visual-replace--setup-invisibility-spec' sets the appropriate
spec for the current state."
  (let ((all-scopes '(region from-point full)))
    (mapconcat (lambda (scope)
                 (let ((text (pcase scope
                               ('region
                                (format "in region (%sL)"
                                        (if (mark)
                                            (1+ (- (line-number-at-pos (max (point) (mark)))
                                                   (line-number-at-pos (min (point) (mark)))))
                                          0)))
                               ('from-point "from point")
                               ('full "in buffer"))))
                   (add-text-properties 0 (length text)
                                        (list 'invisible scope)
                                        text)
                   text))
               all-scopes
               "")))

(defun visual-replace--scope-ranges ()
  "Return the regions replacement should work on.

Returns a list of (start . end)"
  (with-current-buffer visual-replace--calling-buffer
    (pcase visual-replace--scope
      ('from-point (list (cons visual-replace--calling-point (point-max))))
      ('full (list (cons (point-min) (point-max))))
      ('region visual-replace--calling-region))))

(defun visual-replace--setup-invisibility-spec ()
  "Setup invisibility spec to display the appropriate prompt.

Invisibility spec must be updated every time `visual-replace--scope'
is changed."
  (dolist (s '(region from-point full))
    (if (eq s visual-replace--scope)
        (remove-from-invisibility-spec s)
      (add-to-invisibility-spec s))))

(defun visual-replace--warn (from)
  "Warn if FROM contains \\n or \\t."
  (and (string-match "\\(\\`\\|[^\\]\\)\\(\\\\\\\\\\)*\\(\\\\[nt]\\)" from)
       (let ((match (match-string 3 from)))
         (cond
          ((string= match "\\n")
           (message "Note: `\\n' here doesn't match a newline; to do that, type C-q C-j instead"))
          ((string= match "\\t")
           (message "Note: `\\t' here doesn't match a tab; to do that, just type TAB")))
         (sit-for 2))))

(defun visual-replace--update-separator (args &optional forced)
  "Update the separator in the current minibuffer to show ARGS.

This function updates or inserts a field separator that reflect
the current settings, captured by a `visual-replace-args' struct with
no from or to slot set.

If FORCED is non-nil, update the separator even if it looks like
it would not change anything."
  (let ((start (visual-replace--separator-start))
        (end (visual-replace--separator-end))
        (separator (visual-replace-args--separator args)))
    (cond ((not start)
           (save-excursion
             (goto-char (point-max))
             (insert separator)))
          ((or forced (and start end
                           (not (equal (buffer-substring start end)
                                       separator))))
           (let ((start-point (point)))
             (save-excursion
               (delete-region start end)
               (goto-char start)
               (insert separator))
             (when (equal start-point end)
               (goto-char (+ start (length separator)))))))))

(defun visual-replace--separator-start (&optional string)
  "Return the start position of the separator.

The separate is looked for in STRING or, if nil, the current buffer."
  (if string
      (text-property-any 0 (length string) 'separator t string)
    (text-property-any (minibuffer-prompt-end) (point-max) 'separator t)))

(defun visual-replace--separator-end ()
  "Return the end position of the separator in the current buffer."
  (or (when-let ((start (visual-replace--separator-start)))
        (text-property-not-all (1+ start) (point-max) 'separator t))
      (point-max)))

(defun visual-replace-preprocess (args)
  "Pre-process ARGS, a `visual-replace-args', just before executing.

This function applies `visual-replace-functions' to ARGS and
returns the result. This allows pre-processing regexp or replacement
text just before it's executed."
  (if visual-replace-functions
      (let ((args (visual-replace-copy-args args)))
        (run-hook-with-args 'visual-replace-functions args)
        args)
    args))

(defun visual-replace--highlight-visible-matches (args ranges)
  "Display matches in the visible portions of the buffer.

If to is not nil in ARGS, a `visual-replace-args' struct.

Only highlight matches withing RANGES, a list of (start . end) as
returned by `region-ranges'.

Adds the overlays to `visual-replace--overlays'"
  (let* ((preview-start (get-internal-run-time))
         (args (visual-replace-preprocess args))
         (from (visual-replace-args-from args))
         (replace-lax-whitespace (visual-replace-args-lax-ws-non-regexp args))
         (replace-regexp-lax-whitespace (visual-replace-args-lax-ws-regexp args))
         (case-fold-search (if (and (visual-replace-args-case-fold args) search-upper-case)
                               (isearch-no-upper-case-p (visual-replace-args-from args)
                                                        (visual-replace-args-regexp args))
                             (visual-replace-args-case-fold args)))
         (ranges (visual-replace--range-intersect-sorted
                  (visual-replace--ranges-fix ranges)
                  (visual-replace--visible-ranges (current-buffer)))))
    (catch 'visual-replace-timeout
      (dolist (range ranges)
        (let ((start (car range))
              (end (cdr range)))
          (save-excursion
            (goto-char start)
            (while (replace-search
                    from end
                    (visual-replace-args-regexp args)
                    (visual-replace-args-word args)
                    case-fold-search)
              (let ((m-start (match-beginning 0))
                    (m-end (match-end 0)))
                (when (or (= m-end m-start)
                          (>= (float-time
                               (time-subtract (get-internal-run-time)
                                              preview-start))
                              visual-replace-preview-max-duration))
                  (visual-replace--clear-preview)
                  (throw 'visual-replace-timeout nil))
                (when-let ((ov (visual-replace--overlay
                                m-start m-end
                                (condition-case nil
                                    (match-substitute-replacement
                                     (visual-replace-args-to args))
                                  ;; ignore invalid replacements
                                  (error nil)))))
                  (push ov visual-replace--overlays))))))))))

(defun visual-replace--visible-ranges (buf)
  "Return the visible ranges of BUF.

Returned ranges are sorted and non-overlapping."
  (visual-replace--ranges-nmerge
   (mapcar (lambda (win) (cons (window-start win) (window-end win)))
           (get-buffer-window-list buf))))

(defun visual-replace--overlay (start end replacement)
  "Create an overlay highlighting text and replacement.

The text between START and END is the text being replacement.
REPLACEMENT, if non-nil, is its replacement."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'priority 1000)
    (overlay-put ov 'visual-replace t)
    (cond
     
     ;; skip read-only text
     ((text-property-not-all start end 'read-only nil))

     ;; no replacement
     ((null replacement)
      (overlay-put ov 'face 'visual-replace-match))
     
     ;; replaced with the empty string
     ((zerop (length replacement))
      (overlay-put ov 'face 'visual-replace-delete-match))

     ;; show text and replacement
     (t (let ((match-len (- end start))
              (display-string
               (concat (buffer-substring start end)
                       (or replacement ""))))
          (put-text-property
           0 match-len
           'face 'visual-replace-delete-match display-string)
          (put-text-property
           match-len (length display-string)
           'face 'visual-replace-replacement display-string)
          (overlay-put ov 'display display-string))))
    ov))

(defun visual-replace--update-preview ()
  "Update the preview to reflect the content of the minibuffer.

This is meant to be called from a timer. The result of this
call is a set of overlays, stored in `visual-replace--overlays'."
  (visual-replace--clear-preview)
  (let* ((args (visual-replace-args--from-minibuffer))
         (ranges (visual-replace--scope-ranges)))
    (when (> (length (visual-replace-args-from args)) 2)
      (with-current-buffer visual-replace--calling-buffer
        (condition-case nil
            (visual-replace--highlight-visible-matches args ranges)
          (invalid-regexp nil))))))

(defun visual-replace--clear-preview ()
  "Delete all overlays in `visual-replace--overlays', if any."
  (with-current-buffer visual-replace--calling-buffer
    (dolist (overlay visual-replace--overlays)
      (delete-overlay overlay)))
  (setq visual-replace--overlays nil))

(defun visual-replace--after-change (&rest _ignored)
  "Update `visual-replace--incomplete'.

This is added to `after-change-functions' for the minibuffer."
  (setq visual-replace--incomplete
        (let ((content (minibuffer-contents)))
          (when (> (length content) 0) content))))

(defun visual-replace--overlay-at-p (pos)
  "Check whether `visual-replace' added an overlay at POS."
  (memq t (mapcar (lambda (ov) (overlay-get ov 'visual-replace))
                  (overlays-at pos))))

(defun visual-replace--ranges-nmerge (ranges)
  "Merge overlapping range in RANGES (destructive).

Returns a list of sorted, non-overlapping range."
  (let ((sorted (sort ranges (lambda (a b) (< (car a) (car b)))))
        (merged))
    (dolist (new sorted)
      (let ((last (car merged)))
        (if (and last (<= (car new) (cdr last)))
            ;; overlap detected, either extend last region or skip.
            (when (> (cdr new) (cdr last))
              (setcdr last (cdr new)))
          ;; non-overlapping
          (push new merged))))
    ;; return sorted ranges
    (nreverse merged)))

(defun visual-replace--ranges-fix (ranges)
  "Fix RANGES, as returned by `region-bounds'.

Returns a set of sorted, non-overlapping, non-empty range, with
all ranges satisfying start < end."
  (visual-replace--ranges-nmerge
   (delq nil
         (mapcar
          (lambda (range)
            (let ((start (car range))
                  (end (cdr range)))
              (cond ((< start end) (cons start end))
                    ((> start end) (cons end start)))))
          ranges))))

(defun visual-replace--range-intersect-sorted (aranges branges)
  "Intersect sorted, non-overlapping ranges ARANGES and BRANGES.

`visual-replace--range-fix' or `visual-replace--ranges-nmerge' can make
sure that ARANGES and BRANGES are sorted and non-overlapping,

Returns a set of sorted, non-overlapping ranges."
  (let ((intersect))
    (while (and aranges branges)
      (let* ((arange (car aranges))
             (brange (car branges))
             (i-start (max (car arange) (car brange)))
             (i-end (min (cdr arange) (cdr brange))))
        (when (> i-end i-start)
          (push (cons i-start i-end) intersect))
        (if (= i-end (cdr arange))
            (setq aranges (cdr aranges))
          (setq branges (cdr branges)))))
    (nreverse intersect)))

(provide 'visual-replace)

;;; visual-replace.el ends here

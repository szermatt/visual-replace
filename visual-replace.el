;;; visual-replace.el --- A prompt for replace-string and query-replace -*- lexical-binding: t -*-

;; Copyright (C) 2020 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmail.com>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 0.1
;; Keywords: convenience matching
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
;;
;; Additionally `visual-replace-thing-at-point` starts a visual
;; replace session with the symbol at point - or another "thing"
;; understood by `thing-at-point`.
;;
;; `visual-replace-selected` starts with the text within the current
;; active region.


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
(require 'thingatpt)
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

(defcustom visual-replace-first-match t
  "Jump to the first match if there isn't one visible.

With this set, the buffer might jump around just so it can show a
match.

This option ensures that there's always a match visible, so you
can see what the replacement will look like, once it's applied."
  :type 'boolean
  :group 'visual-replace)

(defcustom visual-replace-first-match-max-duration 0.05
  "How much time to spend looking for the first match."
  :type 'number
  :group 'visual-replace)

(defcustom visual-replace-initial-scope nil
  "Set initial scope for visual replace sessions.

By default, the initial scope is:
 - the active region, if there is one
 - from point if `visual-replace-default-to-full-scope' nil
 - the full buffer otherwise

With this option set, the initial scope ignores the active region
entirely and is always set to either \\='from-point or \\='full."
  :type '(choice
          (const :tag "Default" nil)
          (const :tag "From Point" from-point)
          (const :tag "Full Buffer" full))
  :group 'visual-replace)

(defcustom visual-replace-default-to-full-scope nil
  "Have scope default to full if there's no active region.

With this option set and there is no active region, the region is
set to \\='full instead of \\='from-point.

Ignored if `visual-replace-initial-scope' is set.

See also `visual-replace-initial-scope'."
  :type 'boolean
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

;;;###autoload
(define-minor-mode visual-replace-global-mode
  "Global mode for remapping `query-replace' to `visual-replace'."
  :keymap visual-replace-global-mode-map
  :global t
  :group 'visual-replace)

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

(cl-defstruct
    (visual-replace--scope
     (:copier nil)
     (:constructor visual-replace--make-scope
                   (initial-scope
                    &aux
                    (type (cond
                           (visual-replace-initial-scope visual-replace-initial-scope)
                           ((and (numberp initial-scope) visual-replace-default-to-full-scope) 'full)
                           ((numberp initial-scope) 'from-point)
                           ((eq initial-scope 'from-point) 'from-point)
                           ((eq initial-scope 'region) 'region)
                           ((eq initial-scope 'full) 'full)
                           (initial-scope (error "Invalid INITIAL-SCOPE value: %s" initial-scope))
                           ((region-active-p) 'region)
                           (visual-replace-default-to-full-scope 'full)
                           (t 'from-point)))
                    (point (if (numberp initial-scope) initial-scope (point)))
                    (bounds (when (region-active-p)
                              (visual-replace--ranges-fix
                               (region-bounds)))))))
  "Stores the current scope and all possible scopes and their ranges.

The scope is tied to the buffer that was active when
`visual-replace--make-scope' was called."
  ;; 'from-point, 'full or 'region. See also visual-replace--scope-types.
  type
  ;; value of (point) at creation time, for 'from-point
  (point nil :read-only t)
  ;; (region-bounds) at creation time, for 'region
  (bounds nil :read-only t))

(defconst visual-replace--scope-types '(region from-point full)
  "Valid values for `visual-replace--scope-type'.")

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
  "What replace applies to.

This is an instance of the struct `visual-replace--scope'.")

(defvar visual-replace--calling-buffer nil
  "Buffer from which `visual-replace' was called.")

(defvar visual-replace--calling-window nil
  "Window from which `visual-replace' was called.")

(defvar visual-replace--overlays nil
  "Overlays added for the preview in the calling buffer.")

(defvar visual-replace--incomplete nil
  "Replacement text entered, but not confirmed.")

(defvar visual-replace--first-match-timer nil
  "Timer scheduled to search for a first match to display.")

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

(defun visual-replace-toggle-scope (&optional scope)
  "Toggle the SCOPE type.

If unspecified, SCOPE defaults to the variable
`visual-replace--scope'."
  (interactive)
  (let* ((scope (or scope visual-replace--scope))
         (type (visual-replace--scope-type scope)))
    (setf (visual-replace--scope-type scope)
          (if (visual-replace--scope-bounds scope)
              (pcase type
                ('region 'full)
                (_ 'region))
            (pcase type
              ('from-point 'full)
              (_ 'from-point)))))
  (visual-replace--setup-invisibility-spec))

(defun visual-replace-read (&optional initial-args initial-scope)
  "Read arguments for `query-replace'.

INITIAL-ARGS is used to set the prompt's initial state, if
specified. It must be a `visual-replace-args' struct.

INITIAL-SCOPE is used to initialize the replacement scope,
\\='region \\='from-point or \\='full. If it is a number, it is
used as point for \\='from-point. By default, the scope is
\\='region if the region is active, or \\='from-point otherwise."
  (barf-if-buffer-read-only)
  (let ((history-add-new-input nil)
        (visual-replace--calling-buffer (current-buffer))
        (visual-replace--calling-window (selected-window))
        (visual-replace--scope (visual-replace--make-scope initial-scope))
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
               (query-replace-compile-replacement
                (visual-replace-args-to args)
                (visual-replace-args-regexp args))
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

;;;###autoload
(defun visual-replace-thing-at-point (&optional thing)
  "Start visual replace for the thing at point.

THING defaults to symbol. It can be set to anything that
 `thing-at-point` understands."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point (or thing 'symbol))))
    (apply
     'visual-replace
     (visual-replace-read
      (visual-replace-make-args
       :from (buffer-substring-no-properties
              (car bounds)
              (cdr bounds)))
      (car bounds)))))

;;;###autoload
(defun visual-replace-selected ()
  "Start visual replace for replacing text in region or the current word.

Falls back to `visual-replace-thing-at-point' if the region is
not active."
  (interactive)
  (if (region-active-p)
      (apply
       'visual-replace
       (visual-replace-read
        (visual-replace-make-args
         :from (buffer-substring-no-properties
                (min (mark) (point))
                (max (mark) (point))))
        (min (mark) (point))))
    (visual-replace-thing-at-point)))

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
             visual-replace--scope-types
             ""))

(defun visual-replace--scope-ranges (&optional scope)
  "Return the regions replacement with SCOPE should work on.

If unspecified, SCOPE defaults to the variable
`visual-replace--scope'.

Returns a list of (start . end)"
  (with-current-buffer visual-replace--calling-buffer
    (let ((scope (or scope visual-replace--scope)))
      (pcase (visual-replace--scope-type scope)
        ('from-point (list (cons (visual-replace--scope-point scope) (point-max))))
        ('full (list (cons (point-min) (point-max))))
        ('region (visual-replace--scope-bounds scope))))))

(defun visual-replace--setup-invisibility-spec (&optional scope)
  "Setup invisibility spec for SCOPE.

If unspecified, SCOPE defaults to the variable
`visual-replace--scope'.

Invisibility spec must be updated every time
`visual-replace--scope' is changed."
  (let ((scope (or scope visual-replace--scope)))
    (dolist (s visual-replace--scope-types)
      (if (eq s (visual-replace--scope-type scope))
          (remove-from-invisibility-spec s)
        (add-to-invisibility-spec s)))))

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

(defun visual-replace--search (args sorted-ranges &optional max-duration max-matches)
  "Look for matches for ARGS within SORTED-RANGES.

ARGS is a `visual-replace-args' struct.

If MAX-MATCHES is set, stop once that number of matches is
reached and return the matches. If MAX-DURATION is set, stop
after that much time has passed and return nothing.

Return a list of (start end replacement)."
  (let* ((preview-start (get-internal-run-time))
         (args (visual-replace-preprocess args))
         (from (visual-replace-args-from args))
         (replace-lax-whitespace (visual-replace-args-lax-ws-non-regexp args))
         (replace-regexp-lax-whitespace (visual-replace-args-lax-ws-regexp args))
         (case-fold-search (if (and (visual-replace-args-case-fold args) search-upper-case)
                               (isearch-no-upper-case-p (visual-replace-args-from args)
                                                        (visual-replace-args-regexp args))
                             (visual-replace-args-case-fold args)))
         (nocasify (not (and case-replace case-fold-search)))
         (regexp-flag (visual-replace-args-regexp args))
         (literal (or (not regexp-flag) (eq regexp-flag 'literal)))
         (replacement-count 0)
         (matches))
    (catch 'visual-replace-return
      (dolist (range sorted-ranges)
        (let ((start (car range))
              (end (cdr range))
              (replacement
               (condition-case nil
                   (if (visual-replace-args-to args)
                       (query-replace-compile-replacement
                        (visual-replace-args-to args)
                        (visual-replace-args-regexp args)))
                 (error nil))))
          (save-excursion
            (goto-char start)
            (while (condition-case nil
                       (replace-search
                        from end
                        (visual-replace-args-regexp args)
                        (visual-replace-args-word args)
                        case-fold-search)
                     ;; Given an invalid regexp, just return nothing
                     (invalid-regexp (throw 'visual-replace-return nil)))
              (let ((m-start (match-beginning 0))
                    (m-end (match-end 0))
                    (m-replacement))
                (when (or (= m-end m-start)
                          (>= (float-time
                               (time-subtract (get-internal-run-time)
                                              preview-start))
                              max-duration))
                  (throw 'visual-replace-return nil))
                (condition-case nil
                    (setq m-replacement
                          (cond
                           ((stringp replacement)
                            (match-substitute-replacement replacement nocasify literal))
                           ((consp replacement)
                            (prog1
                                (funcall (car replacement) (cdr replacement)
                                         replacement-count)
                              (cl-incf replacement-count)))))
                  ;; ignore invalid replacements
                  (error nil))
                (push (list m-start m-end m-replacement) matches)
                (when (and max-matches (>= (length matches) max-matches))
                  (throw 'visual-replace-return (nreverse matches))))))))
      (nreverse matches))))

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

(defun visual-replace--update-preview (&optional no-first-match)
  "Update the preview to reflect the content of the minibuffer.

This is meant to be called from a timer. The result of this
call is a set of overlays, stored in `visual-replace--overlays'."
  (visual-replace--clear-preview)
  (when visual-replace--first-match-timer
    (cancel-timer visual-replace--first-match-timer)
    (setq visual-replace--first-match-timer nil))
  (let* ((args (visual-replace-args--from-minibuffer))
         (ranges (visual-replace--scope-ranges)))
    (when (> (length (visual-replace-args-from args)) 2)
      (with-current-buffer visual-replace--calling-buffer
        (let ((matches (visual-replace--search
                        args
                        (visual-replace--range-intersect-sorted
                         ranges
                         (visual-replace--visible-ranges (current-buffer)))
                        visual-replace-preview-max-duration)))
          (if matches
              (dolist (m matches)
                (when-let ((ov (visual-replace--overlay
                                (nth 0 m) (nth 1 m) (nth 2 m))))
                  (push ov visual-replace--overlays)))
            ;; no matches within the visible region
            (when (and visual-replace-first-match (not no-first-match))
              (visual-replace--schedule-first-match
               args ranges
               (visual-replace--scope-point
                visual-replace--scope)
               (point-max)))))))))

(defun visual-replace--schedule-first-match (args ranges start end)
  "Schedule a run of `visual-replace--first-match'.

ARGS is the `visual-replace-args' instance to use for searching.
RANGES the ranges that correspond to the scope. START and END
define a subset of the buffer to search in this step."
  (setq visual-replace--first-match-timer
        (run-with-idle-timer 0 nil #'visual-replace--first-match
                             args ranges start end)))

(defun visual-replace--first-match (args ranges start end)
  "Look for a match to display.

This function is meant to be called exclusively from an idle
timer, stored in `visual-replace--first-match-timer', by
`visual-replace--update-preview' when it cannot find any match.

ARGS is the `visual-replace-args' instance to use for searching.
RANGES the ranges that correspond to the scope. START and END
define a subset of the buffer to search in this step.

This executes one step, searching at most 80 lines, then
schedules another run for executing the next step.

Note that calling `visual-replace--update-preview' cancels the
timer"
  (setq visual-replace--first-match-timer nil)
  (with-current-buffer visual-replace--calling-buffer
    (save-excursion
      (goto-char start)
      (let* ((limit (min end (let ((inhibit-field-text-motion t))
                               (line-beginning-position 80))))
             (match (car (visual-replace--search
                          args (visual-replace--range-intersect-sorted
                                ranges `((,start . ,limit)))
                          visual-replace-first-match-max-duration 1))))
        (cond
         (match
          (with-selected-window visual-replace--calling-window
            (goto-char (car match))
            (recenter))
          (visual-replace--update-preview 'no-first-match))
         ((< limit end)
          ;; there's more to search. Schedule another step.
          (visual-replace--schedule-first-match args ranges limit end))
         ((= end (point-max))
          ;; we've reached the end. Rewind to search from the
          ;; beginning of the buffer.
          (visual-replace--schedule-first-match
           args ranges
           (point-min) (visual-replace--scope-point
                        visual-replace--scope))))))))

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

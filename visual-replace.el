;;; visual-replace.el --- A prompt for replace-string and query-replace -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmail.com>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 1.0.1snapshot
;; Keywords: convenience, matching, replace
;; URL: http://github.com/szermatt/visual-replace
;; Package-Requires: ((emacs "26.1"))

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
;; For details, see the documentation, at
;; https://visual-replace.readthedocs.io/en/latest/ or in the Info
;; node visual-replace, if it is installed.

(require 'seq)
(require 'thingatpt)
(require 'rect)
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
  '((t (:inherit (match))))
  "How to display the replacement string.

This is the face that's used to show the replacement string, once
a replacement has been defined."
  :group 'visual-replace)

(defface visual-replace-delete-match-highlight
  '((t (:weight bold :inherit (visual-replace-delete-match))))
  "How to display the string to be replaced, in a highlighted match.

This is the face that's used to show the replacement string when
the pointer is currently inside the match."
  :group 'visual-replace)

(defface visual-replace-replacement-highlight
  '((t (:weight bold :inherit (visual-replace-replacement))))
  "How to display the replacement string, in a highlighted match.

This is the face that's used to show the replacement string, when
the pointer is currently inside the match."
  :group 'visual-replace)

(defface visual-replace-region
  '((t :inherit region))
  "Highlight for the region in which replacements occur."
  :group 'visual-replace)

(defcustom visual-replace-highlight-match-at-point t
  "If non-nil, highlight match at point in the preview.

Visual replace normally the highlight match at point, to make it
easier to see the current match when navigating with
`visual-replace-next' and `visual-replace-prev'.

Set this to nil to turn it off."
  :type 'boolean
  :group 'visual-replace)

(defvar visual-replace-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap isearch-toggle-regexp] #'visual-replace-toggle-regexp)
    (define-key map [remap isearch-toggle-word] #'visual-replace-toggle-word)
    (define-key map [remap isearch-toggle-case-fold] #'visual-replace-toggle-case-fold)
    (define-key map [remap isearch-toggle-lax-whitespace] #'visual-replace-toggle-lax-ws)
    (define-key map (kbd "RET") #'visual-replace-enter)
    (define-key map (kbd "<return>") #'visual-replace-enter)
    (define-key map (kbd "TAB") #'visual-replace-tab)
    (define-key map (kbd "<tab>") #'visual-replace-tab)
    (define-key map (kbd "<up>") #'visual-replace-prev-match)
    (define-key map (kbd "<down>") #'visual-replace-next-match)
    (define-key map [remap yank] #'visual-replace-yank)
    (define-key map [remap yank-pop] #'visual-replace-yank-pop)
    (define-key map [remap kill] #'visual-replace-kill)
    (define-key map [remap kill-whole-line] #'visual-replace-kill-whole-line)
    map)
"Map of minibuffer keyboard shortcuts available when editing a query.

Note also the shortcuts bound to a prefix key that correspond to
the shortcut used to start `visual-replace'. See
`visual-replace-secondary-mode-map'.

Inherits from `minibuffer-mode-map'.")

(defvar visual-replace-secondary-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "r") #'visual-replace-toggle-regexp)
    (define-key map (kbd "SPC") #'visual-replace-toggle-scope)
    (define-key map (kbd "q") #'visual-replace-toggle-query)
    (define-key map (kbd "w") #'visual-replace-toggle-word)
    (define-key map (kbd "c") #'visual-replace-toggle-case-fold)
    (define-key map (kbd "s") #'visual-replace-toggle-lax-ws)
    (define-key map (kbd "a")
                (if (eval-when-compile (>= emacs-major-version 29))
                    ;; not using #' to avoid by-compilation error,
                    ;; because of the version-specific availability.
                    'visual-replace-apply-one-repeat
                #'visual-replace-apply-one))
    (define-key map (kbd "u") #'visual-replace-undo)
    map)
  "Keyboard shortcuts specific to `visual-replace'.

This map is, by default, bound to the prefix that corresponds to
the shortcut that was used to trigger `visual-replace'. It is
Active while `visual-replace-read' is running on the minibuffer.")

(defvar visual-replace-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<down>") #'visual-replace-next-match)
    (define-key map (kbd "<up>") #'visual-replace-prev-match)
    (define-key map (kbd "u") #'visual-replace-undo)
    map)
  "Keyboard shortcuts installed by `visual-replace-apply-on-repeat'.

The keys defined here are installed in a transient map installed after
applying one replacement. This allows applying or skipping other replacements.

Visual replace adds to this the last key of the key sequence used
to call `visual-replace-apply-one-repeat', to easily repeat the command.

To leave the map, type anything that's not on the map.")

(define-minor-mode visual-replace-minibuffer-mode
  "Local minibuffer mode for `visual-replace'.

Not normally turned on manually."
  :keymap visual-replace-mode-map)

(defvar visual-replace-global-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap query-replace] #'visual-replace)
    (define-key map [remap replace-string] #'visual-replace)
    (define-key map [remap isearch-query-replace] #'visual-replace-from-isearch)
    (define-key map [remap isearch-query-replace-regexp] #'visual-replace-from-isearch)
    map))

;;;###autoload
(define-minor-mode visual-replace-global-mode
  "Global mode for remapping `query-replace' to `visual-replace'."
  :keymap visual-replace-global-mode-map
  :global t
  :group 'visual-replace)

(defvar visual-replace--on-click-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") 'visual-replace-on-click)
    map)
  "Call `visual-replace-on-click' when a match is clicked.")

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
                               (region-bounds))))
                    (left-col (when (and bounds rectangle-mark-mode)
                                (apply #'min
                                       (mapcar (lambda (range)
                                                 (visual-replace--col (car range)))
                                               bounds))))
                    (right-col (when (and bounds rectangle-mark-mode)
                                (apply #'max
                                       (mapcar (lambda (range)
                                                 (visual-replace--col (cdr range)))
                                               bounds))))
                    (topleft-edge (when bounds
                                    (apply #'min (mapcar #'car bounds))))
                    (line-count
                     (if (region-active-p)
                         (count-lines (region-beginning) (region-end))
                       0)))))
  "Stores the current scope and all possible scopes and their ranges.

The scope is tied to the buffer that was active when
`visual-replace--make-scope' was called."
  ;; 'from-point, 'full or 'region. See also visual-replace--scope-types.
  type
  ;; value of (point) at creation time, for 'from-point
  (point nil :read-only t)
  ;; (region-bounds) at creation time, for 'region
  (bounds nil :read-only t)
  ;; column of the left edge, if the region is a rectangle.
  (left-col nil :read-only t)
  ;; column of the right edge, if the region is a rectangle.
  (right-col nil :read-only t)
  ;; point containing the top/left edge of the region
  (topleft-edge nil :read-only t)
  ;; number of line the region contains or 0
  (line-count 0 :read-only t))

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

(defvar visual-replace--match-ovs nil
  "Overlays added for the preview in the calling buffer.")

(defvar visual-replace--scope-ovs nil
  "Overlay that highlight the replacement region.")

(defvar visual-replace--incomplete nil
  "Replacement text entered, but not confirmed.")

(defvar visual-replace--first-match-timer nil
  "Timer scheduled to search for a first match to display.")

(defvar visual-replace--undo-marker nil
  "A marker put into the undo list.

This marker is added to `buffer-undo-list' by the first call to
`visual-replace-apply-one' to mark the beginning of history for
`visual-replace-undo'.")

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
                    (forward-symbol 1)
                    (buffer-substring-no-properties start (point)))))))))

(defun visual-replace-yank-pop ()
  "Replacement for `yank-pop' while building args for `visual-replace'.

The first time it's called, executes a `yank', then a `yank-pop'."
  (interactive)
  (if (memq last-command '(yank yank-pop))
      (progn  (setq this-command 'yank-pop)
              (call-interactively #'yank-pop))
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
  (visual-replace--show-scope))

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
        (visual-replace--undo-marker nil)
        (minibuffer-allow-text-properties t) ; separator uses text-properties
        (minibuffer-history (mapcar #'visual-replace-args--text visual-replace-read-history))
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
            (deactivate-mark)
            (when visual-replace-preview
              (setq timer (run-with-idle-timer
                           visual-replace-preview-delay
                           #'repeat #'visual-replace--update-preview)))
            (minibuffer-with-setup-hook
                (lambda ()
                  (when visual-replace-keep-incomplete
                    (add-hook 'after-change-functions #'visual-replace--after-change 0 'local))
                  (when trigger
                    (local-set-key trigger visual-replace-secondary-mode-map))
                  (visual-replace-minibuffer-mode t)
                  (visual-replace--show-scope)
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
        (visual-replace--clear-scope)
        (visual-replace--clear-preview)))
    (unless quit-flag (setq visual-replace--incomplete nil))
    (let* ((final-args (visual-replace-args--from-text text))
           (from (visual-replace-args-from final-args))
           (to (visual-replace-args-to final-args)))
      (cond
       ((or quit-flag (null to) nil)
        (setq final-args (visual-replace-make-args)))
       ((and (zerop (length from)) (zerop (length to)))
        (setq final-args (car visual-replace-read-history))
        (unless final-args
          (error "Nothing to replace")))
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
         (from (visual-replace-args-from args))
         (ranges (visual-replace--ranges-fix ranges)))
    (unless ranges
      (error "Empty range; nothing to replace"))
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
                (query-replace-skip-read-only t)
                (start (apply #'min (mapcar #'car ranges)))
                (end (apply #'max (mapcar #'cdr ranges)))
                (noncontiguous-p (if (cdr ranges) t nil))

                ;; when noncontiguous-p is non-nil, perform-replace
                ;; calls region-extract-function to get the ranges to
                ;; apply the searches on.
                (region-extract-function
                 (lambda (arg)
                   (unless (eq arg 'bounds)
                     (error "unsupported: (funcall region-extract-function %s)" arg))
                   (visual-replace--ranges-fix ranges))))
            (perform-replace
             from
             (query-replace-compile-replacement
              (visual-replace-args-to args)
              (visual-replace-args-regexp args))
             (visual-replace-args-query args)
             (visual-replace-args-regexp args)
             (visual-replace-args-word args)
             1 nil start end nil noncontiguous-p))
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
    (apply #'visual-replace (visual-replace-read args))))

;;;###autoload
(defun visual-replace-thing-at-point (&optional thing)
  "Start visual replace for the thing at point.

THING defaults to symbol. It can be set to anything that
 `thing-at-point` understands."
  (interactive)
  (let* ((thing (or thing 'symbol))
         (bounds (bounds-of-thing-at-point thing)))
    (unless bounds
      (error "No %s at point" (symbol-name thing)))
    (apply
     #'visual-replace
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
       #'visual-replace
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
spec. `visual-replace--show-scope' sets the appropriate
spec for the current state."
  (mapconcat
   (lambda (type)
     (let ((text (pcase type
                   ('region
                    (format "in region (%sL)"
                            (visual-replace--scope-line-count visual-replace--scope)))
                   ('from-point "from point")
                   ('full "in buffer"))))
       (add-text-properties 0 (length text)
                            (list 'invisible type)
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

(defun visual-replace--show-scope (&optional scope)
  "Update the display to reflect the state of SCOPE.

If unspecified, SCOPE defaults to the variable
`visual-replace--scope'.

This must be called every time `visual-replace--scope' is
changed."
  (let* ((scope (or scope visual-replace--scope))
         (type (visual-replace--scope-type scope))
         (buf visual-replace--calling-buffer))
    (dolist (s visual-replace--scope-types)
      (if (eq s type)
          (remove-from-invisibility-spec s)
        (add-to-invisibility-spec s)))
    (let ((ovs (delq nil
                     (mapcar
                      (lambda (ov)
                        (if (and ov (eq buf (overlay-buffer ov)))
                            ov
                          (delete-overlay ov)))
                      visual-replace--scope-ovs)))
          (new-ovs nil)
          (left-col (visual-replace--scope-left-col scope))
          (right-col (visual-replace--scope-right-col scope)))
      (with-current-buffer buf
        (cond
         ;; full doesn't need highlighting
         ((eq 'full type))

         ;; highlight a rectangular region
         ((and (eq 'region type) left-col)
          (save-excursion
            (goto-char (visual-replace--scope-topleft-edge scope))
            (dotimes (_ (visual-replace--scope-line-count scope))
              (let ((ov (or (car ovs) (make-overlay 1 1)))
                    (before "")
                    (after "")
                    left-point right-point)
                (setq ovs (cdr ovs))
                (push ov new-ovs)

                (move-to-column left-col)
                (setq left-point (point))
                (if (< (current-column) left-col)
                    (setq before (spaces-string (- left-col (current-column)))
                          after (spaces-string (- right-col left-col))
                          right-point left-point)
                  (move-to-column right-col)
                  (setq right-point (point))
                  (when (< (current-column) right-col)
                    (setq after (spaces-string (- right-col (current-column))))))
                (put-text-property 0 (length before) 'face 'default before)
                (put-text-property 0 (length after) 'face 'visual-replace-region after)
                (forward-line)

                (overlay-put ov 'priority 1000)
                (overlay-put ov 'face 'visual-replace-region)
                (overlay-put ov 'before-string before)
                (overlay-put ov 'after-string after)
                (move-overlay ov left-point right-point)))))

         ;; highlight the scope ranges
         (t
           (dolist (range (visual-replace--scope-ranges scope))
             (let ((ov (or (car ovs) (make-overlay 1 1))))
               (setq ovs (cdr ovs))
               (push ov new-ovs)
               (overlay-put ov 'priority 1000)
               (overlay-put ov 'face 'visual-replace-region)
               (move-overlay ov (car range) (cdr range)))))))
      (dolist (ov ovs)
        (delete-overlay ov))
      (setq visual-replace--scope-ovs (nreverse new-ovs)))))

(defun visual-replace--clear-scope ()
  "Get rid of any scope highlight overlay."
  (when visual-replace--scope-ovs
    (dolist (ov visual-replace--scope-ovs)
      (delete-overlay ov))
    (setq visual-replace--scope-ovs nil)))

(defun visual-replace--warn (from)
  "Warn if \\n or \\t appear within FROM."
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

(defun visual-replace--search (args sorted-ranges
                                    &optional max-duration max-matches backward)
  "Look for matches for ARGS within SORTED-RANGES.

ARGS is a `visual-replace-args' struct.

If MAX-MATCHES is set, stop once that number of matches is
reached and return the matches. If MAX-DURATION is set, stop
after that much time has passed and return nothing.

If BACKWARD is non-nil, search backward from the end of the
regions to the beginning. This only matters when MAX-MATCHES is
set.

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
          (when backward
            (setq start (cdr range))
            (setq end (car range)))
          (save-excursion
            (goto-char start)
            (while (condition-case nil
                       (replace-search
                        from end
                        (visual-replace-args-regexp args)
                        (visual-replace-args-word args)
                        case-fold-search
                        backward)
                     ;; Given an invalid regexp, just return nothing
                     (invalid-regexp (throw 'visual-replace-return nil)))
              (let ((m-start (match-beginning 0))
                    (m-end (match-end 0))
                    (m-replacement))
                (when (and max-duration
                           (or (= m-end m-start)
                               (>= (float-time
                                    (time-subtract (get-internal-run-time)
                                                   preview-start))
                                   max-duration)))
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
  (unless (text-property-not-all start end 'read-only nil)
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'priority 1000)
      (overlay-put ov 'visual-replace t)
      (overlay-put ov 'visual-replace-replacement replacement)
      (if (null replacement)
          ;; no replacement yet
          (overlay-put ov 'face 'visual-replace-match)

        ;; show text and replacement, highlight if necessary
        (let* ((highlight
                (and visual-replace-highlight-match-at-point
                     (>= (point) start)
                     (< (point) end)))
               (match-face
                (if highlight
                    'visual-replace-delete-match-highlight
                  'visual-replace-delete-match))
               (repl-face
                (if highlight
                    'visual-replace-replacement-highlight
                  'visual-replace-replacement)))
          (overlay-put ov 'face match-face)
          (overlay-put ov 'help-echo "mouse-1: apply")
          (overlay-put ov 'keymap visual-replace--on-click-map)
          (when replacement
            (overlay-put
             ov 'after-string
             (propertize replacement
                         'face repl-face
                         'help-echo "mouse-1: apply"
                         'keymap visual-replace--on-click-map)))))
      ov)))

(defun visual-replace--update-preview (&optional no-first-match)
  "Update the preview to reflect the content of the minibuffer.

This is meant to be called from a timer. The result of this
call is a set of overlays, stored in `visual-replace--match-ovs'.

It looks for matches within the visible portions of the buffer
and highlights them. If no matches can be found in the visible
portion of the buffer, it triggers a search for some other
matches to display unless NO-FIRST-MATCH is non-nil."
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
                  (push ov visual-replace--match-ovs)))
            ;; no matches within the visible region
            (when (and visual-replace-first-match (not no-first-match))
              (let ((origin (save-excursion
                              (goto-char (visual-replace--scope-point
                                          visual-replace--scope))
                              (line-beginning-position))))
                (visual-replace--schedule-first-match
                 args
                 (visual-replace--small-ranges
                  (append
                   ;; first search for a match below the original
                   ;; position of the point.
                   (visual-replace--range-intersect-sorted
                    ranges
                    `((,origin . ,(point-max))))
                   ;; if none can be found, look for one above.
                   (visual-replace--range-intersect-sorted
                    ranges
                    `((,(point-min) . ,origin))))))))))))))

(defun visual-replace--schedule-first-match (args ranges)
  "Schedule a run of `visual-replace--first-match'.

ARGS is the `visual-replace-args' instance to use for searching.
RANGES is a series of (cons START END) to search, one after the
other."
  (when ranges
    (setq visual-replace--first-match-timer
          (run-with-idle-timer 0 nil #'visual-replace--first-match
                               args ranges))))

(defun visual-replace--first-match (args ranges)
  "Look for a match to display.

This function is meant to be called exclusively from an idle
timer, stored in `visual-replace--first-match-timer', by
`visual-replace--update-preview' when it cannot find any match.

ARGS is the `visual-replace-args' instance to use for searching.
RANGES is a series of (cons START END) to search, one after the
other.

This searches one range, then schedules another run for searching
the next range, until RANGES is empty.

Note that calling `visual-replace--update-preview' cancels the
timer"
  (setq visual-replace--first-match-timer nil)
  (with-current-buffer visual-replace--calling-buffer
    (let* ((range (car ranges))
           (match (car (visual-replace--search
                        args (list range)
                        visual-replace-first-match-max-duration 1))))
      (if match
          (progn
            (with-selected-window visual-replace--calling-window
              (goto-char (car match))
              (recenter))
            (visual-replace--update-preview 'no-first-match))
        (visual-replace--schedule-first-match
         args (cdr ranges))))))

(defun visual-replace--clear-preview ()
  "Delete all overlays in `visual-replace--match-ovs', if any."
  (with-current-buffer visual-replace--calling-buffer
    (dolist (overlay visual-replace--match-ovs)
      (delete-overlay overlay)))
  (setq visual-replace--match-ovs nil))

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

(defun visual-replace--small-ranges (ranges)
  "Split RANGES into ranges of at most 80 lines.

RANGES and the return value are both lists of (cons START END)
with START and END positions in the current buffer that mark a
certain range.

Also skips empty ranges."
  (save-excursion
    (let ((result))
      (dolist (range ranges)
        (let ((start (car range))
              (end (cdr range)))
          (save-restriction
            (narrow-to-region start end)
            (while (< start end)
              (goto-char start)
              (forward-line 80)
              (push `(,start . ,(point)) result)
              (setq start (point))))))
      (nreverse result))))

(defun visual-replace-next-match ()
  "Move the point to the next match."
  (interactive)
  (with-selected-window visual-replace--calling-window
    (let ((match (car (visual-replace--search
                       (visual-replace-args--from-minibuffer)
                       (visual-replace--range-intersect-sorted
                        (visual-replace--scope-ranges)
                        `((,(1+ (point)) . ,(point-max))))
                       nil 1))))
      (unless match
        (error "No next match"))
      (goto-char (car match)))))

(defun visual-replace-prev-match ()
  "Move the point to the previous match."
  (interactive)
  (with-selected-window visual-replace--calling-window
    (let ((match (car (visual-replace--search
                       (visual-replace-args--from-minibuffer)
                       (visual-replace--range-intersect-sorted
                        (visual-replace--scope-ranges)
                        `((,(point-min) . ,(point))))
                       nil 1 'backward))))
      (unless match
        (error "No previous match"))
      (goto-char (car match)))))

(defun visual-replace--col (pos)
  "Return the column at POS."
  (save-excursion
    (goto-char pos)
    (current-column)))

(when (eval-when-compile (>= emacs-major-version 29))
  (defun visual-replace-apply-one-repeat (&optional num)
    "Apply the replacement at or after point, then set a transient map.

With a prefix argument NUM, repeat the replacement that many times.

This command set a transient map that allows repeating the
replacement by typing the last key, so if this command is
triggered by M-% a, pressing a again replace the next match,
pressing <down> allows skipping matches, pressing <up> allows
going to a previous match. Anything else leaves the transient
map."
    (interactive "p")
    (visual-replace-apply-one num)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map visual-replace-transient-map)
      (define-key map (vector last-input-event) #'visual-replace-apply-one)
      (set-transient-map map t nil "Apply replacements: %k"))))

(defun visual-replace-apply-one (&optional num)
  "Apply the replacement at or after point, when in preview mode.

With a prefix argument NUM, repeat the replacement that many times."
  (interactive "p")
  (with-selected-window visual-replace--calling-window
    (when (null visual-replace--undo-marker)
      (setq visual-replace--undo-marker (cl-gensym))
      (visual-replace--add-undo-marker))
    (let* ((args (visual-replace-args--from-minibuffer))
           (num (or num 1))
           (matches (visual-replace--search
                     args
                     (visual-replace--range-intersect-sorted
                      (visual-replace--scope-ranges)
                      `((,(point) . ,(point-max))))
                     nil num))
           (first-match (car matches))
           (last-match (car (last matches))))
      (unless first-match
        (error "No match"))
      (visual-replace args (list (cons (car first-match)
                                       (nth 1 last-match))))
      (when-let ((next (car (visual-replace--search
                             args
                             (visual-replace--range-intersect-sorted
                              (visual-replace--scope-ranges)
                              `((,(point) . ,(point-max))))
                             nil 1))))
        (goto-char (car next))))))

(defun visual-replace-undo ()
  "Execute undo in the original buffer.

This command is meant to undo replacements applied by
`visual-replace-apply-one'.

It just executes `undo' in the original buffer. Its behavior and
arguments is identical to `undo', which see. The single different
is that it'll refuse to undo past the beginning of the current
visual replace session.

A prefix argument serves as a repeat count for `undo'."
  (interactive)
  (with-selected-window visual-replace--calling-window
    (let ((marker-cell (visual-replace--find-undo-marker-cell)))
      (unless marker-cell
        (error "No replacement to undo"))
      (unwind-protect
          ;; Temporarily truncate the undo history to avoid going past
          ;; the first call to visual-replace-apply-one and execute
          ;; undo.
          (let ((buffer-undo-rest (cdr marker-cell)))
            (unwind-protect
                (progn
                  ;; Cut buffer-undo-list after the marker
                  (setcdr marker-cell nil)
                  (call-interactively #'undo))
              ;; Restore the full undo list, minus the part that was
              ;; just undone.
              (setq marker-cell (visual-replace--find-undo-marker-cell))
              (if marker-cell
                  (setcdr marker-cell buffer-undo-rest)
                ;; Everything was undone including the marker, put it
                ;; back.
                (setq buffer-undo-list buffer-undo-rest)
                (visual-replace--add-undo-marker))))
        (visual-replace--update-preview 'no-first-match)))))

(defun visual-replace--marker (&rest _)
  "A no-op function, to hold the marker in an undo list.")

(defun visual-replace--add-undo-marker ()
  "Add a no-op marker to the undo list of the current buffer.

The marker is added to the current buffer `buffer-undo-list'
unless undo is disabled. It can be read back by
`visual-replace--find-undo-marker-cell'.

The marker `visual-replace--undo-marker' must have been created
beforehand."
  (cl-assert visual-replace--undo-marker)
  (when (listp buffer-undo-list)
    (push `(apply visual-replace--marker ,visual-replace--undo-marker)
          buffer-undo-list)))

(defun visual-replace--find-undo-marker-cell ()
  "Find the marker inserted by `visual-replace--add-undo-marker'.

This returns a cell containing a marker with the same value of
`visual-replace--undo-marker', that is, the marker must have been
added by the same visual replace session.

Return nil if the marker doesn't exist, wasn't found or if undo
is disabled."
  (when (and visual-replace--undo-marker
             (listp buffer-undo-list))
    (let ((rest buffer-undo-list))
      (while
          (and rest
               (not
                (equal (car rest)
                       `(apply visual-replace--marker
                               ,visual-replace--undo-marker))))
        (setq rest (cdr rest)))
      rest)))

(defun visual-replace-on-click (ev)
  "React to a click on a match preview.

EV is the event that triggered the command. It must be a mouse
event containing a buffer position for this command to work
properly.

This calls `visual-replace-apply-one' for the match that was
clicked."
  (interactive "e")
  (let* ((pos (posn-point (nth 1 ev)))
         (ov (cl-find-if
              (lambda (ov)
                (overlay-get ov 'visual-replace))
              (append (overlays-at pos)
                      ;; if clicked on the after-string, the pos
                      ;; might be just after the match overlay.
                      (overlays-at (1- pos))))))
    (unless ov
      (error "No match at this position"))
    (save-excursion
      (goto-char (overlay-start ov))
      (visual-replace-apply-one))
    (select-window
     (or (active-minibuffer-window)
         (minibuffer-window)))))

(provide 'visual-replace)

;;; visual-replace.el ends here

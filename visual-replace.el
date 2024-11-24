;;; visual-replace.el --- A prompt for replace-string and query-replace -*- lexical-binding: t -*-

;; Copyright (C) 2020-2024 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmail.com>
;; Maintainer: Stephane Zermatten <szermatt@gmail.com>
;; Version: 1.1.1snapshot
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

(require 'gv)
(require 'rect)
(require 'seq)
(require 'thingatpt)
(require 'cl-lib)
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

(defcustom visual-replace-display-total nil
  "If non-nil, display the total match count in the prompt.

When enabled, Visual Replace counts all matches within the buffer
with a lower priority than the preview highlights and displays
the result in the prompt, just before the arrow."
  :type 'boolean
  :group 'visual-replace)

(defcustom visual-replace-max-matches-for-total 1000
  "Maximum number of matches to process in the preview.

If there are more than that many matches, stop attempting to
compute the total even if `visual-replace-display-total' is
non-nil."
  :type 'number
  :group 'visual-replace)

(defcustom visual-replace-max-size-for-search (* 4 1024 1024)
  "Maximum buffer region to search, in bytes.

Visual replace will not attempt to count matches if the buffer is
that large and will stop looking for a first match after going
through that much data."
  :type 'number
  :group 'visual-replace)

(defcustom visual-replace-min-length 3
  "Only do search or preview for string lengths >= this value.

Setting this too low a number might result in strange highlights
happening when starting to type, and possibly slowdowns."
  :type 'number
  :group 'visual-replace)

(defcustom visual-replace-keep-initial-position nil
  "If non-nil, always go back to the point `visual-replace' was called from.

If nil the point stays where it was moved in preview mode, by
commands such as with `visual-replace-next-match'. A mark is
pushed at the original position to go back to with
`exchange-point-and-mark', if necessary."
  :type 'boolean
  :group 'visual-replace)

(defface visual-replace-match
  '((t :inherit query-replace))
  "How to display the string that was matched.

This is the face that's used to highlight matches, before a
replacement has been defined."
  :group 'visual-replace)

(defface visual-replace-total
  '((t :inherit minibuffer-prompt))
  "How to display match count in the prompt."
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

(defface visual-replace-match-highlight
  '((t :weight bold :inherit (visual-replace-match)))
  "How to display the matched string, in a highlighted match.

This is the face that's used to highlight matches at point,
before a replacement has been defined."
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

(defcustom visual-replace-defaults-hook nil
  "Hook run when visual replace is called with no initial arguments.

Functions registered to this hook are run when visual replace is
called normally, with no initial text or setup. That is, when you
call `visual-replace' and not `visual-replace-from-isearch' or
`visual-replace-thing-at-point'.

This allows changing the search arguments for visual replace, by
registering the relevant command to this hook. For example, if
you always want to start in regexp mode, run
`visual-replace-toggle-regexp' from this hook.

To run code in every case, register it with
`visual-replace-minibuffer-mode-hook' instead."
  :type 'hook
  :group 'visual-replace
  :options '(visual-replace-toggle-regexp
             visual-replace-toggle-word
             visual-replace-toggle-case-fold
             visual-replace-toggle-lax-ws))

(defcustom visual-replace-minibuffer-mode-hook nil
  "Hook run when visual replace starts.

Functions registered to this hook are run when visual replace is
started in the minibuffer. This allows changing the initial
state. It's also useful to enable the query mode by default, by
calling `visual-replace-toggle-query' from this hook..

To manipulate search and replace arguments, you most likely want
to customize `visual-replace-defaults-hook' instead."
  :type 'hook
  :group 'visual-replace
  :options '(visual-replace-toggle-query))

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
  "Window from which `visual-replace' was called.

As window layout might change, always access it through
`visual-replace--find-window'.")

(defvar visual-replace--minibuffer nil
  "Minibuffer in which `visual-replace' is running.")

(defvar visual-replace--match-ovs nil
  "Overlays added for the preview in the calling buffer.")

(defvar visual-replace--total-ov nil
  "Overlay added to the minibuffer to display match count.

Only relevant if `visual-replace-display-total' is non-nil.")

(defvar visual-replace--preview-state nil
  "Represents the state of the preview.

The preview is a set of overlays stored in
`visual-replace--match-ovs'. This variable keeps track of the
state that was current when that set of overlays was created.

If non-nil, this is a vector:
 [ARGS RANGES POINT IS-COMPLETE SCOPE TOO-MANY-MATCHES]

ARGS is a `visual-replace-range' element that was used to produce
these overlays.

RANGES is the range within the buffer Visual Replace looked for
matches for the preview.

POINT is the value of (point) that was last used to update the
highlights of the overlays.

IS-COMPLETE is non-nil once the whole buffer was searched, so
overlays are complete.

SCOPE is the scope that was current at the time the state was
setup.

TOO-MANY-MATCHES is non-nil if search for all matches was
attempted but hit the max matches limit.")

(defsubst visual-replace--init-preview-state ()
  "Initialize `visual-replace--preview-state'."
  (setq visual-replace--preview-state (make-vector 6 nil)))

(defsubst visual-replace--preview-args ()
  "`visual-replace-args' current when the preview was last updated."
  (when-let ((s visual-replace--preview-state))
    (aref s 0)))
(gv-define-setter visual-replace--preview-args (args)
  `(setf (aref visual-replace--preview-state 0) ,args))

(defsubst visual-replace--preview-ranges ()
  "Visible ranges current when the preview was last updated."
  (when-let ((s visual-replace--preview-state))
    (aref s 1)))
(gv-define-setter visual-replace--preview-ranges (ranges)
  `(setf (aref visual-replace--preview-state 1) ,ranges))

(defsubst visual-replace--preview-point ()
  "Point current when the preview was last updated."
  (when-let ((s visual-replace--preview-state))
    (aref s 2)))
(gv-define-setter visual-replace--preview-point (point)
  `(setf (aref visual-replace--preview-state 2) ,point))

(defsubst visual-replace--preview-is-complete ()
  "Non-nil once the set of match overlays is complete."
  (when-let ((s visual-replace--preview-state))
    (aref s 3)))
(gv-define-setter visual-replace--preview-is-complete (is-complete)
  `(setf (aref visual-replace--preview-state 3) ,is-complete))

(defsubst visual-replace--preview-scope ()
  "Scope that was current when the preview was last updated."
  (when-let ((s visual-replace--preview-state))
    (aref s 4)))
(gv-define-setter visual-replace--preview-scope (scope)
  `(setf (aref visual-replace--preview-state 4) ,scope))

(defsubst visual-replace--preview-too-many-matches ()
  "Scope that was current when the preview was last updated."
  (when-let ((s visual-replace--preview-state))
    (aref s 5)))
(gv-define-setter visual-replace--preview-too-many-matches (val)
  `(setf (aref visual-replace--preview-state 5) ,val))

(defvar visual-replace--scope-ovs nil
  "Overlay that highlight the replacement region.")

(defvar visual-replace--incomplete nil
  "Replacement text entered, but not confirmed.")

(defvar visual-replace--idle-search-timer nil
  "Timer used to run search in the background.

Such timer is created by `visual-replace--schedule-idle-search'
and can be cancelled by
`visual-replace--reset-idle-search-timer'.")

(defvar visual-replace--undo-marker nil
  "A marker put into the undo list.

This marker is added to `buffer-undo-list' by the first call to
`visual-replace-apply-one' to mark the beginning of history for
`visual-replace-undo'.")

(defvar-local visual-replace-last-tab-marker nil
  "Marker on where the cursor was at when TAB was last called.

This is a local variable in the minibuffer in visual replace
mode.")

(defun visual-replace-enter ()
  "Confirm the current text to replace.

If both the text to replace and its replacement have been
defined, execute the replacement. If only the text to replace
has been defined, create a new field to fill in the replacement.

See also `visual-replace-tab'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
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
  (visual-replace--assert-in-minibuffer)
  (visual-replace--update-separator (visual-replace-args--from-minibuffer))
  (let ((separator-start (visual-replace--separator-start))
        (separator-end (visual-replace--separator-end))
        (marker visual-replace-last-tab-marker)
        (start-pos (point))
        (goal-area))

    (if (<= (point) separator-start)
        ;; search string -> replacement
        (setq goal-area (cons separator-end (point-max)))
      ;; replacement -> search string
      (setq goal-area (cons (minibuffer-prompt-end)
                            separator-start)))

    ;; go to the beginning of the goal area or to the position
    ;; the cursor was previously.
    (if (and (markerp marker)
             (>= marker (car goal-area))
             (<= marker (cdr goal-area)))
        (goto-char marker)
      (goto-char (cdr goal-area)))

    ;; remember the position TAB was called for next time.
    (unless (markerp marker)
      (setq marker (make-marker))
      (setq visual-replace-last-tab-marker marker))
    (move-marker marker start-pos)))

(defun visual-replace-yank ()
  "Replacement for `yank' while building args for `visual-replace'.

When editing the text to be replaced, insert the text at point.
Multiple calls to `visual-replace-yank` put more and more of the text
at point into the field.

When editing the replacement text, insert the original text.

See also `visual-replace-yank-pop'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
  (let* ((separator-start (visual-replace--separator-start))
         (separator-end (visual-replace--separator-end))
         (from-text (buffer-substring-no-properties
                     (minibuffer-prompt-end)
                     (or separator-start (point-max)))))
    (cond
     ;; in the modification section
     ((and separator-start (>= (point) separator-end))
      (insert from-text))
     ;; in the original section
     (t (insert (with-current-buffer visual-replace--calling-buffer
                  ;; If the text we're looking at is exactly
                  ;; from-text, we likely moved to that point with
                  ;; visual-replace-next or -prev. Skip the matching
                  ;; text.
                  (when (and (> (length from-text) 0)
                             (equal from-text (buffer-substring-no-properties
                                               (point)
                                               (min (+ (length from-text) (point))
                                                    (point-max)))))
                    (goto-char (+ (length from-text) (point))))
                  (let ((start (point)))
                    (skip-syntax-forward " ")
                    (or
                     (> (skip-syntax-forward "w_") 0)
                     (progn (goto-char (1+ (point))) t))
                    (buffer-substring-no-properties start (point)))))))))

(defun visual-replace-yank-pop ()
  "Replacement for `yank-pop' while building args for `visual-replace'.

The first time it's called, executes a `yank', then a `yank-pop'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
  (if (memq last-command '(yank yank-pop))
      (progn  (setq this-command 'yank-pop)
              (call-interactively #'yank-pop))
    ;; If previous command was not a yank, call yank. This gives
    ;; access to yank for the modified test.
    (setq this-command 'yank)
    (yank)))

(defun visual-replace-toggle-regexp ()
  "Toggle the regexp flag while building arguments for `visual-replace'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
  (let ((args (visual-replace-args--from-minibuffer)))
    (if (visual-replace-args-regexp args)
        (setf (visual-replace-args-regexp args) nil)
      (setf (visual-replace-args-regexp args) t)
      (setf (visual-replace-args-word args) nil))
    (visual-replace--update-separator args)))

(defun visual-replace-toggle-query ()
  "Toggle the query flag while building arguments for `visual-replace'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
  (let ((args (visual-replace-args--from-minibuffer)))
    (setf (visual-replace-args-query args)
          (not (visual-replace-args-query args)))
    (visual-replace--update-separator args)))

(defun visual-replace-toggle-word ()
  "Toggle the word-delimited flag while building arguments for `visual-replace'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
  (let ((args (visual-replace-args--from-minibuffer)))
    (if (visual-replace-args-word args)
        (setf (visual-replace-args-word args) nil)
      (setf (visual-replace-args-word args) t)
      (setf (visual-replace-args-regexp args) nil))
    (visual-replace--update-separator args)))

(defun visual-replace-toggle-case-fold ()
  "Toggle the case-fold flag while building arguments for `visual-replace'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
  (let ((args (visual-replace-args--from-minibuffer)))
    (setf (visual-replace-args-case-fold args)
          (not (visual-replace-args-case-fold args)))
    (visual-replace--update-separator args)))

(defun visual-replace-toggle-lax-ws ()
  "Toggle the lax-ws flag while building arguments for `visual-replace'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
  (let* ((args (visual-replace-args--from-minibuffer))
         (newval (not (visual-replace-args-lax-ws args))))
    (setf (visual-replace-args-lax-ws-regexp args) newval)
    (setf (visual-replace-args-lax-ws-non-regexp args) newval)
    (visual-replace--update-separator args)))

(defun visual-replace-toggle-scope (&optional scope)
  "Toggle the SCOPE type.

If unspecified, SCOPE defaults to the variable
`visual-replace--scope'."
  (interactive)
  (visual-replace--assert-in-minibuffer)
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
  (visual-replace--show-scope)
  (visual-replace--reset-preview)
  (visual-replace--update-preview))

(defun visual-replace-read (&optional initial-args initial-scope)
  "Read arguments for `query-replace'.

INITIAL-ARGS is used to set the prompt's initial state, if
specified. It must be a `visual-replace-args' struct.

INITIAL-SCOPE is used to initialize the replacement scope,
\\='region \\='from-point or \\='full. If it is a number, it is
used as point for \\='from-point. By default, the scope is
\\='region if the region is active, or \\='from-point otherwise."
  (barf-if-buffer-read-only)
  (if visual-replace-keep-initial-position
      (save-excursion
        (visual-replace-read--internal initial-args initial-scope nil))
    (visual-replace-read--internal initial-args initial-scope 'push-mark)))

(defun visual-replace-read--internal (&optional initial-args initial-scope push-mark)
  "Private implementation of `visual-replace-read'.

See `visual-replace-read' for a description of the behavior of
this function and of INITIAL-ARGS and INITIAL-SCOPE.

If PUSH-MARK is non-nil, push a mark to the current point."
  (let ((history-add-new-input nil)
        (visual-replace--calling-buffer (current-buffer))
        (visual-replace--calling-window (selected-window))
        (visual-replace--minibuffer nil)
        (visual-replace--scope (visual-replace--make-scope initial-scope))
        (visual-replace--undo-marker nil)
        (minibuffer-allow-text-properties t) ; separator uses text-properties
        (minibuffer-history (mapcar #'visual-replace-args--text visual-replace-read-history))
        (initial-input (let* ((args (or initial-args (visual-replace-make-args)))
                              (text (visual-replace-args--text args))
                              (from (visual-replace-args-from args)))
                         (cons text (if from (1+ (length text)) 0))))
        (visual-replace--preview-state nil)
        (after-change (lambda (_beg _end _len)
                        (visual-replace--reset-preview)
                        (visual-replace--update-preview)))
        (trigger (this-command-keys-vector))
        (visual-replace--total-ov nil)
        (default-value)
        (text)
        (timer))
    (setq default-value (car minibuffer-history))
    (when visual-replace--incomplete
      (push visual-replace--incomplete minibuffer-history))
    (when push-mark
      (push-mark nil 'nomsg))
    (unwind-protect
        (progn
          (deactivate-mark)
          (add-hook 'after-change-functions after-change 0 'local)
          (when visual-replace-preview
            (setq timer (run-with-idle-timer
                         visual-replace-preview-delay
                         'repeat #'visual-replace--update-preview)))
          (minibuffer-with-setup-hook
              (lambda ()
                (setq visual-replace--total-ov
                      (when visual-replace-display-total
                        (let ((ov (make-overlay (point-min) (point-min))))
                          (overlay-put ov 'face 'visual-replace-total)
                          ov)))
                (when visual-replace-keep-incomplete
                  (add-hook 'after-change-functions #'visual-replace--after-change 0 'local))
                (setq visual-replace--minibuffer (current-buffer))
                (visual-replace-minibuffer-mode t)
                (unless initial-args
                  (run-hooks 'visual-replace-defaults-hook))
                (when trigger
                  (let ((mapping
                         ;; Emacs 26 lookup-key cannot take a list
                         ;; of keymaps, using this code for backward
                         ;; compatibility.
                         (catch 'has-binding
                           (dolist (map (current-active-maps))
                             (let ((func (lookup-key map trigger)))
                               (when (functionp func)
                                 (throw 'has-binding func)))))))
                    (when (or (eq mapping #'visual-replace)
                              (eq (command-remapping mapping) #'visual-replace))
                      (local-set-key trigger visual-replace-secondary-mode-map))))
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
      (remove-hook 'after-change-functions after-change 'local)
      (visual-replace--reset-idle-search-timer)
      (when timer
        (cancel-timer timer))
      (visual-replace--clear-scope)
      (visual-replace--clear-preview))
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
            (cl-letf (((symbol-function 'push-mark) (lambda (&rest _args))))
              ;; perform-replace sets the mark at an uninteresting
              ;; position. Redefining push-mark avoids that.

              (perform-replace
               from
               (query-replace-compile-replacement
                (visual-replace-args-to args)
                (visual-replace-args-regexp args))
               (visual-replace-args-query args)
               (visual-replace-args-regexp args)
               (visual-replace-args-word args)
               1 nil start end nil noncontiguous-p)))
          (goto-char origin))
      (set-marker origin nil))))

;;;###autoload
(defun visual-replace-from-isearch ()
    "Switch from isearch to `visual-replace'.

This function attempts to copy as much of the current state of
isearch as possible, with the text being searched set as query
for `visual-replace'. Replacement starts at the current point."
  (interactive)
  (when (and isearch-other-end (< isearch-other-end (point)))
    (goto-char isearch-other-end))
  (let ((args
         (visual-replace-make-args
          :from isearch-string
          :to "" ; Go directly to the replacement prompt.
          :regexp isearch-regexp
          :word isearch-regexp-function
          :case-fold isearch-case-fold-search)))
    (when (seq-position isearch-string ?\ )
      (if isearch-regexp
          (setf (visual-replace-args-lax-ws-regexp args)
                isearch-regexp-lax-whitespace)
        (setf (visual-replace-args-lax-ws-non-regexp args)
              isearch-lax-whitespace)))
    (isearch-done nil t)
    (isearch-clean-overlays)
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
              (cdr bounds))
       ;; Go directly to the replacement prompt.
       :to "")
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
                (max (mark) (point)))
         :to "")
        (min (mark) (point))))
    (visual-replace-thing-at-point)))

(defun visual-replace-args--case-fold-search (args)
  "Compute value of `case-fold-search' for the given ARGS."
  (if (and (visual-replace-args-case-fold args) search-upper-case)
      (isearch-no-upper-case-p (visual-replace-args-from args)
                               (visual-replace-args-regexp args))
    (visual-replace-args-case-fold args)))

(defun visual-replace-args--nocasify (args)
  "Return non-nil if case-replacement should be disabled for ARGS.

This builds the argument nocasify of `match-substitute-replacement'."
  (not (and case-replace (visual-replace-args--case-fold-search args))))

(defun visual-replace-args--literal (args)
  "Return non-nil if regexp replacement should be literal for ARGS.

This builds the argument literal of `match-substitute-replacement'."
  (let ((regexp-flag (visual-replace-args-regexp args)))
    (or (not regexp-flag) (eq regexp-flag 'literal))))

(defun visual-replace-args--text (args)
  "Build the text representation of ARGS, a `visual-replace-args' struct.

The text representation is the content of minibuffer that would result
in such a struct being returned by `visual-replace-read'.

Only add a separator if necessary, to capture flags defined in
ARGS."
  (let ((flag-text (visual-replace-args--flag-text args))
        (from-text (or (visual-replace-args-from args) "")))
    (if (and (null (visual-replace-args-to args))
             (equal "" flag-text))
        from-text
      (concat
       (propertize from-text 'field 'search)
       (visual-replace-args--separator args flag-text) ;
       (propertize (or (visual-replace-args-to args) "") 'field 'replace)))))

(defun visual-replace-args--flag-text (args)
  "Build the text representation of the flags in ARGS.

This function takes the set of options from ARGS, a
`visual-replace-args' and returns its text representation, to be
displayed in the prompt following the arrow."
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

(defun visual-replace-args--equiv-for-match-p (a b)
  "Return non-nil if A and B are equivalent for matching.

Always returns nil if A or B are nil."
  (and
   a b
   (equal (visual-replace-args-from a)
          (visual-replace-args-from b))
   (eq (visual-replace-args-word a)
       (visual-replace-args-word b))
   (eq (visual-replace-args-regexp a)
       (visual-replace-args-regexp b))
   (eq (visual-replace-args-case-fold a)
       (visual-replace-args-case-fold b))
   (eq (visual-replace-args-lax-ws a)
       (visual-replace-args-lax-ws b))))

(defun visual-replace-args--separator (args &optional flag-text)
  "Return the separator for ARGS, a `visual-replace-args'.

FLAG-TEXT is the text that follows the arrow in the separator. If
nil, it is built based on ARGS."
  (let* ((flag-text (or flag-text (visual-replace-args--flag-text args))))
    (propertize
     " "
     'display (concat " →" flag-text " ")
     'visual-replace-args (let ((args (visual-replace-copy-args args)))
                            (setf (visual-replace-args-from args) nil)
                            (setf (visual-replace-args-to args) nil)
                            args)
     'face 'minibuffer-prompt
     'front-sticky nil
     'rear-nonsticky t
     'insert-behind-hooks (list #'visual-replace--insert-replace-field)
     'separator t)))

(defun visual-replace--insert-replace-field (beg end)
  "Set field property between BEG and END to replace."
  (add-text-properties beg end '(field replace)))

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
   (with-current-buffer visual-replace--minibuffer
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

(defun visual-replace--update-separator (args)
  "Update the separator in the current minibuffer to show ARGS.

This function updates or inserts a field separator that reflect
the current settings, captured by a `visual-replace-args' struct with
no from or to slot set."
  (let ((start (visual-replace--separator-start))
        (end (visual-replace--separator-end))
        (separator (visual-replace-args--separator args)))
    (if (not start)
        (save-excursion
          (setq start (point-max))
          (goto-char (point-max))
          (insert separator)
          (setq end (point)))
      (let ((start-point (point)))
        (save-excursion
          (delete-region start end)
          (goto-char start)
          (insert separator))
        (when (equal start-point end)
          (goto-char (+ start (length separator))))))
    (add-text-properties
     (minibuffer-prompt-end) start '(field search))
    (add-text-properties
     end (point-max) '(field replace))))

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
  (let ((preview-start (get-internal-run-time))
        (args (visual-replace-preprocess args))
        (from (visual-replace-args-from args))
        (replace-lax-whitespace (visual-replace-args-lax-ws-non-regexp args))
        (replace-regexp-lax-whitespace (visual-replace-args-lax-ws-regexp args))
        (case-fold-search (visual-replace-args--case-fold-search args))
        (replacement-count 0)
        (matches))
    (catch 'visual-replace-return
      (dolist (range sorted-ranges)
        (let ((start (car range))
              (end (cdr range))
              (replacement (visual-replace--compile-replacement args)))
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
                (setq m-replacement (visual-replace--replacement-for-match-data
                                     replacement replacement-count args))
                (cl-incf replacement-count)
                (push (list m-start m-end m-replacement (match-data)) matches)
                (when (and max-matches (>= (length matches) max-matches))
                  (throw 'visual-replace-return (nreverse matches))))))))
      (nreverse matches))))

(defun visual-replace--compile-replacement (args)
  "Prepare replacement for ARGS.

The return value is meant to be passed to
 `visual-replace--replacement-for-match-data'."
  (condition-case nil
      (if (visual-replace-args-to args)
          (query-replace-compile-replacement
           (visual-replace-args-to args)
           (visual-replace-args-regexp args)))
    (error nil)))

(defun visual-replace--replacement-for-match-data (replacement replacement-count args)
  "Build replacement string for the current match data.

REPLACEMENT should be a compiled replacement, built by
`visual-replace--compile-replacement'.

REPLACEMENT-COUNT is the index of the current replacement.

ARGS is the `visual-replace-args' to take relevant flags from."
  (condition-case nil
      (let ((replace-lax-whitespace (visual-replace-args-lax-ws-non-regexp args))
            (replace-regexp-lax-whitespace (visual-replace-args-lax-ws-regexp args))
            (case-fold-search (visual-replace-args--case-fold-search args)))
        (cond
         ((null replacement) nil)
         ((stringp replacement)
          (match-substitute-replacement replacement
                                        (visual-replace-args--nocasify args)
                                        (visual-replace-args--literal args)))
         ((consp replacement)
          (funcall (car replacement) (cdr replacement)
                   replacement-count))))
    ;; ignore invalid replacements
    (error nil)))

(defun visual-replace--visible-ranges (buf)
  "Return the visible ranges of BUF.

Returned ranges are sorted and non-overlapping."
  (visual-replace--ranges-nmerge
   (mapcar (lambda (win)
             (cons (window-start win)
                   ;; This approximates (window-end win), in a way
                   ;; that doesn't require a redisplay.
                   (with-current-buffer buf
                     (save-excursion
                       (goto-char (window-start win))
                       (forward-line (window-height win))
                       (point)))))
           (get-buffer-window-list buf))))

(defun visual-replace--overlay (start end replacement match-data)
  "Create an overlay highlighting text and replacement.

The text between START and END is the text being replacement.
REPLACEMENT, if non-nil, is its replacement.
MATCH-DATA is the value of (match-data) for the match."
  (unless (text-property-not-all start end 'read-only nil)
    (let ((ov (make-overlay start end)))
      (overlay-put ov 'priority 1000)
      (overlay-put ov 'visual-replace t)
      (overlay-put ov 'visual-replace-match-data match-data)
      (visual-replace--set-ov-replacement ov replacement)
      (visual-replace--set-ov-highlight ov)
      ov)))

(defun visual-replace--set-ov-replacement (ov replacement)
  "Fill in overlay OV with the given REPLACEMENT."
  (if (null replacement)
      ;; no replacement
      (progn
        (overlay-put ov 'face 'visual-replace-match)
        (overlay-put ov 'help-echo nil)
        (overlay-put ov 'keymap nil)
        (overlay-put ov 'after-string nil))

    ;; show text and replacement, highlight if necessary
    (overlay-put ov 'help-echo "mouse-1: apply")
    (overlay-put ov 'keymap visual-replace--on-click-map)
    (overlay-put
     ov 'after-string
     (propertize replacement
                 'help-echo "mouse-1: apply"
                 'keymap visual-replace--on-click-map))
    (visual-replace--set-ov-highlight ov)))

(defun visual-replace--set-ov-highlight (ov)
  "Highlight or de-highlight OV as appropriate for point."
  (let ((highlight (and visual-replace-highlight-match-at-point
                        (>= (point) (overlay-start ov))
                        (< (point) (overlay-end ov)))))
    (if-let ((after-string (overlay-get ov 'after-string)))
        (progn
          (overlay-put ov 'face (if highlight
                                    'visual-replace-delete-match-highlight
                                  'visual-replace-delete-match))
          (set-text-properties
           0 (length after-string)
           `(face ,(if highlight
                       'visual-replace-replacement-highlight
                     'visual-replace-replacement))
           after-string))
      (overlay-put ov 'face (if highlight
                                'visual-replace-match-highlight
                              'visual-replace-match)))))

(defun visual-replace--set-ov-highlight-at-pos (pos)
  "(De-)Highlight overlays at POS, as appropriate for point."
  (when pos
    (dolist (ov (overlays-at pos))
      (when (overlay-get ov 'visual-replace)
        (visual-replace--set-ov-highlight ov)))))

(defun visual-replace--update-total ()
  "Update the index and total in the prompt.

This should be called whenever the total changes or the point
changes."
  (when-let ((ov visual-replace--total-ov))
    (when (visual-replace--preview-is-complete)
      (let ((total (length visual-replace--match-ovs)))
        (overlay-put
         ov 'before-string
         (if-let ((ov
                   (seq-find
                    (lambda (ov) (overlay-get ov 'visual-replace-idx))
                    (with-current-buffer visual-replace--calling-buffer
                      (overlays-at (point))))))
             (format "[%d/%d] " (1+ (overlay-get ov 'visual-replace-idx)) total)
           (format "[%d] " total)))))))

(defun visual-replace--reset-preview ()
  "Reset the preview state and rebuild the preview."
  (when visual-replace--preview-state
    (setq visual-replace--preview-state nil)
    (visual-replace--clear-preview)))

(defun visual-replace--update-preview (&optional no-first-match)
  "Update the preview to reflect the content of the minibuffer.

This is meant to be called from a timer. The result of this
call is a set of overlays, stored in `visual-replace--match-ovs'.

It looks for matches within the visible portions of the buffer
and highlights them. If no matches can be found in the visible
portion of the buffer, it triggers a search for some other
matches to display unless NO-FIRST-MATCH is non-nil."
  (when visual-replace-preview
    (with-current-buffer visual-replace--calling-buffer
      (let* ((args (visual-replace-args--from-minibuffer))
             (ranges (visual-replace--scope-ranges))
             (visible-ranges (visual-replace--range-intersect-sorted
                              ranges
                              (visual-replace--visible-ranges
                               visual-replace--calling-buffer)))
             (old-args (visual-replace--preview-args))
             (old-visible-ranges (visual-replace--preview-ranges))
             (old-point (visual-replace--preview-point))
             (is-complete (visual-replace--preview-is-complete))
             (old-scope (visual-replace--preview-scope))
             equiv)
        (cond
         ;; Preview is turned off. Reset it if necessary.
         ((< (length (visual-replace-args-from args))
             visual-replace-min-length)
          (setq visual-replace--preview-state nil)
          (when-let ((ov visual-replace--total-ov))
            (overlay-put ov 'before-string nil))
          (visual-replace--clear-preview))

         ;; Preview overlays are correct.
         ((and (setq equiv (and (eq old-scope (visual-replace--scope-type visual-replace--scope))
                                (visual-replace-args--equiv-for-match-p args old-args)
                                (or is-complete (equal old-visible-ranges visible-ranges))))
               (equal (point) old-point)
               (string= (visual-replace-args-to args)
                        (visual-replace-args-to old-args))))

         ;; Preview overlays are complete; replacement needs updating.
         ;; This also updates highlights.
         ((and equiv
               (not (string= (visual-replace-args-to args)
                             (visual-replace-args-to old-args))))
          (let ((replacement (visual-replace--compile-replacement args))
                (replacement-count 0))
            (dolist (ov visual-replace--match-ovs)
              (set-match-data (overlay-get ov 'visual-replace-match-data))
              (visual-replace--set-ov-replacement
               ov (visual-replace--replacement-for-match-data
                   replacement
                   replacement-count
                   args))
              (cl-incf replacement-count)))
          (setf (visual-replace--preview-args) args)
          (when (not (equal (point) old-point))
            (visual-replace--update-total)
            (setf (visual-replace--preview-point) (point))))

         ;; Preview overlays are complete; highlight needs updating.
         ((and equiv (not (equal (point) old-point)))
          (visual-replace--set-ov-highlight-at-pos old-point)
          (visual-replace--set-ov-highlight-at-pos (point))
          (visual-replace--update-total)
          (setf (visual-replace--preview-point) (point)))

         ;; Preview overlays need to be re-created. Run searches to
         ;; build the preview, looking for a match to show if
         ;; necessary.
         (t
          (let ((first-match (and (not no-first-match)
                                  visual-replace-first-match))
                (count-matches (and (not no-first-match)
                                    visual-replace-display-total
                                    (not (visual-replace--preview-too-many-matches))
                                    (< (visual-replace--ranges-bytesize ranges)
                                       visual-replace-max-size-for-search)))
                work-queue consumers)

            (visual-replace--init-preview-state)
            (setf (visual-replace--preview-args) args)
            (setf (visual-replace--preview-ranges) visible-ranges)
            (setf (visual-replace--preview-point) (point))
            (setf (visual-replace--preview-scope)
                  (visual-replace--scope-type visual-replace--scope))

            ;; This section prepares searches in work-queue and runs
            ;; them. See visual-replace--run-idle-search for the
            ;; format of work-queue and consumers.
            (push `(,(append
                      (list #'visual-replace--goto-closest-match)
                      (unless count-matches
                        (list (lambda (ranges matches)
                                (visual-replace--highlight-matches ranges matches)))))

                    ;; call (visual-replace-search args ...
                    ,visible-ranges ,visual-replace-preview-max-duration)
                  work-queue)

            (when count-matches
              (push (lambda (ranges matches is-done)
                      (if (not is-done)
                          (if (and matches
                                   (<= visual-replace-max-matches-for-total
                                       (+ (length matches)
                                          (length visual-replace--match-ovs))))
                              (prog1 nil ; stop counting matches
                                (setf (visual-replace--preview-too-many-matches) t))
                            (prog1 t ; continue counting matches
                              (visual-replace--highlight-matches ranges matches)))

                        (when (visual-replace-args--equiv-for-match-p
                               args (visual-replace--preview-args))
                          (setf (visual-replace--preview-is-complete) t)
                          (setq visual-replace--match-ovs
                                (sort visual-replace--match-ovs
                                      (lambda (a b)
                                        (< (overlay-start a)
                                           (overlay-start b)))))
                          (let ((idx 0))
                            (dolist (ov visual-replace--match-ovs)
                              (overlay-put ov 'visual-replace-idx idx)
                              (cl-incf idx)))
                          (visual-replace--update-total))))
                    consumers))

            (when first-match
                (push (lambda (_ranges matches _is-done)
                        (visual-replace--show-first-match
                         matches visible-ranges (not count-matches)))
                      consumers))

            (when (or count-matches first-match)
              (let ((invisible-ranges (mapcar
                                       (lambda (range)
                                         (if (> (cdr range) visual-replace-max-size-for-search)
                                             (cons (car range) visual-replace-max-size-for-search)
                                           range))
                                       (visual-replace--range-substract-sorted
                                        ranges
                                        visible-ranges)))
                    (origin (save-excursion
                              (goto-char (visual-replace--scope-point
                                          visual-replace--scope))
                              (line-beginning-position))))
                ;; first search for a match below the original
                ;; position of the point.
                (dolist (small-range (visual-replace--small-ranges
                                      (visual-replace--range-intersect-sorted
                                       invisible-ranges
                                       `((,origin . ,(point-max))))))
                  (push `(nil
                          ;; call (visual-replace-search args ...
                          ,(list small-range)
                          ,visual-replace-first-match-max-duration
                          ,(if count-matches nil 1))
                        work-queue))
                ;; if none can be found, look for one above, going backward.
                (dolist (small-range (nreverse (visual-replace--small-ranges
                                                (visual-replace--range-intersect-sorted
                                                 invisible-ranges
                                                 `((,(point-min) . ,origin))))))
                  (push `(nil
                          ;; call (visual-replace-search args ...
                          ,(list small-range)
                          ,visual-replace-first-match-max-duration
                          ,(if count-matches nil 1)
                          backward)
                        work-queue))))
            (visual-replace--run-idle-search
             args (nreverse work-queue) consumers))))))))

(defun visual-replace--goto-closest-match (_ranges matches)
  "Go to the match from MATCHES closest to the current point.

This function works within the window targeted by Visual Replace.
It does nothing if there isn't one."
  (when-let ((win (visual-replace--find-window-noselect)))
    (with-selected-window win
      (let ((min-dist)
            (closest-match)
            (pos (point)))
        (dolist (m matches)
          (let ((dist (min (abs (- (car m) pos))
                           (abs (- (nth 1 m) pos)))))
            (when (or (not min-dist)
                      (< dist min-dist))
              (setq min-dist dist)
              (setq closest-match m))))
        (when closest-match
          (goto-char (car closest-match)))))))

(defun visual-replace--highlight-matches (ranges matches)
  "Create overlays to highlight MATCHES in the current buffer.

MATCHES should be the complete set of matches found within
RANGES. It may be nil if there are no matches.

This function is meant to be used as step consumer for
`visual-replace--run-idle-search' and fed matches found within
the visible range of the buffer."
  (visual-replace--clear-preview-in-ranges ranges)
  (dolist (m matches)
    (when-let ((ov (apply #'visual-replace--overlay m)))
      (push ov visual-replace--match-ovs))))

(defun visual-replace--show-first-match (matches visible-ranges update-preview)
  "Make sure the first match is visible.

This function is meant to be used as consumer for
`visual-replace--run-idle-search'.

This function waits for a match. It returns t as long as MATCHES
is empty, to ask to be called again with matches.

Once this function has gotten its first match, it checks it
against VISIBLE-RANGES. If the first match is not visible, it
moves the calling window to display that first match.

If UPDATE-PREVIEW is non-nil, call
`visual-replace--update-preview' after moving the window. This is
necessary if there only are overlays in the visible range."
  (if matches
      (prog1 ; match found
          nil ; stop searching for more matches
          (let ((pos (car (car matches)))
                (win (visual-replace--find-window-noselect)))
            (when (and win
                       (not (visual-replace--range-contains-sorted
                             visible-ranges pos)))
              (with-selected-window win
                (let ((orig-start (window-start win)))
                  (goto-char pos)
                  (recenter)
                  (when (and update-preview
                             (not (= orig-start (window-start win))))
                    (visual-replace--update-preview 'no-first-match)))))))
      ;; no matches, continue
      t))

(defun visual-replace--run-with-idle-timer (func &rest args)
  "Schedule FUNC with ARGS from an idle timer.

When called when Emacs is not idle, FUNC is run once Emacs has
been idle for `visual-replace-preview-delay'.

When called while Emacs is idle, FUNC is added in a way that asks
for it to be run right away but still allows other idle timers to
run."
  (let ((cur (current-idle-time)))
    (apply #'run-with-idle-timer
           (or cur visual-replace-preview-delay)
           nil func args)))

(defun visual-replace--run-idle-search (args work-queue consumers)
  "Run an idle search for ARGS, a `visual-replace-args' struct.

This functions goes though WORK-QUEUE, running searches as
indicated, on after the other. The first search is run directly
and any subsequent searches are run in a idle timer.

WORK-QUEUE is a list of (list-of-step-consumers . search-args).
Each element of WORK-QUEUE describes a call to
`visual-replace--search' with ARGS and search-args as arguments.
The matches returned by that call is fed to the step consumers in
list-of-step-consumers for the current element, and to the
consumers in CONSUMERS.

Consumers in list-of-step-consumers are called one after the
other with a single argument, the matches that were found,
possibly nil. The return value doesn't matter.

Consumers in CONSUMERS are also called one after the other with
the matches that were found. They must return either non-nil, to
indicate that they want to receive more matches, or nil, to
indicate that they're done.

`visual-replace--run-idle-search' doesn't run searches for which
there are no step consumers and no consumers."
  (when (buffer-live-p visual-replace--calling-buffer)
    (with-current-buffer visual-replace--calling-buffer
      (let* ((step-consumers (car (car work-queue)))
             (search-args (cdr (car work-queue)))
             (ranges (car search-args)))
        (when (or step-consumers consumers)
          (let ((matches (apply #'visual-replace--search
                                args search-args)))
            ;; Send matches to each step consumers.
            (dolist (c step-consumers)
              (funcall c ranges matches))

            ;; Send matches to each global consumers, remove consumers
            ;; that return nil, meaning they're done.
            (let ((c consumers))
              (while c
                (unless (funcall (car c) ranges matches nil)
                  (setcar c nil))
                (setq c (cdr c))))
            (setq consumers (delq nil consumers))))

        ;; Prepare work-queue for the next run, skipping items without any
        ;; consumers.
        (setq work-queue (cdr work-queue))
        (unless consumers
          (while (and work-queue (null (car (car work-queue))))
            (setq work-queue (cdr work-queue))))

        ;; Schedule the next run
        (if (null work-queue)
            ;; Let any remaining consumers know we're done.
            (dolist (c consumers)
              (funcall c nil nil 'is-done))

          ;; Schedule another run on an idle timer
          (visual-replace--reset-idle-search-timer)
          (setq visual-replace--idle-search-timer
                (visual-replace--run-with-idle-timer
                 (lambda ()
                   (setq visual-replace--idle-search-timer nil)
                   (visual-replace--run-idle-search
                    args work-queue consumers)))))))))

(defun visual-replace--reset-idle-search-timer ()
  "Cancel `visual-replace--idle-search-timer' if necessary."
  (when visual-replace--idle-search-timer
    (cancel-timer visual-replace--idle-search-timer)
    (setq visual-replace--idle-search-timer nil)))

(defun visual-replace--clear-preview ()
  "Delete all overlays in `visual-replace--match-ovs', if any."
  (with-current-buffer visual-replace--calling-buffer
    (dolist (overlay visual-replace--match-ovs)
      (delete-overlay overlay)))
  (setq visual-replace--match-ovs nil))

(defun visual-replace--clear-preview-in-ranges (ranges)
  "Delete all overlays in `visual-replace--match-ovs' in RANGES.

RANGES must be sorted and non-overlapping.

Overlays that start within RANGES are deleted and removed from
the list."
  (with-current-buffer visual-replace--calling-buffer
    (let ((ovs visual-replace--match-ovs))
      (setq visual-replace--match-ovs nil)
      (dolist (ov ovs)
        (if (visual-replace--range-contains-sorted
             ranges (overlay-start ov))
            (delete-overlay ov)
          (push ov visual-replace--match-ovs))))))

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

(defun visual-replace--ranges-bytesize (ranges)
  "Compute the size, in bytes of the buffer range covered by RANGES.

The range must be sorted and non-overlapping."
  (if ranges
      (- (cdr (car (last ranges)))
         (car (car ranges)))
    0))

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

(defun visual-replace--range-substract-sorted (aranges branges)
  "Substract sorted, non-overlapping ranges BRANGES from ARANGES.

`visual-replace--range-fix' or `visual-replace--ranges-nmerge' can make
sure that ARANGES and BRANGES are sorted and non-overlapping,

Returns a set of sorted, non-overlapping ranges."
  (let ((result))
    (while (and aranges branges)
      (let* ((arange (car aranges))
             (brange (car branges))
             (left (when (< (car arange) (car brange))
                     (cons (car arange)
                           (min (cdr arange) (car brange)))))
             (right (when (and (> (cdr arange) (cdr brange))
                               (< (car arange) (cdr brange)))
                      (cons (cdr brange) (cdr arange)))))
        (when left (push left result))
        (if right
            (setq aranges (cons right (cdr aranges))
                  branges (cdr branges))
          (when (<= (cdr arange) (cdr brange))
            (setq aranges (cdr aranges)))
          (when (<= (cdr brange) (cdr arange))
            (setq branges (cdr branges))))))
    (append (nreverse result) aranges)))

(defun visual-replace--range-contains-sorted (ranges pos)
  "Check whether POS is in RANGES.

`visual-replace--range-fix' or `visual-replace--ranges-nmerge' can make
sure that RANGES are sorted and non-overlapping,

Returns non-nil if RANGES contains POS."
  (while (and ranges (> pos (cdr (car ranges))))
    (setq ranges (cdr ranges)))
  (when ranges
    (>= pos (car (car ranges)))))

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
  (with-selected-window (visual-replace--find-window)
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
  (with-selected-window (visual-replace--find-window)
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
  (with-selected-window (visual-replace--find-window)
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
  (with-selected-window (visual-replace--find-window)
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
  (unless (eq (current-buffer)
              visual-replace--calling-buffer)
    (error "No active Visual Replace session for buffer"))
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
    (select-window (get-buffer-window
                    visual-replace--minibuffer))))

(defun visual-replace--find-window ()
  "Find a live window to work on or fail with an error.

This function looks for a window showing the calling buffer,
preferring the window Visual Replace was called from. It displays
the buffer, if necessary."
  (let ((buf visual-replace--calling-buffer)
        (win visual-replace--calling-window))
    (cond
     ((and (window-live-p win)
           (eq (window-buffer win) buf))
      win)
     ((not (buffer-live-p buf))
      (error "No buffer for Visual Replace to work on"))
     ((setq win (or (get-buffer-window buf)
                    (display-buffer buf)))
      (setq visual-replace--calling-window win)
      win)
     (t
      (error "No window for Visual Replace to work on")))))

(defun visual-replace--find-window-noselect ()
  "Find a live window to work on or return nil.

Contrary to `visual-replace--find-window', this function will not
display the buffer or fail if there's no window for it. It'll
just return nil."
  (let ((win visual-replace--calling-window)
        (buf visual-replace--calling-buffer))
    (or
     (when (and (window-live-p win)
                (eq buf (window-buffer win)))
       win)
     (when (buffer-live-p buf)
       (get-buffer-window buf)))))

(defun visual-replace--assert-in-minibuffer ()
  "Raise an error unless called from a minibuffer in the right mode."
  (unless (eq (current-buffer) visual-replace--minibuffer)
    (error "Not in a Visual Replace minibuffer")))

;; Older versions of Visual Replace defined their own version of kill
;; and kill-whole-line that avoided deleting the separator. Recent
;; versions achieve the same by defining two fields, so kill and
;; kill-whole-line, as well as beginning-of-line and end-of-line, know
;; how to behave.
(defalias 'visual-replace-kill 'kill)
(make-obsolete 'visual-replace-kill 'kill "2024-11-07")
(defalias 'visual-replace-kill-whole-line 'kill-whole-line)
(make-obsolete 'visual-replace-kill-whole-line 'kill-whole-line "2024-11-07")

(provide 'visual-replace)

;;; visual-replace.el ends here

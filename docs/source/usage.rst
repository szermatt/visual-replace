Usage
=====

Calling Visual Replace
----------------------

Visual Replace needs to be bound to a key to be of any use.

Choose a reasonably short key combination and bind
``visual-replace`` to it. It should be reasonably short, because
``visual-replace``, by default, uses the key combination it's
called with as prefix for the commands available in the minibuffer.

Here's an example that uses :kbd:`M-%` as key combination, since this
is bound by default to ``query-replace``, which Visual Replace
then, well, replaces:

.. code-block:: elisp

  (use-package visual-replace
    :defer t
    :bind (("M-%" . visual-replace)
           :map isearch-mode-map
           ("M-%" . visual-replace-from-isearch))
    :config
    (define-key visual-replace-mode-map (kbd "M-%")
                visual-replace-secondary-mode-map))

The above example also binds :kbd:`M-%` in isearch, so you can just
switch from isearch to Visual Replace. Additionally, while Visual
Replace is active :kbd:`M-%` is the prefix for Visual Replace
commands, so, for example, toggling regexp mode on and off is
:kbd:`M-% r`.

An alternative, which you might prefer to try things out, is to
replace ``query-replace`` and others with Visual Replace. This
then uses whatever shortcut you've already installed.

.. code-block:: elisp

  (use-package visual-replace
    :defer nil
    :config
    (visual-replace-global-mode 1))

Once this is done, launch ``visual-replace`` with the keybinding you chose.

Visual Replace Mode
-------------------

When Visual Replace is running, you'll see, something like the
following in the minibuffer `Replace from point [...]: ┃ →`. The text
before the arrow is the text to replace and the text after the arrow
is the replacement. You can navigate back and forth with :kbd:`TAB` or
by moving the cursor.

See also the example below.

  .. image:: ../../images/cast.gif
    :width: 600
    :alt: Screen grab showing Visual Replace in action

Once both fields are filled, press :kbd:`RET` to execute the
replacement.

When there's no replacement :kbd:`RET` instead moves the cursor to the
replacement, in case muscle memory kicks in and you type: *text to
replace* :kbd:`RET` *replacement* :kbd:`RET`. That'll work.

The prompt also displays the mode of replacement:

* *text* → *replacement* executes `string-replace`
* *text* →? *replacement* executes `query-replace`
* *text* →.* *replacement* executes `replace-regexp`
* *text* →?.* *replacement* executes `query-replace-regexp`

After typing a few characters of the string to match `visual-replace`
enters preview mode, and highlights the matches. It also scrolls the
window to keep at least one example of matches visible. You can also
press up and down to go through the matches.

In Visual Replace mode:

* :kbd:`TAB` navigates between the text to replace and the
  replacement string

* :kbd:`RET` switches to the replacement string, the first time, then
  executes the replacement

* :kbd:`M-% r` toggles regexp mode on and off. You know this mode is
  on when a ``.*`` follows the arrow.

* :kbd:`M-% q` toggles query mode one and off, that is, it toggles
  between calling ``replace-string`` and ``query-replace``.
  You know this mode is on when a ``?`` follows the arrow. For an
  alternative way of replacing only some matches, see :ref:`single`.

* :kbd:`M-% SPC` switches between different scopes: full buffer, from
  point, in region. The scope is indicated in the prompt.
  Additionally, for from point and in region, the region is
  highlighted.

* :kbd:`M-% w` toggle limiting search to whole words. You know this
  mode is on when a ``w`` follows the arrow.

* :kbd:`M-% c` toggle case-fold. You know this mode is on when a
  ``c`` follows the arrow.

* :kbd:`M-% s` toggle lax whitespace. You know this mode is on when
  ``(lax ws)`` follows the arrow.

* :kbd:`M-% b` toggle backward replace. You know this mode is on when
  a ``↩`` follows the arrow. This only matters if query search mode
is on of when a replacement contains ``\\#``.

* :kbd:`<up>` and :kbd:`<down>` move the cursor to the next or
  previous match, scrolling if necessary.

* :kbd:`M-% a` applies a single replacement, to the match right under
  the cursor or following the cursor, then move on to the next match.
  With a prefix argument N, apply N replacements. See also :ref:`single`.

* :kbd:`M-% u` calls ``undo`` on the original buffer, to revert a
  previous replacement. With a prefix argument N, repeat undo N times.

* As usual, :kbd:`C-p` and `C-n` go up and down the history, like on any prompt.

(Reminder: replace *M-%* with the keyboard shortcut you chose.)

If you leave ``visual-replace`` without confirming, with :kbd:`C-g`, you can
continue where you left off next time by going up in the history.

See `Search
<https://www.gnu.org/software/emacs/manual/html_node/emacs/Search.html>`_
in the Emacs manual for details of the different modes listed above.

.. _yank:

Yank and Pop
------------

.. index::
   pair: function; visual-replace-yank
   pair: function; visual-replace-yank-pop

Yank, usually bound to :kbd:`C-y`, works differently in Visual Replace
than it does normally. In Visual Replace mode, it calls
``visual-replace-yank``.

* In the search section, yanking copies text from the current buffer
  into the search section. This avoids typing text when it's right
  under the point.

  You can also move to a match with :kbd:`<up>` and :kbd:`<down>` to
  capture more text from the buffer.

* In the replacement section, yanking copies text from the search
  section. This avoids typing the search string again when you just
  want to make some small changes to it.

The normal yank can be executed by calling ``yank-pop``, usually
bound to :kbd:`M-y`.

This can be configured by editing `visual-mode-map`. For example, to
use the normal yank commands, you can do:

.. code-block:: elisp

  (define-key visual-replace-mode-map [remap yank] nil)
  (define-key visual-replace-mode-map [remap yank-pop] nil)


.. _single:

Single replacements
-------------------

If you want to replace only *some* matches within the scope, you can:

* use the ``query-replace`` UI to go through all matches using
  :kbd:`M-% q`, then typing :kbd:`RET` to enter Query Replace mode. `

* in preview mode, click on the replacements you want to apply. You
  can scroll the buffer as needed, normally or, from the minibuffer
  with :kbd:`<up>` and :kbd:`<down>`.

* navigate to the replacements you want to apply with :kbd:`<up>` and
  :kbd:`<down>`, the call :kbd:`M-% a` to apply one replacement.

  On Emacs 29.1 or later, this enters a mode that allows applying
  replacement with :kbd:`a`, the last part of the key sequence, and
  also moving through the matches with :kbd:`<down>` or :kbd:`<up>`.
  :kbd:`u` reverts the last replacement.

.. _options:

Customization
-------------

.. index::
   pair: variable; visual-replace-keep-initial-position
   pair: variable; visual-replace-display-total
   pair: variable; visual-replace-preview
   pair: variable; visual-replace-first-match
   pair: variable; visual-replace-initial-scope
   pair: variable; visual-replace-default-to-full-scope
   pair: variable; visual-replace-defaults-hook
   pair: variable; visual-replace-minibuffer-mode-hook
   pair: variable; visual-replace-min-length

This section lists a few of the most interesting customization options
available in visual replace. Call :kbd:`M-x customize-group
visual-replace` to see all options. For face customization, see the
:ref`next section<faces>`.

visual-replace-preview : customization option
  With this option enabled, Visual Replace highlights matches and
  offer a preview of their replacements. This is enabled by default.

visual-replace-first-match : customization option
  With this option enabled, Visual Replace always tries to have at
  least one match visible in the preview, even if it means jumping to
  another section of the buffer. This is enabled by default.

keep-initial-position : customization option
  With this option enabled, Visual Replace goes back to the point it
  was called from, even if the point was moved during preview, to
  display the first match, or manually with :kbd:`<down>` or
  :kbd:`<up>`.

  Note that in the case where the point is moved during preview,
  Visual Replace sets a mark at the original location, to go back too
  if necessary.

visual-replace-display-total : customization option
  By default, in preview mode, visual Replace only searches for and
  display matches in the visible portions of the buffer. With this
  option enabled, Visual Replace searches the whole buffer, in an idle
  timer, and displays the total number of matches in the prompt.

  When the point is on a match, the index of the match is also
  displayed, in front of the total.

  The total might be slow to update on large buffers or when using
  complicated regexps.

  This is not enabled by default.

visual-replace-initial-scope : customization option
  With this option set, the initial scope ignores the active region
  entirely and is always set to either "From Point" or "Full Buffer".

  By default, the initial scope is:

    * the active region, if there is one

    * from point if ``visual-replace-default-to-full-scope`` is nil, see below

    * the full buffer otherwise

visual-replace-default-full-scope : customization option
  With this option set, when no region is active, replacement applies
  by default to the full buffer, instead of to the region following
  the point.

visual-replace-defaults-hook : customization option
  To modify search and replace defaults, such as, for example, having
  searches default to regular expressions or search default to word
  mode, call the command that turns it on from this hook. This is
  called when Visual Replace is started with no initial text, so these
  customizations won't apply to ``visual-replace-from-isearch``,
  for example.

visual-replace-minibuffer-mode-hook : customization option
  This hook is called when Visual Replace is started in the
  minibuffer. It can be used to turn on query mode in all cases by
  registering the command ``visual-replace-toggle-query`` in this
  hook.

  Rather than setting the as a customization, with
  ``use-package``, you can force Visual Replace to call
  ``query-replace`` by default with:

  .. code-block:: elisp

    (use-package visual-replace
      [...]
      :hook ((visual-replace-minibuffer-mode . visual-replace-toggle-query))

visual-replace-min-length : customization option
  This specifies the minimum number of characters that need to be
  typed before Visual Replace enters preview mode.

  Setting this too low might result in strange highlights happening
  when starting to type in the match string.

.. _faces:

Face Customization
------------------

.. index::
   pair: variable; visual-replace-delete-match
   pair: variable; visual-replace-delete-match-highlight
   pair: variable; visual-replace-match
   pair: variable; visual-replace-match-count
   pair: variable; visual-replace-match-highlight
   pair: variable; visual-replace-region
   pair: variable; visual-replace-replacement
   pair: variable; visual-replace-replacement-highlight
   pair: variable; visual-replace-separator


Visual Replace relies an a large number of faces to display things the way they should be:

  * ``visual-replace-match``, for matches with no replacement

  * ``visual-replace-match-highlight``, for matches at point with no replacement

  * ``visual-replace-replacement``, for match replacement

  * ``visual-replace-replacement-highlight``, for match replacement, at point

  * ``visual-replace-delete-match``, for text to be deleted

  * ``visual-replace-delete-match-highlight``, for text to be
    deleted at point

  * ``visual-replace-match-count``, for displaying the number of
    matches before the prompt

  * ``visual-replace-separator``, for displaying the separator
    between the search and replacement strings in the prompt

  * ``visual-replace-region``, for highlighting the area of the
    buffer to which search and replace apply

The defaults values for this faces attempt to reuse existing faces as
much as possible to try and look reasonable whatever the current Emacs
theme, but the result isn't always too great. In particular,
``visual-replace-region``, which uses the same face as the region,
is typically too bright and in-your-face. It should ideally use a
fainter color than the region, still visible, but not too different
from the normal background as to cause readability issues.

Therefore, it's a good idea to configure the Visual Replace faces to
match your theme and preferences.

The sections below list my attempts at configuring Visual Replace for
the `Modus themes <https://protesilaos.com/emacs/modus-themes>`_, now
installed in Emacs by default, and the `Ef themes
<https://protesilaos.com/emacs/ef-themes>`_, by the same author. This
should hopefully help you get started.

The code snippets rely on ``after-enable-theme-hook`` to detect
theme changes, from the `Section 5.23 of the Emacs 29 Manual
<https://www.gnu.org/software/emacs/manual/html_node/modus-themes/A-theme_002dagnostic-hook-for-theme-loading.html>`_:

.. code-block:: elisp

  (defun run-after-enable-theme-hook (&rest _args)
     "Run `after-enable-theme-hook'."
     (run-hooks 'after-enable-theme-hook))

  (advice-add 'enable-theme :after #'run-after-enable-theme-hook)

Modus Themes
^^^^^^^^^^^^

.. code-block:: elisp

  (defun my-modus-themes-custom-faces ()
    (when (delq nil (mapcar (lambda (t) (string-prefix-p "modus-" (symbol-name t)))
                            custom-enabled-themes))
      (modus-themes-with-colors
        (custom-set-faces
         `(visual-replace-match-count ((t :inherit modus-themes-prompts)))
         `(visual-replace-separator ((t :inherit modus-themes-prompts)))
         `(visual-replace-match ((t :inherit modus-themes-search-success-lazy)))
         `(visual-replace-replacement ((t :background ,bg-diff-added :foreground ,fg-diff-added)))
         `(visual-replace-delete-match ((t :strike-through t :background ,bg-diff-removed :foreground ,fg-diff-removed)))
         `(visual-replace-match-highlight ((t  :inherit modus-themes-search-success)))
         `(visual-replace-delete-match-highlight ((t :strike-through t :background ,bg-diff-refine-removed :foreground ,fg-diff-refine-removed)))
         `(visual-replace-replacement-highlight ((t :background ,bg-diff-refine-added :foreground ,fg-diff-refine-added)))
         `(visual-replace-region ((t :background ,bg-special-faint-cold :extend t )))))))
  (add-hook 'after-enable-theme-hook #'my-modus-themes-custom-faces)

Ef Themes
^^^^^^^^^

.. code-block:: elisp

   (defun my-ef-themes-custom-faces ()
    (when (delq nil (mapcar (lambda (t) (string-prefix-p "ef-" (symbol-name t)))
                            custom-enabled-themes))
      (ef-themes-with-colors
        (let ((bg-region-fainter (my-color-closer bg-region bg-main 0.3)))
          (custom-set-faces
           `(visual-replace-match-count ((,c :foreground ,prompt)))
           `(visual-replace-separator ((,c :foreground ,prompt)))
           `(visual-replace-match ((,c :background ,bg-search-lazy :foreground ,fg-intense)))
           `(visual-replace-replacement ((,c :background ,bg-added :foreground ,fg-added)))
           `(visual-replace-delete-match ((,c :strike-through t :background ,bg-removed-faint :foreground ,fg-removed)))
           `(visual-replace-match-highlight ((,c  :background ,bg-search-match :foreground ,fg-intense )))
           `(visual-replace-delete-match-highlight ((,c :strike-through t :background ,bg-removed-refine :foreground ,fg-intense)))
           `(visual-replace-replacement-highlight ((,c :background ,bg-added-refine :foreground ,fg-intense)))
           `(visual-replace-region ((,c :background ,bg-region-fainter :extend t ))))))))

    (defun my-color-closer (from to fraction)
    "Move FROM luminance closer to TO by the given FRACTION."
    (let* ((from-hsl (apply 'color-rgb-to-hsl (color-name-to-rgb from)))
           (to-hsl (apply 'color-rgb-to-hsl (color-name-to-rgb to))))
      (apply 'color-rgb-to-hex
             (color-hsl-to-rgb
              (nth 0 from-hsl)
              (nth 1 from-hsl)
              (+ (nth 2 from-hsl) (* fraction (- (nth 2 to-hsl) (nth 2 from-hsl))))))))

  (add-hook 'after-enable-theme-hook #'my-ef-themes-custom-faces)



.. _commands:

Commands
--------

.. index::
   pair: command; visual-replace
   pair: command; visual-replace-thing-at-point
   pair: command; visual-replace-selected
   pair: command; visual-replace-from-isearch

visual-replace : command
  This is the main command that starts Visual Replace and then
  executes the search-and-replace. It can replace
  ``replace-string``, ``query-replace``,
  ``replace-regexp`` and ``query-replace-regexp``.

visual-replace-thing-at-point : command
  This command starts a visual replace session with the symbol at
  point as text to replace.

visual-replace-selected : command
  This command starts with the text within the current active region
  as text to replace.

visual-replace-from-isearch : command
  This command switches from an active isearch session to
  ``visual-replace``, keeping the current search text and
  settings, such as regexp mode. This is meant to be called while
  isearch is in progress, and bound to ``isearch-mode-map``.

.. index::
   pair: command; visual-replace-toggle-regexp
   pair: command; visual-replace-toggle-scope
   pair: command; visual-replace-toggle-query
   pair: command; visual-replace-toggle-word
   pair: command; visual-replace-toggle-case-fold
   pair: command; visual-replace-toggle-backwards
   pair: command; visual-replace-toggle-lax-ws
   pair: command; visual-replace-next-match
   pair: command; visual-replace-prev-match
   pair: command; visual-replace-apply-one
   pair: command; visual-replace-apply-one-repeat
   pair: command; visual-replace-undo
   pair: variable; visual-replace-transient-map

The following commands are meant to be called while in Visual Replace
mode, from ``visual-mode-map``. By default, they're bound in
``visual-replace-secondary-mode-map``:

visual-replace-toggle-regexp : <prefix> r , command
    toggles regexp mode on and off.

visual-replace-toggle-scope : <prefix> SPC, command
    changes the scope of the search.

visual-replace-toggle-query : <prefix> q, command
    toggles the query mode on and off.

visual-replace-toggle-word : <prefix> w, command
    toggles the word mode on and off.

visual-replace-toggle-case-fold : <prefix> c, command
    toggles the case fold mode on and off.

visual-replace-toggle-backwards : <prefix> d, command
    toggles backward replacement on and off.

visual-replace-toggle-lax-ws : <prefix> s, command
    toggles the lax whitespace mode on and off.

visual-replace-next-match : <down>, command
    moves cursor to the next match

visual-replace-prev-match : <up>, command
    moves cursor to the previous match

visual-replace-apply-one : <prefix> a, command
    applies a single replacement, to the match at or after the
    cursor, then moves on to the next match. With a prefix argument
    N, apply N replacements instead of just one.

    This command, used together with ``visual-replace-next-match``
    and ``visual-replace-prev-match`` is in many cases functionally
    equivalent to using the query mode, but with a different interface
    that the possibility of changing the query as you go.

visual-replace-apply-one-repeat : <prefix> a, command
    on Emacs 29.1 and later, executes ``visual-replace-apply-one``,
    then installs a transient map that allows:

    * repeating ``visual-replace-apply-one`` by typing the last part
      of the key sequence used to call ``visual-replace-apply-one-repeat``

    * skipping matches with :kbd:`<down>`, which calls ``visual-replace-next-match``

    * going up the match previews with :kbd:`<up>`, which calls ``visual-replace-prev-match``

    * undoing the last replacement with :kbd:`u`

    * Typing anything else deactivates the transient map.

    The keybindings can be configured by modifying the map ``visual-replace-transient-map``.

visual-replace-undo : <prefix> u, command
  reverts the last call to ``visual-replace-apply-one``. This just
  executes ``undo`` in the original buffer. With a prefix argument N,
  call undo N times instead of just one.

Keymaps
-------

.. index::
   pair: variable; visual-replace-mode-map
   pair: variable; visual-replace-secondary-mode-map

visual-replace-mode-map : keymap
  This is the map that is active in the minibuffer in Visual Replace
  mode. You can add your own keybindings to it.

visual-replace-secondary-mode-map : keymap
  This is the map that defines keyboard shortcuts for modifying the
  search mode, such as :kbd:`r` to toggle regexp mode on or off. It is
  bound by default in ``visual-replace-mode-map`` to the shortcut that
  was used to launch Visual Replace, but you can bind it to whatever
  you want, or define custom shortcuts directly in
  ``visual-replace-mode-map``.

In the example below, :kbd:`C-l` is bound to secondary mode map and
:kbd:`C-r` toggles the regexp mode, so it is possible to toggle the
regexp mode using either :kbd:`C-l r` or :kbd:`C-r`.

.. code-block:: elisp

  (use-package visual-replace
    :defer t
    :bind (("C-c l" . visual-replace)
           :map visual-replace-mode-map
           ("C-r" . visual-replace-toggle-regexp))
    :config
    (define-key visual-replace-mode-map (kbd "C-l")
        visual-replace-secondary-mode-map))

Hooks
-----

.. index::
   pair: hook; visual-replace-minibuffer-mode-hook
   pair: hook; visual-replace-functions
   pair: variable; visual-replace-defaults-hook

visual-replace-minibuffer-mode-hook : hook
  This is a normal hook that is run when entering the visual replace
  mode, so you can set things up just before Visual Replace starts.

visual-replace-defaults-hook : hook
  This is a normal hook that is run when entering the visual replace
  mode with no initial match or replacement, so you can provide some
  default mode without interfering with
  ``visual-replace-from-isearch`` or
  ``visual-replace-thing-at-point``.

visual-replace-functions : hook
  Functions in this abnormal hook are called just before executing the
  replacement or just before building the previews. They are passed a
  struct of type ``visual-replace-args``, which they can modify. You
  can use it to customize the behavior of the search or modify the
  regexp language.

Limitations
-----------

* Visual Replace avoids executing replacement in the whole buffer
  during preview; it just executes them in the parts of the buffer
  that are currently visible. This means that the preview can show
  incorrect replacement in some cases, such as when replacement uses
  `\\#` directly or within a `\\,` In such cases, the preview can be
  wrong but execution will be correct.

  Replacements that call stateful functions in `\\,` such as a
  function that increment an internal counter, will be executed too
  many times during preview, with unpredictable results.

  In all other cases, the preview should match what is eventually
  executed. If that's not the case, please report an issue.
  (:ref:`reporting`)

* If you use ``visual-replace-apply-one`` to replace single
  matches, ``\\#`` in the replacement is always 1, because single
  matches are applied separately.

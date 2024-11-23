Usage
=====

Calling Visual Replace
----------------------

Visual Replace needs to be bound to a key to be of any use.

Choose a reasonably short key combination and bind
:code:`visual-replace` to it. It should be reasonably short, because
:code:`visual-replace`, by default, uses the key combination it's
called with as prefix for the commands available in the minibuffer.

Here's an example that uses :kbd:`M-%` as key combination, since this
is bound by default to :code:`query-replace`, which Visual Replace
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
replace :code:`query-replace` and others with Visual Replace. This
then uses whatever shortcut you've already installed.

.. code-block:: elisp

  (use-package visual-replace
    :defer nil
    :config
    (visual-replace-global-mode 1))

Once this is done, launch :code:`visual-replace` with the keybinding you chose.

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
  on when a :code:`.*` follows the arrow.

* :kbd:`M-% q` toggles query mode one and off, that is, it toggles
  between calling :code:`replace-string` and :code:`query-replace`.
  You know this mode is on when a :code:`?` follows the arrow. See
  also :ref:`single` for an alternative way of replacing only some
  matches.

* :kbd:`M-% SPC` switches between different scopes: full buffer, from
  point, in region. The scope is indicated in the prompt.
  Additionally, for from point and in region, the region is
  highlighted.

* :kbd:`M-% w` toggle limiting search to whole words. You know this
  mode is on when a :code:`w` follows the arrow.

* :kbd:`M-% c` toggle case-fold. You know this mode is on when a
  :code:`c` follows the arrow.

* :kbd:`M-% s` toggle lax whitespace. You know this mode is on when
  :code:`(lax ws)` follows the arrow.

* :kbd:`<up>` and :kbd:`<down>` move the cursor to the next or
  previous match, scrolling if necessary.

* :kbd:`M-% a` applies a single replacement, to the match right under
  the cursor or following the cursor, then move on to the next match.
  With a prefix argument N, apply N replacements. See also :ref:`single`.

* :kbd:`M-% u` calls :code:`undo` on the original buffer, to revert a
  previous replacement. With a prefix argument N, repeat undo N times.

* As usual, :kbd:`C-p` and `C-n` go up and down the history, like on any prompt.

(Reminder: replace *M-%* with the keyboard shortcut you chose.)

If you leave :code:`visual-replace` without confirming, with :kbd:`C-g`, you can
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
:code:`visual-replace-yank`.

* In the search section, yanking copies text from the current buffer
  into the search section. This avoids typing text when it's right
  under the point.

  You can also move to a match with :kbd:`<up>` and :kbd:`<down>` to
  capture more text from the buffer.

* In the replacement section, yanking copies text from the search
  section. This avoids typing the search string again when you just
  want to make some small changes to it.

The normal yank can be executed by calling :code:`yank-pop`, usually
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

* use the :code:`query-replace` UI to go through all matches using
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
visual-replace` to see all options.

Notably, see that customization group for the available customizable
faces.

* :kbd:`M-x customize-option visual-replace-preview` With this option
  enabled, Visual Replace highlights matches and offer a preview of
  their replacements. This is enabled by default.

* :kbd:`M-x customize-option visual-replace-first-match` With this
  option enabled, Visual Replace always tries to have at least one
  match visible in the preview, even if it means jumping to another
  section of the buffer. This is enabled by default.

* :kbd:`M-x customize-option keep-initial-position` With this option
  enabled, Visual Replace goes back to the point it was called from,
  even if the point was moved during preview, to display the first
  match, or manually with :kbd:`<down>` or :kbd:`<up>`.

  Note that in the case where the point is moved during preview,
  Visual Replace sets a mark at the original location, to go back too
  if necessary.

* :kbd:`M-x customize-option visual-replace-display-total` By default,
  in preview mode, visual Replace only searches for and display
  matches in the visible portions of the buffer. With this option
  enabled, Visual Replace searches the whole buffer, in an idle timer,
  and displays the total number of matches in the prompt.

  When the point is on a match, the index of the match is also
  displayed, in front of the total.

  The total might be slow to update on large buffers or when using
  complicated regexps.

  This is not enabled by default.

* :kbd:`M-x customize-option visual-replace-initial-scope` With this
  option set, the initial scope ignores the active region
  entirely and is always set to either "From Point" or "Full Buffer".

  By default, the initial scope is:

    * the active region, if there is one

    * from point if :code:`visual-replace-default-to-full-scope` is nil, see below

    * the full buffer otherwise

* :kbd:`M-x customize-option visual-replace-default-full-scope` With
  this option set, when no region is active, replacement applies by
  default to the full buffer, instead of to the region following the
  point.

* :kbd:`M-x customize-option visual-replace-defaults-hook` To modify
  search and replace defaults, such as, for example, having searches
  default to regular expressions or search default to word mode, call
  the command that turns it on from this hook. This is called when
  Visual Replace is started with no initial text, so these
  customizations won't apply to :code:`visual-replace-from-isearch`,
  for example.

* :kbd:`M-x customize-option visual-replace-minibuffer-mode-hook` This
  hook is called when Visual Replace is started in the minibuffer. It
  can be used to turn on query mode in all cases by registering the
  command :code:`visual-replace-toggle-query` in this hook.

  Rather than setting the as a customization, with
  :code:`use-package`, you can force Visual Replace to call
  :code:`query-replace` by default with:

  .. code-block:: elisp

    (use-package visual-replace
      [...]
      :hook ((visual-replace-minibuffer-mode . visual-replace-toggle-query))

* :kbd:`M-x customize-option visual-replace-min-length` This specifies
  the minimum number of characters that need to be typed before Visual
  Replace enters preview mode.

  Setting this too low might result in strange highlights happening
  when starting to type in the match string.

.. _commands:

Commands
--------

.. index::
   pair: command; visual-replace
   pair: command; visual-replace-thing-at-point
   pair: command; visual-replace-selected
   pair: command; visual-replace-from-isearch

* :code:`visual-replace` is the main command that starts Visual Replace and
  then executes the search-and-replace. It can replace :code:`replace-string`,
  :code:`query-replace`, :code:`replace-regexp` and :code:`query-replace-regexp`.

* :code:`visual-replace-thing-at-point` starts a visual replace session with
  the symbol at point as text to replace.

* :code:`visual-replace-selected` starts with the text within the current
  active region as text to replace.

* :code:`visual-replace-from-isearch` switches from an active isearch
  session to :code:`visual-replace`, keeping the current search text and
  settings, such as regexp mode. This is meant to be called while
  isearch is in progress, and bound to :code:`isearch-mode-map`.

.. index::
   pair: command; visual-replace-toggle-regexp
   pair: command; visual-replace-toggle-scope
   pair: command; visual-replace-toggle-query
   pair: command; visual-replace-toggle-word
   pair: command; visual-replace-toggle-case-fold
   pair: command; visual-replace-toggle-lax-ws
   pair: command; visual-replace-next-match
   pair: command; visual-replace-prev-match
   pair: command; visual-replace-apply-one
   pair: command; visual-replace-apply-one-repeat
   pair: command; visual-replace-undo
   pair: variable; visual-replace-transient-map

The following commands are meant to be called while in Visual Replace
mode, from :code:`visual-mode-map`. By default, they're bound in
:code:`visual-replace-secondary-mode-map`:

* :code:`visual-replace-toggle-regexp` toggles regexp mode on and off.
* :code:`visual-replace-toggle-scope` changes the scope of the search.
* :code:`visual-replace-toggle-query` toggles the query mode on and off.
* :code:`visual-replace-toggle-word` toggles the word mode on and off.
* :code:`visual-replace-toggle-case-fold` toggles the case fold mode on and off.
* :code:`visual-replace-toggle-lax-ws` toggles the lax whitespace mode on and off.
* :code:`visual-replace-next-match` moves cursor to the next match
* :code:`visual-replace-prev-match` moves cursor to the previous match
* :code:`visual-replace-apply-one` applies a single replacement, to the
  match at or after the cursor, then moves on to the next match. With a
  prefix argument N, apply N replacements instead of just one.

  This command, used together with :code:`visual-replace-next-match`
  and :code:`visual-replace-prev-match` is in many cases functionally
  equivalent to using the query mode, but with a different interface
  that the possibility of changing the query as you go.
* :code:`visual-replace-apply-one-repeat` executes
  :code:`visual-replace-apply-one`, then install a transient map that
  allows:

    * repeating :code:`visual-replace-apply-one` by typing the last part
      of the key sequence used to call :code:`visual-replace-apply-one-repeat`

    * skipping matches with :kbd:`<down>`, which calls :code:`visual-replace-next-match`

    * going up the match previews with :kbd:`<up>`, which calls :code:`visual-replace-prev-match`

    * undoing the last replacement with :kbd:`u`

  Typing anything else deactivates the transient map.

  This can be configured by modifying the map :code:`visual-replace-transient-map`.

  This command is available on Emacs 29.1 or later.

* :code:`visual-replace-undo` reverts the last call to
  :code:`visual-replace-apply-one`. This just executes :code:`undo` in
  the original buffer. With a prefix argument N, call undo N times
  instead of just one.

Keymaps
-------

.. index::
   pair: variable; visual-replace-mode-map
   pair: variable; visual-replace-secondary-mode-map

:code:`visual-replace-mode-map` is the map that is active in the
minibuffer in Visual Replace mode. You can add your own keybindings to
it.

:code:`visual-replace-secondary-mode-map` is the map that defines
keyboard shortcuts for modifying the search mode, such as :kbd:`r` to
toggle regexp mode on or off. It is bound by default in
:code:`visual-replace-mode-map` to the shortcut that was used to
launch Visual Replace, but you can bind it to whatever you want, or
define custom shortcuts directly in :code:`visual-replace-mode-map`.

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

:code:`visual-replace-minibuffer-mode-hook` is a normal hook that is
run when entering the visual replace mode, so you can set things up
just before Visual Replace starts.

:code:`visual-replace-defaults-hook` is a normal hook that is run when
entering the visual replace mode with no initial match or replacement,
so you can provide some default mode without interfering with
:code:`visual-replace-from-isearch` or
:code:`visual-replace-thing-at-point`.

Functions in :code:`visual-replace-functions` are called just before
executing the replacement or just before building the previews. They
are passed a struct of type :code:`visual-replace-args`, which they
can modify. You can use it to customize the behavior of the search or
modify the regexp language.

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
  executed. If that's not the case, please :ref:`report an issue
  <reporting>`.

* If you use :code:`visual-replace-apply-one` to replace single
  matches, :code:`\\#` in the replacement is always 1, because single
  matches are applied separately.

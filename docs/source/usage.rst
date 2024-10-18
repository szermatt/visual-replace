Usage
=====

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
           ("M-%" . visual-replace-from-isearch)))

The above example also binds :kbd:`M-%` in isearch, so you can just
switch from isearch to Visual Replace.

An alternative, which you might prefer to try things out, is to
replace :code:`query-replace` and others with Visual Replace. This
then uses whatever shortcut you've already installed.

.. code-block:: elisp

  (use-package visual-replace
    :defer nil
    :config
    (visual-replace-global-mode 1))

Once this is done, launch :code:`visual-replace` with the keybinding you chose.

You'll see, something like the following in the minibuffer `Replace
from point [...]: ┃ →`. The text before the arrow is the text to
replace and the text after the arrow is the replacement. You can
navigate back and forth with :kbd:`TAB` or by moving the cursor.

See also the example below.

  .. image:: ../../images/capture_blue.png
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

While `visual-replace` is active, it scrolls the window to keep at
least one example of matches visible. You can also press up and down
to go through the matches. Don't worry, though: the cursor goes back
to the original position once you leave Visual Replace.

In Visual Replace mode:

* :kbd:`TAB` navigates between the text to replace and the
  replacement string

* :kbd:`RET` switches to the replacement string, the first time, then
  executes the replacement

* :kbd:`M-% r` toggles regexp mode on and off. You know this mode is
  on when a :code:`.*` follows the arrow.

* :kbd:`M-% q` toggles query mode one and off, that is, it toggles
  between calling :code:`replace-string` and :code:`query-replace`.
  You know this mode is on when a :code:`?` follows the arrow.

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
  This is an alternative to using the query mode for replacing only
  some matches.

* :kbd:`M-% u` reverts the last replacement that was applied.

* As usual, :kbd:`C-p` and `C-n` go up and down the history, like on any prompt.

(Reminder: replace *M-%* with the keyboard shortcut you chose.)

If you leave :code:`visual-replace` without confirming, with :code:`C-g`, you can
continue where you left off next time by going up in the history.

See `Search
<https://www.gnu.org/software/emacs/manual/html_node/emacs/Search.html>`_
in the Emacs manual for details of the different modes listed above.

Commands
--------

.. index::
   pair: command; visual-replace
   pair: command; visual-replace-thing-at-point
   pair: command; visual-replace-selected
   pair: command; visual-replace-from-isearch

* `visual-replace` is the main command that starts Visual Replace and
  then executes the search-and-replace. It can replace `replace-string`,
  `query-replace`, `replace-regexp` and `query-replace-regexp`.

* `visual-replace-thing-at-point` starts a visual replace session with
  the symbol at point as text to replace.

* `visual-replace-selected` starts with the text within the current
  active region as text to replace.

* `visual-replace-from-isearch` switches from an active isearch
  session to `visual-replace`, keeping the current search text and
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
   pair: command; visual-replace-undo

The following commands are meant to be called while in Visual Replace
mode, from :code:`visual-mode-map`. By default, they're bound in
:code:`visual-replace-secondary-mode-map`:

* `visual-replace-toggle-regexp` toggles regexp mode on and off.
* `visual-replace-toggle-scope` changes the scope of the search.
* `visual-replace-toggle-query` toggles the query mode on and off.
* `visual-replace-toggle-word` toggles the word mode on and off.
* `visual-replace-toggle-case-fold` toggles the case fold mode on and off.
* `visual-replace-toggle-lax-ws` toggles the lax whitespace mode on and off.
* `visual-replace-next-match` moves cursor to the next match
* `visual-replace-prev-match` moves cursor to the previous match
* `visual-replace-apply-one` applies a single replacement, to the
  match at or after the cursor, then moves on to the next match.

  This command, used together with `visual-replace-next-match` and
  `visual-replace-prev-match` is in many cases functionally equivalent
  to using the query mode, but with a different interface that the
  possibility of changing the query as you go.

  One difference is that if you use :code:`\\#` in the replacement
  string, its value is always just 1. This is because each such
  replacement is executed separately.
* `visual-replace-undo` reverts one call to
  `visual-replace-apply-one`. This just executes `undo` in the
  original buffer.

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
   pair: hook; visual-replace-mode-hook
   pair: hook; visual-replace-functions

`visual-replace-mode-hook` is a normal hook that is run when entering
the visual replace mode, so you can set things up just before Visual
Replace starts.

Functions in `visual-replace-functions` are called just before
executing the replacement or just before building the previews. They
are passed a struct of type :code:`visual-replace-args`, which they
can modify. You can use it to customize the behavior of the search or
modify the regexp language.

Limitations
-----------

Visual Replace avoids executing replacement in the whole buffer during
preview; it just executes them in the parts of the buffer that are
currently visible. This means that the preview can show incorrect
replacement in some cases, such as when replacement uses `\\#`
directly or within a `\\,` In such cases, the preview can be wrong but
execution will be correct.

Replacements that call stateful functions in `\\,` such as a function
that increment an internal counter, will be executed too many times
during preview, with unpredictable results.

In all other cases, the preview should match what is eventually
executed. If that's not the case, please :ref:`report an issue
<reporting>`.

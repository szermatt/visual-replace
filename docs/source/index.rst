Visual Replace
==============

Visual Replace provides a nicer interface to `Query-Replace <https://www.gnu.org/software/emacs/manual/html_node/emacs/Query-Replace.html>`_
than the built-in one.

The main improvements are:

 * The prompt of Visual Replace includes both the text to be
   replaced and the replacement. This makes it easier to craft
   possibly complex regular expression search and replace.

 * You can see the matches and how they're going to be modified
   as you edit the command arguments.

 * To help craft the search string and its replacement, you
   can navigate between matches with the arrow keys and
   optionally see the number of matches in the prompt.

 * You can apply only some replacements, selectively,
   using keyboard shortcuts, or by clicking on the preview.

 * You can modify the scope and type of the search-and-replace
   command, to the full buffer, the region or everything after
   the point.

Visual Replace is just an interface. The actual replacements are
always done by the standard Emacs commands.

Comparison with other packages
------------------------------

What seems to be unique about Visual Replace is the ability to edit
both the thing to be replaced and the replacement at the same time.
The other packages I've found make it a 2-step process.

`visual-regexp <https://github.com/benma/visual-regexp.el>`_ also
supports a preview, but uses its own search-and-replace mechanism and
only supports a regexp mode.

`anzu <https://github.com/emacsorphanage/anzu>`_ is otherwise very
similar to visual-replace. It was abandoned for a long time, but has
been recently taken up by a new maintainer.

Contents
--------

.. toctree::

   install
   usage
   contrib

API
===

.. index::
   pair: function; visual-replace-read
   pair: function; visual-replace-make-args
   pair: function; visual-replace-make-scope

This package provides several different ways of triggering Visual
Replace, from different initial states. For example,
``visual-replace`` uses the region to restrict the search, while
``visual-replace-selection`` uses the region text as match string.
Another example is ``visual-replace-from-isearch``, which copies the
state of a running search.

.. sidebar:: Public Functions

  Public functions are functions without any double dash in their
  name. New version of Visual Replace attempt to keep any code calling
  public functions backward-compatible, while private function can
  change, be renamed, behave differently or disappear from version to
  version.

Many more variations are possible. You could imagine a variant of
Visual Replace that uses the current region as match string unless it
contains more tan one line, in which case it'd use it to restrict
search.

In an attempt to support such variations, Visual Replace includes some
public functions you're encouraged to use to build your own variation.

To add a variation as the one described above, you could write the
following, which modifies the default behavior in one specific case by
overriding the search arguments and scope :

.. code-block:: elisp

  (defun my-visual-replace ()
    (interactive)
    (let (args scope)
      (when (and (region-active-p)
                 (let ((beg (region-beginning))
                       (end (region-end)))
                   (save-excursion
                     (goto-char beg)
                     (not (search-forward "\n" end 'noerror)))))
        (setq args (visual-replace-make-args
                    :from (buffer-substring-no-properties
                           (region-beginning) (region-end))
                    :to ""))
        (setq scope (visual-replace-make-scope
                    ;; Don't use bounds from the region
                    :bounds nil)))
      (apply #'visual-replace
             (visual-replace-read args scope))))


(visual-replace-read ARGS SCOPE RUN-HOOK) : function
    Build arguments for running `visual-replace` and return
    them as a list, containing search range and modified ``visual-replace-args``.

    To call ``visual-replace`` directly after ``visual-replace-read``, use ``apply``:
    ``(apply #'visual-replace (visual-replace-args args scope))``

    This function takes two optional arguments, ARGS, created with
    ``visual-replace-make-args``, and SCOPE, created with
    ``visual-replace-make-scope``. If an argument is unspecified or
    nil, the default behavior applies.

    If both ARGS and SCOPE are nil, ``visual-replace-read`` calls
    ``visual-replace-defaults-hook`` allow configuring the search
    using hooks unless RUN-HOOK is non-nil.


(visual-replace-make-args KEY-ARGS) : function
    This function builds a struct of type `visual-replace-args`. It can take
    the following key arguments:

    - ``:from`` to specify the search text. It must be a regular
      expression in regexp mode.

    - ``:to`` to specify the replacement text.

      This is often set to the empty string when ``:from`` is specified.

      When ``:to`` is nil, which is the default, only the search text
      appears in the minibuffer and it it selected. When this is
      non-nil, even if it is an empty string, the search text is
      followed by a separator arrow, and the replacement text is
      selected.

    - ``:regexp`` if non-nil, run a regexp search. ``:from`` is a
      regular expression and ``:to`` a replacement string, which might
      include back-references surch as `\\&`, `\\N` or `\\,`.

    - ``:query`` if non-nil, query replacements like
      ``query-replace`` does.

    - ``:word`` if non-nil and ``:regexp`` is nil, ``:from`` is
      searched as a word.

    - ``:case-fold`` if non-nil, search is non case-sensitive and
      replacement are case-aware. Defaults to ``case-fold-search``.

    - ``:lax-ws-non-regexp`` if non-nil, whitespaces in regexp
      searches skip text. Ignored in non-regexp searches.
      Defaults to ``replace-lax-whitespace``.

    - ``:lax-ws-regexp`` if non-nil, whitespaces in non-regexp
      searches skip text. Ignored in regexp searches.


(visual-replace-make-scope KEY-ARGS) : function
    This function configures the available scopes and sets the current
    search and replace scope. It can take the following key arguments:

    - ``:type`` defines the scope ``visual-replace-read`` starts in.
      Setting this overrides the default, customizable by
      `visual-replace-default-to-full-scope`. Leave it to nil to keep
      the default behavior.

    - ``:point`` define the starting point for the search and replace
      in "from point" mode. Defaults to ``(point)``.

    - ``:bounds`` defines the search ranges. This is in the same
      format as the one returned by ``region-bounds``: a list of cons
      cells of the form (START . END). Empty by default.

    - ``:rectangle`` set it to non-nil if ``:bounds`` contain multiple
      cons cells that draw a rectangle. This controls how
      non-contiguous region is drawn. Nil by default.


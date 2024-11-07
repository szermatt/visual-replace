Contributing
============

.. _reporting:

Reporting issues
----------------

At this time, the most useful thing you can do to help is and useful
bug reports to the `Issue Tracker`_

In your report, please discuss what you wanted to happen as well as
what happened. Also, please include enough information to reproduce
the issue.

Please include:

- the version of Emacs you're running, taken, for example, from :kbd:`M-x about-emacs`

- whether you're running Emacs in a window environment or a terminal

- the OS you're running

- the replacement mode - a copy of what's shown in the minibuffer.

- the text you wanted to replace, the replacement text, the
  replacement modes - copying the content of minibuffer will do the
  trick.

.. tip::

  It's a great idea to take a screenshot of the Emacs window in Visual
  Replace mode just before executing it, and then another one after
  executing it and attach that to the issue.

.. _Issue tracker: https://github.com/szermatt/visual-replace/issues

Suggesting features
-------------------

Please add feature suggestions to the `Issue Tracker`_.

Asking questions
----------------

Open an issue on the `Issue Tracker`_ with your question.

Code contributions
------------------

To contribute code to the project, open a `Pull Request`_.

Before you do that, please make sure the any new features is covered
by tests and that the tests pass.

To run the tests, install and setup `eldev`_ then run :command:`eldev
test`.

Tests can also be run from inside of Emacs, using `M-x
ert-run-tests-interactively` but when you do so, be aware that there
might be unexpected interaction with your Emacs configurations. The
tests passing reliably when run using :command:`eldev test` is what
matters.

Please also make sure your commit message follows `Conventional
Commits 1.0.0 <https://www.conventionalcommits.org/en/v1.0.0/>`_.

.. _eldev: https://github.com/emacs-eldev/eldev

Documentation contributions
---------------------------

You don't need to be a developer to contribute! Contribution to the
documentation or code comments are very welcome. Please open a `Pull
Request`_ with your proposed modifications.

The documentation is written in reStructuredText. You'll need to
install `Sphinx <https://www.sphinx-doc.org>`_ to build it:

.. code-block:: bash

   python3 -m venv venv
   . venv/bin/activate # or activate.fish on fish
   pip3 install -r docs/requirements.txt

Then run :command:`eldev html` to build the documentation.

.. _Pull Request: https://github.com/szermatt/emacs-bash-completion/pulls

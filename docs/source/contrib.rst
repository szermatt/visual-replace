Contributing
============

.. _reporting:

Reporting issues
----------------

The most useful thing you can do to help is and useful bug reports to
the `Issue Tracker`_

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

.. _discussion: https://github.com/szermatt/visual-replace/discussions

Suggesting features
-------------------

Please create a new `discussion`_ or in the Ideas category or add a
feature suggestions to the `Issue Tracker`_.

Asking questions
----------------

Please create a new `discussion`_ in the General category with your
question.

Code contributions
------------------

To contribute code to the project, open a `Pull Request`_.

Before you do that, please make sure the any new features is covered
by tests and that the tests pass.

To run the tests, install and setup `eldev`_, then run :command:`eldev
test`.

Tests can also be run from inside of Emacs, using `M-x
ert-run-tests-interactively` but when you do so, be aware that there
might be unexpected interaction with your Emacs configurations. The
tests passing reliably when run using :command:`eldev test` is what
matters.

Please also make sure your commit message follows `Conventional
Commits 1.0.0 <https://www.conventionalcommits.org/en/v1.0.0/>`_, in
short, the commit message of new features should start with "feat: ",
fixes with "fix: ", refactorings with "refactor: " and tests with
"test: ".

.. _eldev: https://github.com/emacs-eldev/eldev

Documentation contributions
---------------------------

You don't need to be a developer to contribute! Contribution to the
documentation or code comments are very welcome. Please open a `Pull
Request`_ with your proposed modifications. To follow `Conventional
Commits 1.0.0 <https://www.conventionalcommits.org/en/v1.0.0/>`_, the
commit message should start with "docs: "

The documentation is written in reStructuredText. You'll need to
install `Sphinx <https://www.sphinx-doc.org>`_ to build it:

.. code-block:: bash

   python3 -m venv venv
   . venv/bin/activate # or activate.fish on fish
   pip3 install -r docs/requirements.txt

Then run :command:`eldev html` to build the documentation.

.. _Pull Request: https://github.com/szermatt/visual-replace/pulls

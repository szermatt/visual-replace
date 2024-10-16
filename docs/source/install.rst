Installation
============

Visual Replace requires Emacs 26.1 or later.

To install Visual Replace, you can:

* On a recent version of Emacs (29 or later), install from the
  repository by doing :kbd:`M-x package-vc-install
  https://github.com/szermatt/visual-replace`

* Use an alternative package managers that support installing
  from source, such as `straight
  <https://github.com/radian-software/straight.el>`_, shown here:

  .. code-block:: elisp

    (use-package visual-replace
      :straight (:type git :repo "https://github.com/szermatt/visual-replace.git"))

* Install it the old-fashioned way, and copy `visual-replace.el
  <https://raw.githubusercontent.com/szermatt/visual-replace/refs/heads/master/visual-replace.el>`_
  into your :file:`.emacs.d` directory, but you'll have to keep it
  up-to-date manually as well

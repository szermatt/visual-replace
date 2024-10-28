Installation
============

Install Visual Replace:

- from `MELPA or MELPA Stable <https://melpa.org/#/getting-started>`_ using :kbd:`M-x package-install visual-replace`

- on a recent version of Emacs (29 or later), from the
  github repository by doing :kbd:`M-x package-vc-install
  https://github.com/szermatt/visual-replace`

- from source, using an alternative package managers, such as `straight
  <https://github.com/radian-software/straight.el>`_, shown here:

  .. code-block:: elisp

    (use-package visual-replace
      :straight (:type git :repo "https://github.com/szermatt/visual-replace.git"))

- the old-fashioned way, copying `visual-replace.el
  <https://raw.githubusercontent.com/szermatt/visual-replace/refs/heads/master/visual-replace.el>`_
  into your :file:`.emacs.d` directory.

Visual Replace requires Emacs 26.1 or later.

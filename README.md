# visual-replace  [![test](https://github.com/szermatt/visual-replace/workflows/test/badge.svg)](https://github.com/szermatt/visual-replace/actions)

`visual-replace` provides a nicer interface
[`query-replace`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Query-Replace.html)
and `string-replace` on Emacs. `visual-replace` lets see what is going
to be modified as you edit the query.

The prompt of `visual-replace` includes both the text to be replaced
and the replacement, make it easier to craft possibly complex regular
expression search and replace.

The prompt also displays the mode of replacement:

* *text* → *replacement* executes `string-replace`
* *text* →? *replacement* executes `query-replace`
* *text* →.* *replacement* executes `replace-regexp`
* *text* →?.* *replacement* executes `query-replace-regexp`

![example](images/capture_blue.png)

More flags can be toggled, to control:

* the scope of the replacement, region or whole buffer
* whether search and replace applies on whole words only
* with or without `case-fold-search`
* with or without `replace-lex-whitespace`

If you leave `visual-replace` without confirming, with C-g, you can
continue where you left off by going up in the history.

## Installation

`visual-replace` requires Emacs 26.1.

To replace `query-replace' with `visual-replace' globally, do:

```elisp
(require 'visual-replace)
(visual-replace-global-mode 1)
```

With [use-package](https://github.com/jwiegley/use-package) and with a custom key binding:

```elisp
(use-package visual-replace
   :defer t
   :bind (("C-c r" . visual-replace)
          :map isearch-mode-map
          ("C-c r" . visual-replace-from-isearch)))
```

## Usage

Launch `visual-replace' with the keybinding you chose when installing, "M-%" by default. 

In that mode:

* "TAB" navigates between the text to replace and the replacement string
* "RET" switches to the replacement string, the first time, then executes the replacement
* "M-% r" toggles regexp mode on and off
* "M-% l" toggles query mode one and off 
* "M-% SPC" toggles query mode one and off 
* "M-% w" toggle limiting search to whole words
* "M-% c" toggle case-fold
* "M-% s" toggle lax whitespace 

If you started `visual-replace` with another keybinding, replace M-X with that keybinding. Modify `visual-replace-mode-map` and `visual-replace-secondary-mode-map` to customize the keybindings.

`visual-replace-from-isearch` switches from an active isearch session
to `visual-replace`.

`visual-replace-thing-at-point` starts a visual replace session with
the symbol at point, or a specified thing as understood by
`thing-at-point`.

## Testing

Install [Cask](https://github.com/cask/cask) and run tests with:

```sh
cask exec ert-runner
```

## License

This project is licensed under the GPLv2 - see the [license](license) file for details

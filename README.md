# harpoon.el

Harpoon (<https://github.com/ThePrimeagen/harpoon>) rudimentary replica for Emacs.

## Introduction

**harpoon.el** is a quick project-based bookmarking system which simplifies navigating between commonly used files in a project. It leverages the built-in `project.el` and `bookmark.el` libraries.

**Please note that this is under development and may not yet be thoroughly tested.**

## Usage Instructions

It's pretty simple:

1. Enable `harpoon-minor-mode`.
2. To set a bookmark **from inside a known project**, invoke `harpoon-set`.
3. Upon setting a bookmark, the `*harpoon*` buffer will appear. You can also invoke it manually with the `harpoon-buffer` command.
4. You can now adjust the order of your harpoon bookmarks and navigate as you please. If you manually edit the list, remember to press `C-c C-c` to update the harpoon list and close the buffer.
5. You can switch to the *n*-th bookmark by employing the `(harpoon-jump n)` command.

For instance, you can set keys combinations, e.g. `C-c h`, `C-c j`, `C-c k`, and `C-c l` (i.e. home row keys) to quickly jump to harpoons 1 through 4, respectively.

You can use harpoon.el to swiftly switch between frequently accessed files in your project, increasing productivity and reducing navigation time.

You can read more about the concept of harpoon on the GitHub of the original implementation <https://github.com/ThePrimeagen/harpoon>.

## Example configuration

```emacs-lisp
(use-package harpoon
  :load-path "/path/to/harpoon.el/"
  :bind (:map harpoon-minor-mode-map
	      ("C-c h m" . 'harpoon-set)
	      ("C-c h l" . 'harpoon-buffer)
	      ("C-c h h" . (lambda () (interactive) (harpoon-jump 0)))
	      ("C-c h j" . (lambda () (interactive) (harpoon-jump 1)))
	      ("C-c h k" . (lambda () (interactive) (harpoon-jump 2)))
	      ("C-c h l" . (lambda () (interactive) (harpoon-jump 3))))
  :config
  (harpoon-minor-mode 1))
```

## Alternatives

Users seeking the full Harpoon experience should probably check out the other Emacs implementation <https://github.com/otavioschwanck/harpoon.el>, which seems to be fully featured. This implementation is very simple and rudimentary. However, if you just need the basic functionality and usually rely on `project.el`, you can give it a try.

# harpoon.el

Harpoon (<https://github.com/ThePrimeagen/harpoon>) rudimentary replica for Emacs.

## Introduction

**harpoon.el** is a quick project-based bookmarking system which simplifies navigating between commonly used files in a project. It leverages the built-in `project.el` and `bookmark.el` libraries.

**Please note that this is under development and may not yet be thoroughly tested.**

## Usage Instructions

It's pretty simple:

1. Enable `harpoon-minor-mode`.
2. To set a bookmark, **from inside a known project**, invoke `harpoon-set`.
3. Upon setting a bookmark, the `*harpoon*` buffer will appear. You can also invoke it manually with the `harpoon-buffer` command.
4. You can now adjust the order of your harpoon bookmarks, delete any, or navigate as you please (press `<return>` while the point is on an entry). If you manually edit the list, remember to press `C-c C-c` to update the harpoon list and close the buffer (you could also manually kill the buffer).
5. You can switch to the *n*-th bookmark by employing the `harpoon-jump-n` commands, where *n* is the bookmark position in the harpoon buffer.

For instance, you can set keys combinations, e.g. `C-c h`, `C-c j`, `C-c k`, and `C-c l` (i.e. home row keys) to quickly jump to harpoons 1 through 4, respectively (see Example configuration below).

You can use harpoon.el to swiftly switch between frequently accessed files in your project, increasing productivity and reducing navigation time.

You can read more about the concept of harpoon on the GitHub of the original implementation <https://github.com/ThePrimeagen/harpoon>.

## Example configuration

```emacs-lisp
(use-package harpoon
  :load-path "/path/to/harpoon.el/"
  ;; Example bindings
  :bind (:map harpoon-minor-mode-map
	      ("C-c h m" . 'harpoon-set)
	      ("C-c h b" . 'harpoon-buffer)
	      ("C-c h h" . 'harpoon-jump-1)
	      ("C-c h j" . 'harpoon-jump-2)
	      ("C-c h k" . 'harpoon-jump-3)
	      ("C-c h l" . 'harpoon-jump-4))
  :config
  ;; Example *harpoon* buffer config
  (add-to-list
   'display-buffer-alist
   '("\\*harpoon\\*"
     (display-buffer-in-side-window)
     (side . bottom)
     (slot . -1)
     (window-height . 0.33)
     (window-parameters
      (no-delete-other-windows . nil))))
  ;; Activate the minor mode
  (harpoon-minor-mode 1))
```

### Using `elpaca`

<https://github.com/progfolio/elpaca>

```emacs-lisp
(use-package harpoon
  :elpaca (harpoon :host github :repo "kofm/harpoon.el")
  ;; Your config here.
  )
```

## Alternatives

Users seeking the full Harpoon experience should probably check out the other Emacs implementation <https://github.com/otavioschwanck/harpoon.el>, which seems to be fully featured. This implementation is very simple and rudimentary. However, if you just need the basic functionality and usually rely on `project.el`, you can give it a try.

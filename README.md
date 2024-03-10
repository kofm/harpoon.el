# harpoon.el

[Harpoon](https://github.com/ThePrimeagen/harpoon "Harpoon") rudimentary replica for Emacs.

## Introduction

**harpoon.el** is a quick project-based bookmarking system which
simplifies quick navigation between commonly used files in a
project. It leverages the built-in `project.el` and `bookmark.el`
libraries. An harpoon is simply an Emacs bookmark; therefore, you can
set bookmarks for every buffer which has bookmark support (for
example, shell buffers). Defining bookmark support for unsupported
buffers should also be very simple, if needed.

## Usage

It's pretty simple:

1. Enable `harpoon-minor-mode`.
2. To set a bookmark, **from inside a known project**, invoke
   `harpoon-set`.
3. Upon setting a bookmark, the `*harpoon*` buffer will appear. You
   can also invoke it manually with the `harpoon-buffer` command.
4. You can now adjust the order of your harpoon bookmarks, delete any,
   or navigate as you please (press `<return>` while the point is on
   an entry). If you manually edit the list, remember to press `C-c
   C-c` to update the harpoon list and close the buffer (you can also
   manually kill the buffer).
5. You can switch to the *n*-th bookmark by employing the
   `harpoon-jump-n` commands, where *n* is the bookmark position in
   the harpoon buffer.

For instance, you can set keys combinations, e.g. `C-c h`, `C-c j`,
`C-c k`, and `C-c l` (i.e. right-handed home row keys) to quickly jump
to harpoons 1 through 4 (see the example configuration below). You can
also move forward and backward through your harpoon list using
commands `harpoon-next` and `harpoon-prev`, respectively.

You can use `harpoon.el` to swiftly switch between frequently accessed
files (but not only files) in your project, increasing productivity
and reducing navigation time. You can read more about the concept of
harpoon on the GitHub repository of the original implementation
(<https://github.com/ThePrimeagen/harpoon>).

### Persisting harpoons across sessions

Your project-specific harpoons are saved in `harpoon-list-file`
(user-configurable, defaulting to a file named `harpoons` in your
`user-emacs-directory`). Following the design of `bookmark.el`, your
harpoons are saved to disk only before closing the Emacs session. If
you want to force a save at any moment you can use `harpoon-file-save`
(bound by default to `C-c C-s` while in the `harpoon` buffer).

### Bookmarking files that are outside the current project

You can achieve this by leveraging `project.el` builtin functionality:
by defining `project-current-directory-override` as directory local
variable you can tell Emacs to treat the current directory (and its
sub-directories) as if it is part of another project. To do this, you
can use interactive command `add-dir-local-variable`, like this:
<kbd>M-x</kbd> `add-dir-local-variable` <kbd>RET</kbd> `nil` (apply to
any mode) <kbd>RET</kbd> `project-current-directory-override`
<kbd>RET</kbd> `/path/to/other/project` <kbd>RET</kbd>. Alternatively,
you can manually edit the `.dir-locals` file of interest:

```elisp
;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((project-current-directory-override . "/path/to/other/project"))))
```

## Example configuration

The following is an example configuration that uses `C-c h` as prefix
and then right hand home row keys to jump to harpoons 1 to 4.

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

If you don't want to manually clone the repository or handle the files
you can use a package manager like [elpaca](https://github.com/progfolio/elpaca).

```emacs-lisp
(use-package harpoon
  :elpaca (harpoon :host github :repo "kofm/harpoon.el")
  ;; Your config here.
  )
```

## Alternative implementation

Users seeking the full Harpoon experience should probably check out
the other Emacs implementation
<https://github.com/otavioschwanck/harpoon.el>, which seems to be
fully featured. This implementation is very simple and
rudimentary. However, if you just need the basic functionality and
usually rely on `project.el`, you can give it a try.

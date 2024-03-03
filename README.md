# harpoon.el

Harpoon (<https://github.com/ThePrimeagen/harpoon>) replica for Emacs.

## Introduction

**harpoon.el** is a quick project-based bookmarking system which simplifies navigating between commonly used files in a project.

**Please note that this is under development and may not yet be thoroughly tested.**

## Usage Instructions

It's pretty simple:

1. Enable `harpoon-minor-mode`.
2. To set a bookmark within a `project`, invoke `harpoon-set`.
3. Upon setting a bookmark, the `*harpoon*` buffer will appear. You can also invoke it manually with the `harpoon-buffer` command.
4. You can now adjust the order of your harpoon bookmarks and navigate as you please. If you manually edit the list, remember to press `C-c C-c` to update the harpoon list and close the buffer.
5. You can switch to the *n*-th bookmark by employing the `(harpoon-jump n)` command.

For instance, you can set keys combinations, e.g. `C-c h`, `C-c j`, `C-c k`, and `C-c l` (i.e. home row keys) to quickly jump to harpoons 1 through 4, respectively.

You can use harpoon.el to swiftly switch between frequently accessed files in your project, increasing productivity and reducing navigation time.

You can read more about the concept of harpoon on the GitHub of the original implementation <https://github.com/ThePrimeagen/harpoon>.

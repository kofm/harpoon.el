# harpoon.el

Harpoon (<https://github.com/ThePrimeagen/harpoon>) replica for Emacs.

## Introduction

**harpoon.el** is a quick project-based bookmarking system which simplifies navigating between commonly used files in a project. Please note that this is currently under development and may not yet be thoroughly tested.

## Usage Instructions

1. To set a bookmark within a `project`, invoke `harpoon-set`.
2. Upon setting a bookmark, the `*harpoon*` buffer will appear. You can also invoke it manually with the `harpoon-buffer` command.
3. You can now adjust the order of your harpoon bookmarks and navigate as you please. If you manually edit the list, remember to press `C-c C-c` to update the harpoon list and close the buffer.
4. You can switch to the *n*-th bookmark by employing the `(harpoon-jump n)` command.

For instance, you can set keys combinations, e.g. `SPC z h`, `SPC z j`, `SPC z k`, and `SPC z l` (corresponding to home row keys) to quickly jump to harpoons 1 through 4, respectively.

Use Harpoon for Emacs to swiftly switch between frequently accessed files in your project, increasing productivity and reducing navigation time.

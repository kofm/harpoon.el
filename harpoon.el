;;; harpoon.el --- Harpoon rudimentary replica for Emacs.  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Gabriele Mongiano

;; Author: Gabriele Mongiano <g.mongiano@me.com>
;; Created: March 2024
;; Keywords: project navigation bookmark
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'bookmark)
(require 'project)
(require 'files)
(require 'font-core)

;;;; Customization

(defgroup harpoon nil
  "Harpoon project-based quick bookmarking."
  :version "29.1"
  :group 'tools)

(defcustom harpoon-buffer "*harpoon*"
  "The name of the harpoon buffer."
  :type 'string
  :version "29.1"
  :group 'harpoon)

(defcustom harpoon-list-file (locate-user-emacs-file "harpoons")
  "The file where the harpoons for all the projects are saved."
  :type 'file
  :version "29.1"
  :group 'harpoon
  )

;;;; Variables

(defvar harpoon-alist ()
  "Harpoon list working memory.")

(defvar harpoon-global-list (make-hash-table :test 'equal)
  "Global hash table mapping harpoons for different projects.")

;;;; Functions

(defun harpoon--update-global-list ()
  "Update the harpoon global list with the current project's harpoons."
  (when-let* ((proj (project-current))
	      (root (project-root proj)))
    (puthash root harpoon-alist harpoon-global-list)))

(defun harpoon--retrieve ()
  "Update the project's harpoons from the harpoon global list."
  (when-let* ((proj (project-current))
	      (root (project-root proj)))
    (let ((harpoons (gethash root harpoon-global-list)))
      (setq harpoon-alist harpoons))))

(defun harpoon--buffer-name ()
  "Return a string to be used as bookmark name.

If the buffer is associated with a file, it will return the file
path relative to `project-root'.  For all the other supported
bookmark types (e.g. eshell) it will use the
`bookmark-buffer-name' command."
  (when-let*
      ((proj (project-current))
       (root (project-root proj)))
    (if (buffer-file-name)
	(file-relative-name (buffer-file-name) root)
      (bookmark-buffer-name))))

(defun harpoon--position (harpoon)
  "Return the 0-indexed current position of the harpoon named HARPOON.

It returns nil if no one is matching."
  (cl-position harpoon harpoon-alist
	       :test (lambda (key element)
		       (string= key (car element)))))

(defun harpoon--alist-get-by-key (key)
  "Get an entry from `harpoon-alist' by KEY."
  (assoc key harpoon-alist))

(defun harpoon--buffer-get-lines ()
  "Return a list of lines from the Harpoon buffer.

If there is no Harpoon buffer return nil."
  (when-let* ((buffer (get-buffer harpoon-buffer)))
    (with-current-buffer buffer
      (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n" t))))

(defun harpoon--process-buffer ()
  "Process the harpoon buffer to reflect deletions or changed harpoons order."
  (let* ((lines (harpoon--buffer-get-lines))
	 (alist (mapcar #'harpoon--alist-get-by-key lines)))
    (setq harpoon-alist (delq nil alist))
    (harpoon--update-global-list)))

(defun harpoon--insert-help (project-name-string)
  "Insert help string into the harpoon buffer for project PROJECT-NAME-STRING."
  (let ((msg (format
	      (string-join
	       '(""
		 "Harpoons for project: `%s'"
		 "You can reorder bookmarks however you like."
		 "Lines that do not represent an existing harpoon are ignored."
		 "When done you can kill the buffer with `C-c C-c'")
	       "\n# ")
	      project-name-string)))
    (insert msg)))

(defun harpoon--write-file (file data)
  "Write the DATA to FILE to persist it across Emacs sessions."
  (with-temp-file file
    (prin1 data (current-buffer))))

(defun harpoon--read-file (file)
  "Read data from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun harpoon--file-load ()
  "Update `harpoon-global-list' from the persistent file (`harpoon-list-file')."
  (when (file-exists-p harpoon-list-file)
    (let ((contents (harpoon--read-file harpoon-list-file)))
      (setq harpoon-global-list contents)
      (message "Harpoons loaded from file!"))))

;;;; Commands

(defun harpoon-set ()
  "Set an Harpoon for the current project.

If the file is already harpoon'd just update the existing harpoon
with the new location.  Otherwise, append the new harpoon to the
project's list of harpoons.  If the current buffer does not belong
to a project, fail silently."
  (interactive)
  (cl-assert harpoon-minor-mode nil "harpoon-minor-mode not enabled")
  (when-let ((harpoon-name (harpoon--buffer-name))
	     (harpoon (funcall bookmark-make-record-function)))
    (if-let ((existing (assoc harpoon-name harpoon-alist)))
        (setcdr existing harpoon)
	(add-to-list 'harpoon-alist (cons harpoon-name harpoon) t))
    (harpoon--update-global-list)
    (harpoon-buffer)))

(defun harpoon-step (&optional backward)
  "Move to the next harpoon, or to the previous harpoon if BACKWARD is not nil."
  (when-let* ((name (harpoon--buffer-name))
	      (index (harpoon--position name)))
    (harpoon-jump (if backward (1- index) (1+ index)))))

(defun harpoon-next ()
  "Move to the next harpoon in the list."
  (interactive)
  (harpoon-step))

(defun harpoon-prev ()
  "Move to the previous harpoon in the list."
  (interactive)
  (harpoon-step t))

(defun harpoon-jump (n)
  "Generic function to jump to the N-th harpoon in the harpoon list.
Mind that N is 0-indexed.

There are available aliases pre-defined like `harpoon-jump-1',
`harpoon-jump-2', and so on, which are also 1-indexed to reflect
the harpoon in the harpoon buffer."
  (interactive)
  (when-let ((bookmark (nth n harpoon-alist)))
    (bookmark-jump bookmark)))

;; Function factory to create harpoon-jump aliases. See `harpoon-jump' docstring.
(dotimes (n 8)
  (let ((func-name (intern (format "harpoon-jump-%d" (1+ n)))))
    (defalias func-name `(lambda () (interactive) (harpoon-jump ,n))
      (format "Jump to harpoon #%d." (1+ n)))))


(define-derived-mode harpoon-mode
  fundamental-mode "Harpoon"
  "Major mode for managing Harpoon bookmarks."

  (setq-local font-lock-defaults '((("^#.*" . 'font-lock-comment-face))))

  (setq display-line-numbers t)
  
  ;; Keymaps
  (let ((map harpoon-mode-map))
    (define-key map (kbd "g") 'harpoon-buffer)
    (define-key map (kbd "C-c C-c") 'harpoon-process-buffer-and-quit)
    (define-key map (kbd "C-c C-s") 'harpoon-file-save)))

;;;###autoload
(define-minor-mode harpoon-minor-mode
  "A minor mode for using harpoon with Emacs."
  :lighter " Hp"
  :keymap (let ((map (make-sparse-keymap)))
	    map)
  :global t
  (if harpoon-minor-mode
      (progn
	(harpoon--file-load)
	(add-hook 'buffer-list-update-hook #'harpoon--retrieve)
	(add-hook 'kill-emacs-hook #'harpoon-file-save))
    (progn
      (harpoon-file-save)
      (remove-hook 'buffer-list-update-hook #'harpoon--retrieve)
      (remove-hook 'kill-emacs-hook #'harpoon-file-save))))


(defun harpoon-buffer ()
  "Invoke the `harpoon' buffer.

From the `harpoon' buffer you can rearrange and/or delete
harpoons by swapping or deleting lines, respectively, using
standard Emacs text motions."
  (interactive)
  (cl-assert harpoon-minor-mode nil "harpoon-minor-mode not enabled")
  (if-let ((proj (project-current))
	   (buffer (get-buffer-create harpoon-buffer))
	   (files-list (mapcar #'car harpoon-alist)))
      (progn
	(with-current-buffer buffer
	  (erase-buffer)
	  (harpoon-mode)
	  (dolist (file files-list)
	    (insert (format "%s\n" file)))
	  (harpoon--insert-help (project-name proj))
	  (goto-char (point-min))
	  (unless (memq 'harpoon--process-buffer kill-buffer-hook)
	    (add-hook 'kill-buffer-hook 'harpoon--process-buffer nil t)))
	(pop-to-buffer buffer))
    (message "You either aren't in a project or there are no harpoons defined.")))

(defun harpoon-process-buffer-and-quit ()
  "Helper function to call `harpoon--process-buffer' and close the harpoon buffer."
  (interactive)
  (when-let ((buffer (get-buffer harpoon-buffer)))
    (harpoon--process-buffer)
    (kill-buffer buffer)))

(defun harpoon-file-save ()
  "Save the global harpoon hash table to `harpoon-list-file'.

If the hash table is empty don't do anything."
  (interactive)
  (unless (eq (hash-table-count harpoon-global-list) 0)
    (harpoon--write-file harpoon-list-file harpoon-global-list)))

(provide 'harpoon)
;;; harpoon.el ends here

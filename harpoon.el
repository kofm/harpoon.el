;;; harpoon.el --- Harpoon rudimentary replica for Emacs.  -*- lexical-binding: t -*-

;; Copyright (C) 2024 Gabriele Mongiano

;; Author: Gabriele Mongiano <g.mongiano@me.com>
;; Created: March 2024
;; Keywords: project navigation bookmark
;; Version: 0.1

;; This file is NOT part of GNU Emacs.


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
(defgroup harpoon nil
  "Harpoons quick bookmarking."
  :version "29.1"
  :group 'tools)

(defcustom harpoon-buffer "*harpoon*"
  "The name of the harpoon buffer"
  :type 'string
  :version "29.1"
  :group 'harpoon)

(defcustom harpoon-list-file (locate-user-emacs-file "harpoons")
  "The file where the harpoons for all the projects are saved"
  :type 'file
  :version "29.1"
  :group 'harpoon
  )

(defvar harpoon-global-list (make-hash-table :test 'equal)
  "Global hash table mapping harpoons for different projects.")

(defun harpoon--update-global-list ()
  "Update the harpoon global list with the current project's harpoons."
  (when-let* ((proj (project-current))
	      (root (project-root proj))
	      (harpoons harpoon-alist))
    (puthash root harpoons harpoon-global-list)))

(defun harpoon--retrieve ()
  "Update the project's harpoons from the harpoon global list"
  (when-let* ((proj (project-current))
	      (root (project-root proj)))
    (let ((harpoons (gethash root harpoon-global-list)))
      (setq harpoon-alist harpoons))))

(defvar harpoon-alist ()
  "Harpoon list working memory.")

(defun harpoon--get-bookmark-name ()
  (when-let*
      ((proj (project-current))
       (root (project-root proj))
       (file (buffer-file-name)))
    (file-relative-name file root)))

(define-derived-mode harpoon-mode
  fundamental-mode "Harpoon"
  "Major mode for managing Harpoon bookmarks."

  (setq font-lock-defaults '((("^#.*" . 'font-lock-comment-face))))

  (setq display-line-numbers t)
  
  ;; Keymaps
  (let ((map harpoon-mode-map))
    (define-key map (kbd "g") 'harpoon-buffer)
    (define-key map (kbd "C-c C-c") 'harpoon--process-buffer-and-quit)
    (define-key map (kbd "C-c C-s") 'harpoon-file-save)
    (define-key map (kbd "<return>") 'find-file-at-point)))

(defun harpoon-set ()
  "Set an Harpoon for the current project.

If the file is already harpoon'd just update the existing harpoon with the new location. Otherwise, append the new harpoon to the project's list of harpoons. If the current buffer does not belong to a project, fail silently."
  (interactive)
  (cl-assert harpoon-minor-mode nil "Harpoon minor mode should be enabled")
  (when-let ((harpoon-name (harpoon--get-bookmark-name))
	     (harpoon (funcall bookmark-make-record-function)))
    (if-let ((existing (assoc harpoon-name harpoon-alist)))
        (setcdr existing harpoon)
      (progn 
	(push (cons harpoon-name harpoon) harpoon-alist)
	(setq harpoon-alist (nreverse harpoon-alist))))
    (harpoon--update-global-list)
    (harpoon-buffer)))

(defun harpoon-jump (n)
  (interactive)
  (when-let ((bookmark (nth n harpoon-alist)))
    (bookmark-jump bookmark)))

(defun harpoon--alist-get-by-key (key)
  "Get an entry from harpoon-alist by association key."
  (assoc key harpoon-alist))

(defun harpoon--buffer-get-lines ()
  "Return a list of lines from the Harpoon buffer.

If there is no Harpoon buffer return nil."
  (when-let* ((buffer (get-buffer harpoon-buffer)))
    (with-current-buffer buffer
      (split-string (buffer-string) "\n" t))))

(defun harpoon--process-buffer ()
  (let* ((lines (harpoon--buffer-get-lines))
	 (alist (mapcar #'harpoon--alist-get-by-key lines)))
    (setq harpoon-alist (delq nil alist))
    (harpoon--update-global-list)))

(defun harpoon--process-buffer-and-quit ()
  (interactive)
  (when-let ((buffer (get-buffer harpoon-buffer)))
    (harpoon--process-buffer)
    (kill-buffer buffer)))

(defun harpoon--insert-help (project-name-string)
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

(defun harpoon-buffer ()
  (interactive)
  (cl-assert harpoon-minor-mode nil "Harpoon minor mode should be enabled")
  (let ((buffer (get-buffer-create harpoon-buffer))
	(files-list (mapcar #'car harpoon-alist)))
    (with-current-buffer buffer
      (erase-buffer)
      (harpoon-mode)
      (dolist (file files-list)
	(insert (format "%s\n" file)))
      (harpoon--insert-help (project-name (project-current)))
      (goto-char (point-min))
      (unless (memq 'harpoon--process-buffer kill-buffer-hook)
	(add-hook 'kill-buffer-hook 'harpoon--process-buffer nil t))
      (switch-to-buffer buffer))))

(defun harpoon--write-file (file data)
  (with-temp-file file
    (prin1 data (current-buffer))))

(defun harpoon--read-file (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read (current-buffer))))

(defun harpoon-file-save ()
  "Save the global harpoon hash table to `harpoon-list-file'.

If the hash table is empty don't do anything."
  (interactive)
  (unless (eq (hash-table-count harpoon-global-list) 0)
    (harpoon--write-file harpoon-list-file harpoon-global-list)))

(defun harpoon--file-load ()
  (when (file-exists-p harpoon-list-file)
    (let ((contents (harpoon--read-file harpoon-list-file)))
      (setq harpoon-global-list contents)
      (message "Harpoons loaded from file!"))))

;;;###autoload
(define-minor-mode harpoon-minor-mode
  "A minor mode for using harpoon with emacs."
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

(provide 'harpoon)
;;; harpoon.el ends here

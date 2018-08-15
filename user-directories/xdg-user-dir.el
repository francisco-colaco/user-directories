;;; xdg-user-dir.el --- Use xdg-user-dir to find user directories   -*- lexical-binding: t -*-

;; Copyright (C)2018 Free Software Foundation

;; Author: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Maintainer: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Version: 1
;; Created: 2018-07-20
;; Keywords: emacs
;; Homepage: https://github.com/francisco.colaco/emacs-directories
;; Package-Requires: (cl directories)

;; This file is not yet part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The binary xdg-user-dir is present in virtually all Linux and
;; FreeBSD distributions.
;;
;; Several types of directories are possible to be found, like:
;;
;;  - :desktop
;;  - :documents
;;  - :download
;;  - :pictures
;;  - :publicshare
;;  - :templates
;;  - :videos
;;
;; Suitable defaults are provided when the directory definition is not
;; found.

;;; Code:

(eval-when-compile
 (require 'cl))
(require 'directories)


(defconst xdg-user-dir-exists
  (not (null (locate-file "xdg-user-dir" exec-path)))
  "Tells if the command xdg-user-dir was found in the executable path.

Most recent Linux and FreeBSD distributions have xdg-user-dir.
Older than 2010 may have not.  This constant determines if the
command is safe to use: exists and is available at the executable
path.")


(defvar xdg-user-dir-directory-defaults
  '(:desktop "~/Desktop"
    :download "~/Downloads"
    :templates "~/Templates"
    :publicshare "~/Public"
    :documents "~/Documents"
    :pictures "~/Images"
    :videos "~/Videos")
  "A list of XDG directories that will be searched.

Each of the associations has a key and a default value, which the
user directory will take if the command xdg-user-dir does not
exist at the executable path.")


(defun xdg-user-dir (type)
  "Find a XDG user directory of TYPE.

Uses the binary 'xdg-user-dir' if available."
  (if xdg-user-dir-exists
      (let ((key (upcase (replace-regexp-in-string ":" "" (symbol-name type)))))
        (substring (shell-command-to-string (concat "xdg-user-dir " key)) 0 -1))))


(defun xdg-user-dir-assign-directories ()
  "Assign the XDG user directories.

Uses the tool ’xdg-user-dir’ if available."
  (let ((config-dir (or (getenv "XDG_CONFIG_HOME") (expand-file-name "~/.config/")))
        (data-dir (or (getenv "XDG_DATA_HOME") (expand-file-name "~/.local/share/")))
        (cache-dir (or (getenv "XDG_CACHE_HOME") (expand-file-name "~/.cache/")))
        (runtime-dir (getenv "XDG_RUNTIME_DIR")))
    ;; Add the directories to the user directories file, creating them if absent.
    (set-user-directory :config (expand-file-name "emacs/" config-dir) t)
    (set-user-directory :data (expand-file-name "emacs/" data-dir) t)
    (set-user-directory :cache (expand-file-name "emacs/" cache-dir) t)
    (set-user-directory :runtime (expand-file-name "emacs/" runtime-dir) t)

    ;; Set the user Lisp directories, adding them and their subdirs to `load-path'.
    ;; Create them if needed.
    (setup-user-lisp-directories))

  ;; Assign all the user visible directories.
  (cl-loop for (type default) on xdg-user-dir-directory-defaults by (function cddr) do
     (set-user-directory type (or (xdg-user-dir type) (expand-file-name default)))))


(provide 'xdg-user-dir)
;;; xdg-user-dir.el ends here

;;; user-directories-darwin.el --- Darwin specification for User Emacs directories   -*- lexical-binding: t -*-

;; Copyright (C)2018 Free Software Foundation

;; Author: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Maintainer: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Version: 1
;; Created: 2018-07-11
;; Keywords: emacs
;; Homepage: https://github.com/francisco.colaco/emacs-directories
;; Package-Requires: (cl)

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

;; The Darwin configuration of the user-directories package uses the
;; system default names.  MacOS changes the presentation of the file
;; to the user, not the name of the file in itself.
;;
;; There are directories for user :data, :config, :cache and :runtime
;; files.  Aditionally, the following directories are defined:
;;
;;  - :desktop
;;  - :documents
;;  - :download
;;  - :pictures
;;  - :publicshare
;;  - :videos
;;

;;; Code:

(eval-when-compile
 (require 'cl))
(require 'user-directories)


;;;; Darwin specific code.

(defvar darwin-xdg-folder-definitions
  '(:desktop "~/Desktop"
    :download "~/Downloads"
    :publicshare "~/Public"
    :documents "~/Documents"
    :pictures "~/Pictures"
    :videos "~/Movies")
  "A list of Darwin directory that will be searched.

Each of the associations has a key and a default value, which is
the canonical (untranslated) name of the folder.  The name that
is presented to the user in Finder, varying according to the
locale, is phony, being the untranslated name the real directory
name.")


(defun setup-user-directories-darwin ()
  "Set up the user directories on Darwin based systems."

  ;; Set the user folders.
  (let ((user-library-dir (expand-file-name "~/Library")))
    (set-user-directory :config (expand-file-name "emacs/config" user-library-dir) t)
    (set-user-directory :data (expand-file-name "emacs/data" user-library-dir) t)
    (set-user-directory :cache (expand-file-name "emacs/cache" user-library-dir) t)
    (set-user-directory :runtime (expand-file-name "emacs/runtime" user-library-dir) t))

  ;; Set the additional folders.
  (cl-loop for (type default) on darwin-xdg-folder-definitions by (function cddr) do
    (set-user-directory type (expand-file-name default)))

  ;; Set the user Lisp directories, adding them and their subdirs to `load-path'.
  ;; Create them if needed.
  (setup-user-lisp-directories))


(provide 'user-directories-darwin)
;;; user-directories-darwin.el ends here

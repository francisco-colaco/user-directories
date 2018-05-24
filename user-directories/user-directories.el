;;; user-directories.el --- User Emacs directories    -*- lexical-binding: t -*-

;; Copyright (C)2018 Free Software Foundation

;; Author: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Maintainer: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Version: 1
;; Created: 2018-05-05
;; Keywords: emacs
;; Homepage: https://github.com/francisco.colaco/emacs-directories
;; Package-Requires: (map seq)

;; This file is not yet part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The users of Emacs have traditionally been plagued with a myriad of
;; configuration files at the home directory, staring with ~/.emacs.
;; Recently, the use of ~/.emacs.d/ has essentially solved the former
;; problem, but another still persists: which files are important to
;; back up and which are disposable?
;;
;; Following the XDG Base Directory Specification
;; (https://wiki.archlinux.org/index.php/XDG_Base_Directory_support),
;; user-directories strives to allow a package writer or an Emacs user
;; to segregate the various Emacs configuration and runtime files into
;; appropriate directories.
;;
;; Thus, following the specification, a configuration file (normally a
;; file that is written by the user or in behalf of him) will reside
;; in the configuration directory (normally ~/.config/emacs).  It ca
;; be located with:
;;
;;   (locate-user-config-file "init.el")
;;   "/home/fhc/.config/emacs/init.el"
;;
;; Similarly, there are locator functions for third party files ---
;; `locate-user-data-file' --- and for cache files ---
;; `locate-user-cache-file'.  Run time files, locatable with
;; `locate-user-runtime-files' will be erased at the last logout,
;; according to the specification, and are thus appropriate for
;; security-sensitive data.
;;
;; There are also directories defined for :documents, :music, :videos,
;; :pictures and :downloads.  In Linux, also other that the command
;; xdg-user-dir can reference, like :templates or :publicshare.
;; Locator functions are concomitantly created.  For instance, in one
;; of my machines (Linux, portuguese):
;;
;;   (locate-user-documents-file "org/index.txt")
;;   "/home/fhc/Documentos/org/index.txt"
;;
;;   (locate-user-pictures-file "image.png")
;;   "/home/fhc/Imagens/image.png"
;;
;; Emacs Lisp package writers may use the former functions to
;; segregate files among different concerns.  For instance:
;;
;;   (setq abbrev-file-name (locate-user-config-file "abbrev_defs"))
;;   (setq projectile-known-projects-file (locate-user-data-file "projectile-bookmarks.eld"))
;;   (setq projectile-cache-file (locate-user-cache-file "projectile.cache"))
;;

;;; Code:

(require 'map)
(require 'seq)


(defvar user-directories (make-hash-table :test 'equal :size 15))


(defun locate-user-file (directory-type filename &optional only-if-exists)
  "Given the DIRECTORY-TYPE, locate FILENAME therein.

If a file with that name does not exist and ONLY-IF-EXISTS is non-nil,
then return nil."
  (let* ((dirname (map-elt user-directories directory-type))
	 (path (expand-file-name filename dirname)))
    (when (null dirname)
      ())
    (if (and only-if-exists
	     (not (file-exists-p path)))
	nil
      path)))


(defun get-user-directory (type)
  "Get the user directory keyed TYPE.

DIR is a path to a the TYPE within the user directories."
  (map-elt user-directories type))


(defun set-user-directory (type directory &optional ensure add-to-path)
  "Set the user directory keyed TYPE to DIRECTORY.

DIR is a path to a the TYPE within the user directories.  If ENSURE
is t, the directory will be created along with it's parents.

If ADD-TO-PATH is t, the directory is added to ‘load-path’.  If
it is :recursive, then all descendents are also added."
  (map-put user-directories type directory)

  ;; Create the directory when non existent if ENSURE is set.
  (when (and ensure  (not (file-exists-p directory)))
    (make-directory directory t))

  ;; See if the directory is to be added to load-path.
  (pcase add-to-path
    ('(t 1 :self) (add-to-list 'load-path directory))
    ('(:recursive) (let ((default-directory directory))
		     (normal-top-level-add-subdirs-to-load-path)))))


(defun make-locate-user-file-fn (type)
  "Make a location function for an user file.

Create a file, given the TYPE of directory.  Type is an argument
that should be present in `user-directories'.  The function is
interactive and has a suitable docstring."
  (let* ((name (substring (symbol-name type) 1))
	 (symb (intern (format "locate-user-%s-file" name))))
    (fset symb
	  `(lambda (filename &optional only-if-exists)
	     ,(concat "Locate FILENAME at the " (symbol-name type) " directory.
If ONLY-IF-EXISTS is non-nil then, if the file is absent, return nil.")
      (interactive "sName of the file: \n")
      ;; ::CHECK:: is a string an appropriate type for the name of the file?
      ;; ::CHECK:: should only-if-exists also be taken into account?
      (locate-user-file ',type filename only-if-exists)))))


(provide 'user-directories)
;;; user-directories.el ends here

;;; setup-user-directories.el --- Set up all user directories, according to the system type.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Francisco Miguel Colaço

;; Author: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Maintainer: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Version: 1
;; Created: 2018-05-05
;; Keywords: emacs
;; Homepage: https://github.com/francisco.colaco/emacs-directories
;; Package-Requires: (map user-directories)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; The users of Emacs have traditionally been plagued with a myriad of
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
(require 'user-directories)


(defun setup-user-directories-default ()
  "Set up the user directories on unknown systems.

When the system is unknown, or there is no specific
initialisation procedure for it, the directories are set in
`user-emacs-directory', but separated therein."
  (let ((config-dir (expand-file-name "config/" user-emacs-directory))
	(data-dir (expand-file-name "data/" user-emacs-directory))
	(cache-dir (expand-file-name "cache/" user-emacs-directory))
	(runtime-dir (expand-file-name "runtime/" user-emacs-directory)))
    ;; Add the directories to the user directories file, creating them if absent.
    (set-user-directory :config config-dir t)
    (set-user-directory :data data-dir t)
    (set-user-directory :cache cache-dir t)
    (set-user-directory :runtime runtime-dir t)

    (dolist (type '(:desktop :documents :download :pictures :videos))
      (set-user-directory type (expand-file-name "~/")))

    ;; Create user Lisp directories, adding them and their subdirs to `load-path'.
    (setup-user-lisp-directories)))


;;;###autoload
(defun setup-user-directories ()
  "Set up the user directories, according to the operating system.

Find which are sensible names for the user directories."
  (let* ((os (car (last (split-string (symbol-name system-type) "/"))))
	 (lib (concat "user-directories-" os)))
    ;; One either finds the os specific libraries or uses the default.
    (if (null (locate-library lib))
	(setup-user-directories-default)
      (progn
	;; Load the library and call the setup function.
	(load-library lib)
	(funcall (intern (concat "setup-user-directories-" os)))))))


(provide 'setup-user-directories)
;;; setup-user-directories.el ends here

;;; user-directories-windows-nt.el --- MS Windows specification for User Emacs directories   -*- lexical-binding: t -*-

;; Copyright (C)2018 Free Software Foundation

;; Author: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Maintainer: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Version: 1
;; Created: 2018-05-05
;; Keywords: emacs
;; Homepage: https://github.com/francisco.colaco/emacs-directories
;; Package-Requires: (cl subr-x user-directories)

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

;; The Windows configuration is probably based on erroneous
;; assumptions.  I humbly assume my general inexperience with
;; Microsoft products.
;;
;; There are directories for user :data, :config, :cache and :runtime
;; files.  Aditionally, the following directories are queried the from
;; Windows shell:
;;
;;  - :computer
;;  - :desktop
;;  - :documents
;;  - :download
;;  - :templates
;;  - :pictures
;;  - :music
;;  - :videos
;;

;;; Code:

(eval-when-compile
 (require 'cl)
 (require 'subr-x))
(require 'user-directories)


;; Read a registry key.  This should really not be in this library,
;; since it has a broader application.
(defun windows-read-registry-value (key value)
  "From a registry KEY, reads VALUE, when on MS Windows."
  (let ((command (concat "REG QUERY \"" key "\" /V \"" value "\""))
        result tokens last-token)
    (setq result (shell-command-to-string command)
          tokens (split-string result nil t)
          last-token (nth (1- (length tokens)) tokens))
    (and (not (string= last-token "value.")) last-token)))


;;;; Windows NT specific code.

(defvar windows-shell-folder-definitions
  '(:computer "MyComputerFolder"
    :desktop "Desktop"
    :documents "Personal"
    :download "Downloads"
    :templates "Templates"
    :pictures "My Pictures"
    :music "My Music"
    :videos "My Videos")
  "A list of Microsoft Windows shell folders to search for existence.

If the shell folder exists (a registry key), the directory will be set.")


(defun windows-shell-folder (folder)
  "Return a user shell folder.

FOLDER is a string describing the folder purpose, like \"My
Documents\".  It is part of the Microsoft Windows specification.

Returns NIL if the key and value is not found."
  (if-let ((result
            (or (windows-read-registry-value "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders" folder)
                (windows-read-registry-value "HKLM\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders" folder))))
    (substitute-in-file-name result)))


(defun setup-user-directories-windows-nt ()
  "Set up the user directories on Windows based systems."
  (let* ((appdata-dir (getenv "APPDATA"))
         (local-appdata-dir (or (getenv "LOCALAPPDATA") appdata-dir))
         (temp-dir (getenv "TEMP")))
    ;; Add the directories to the user directories file, creating them if absent.
    (set-user-directory :config (expand-file-name "emacs/config/" appdata-dir) t)
    (set-user-directory :data (expand-file-name "emacs/data/" local-appdata-dir) t)
    (set-user-directory :cache (expand-file-name "emacs/cache/" local-appdata-dir) t)
    (set-user-directory :runtime (expand-file-name "emacs/runtime/" temp-dir) t)

    ;; Create user Lisp directories, adding them and their subdirs to `load-path'.
    (let ((dir (expand-file-name "emacs/lisp/" local-appdata-dir)))
      (set-user-directory :lisp dir t :recursive)
      (add-to-list 'load-path dir))

    (let ((dir (expand-file-name "emacs/lisp/" appdata-dir)))
      (set-user-directory :user-lisp dir t :recursive)
      (add-to-list 'load-path dir)))

  ;; Add the personal folders.
  (cl-loop for (type default) on windows-shell-folder-definitions by (function cddr) do
    (if-let ((val (windows-shell-folder "MyComputerFolder")))
      (set-user-directory type default))))


(provide 'user-directories-windows-nt)
;;; user-directories-windows-nt.el ends here

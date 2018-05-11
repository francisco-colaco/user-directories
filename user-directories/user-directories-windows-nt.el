;;; user-directories-windows-nt.el --- MS Windows specification for User Emacs directories   -*- lexical-binding: t -*-

;; Copyright (C)2018 Free Software Foundation

;; Author: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Maintainer: Francisco Miguel Colaço <francisco.colaco@gmail.com>
;; Version: 1
;; Created: 2018-05-05
;; Keywords: emacs
;; Homepage: https://github.com/francisco.colaco/emacs-directories
;; Package-Requires: ()

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

;;;; Windows NT specific code.

(defun windows-read-registry-value (key value)
  "From a registry KEY, reads VALUE, when on MS Windows."
  (let ((command (concat "REG QUERY \"" key "\" /V \"" value "\""))
        result tokens last-token)
    (setq result (shell-command-to-string command)
          tokens (split-string result nil t)
          last-token (nth (1- (length tokens)) tokens))
    (and (not (string= last-token "value.")) last-token)))


(defun windows-shell-folder (folder)
    "Returns a user shell folder.

 FOLDER is a string describing the folder purpose, like \"My Documents\"."
  (let ((result (windows-read-registry-value "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Explorer\\User Shell Folders" folder)))
    (substring (shell-command-to-string (concat "echo " result)) 0 -1)))


(defun setup-user-directories-windows-nt ()
  "Set up the user directories on Windows based systems."
  (let ((appdata-dir (getenv "APPDATA"))
	(local-appdata-dir (getenv "LOCALAPPDATA"))
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
  (set-user-directory :computer (windows-shell-folder "MyComputerFolder"))
  (set-user-directory :desktop (windows-shell-folder "Desktop"))
  (set-user-directory :documents (windows-shell-folder "Personal"))
  (set-user-directory :download (windows-shell-folder "Downloads"))
  (set-user-directory :templates (windows-shell-folder "Templates"))
  (set-user-directory :pictures (windows-shell-folder "My Pictures"))
  (set-user-directory :music (windows-shell-folder "My Music"))
  (set-user-directory :videos (windows-shell-folder "My Videos")))


(provide 'user-directories-windows-nt)
;;; user-directories-windows-nt.el ends here

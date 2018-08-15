(require 'user-directories)


(let ((data-dir (get-user-directory :data))
      (config-dir (get-user-directory :config)))
  (setq user-emacs-directory data-dir)

  ;; See if there is an init.el in the user config directory.  If so,
  ;; then set it as the `user-init-file'.
  ;; Ah! How I miss anaphoric macros!
  (let ((init-file (locate-user-config-file "init.el")))
    (when (file-exists-p init-file)
      (setq user-init-file init-file)
      (load init-file))))

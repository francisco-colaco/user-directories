# user-directories

## What is user-directories

**user-directories** is a library that locates the files that normally
reside under ~/.emacs into directories separated by concern.  The user
will be able to locate to files under `:data`, `:config`, `:cache` and
`:runtime` directories.

Additionally, directories can be defined for different user file
domains: `:documents`, `:picture`, `:music`, :templates and so on,
depending on the value of `system-type`.

On Linux, the results will follow the [XDG Base Directory
Specification](https://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html)
and use xdg-user-dir.  On Windows NT, the directories will be aligned
when possible with the [Windows Shell
Folders](https://ss64.com/nt/shell.html).

For instance, on Linux and continental portuguese:

- `(locate-user-config-file "init.el")` on yields "/home/fhc/.config/emacs/init.el"
- `(locate-user-data-file "known-projects.eld")` yields "/home/fhc/.local/share/emacs/known-projects.eld"
- `(locate-user-cache-file "fontnames.cache")` yields "/home/fhc/.cache/emacs/fontnames.cache"
- `(locate-user-documents-file "org/agenda.txt")` yields "/home/fhc/Documentos/org/agenda.txt"

Depending on the OS and language, the returned directories will differ.

As `user-emacs-directory` will be aliased to the `:data` directory,
all files will be transitioned into the new directories.  Elisp
package writers can begin to organise their files away from what used
to be ~/.emacs.d.  So:

- `:config` files are those that the user has to back up;
- `:cache` files, when deleted, may not lose data irreversibily, but make things slower;
- `:runtime` files are those that will be deleted when the user logs out
  on all sessions (in any case, those files cannot survive a reboot);
- `:data` files are everything else (known projects in projectile or
  third party elisp libraries).

Two elisp library directories are defined and added to the path:

- `:lisp` for third party elisp libraries (which can be retrieved elsewhere);
- `:user-lisp` for the libraries the user wrote; and this directory resides in :config.

The other folders, like `:documents`, `:templates` or `:download`
depend solely on `system-type`, being discovered when Emacs boots by a
system of dynamically loaded modules.  If a system does not have a
provider library, there is a default setting, which places the
directories above listed under `user-emacs-directory`.


## Installation

Copy the directories user-directory and site-start.d to a site-lisp
(/usr/share/emacs/site-lisp/ on Linux Fedora 28, for instance).  Then
erase ~/.emacs.d and place your init.el file into the directory
pointed by `(get-user-directory :config)` (in my case
"/home/fhc/.config/emacs/").  That's it!


## Provider libraries

According to system-type, the last part after the slash (/), if there
is one, constitutes the stem of the provider library and function.
For instance, a gnu/linux system-type will load the library
user-libraries-linux and execute `(setup-user-libraries-linux)`
automatically.

If one wishes to provide a library for a new `system-type`, one must
write a library and a at the least function with the name based on that
`system-type`, and can use the ones provided for Windows NT and Linux as
a template.

For instance, the library for darwin will be called
user-directories-darwin.el, and that library will have a function called
`setup-user-directories-darwin`.


## Copyright

The copyright is assigned to the Free Software Foundation, under the
GNU Public License, version 3 or above.

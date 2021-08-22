;; -*- lexical-binding: t -*-

(setq debug-on-error t)

;; This file wraps the primary Eow configuration (which lives in
;; eow.el) so that we don't have to wrap the entire file in various
;; `let' forms, etc. We put as much as possible in eow.el.

;; This allows us to instead load a different Emacs configuration by
;; exporting USER_EMACS_DIRECTORY to another .emacs.d directory.
(let ((alternate-user-emacs-directory (getenv "USER_EMACS_DIRECTORY")))

  (defvar eow--init-file-loaded-p nil
    "Non-nil if the init-file has already been loaded.
This is important for Emacs 27 and above, since our early
init-file just loads the regular init-file, which would lead to
loading the init-file twice if it were not for this variable.")

  (cond
   ;; If already loaded, do nothing. But still allow re-loading, just
   ;; do it only once during init.
   ((and (not after-init-time) eow--init-file-loaded-p))

   ;; Delegate to another Emacs configuration. (We still don't want to
   ;; load it twice.)
   (alternate-user-emacs-directory
    (setq alternate-user-emacs-directory
          (file-name-as-directory alternate-user-emacs-directory))
    (setq user-emacs-directory alternate-user-emacs-directory)
    (setq user-init-file (expand-file-name "init.el" user-emacs-directory))
    (load user-init-file 'noerror 'nomessage))
   (t
    (setq eow--init-file-loaded-p t)

    (defvar eow-minimum-emacs-version "26.1"
      "Eow Emacs does not support any Emacs version below this.")

    (defvar eow-local-init-file
      (expand-file-name "init.local.el" user-emacs-directory)
      "File for local customizations of Eow.")

    ;; Prevent package.el from modifying this file.
    (setq package-enable-at-startup nil)

    ;; Prevent Custom from modifying this file.
    (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
    (load custom-file 'noerror 'nomessage)

    ;; Make sure we are running a modern enough Emacs, otherwise abort
    ;; init.
    (if (version< emacs-version eow-minimum-emacs-version)
        (error (concat "Eow Emacs requires at least Emacs %s, "
                       "but you are running Emacs %s")
               eow-minimum-emacs-version emacs-version)

      (let ((this-file
             ;; This function returns the target of the link. If the
             ;; init-file is not a symlink, then we abort.
             ;;
             ;; We may be loading init.el in batch mode, in which case
             ;; `user-init-file' is nil. In that case, we should have
             ;; some backup options to try.
             (or user-init-file
                 load-file-name
                 buffer-file-name)))

        ;; under noninteractive mode, emacs load file-truename.
        ;; (message "-->%s %s" load-true-file-name this-file)
        (unless (or (file-symlink-p this-file) noninteractive)
          (error "Init-file %S is not a symlink" this-file))

        (defvar eow-lib-file (expand-file-name
                              "eow.el"
                              (file-name-directory (file-truename this-file)))
          "File containing main Eow configuration.
This file is loaded by init.el.")

        (unless (file-exists-p eow-lib-file)
          (error "Library file %S does not exist" eow-lib-file))

        (defvar eow--finalize-init-hook nil
          "Hook run unconditionally after init, even if it fails.
Unlike `after-init-hook', this hook is run every time the
init-file is loaded, not just once.")

        (unwind-protect
            ;; Load the main Eow configuration code. Disable
            ;; `file-name-handler-alist' to improve load time.
            ;;
            ;; Make sure not to load an out-of-date .elc file. Since
            ;; we byte-compile asynchronously in the background after
            ;; init succeeds, this case will happen often.
            (let ((file-name-handler-alist nil)
                  (load-prefer-newer t)
                  (stale-bytecode t))
              (catch 'stale-bytecode
                ;; We actually embed the contents of the local
                ;; init-file directly into the compiled eow.elc, so
                ;; that it can get compiled as well (and its
                ;; macroexpansion can use packages that Eow only
                ;; loads at compile-time). So that means we have to go
                ;; the slow path if the local init-file has been
                ;; updated more recently than the compiled eow.elc.
                (when (file-newer-than-file-p
                       eow-local-init-file
                       (concat eow-lib-file "c"))
                  (throw 'stale-bytecode nil))
                (load
                 (file-name-sans-extension eow-lib-file)
                 nil 'nomessage)
                (setq stale-bytecode nil))
              (when stale-bytecode
                ;; Don't bother trying to recompile, unlike in
                ;; straight.el, since we are going to handle that
                ;; later, asynchronously.
                (ignore-errors
                  (delete-file (concat eow-lib-file "c")))
                (load eow-lib-file nil 'nomessage 'nosuffix)))
          (run-hooks 'eow--finalize-init-hook)))))))

;; -*- lexical-binding: t -*-

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

;;; Detect stale bytecode

;; If Emacs version changed, the bytecode is no longer valid and we
;; must recompile. Also, if the location of Radian changed, our
;; dotfile-finding functions are defined incorrectly and we must
;; recompile.
(eval
 `(unless (equal
           (list
            (emacs-version)
            radian-lib-file)
           ',(eval-when-compile
               (list
                (emacs-version)
                radian-lib-file)))
    (throw 'stale-bytecode nil)))

;;; Variables

(defconst IS-EMACS27+ (> emacs-major-version 26))
(defconst IS-EMACS28+ (> emacs-major-version 27))
(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (eq system-type 'gnu/linux))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))
(defconst IS-BSD      (or IS-MAC (eq system-type 'berkeley-unix)))

;; Directories/files
(defvar radian-directory (file-name-directory
                          (directory-file-name
                           (file-name-directory
                            radian-lib-file)))
  "Path to the Radian Git repository.")

(defconst radian-emacs-dir
  (eval-when-compile (file-truename user-emacs-directory))
  "The path to the currently loaded .emacs.d directory. Must end with a slash.")

(defconst radian-core-dir (concat radian-directory "emacs/core/")
  "The root directory of Radian's core files. Must end with a slash.")

(defconst radian-etc-dir (concat radian-emacs-dir "etc/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst radian-var-dir (concat radian-emacs-dir "var/")
  "Directory for non-volatile local storage.

Use this for files that don't change much, like server binaries, external
dependencies or long-term shared data. Must end with a slash.")

(defconst radian-autoloads-file
  (concat radian-etc-dir "autoloads." emacs-version ".el")
  "Where `radian-reload-core-autoloads' stores its core autoloads.

This file is responsible for informing Emacs where to find all of Radian's
autoloaded core functions (in autoload/*.el).")

;; Ensure `core-dir' is in `load-path'
(add-to-list 'load-path radian-core-dir)

;;; Load utility libraries

(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'core-lib)

;;; Define Radian customization groups

(defgroup radian-hooks nil
  "Startup hooks for Radian Emacs."
  :group 'radian
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/radian"))

(defgroup radian nil
  "Customize your Radian Emacs experience."
  :prefix "radian-"
  :group 'emacs
  :link '(url-link :tag "GitHub" "https://github.com/raxod502/radian"))

;;; Define utility functions. TODO: some function can move into core-lib?

(defmacro radian-protect-macros (&rest body)
  "Eval BODY, protecting macros from incorrect expansion.
This macro should be used in the following situation:

Some form is being evaluated, and this form contains as a
sub-form some code that will not be evaluated immediately, but
will be evaluated later. The code uses a macro that is not
defined at the time the top-level form is evaluated, but will be
defined by time the sub-form's code is evaluated. This macro
handles its arguments in some way other than evaluating them
directly. And finally, one of the arguments of this macro could
be interpreted itself as a macro invocation, and expanding the
invocation would break the evaluation of the outer macro.

You might think this situation is such an edge case that it would
never happen, but you'd be wrong, unfortunately. In such a
situation, you must wrap at least the outer macro in this form,
but can wrap at any higher level up to the top-level form."
  (declare (indent 0))
  `(eval '(progn ,@body)))

;; == letf!
(defmacro radian-flet (bindings &rest body)
  "Temporarily override function definitions using `cl-letf*'.
BINDINGS are composed of `defun'-ish forms. NAME is the function
to override. It has access to the original function as a
lexically bound variable by the same name, for use with
`funcall'. ARGLIST and BODY are as in `defun'.

\(fn ((defun NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  `(cl-letf* (,@(cl-mapcan
                 (lambda (binding)
                   (when (memq (car binding) '(defun lambda))
                     (setq binding (cdr binding)))
                   (cl-destructuring-bind (name arglist &rest body) binding
                     (list
                      `(,name (symbol-function #',name))
                      `((symbol-function #',name)
                        (lambda ,arglist
                          ,@body)))))
                 bindings))
     ,@body))

;; == defadvice!
(defmacro radian-defadvice (name arglist where place docstring &rest body)
  "Define an advice called NAME and add it to a function.
ARGLIST is as in `defun'. WHERE is a keyword as passed to
`advice-add', and PLACE is the function to which to add the
advice, like in `advice-add'. PLACE should be sharp-quoted.
DOCSTRING and BODY are as in `defun'."
  (declare (indent 2)
           (doc-string 5))
  (unless (stringp docstring)
    (error "Radian: advice `%S' not documented'" name))
  (unless (and (listp place)
               (= 2 (length place))
               (eq (nth 0 place) 'function)
               (symbolp (nth 1 place)))
    (error "Radian: advice `%S' does not sharp-quote place `%S'" name place))
  `(progn
     ;; You'd think I would put an `eval-and-compile' around this. It
     ;; turns out that doing so breaks the ability of
     ;; `elisp-completion-at-point' to complete on function arguments
     ;; to the advice. I know, right? Apparently this is because the
     ;; code that gets the list of lexically bound symbols at point
     ;; tries to `macroexpand-all', and apparently macroexpanding
     ;; `eval-and-compile' goes ahead and evals the thing and returns
     ;; only the function symbol. No good. But the compiler does still
     ;; want to know the function is defined (this is a Gilardi
     ;; scenario), so we pacify it by `eval-when-compile'ing something
     ;; similar (see below).
     (defun ,name ,arglist
       ,(let ((article (if (string-match-p "^:[aeiou]" (symbol-name where))
                           "an"
                         "a")))
          (format "%s\n\nThis is %s `%S' advice for `%S'."
                  docstring article where
                  (if (and (listp place)
                           (memq (car place) ''function))
                      (cadr place)
                    place)))
       ,@body)
     (eval-when-compile
       (declare-function ,name nil))
     (advice-add ,place ',where #',name)
     ',name))

(defmacro radian-defhook (name arglist hooks docstring &rest body)
  "Define a function called NAME and add it to a hook.
ARGLIST is as in `defun'. HOOKS is a list of hooks to which to
add the function, or just a single hook. DOCSTRING and BODY are
as in `defun'."
  (declare (indent 2)
           (doc-string 4))
  (unless (listp hooks)
    (setq hooks (list hooks)))
  (dolist (hook hooks)
    (unless (string-match-p "-\\(hook\\|functions\\)$" (symbol-name hook))
      (error "Symbol `%S' is not a hook" hook)))
  (unless (stringp docstring)
    (error "Radian: no docstring provided for `radian-defhook'"))
  (let ((hooks-str (format "`%S'" (car hooks))))
    (dolist (hook (cdr hooks))
      (setq hooks-str (format "%s\nand `%S'" hooks-str hook)))
    `(progn
       (defun ,name ,arglist
         ,(format "%s\n\nThis function is for use in %s."
                  docstring hooks-str)
         ,@body)
       (dolist (hook ',hooks)
         (add-hook hook ',name)))))

;; == eval-if!
;; == eval-when!

;; == quiet!
(defmacro radian--with-silent-load (&rest body)
  "Execute BODY, with the function `load' made silent."
  (declare (indent 0))
  `(radian-flet ((defun load (file &optional noerror _nomessage &rest args)
                   (apply load file noerror 'nomessage args)))
     ,@body))

(defmacro radian--with-silent-write (&rest body)
  "Execute BODY, with the function `write-region' made silent."
  (declare (indent 0))
  `(radian-flet ((defun write-region
                     (start end filename &optional append visit lockname
                            mustbenew)
                   (funcall write-region start end filename append 0
                            lockname mustbenew)
                   (when (or (stringp visit) (eq visit t))
                     (setq buffer-file-name
                           (if (stringp visit)
                               visit
                             filename))
                     (set-visited-file-modtime)
                     (set-buffer-modified-p nil))))
     (cl-letf (((symbol-function #'message) #'ignore))
       ,@body)))

(defmacro radian--with-silent-message (regexps &rest body)
  "Silencing any messages that match REGEXPS, execute BODY.
REGEXPS is a list of strings; if `message' would display a
message string (not including the trailing newline) matching any
element of REGEXPS, nothing happens. The REGEXPS need not match
the entire message; include ^ and $ if necessary. REGEXPS may
also be a single string."
  (declare (indent 1))
  (let ((regexps-sym (cl-gensym "regexps")))
    `(let ((,regexps-sym ,regexps))
       (when (stringp ,regexps-sym)
         (setq ,regexps-sym (list ,regexps-sym)))
       (radian-flet ((defun message (format &rest args)
                       (let ((str (apply #'format format args)))
                         ;; Can't use an unnamed block because during
                         ;; byte-compilation, some idiot loads `cl', which
                         ;; sticks an advice onto `dolist' that makes it
                         ;; behave like `cl-dolist' (i.e., wrap it in
                         ;; another unnamed block) and therefore breaks
                         ;; this code.
                         (cl-block done
                           (dolist (regexp ,regexps-sym)
                             (when (or (null regexp)
                                       (string-match-p regexp str))
                               (cl-return-from done)))
                           (funcall message "%s" str)))))
         ,@body))))

(defun radian--advice-silence-messages (func &rest args)
  "Invoke FUNC with ARGS, silencing all messages.
This is an `:around' advice for many different functions."
  (cl-letf (((symbol-function #'message) #'ignore))
    (apply func args)))

(defun radian--random-string ()
  "Return a random string designed to be globally unique."
  (md5 (format "%s%s%s%s"
               (system-name) (emacs-pid) (current-time) (random))))

(defun radian--list-of-strings-p (obj)
  "Return non-nil if OBJ is a list of strings."
  (and (listp obj)
       (cl-every #'stringp obj)))

(defun radian--path-join (path &rest segments)
  "Join PATH with SEGMENTS using `expand-file-name'.
First `expand-file-name' is called on the first member of
SEGMENTS, with PATH as DEFAULT-DIRECTORY. Then `expand-file-name'
is called on the second member, with the result of the first call
as DEFAULT-DIRECTORY, and so on. If no SEGMENTS are passed, the
return value is just PATH."
  (while segments
    (setq path (expand-file-name (pop segments) path)))
  path)

;;; Define hooks and load local configuration

;; Reset the value of this variable so that stale functions don't
;; stick around.
(setq radian--finalize-init-hook nil)

(defcustom radian-before-straight-hook nil
  "Hook run just before Radian bootstraps straight.el.
For use with `radian-local-on-hook' in init.local.el."
  :group 'radian-hooks
  :type 'hook)

(defcustom radian-after-init-hook nil
  "Hook run after at the very end of init.
For use with `radian-local-on-hook' in init.local.el."
  :group 'radian-hooks
  :type 'hook)

(defvar radian--hook-contents nil
  "Alist mapping local init hooks to lists of forms.
This is used to embed local init hook code directly into the
init-file at the appropriate places during byte-compilation,
without breaking macro-expansion.")

;; Idempotency.
(setq radian--hook-contents nil)

;; Allow binding this variable dynamically before straight.el has been
;; loaded.
(defvar straight-current-profile)

(defmacro radian--load-local-init-file ()
  "Load local init-file, with crazy hacks for byte-compilation.
In particular, if we are byte-compiling, actually macroexpand to
the entire contents of the local init-file, except that the
bodies of invocations to `radian-local-on-hook' are recorded in
`radian--hook-contents'. Otherwise just load the file like
usual."
  (if byte-compile-current-file
      (let ((forms nil))
        (with-temp-buffer
          (ignore-errors
            ;; Can't do this literally because it breaks Unicode
            ;; characters.
            (insert-file-contents radian-local-init-file))
          (condition-case _
              (while t
                (let ((form (read (current-buffer))))
                  (if (and (listp form)
                           (eq (nth 0 form) #'radian-local-on-hook)
                           (nth 1 form)
                           (symbolp (nth 1 form))
                           (nthcdr 2 form))
                      (let* ((name (nth 1 form))
                             (body (nthcdr 2 form))
                             (hook (intern (format "radian-%S-hook" name)))
                             (link (assq hook radian--hook-contents)))
                        (unless link
                          (setq link (cons hook nil))
                          (push link radian--hook-contents))
                        (dolist (subform body)
                          (push subform (cdr link))))
                    (push form forms))))
            (end-of-file)))
        (setq forms (nreverse forms))
        (dolist (link radian--hook-contents)
          (setf (cdr link)
                (nreverse (cdr link))))
        `(progn ,@forms))
    `(load radian-local-init-file 'noerror 'nomessage)))

(defmacro radian-local-on-hook (name &rest body)
  "Register some code to be run on one of Radian's hooks.
The hook to be used is `radian-NAME-hook', with NAME an unquoted
symbol, and the code which is added is BODY wrapped in a `progn'.
See \\[customize-group] RET radian-hooks RET for a list of hooks
which you can use with this macro in your local init-file.

Using this macro instead of defining functions and adding them to
Radian's hooks manually means that a lot of magic happens which
allows Radian to embed your entire local init-file into Radian
during byte-compilation without breaking macroexpansion in
unexpected ways."
  (declare (indent (lambda (&rest _) 0)))
  (let ((func-name (intern (format "radian-local--%S" name)))
        (hook (intern (format "radian-%S-hook" name))))
    `(progn
       (radian-defhook ,func-name ()
         ,hook
         "Automatically-generated local hook function."
         (radian-protect-macros
           ,@body)))))

(defmacro radian--run-hook (name)
  "Run the given local init HOOK.
The hook to be used is `radian-NAME-hook', with NAME an unquoted
symbol. This binds `straight-current-profile', and also has some
gnarly hacks to allow Radian to embed the entire contents of the
hook directly into the init-file during byte-compilation."
  (declare (indent 0))
  (let ((hook (intern (format "radian-%S-hook" name))))
    `(let ((straight-current-profile 'radian-local))
       (run-hooks ',hook)
       ,@(when byte-compile-current-file
           (alist-get hook radian--hook-contents)))))

;; Allow to disable local customizations with a
;; command-line argument.
(if (member "--no-local" command-line-args)

    ;; Make sure to delete --no-local from the list, because
    ;; otherwise Emacs will issue a warning about the unknown
    ;; argument.
    (setq command-line-args
          (delete "--no-local" command-line-args))

  ;; Load local customizations.
  (radian--load-local-init-file))

;;; Startup optimizations

;; After we enabled `load-prefer-newer' in init.el, disable it again
;; for the duration of init. Presumably, it slows things down, and we
;; shouldn't need it for anything but loading radian.el itself.
(setq load-prefer-newer nil)

;; Disable bidirectional text rendering for a modest performance boost. I've set
;; this to `nil' in the past, but the `bidi-display-reordering's docs say that
;; is an undefined state and suggest this to be just as good:
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Disabling the BPA makes redisplay faster, but might produce incorrect display
;; reordering of bidirectional text with embedded parentheses and other bracket
;; characters whose 'paired-bracket' Unicode property is non-nil.
(setq bidi-inhibit-bpa t)  ; Emacs 27 only

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but we inhibit it there anyway. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when IS-WINDOWS
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-pipe-read-delay 0              ; faster ipc
        w32-pipe-buffer-size (* 64 1024))) ; read more at a time (was 4K)

;; Remove command line options that aren't relevant to our current OS; means
;; slightly less to process at startup.
(unless IS-MAC   (setq command-line-ns-option-alist nil))
(unless IS-LINUX (setq command-line-x-option-alist nil))

;; Adopt a sneaky garbage collection strategy of waiting until idle time to
;; collect; staving off the collector while the user is working.
(setq gcmh-idle-delay 5
      gcmh-high-cons-threshold (* 16 1024 1024)  ; 16mb
      gcmh-verbose nil)

;; HACK `tty-run-terminal-initialization' is *tremendously* slow for some
;;      reason; inexplicably doubling startup time for terminal Emacs. Keeping
;;      it disabled will have nasty side-effects, so we simply delay it until
;;      later in the startup process and, for some reason, it runs much faster
;;      when it does.
(unless (daemonp)
  (advice-add #'tty-run-terminal-initialization :override #'ignore)
  (add-hook! 'window-setup-hook
    (defun radian-init-tty-h ()
      (advice-remove #'tty-run-terminal-initialization #'ignore)
      (tty-run-terminal-initialization (selected-frame) nil t))))

;;; Networking

;; Emacs is essentially one huge security vulnerability, what with all the
;; dependencies it pulls in from all corners of the globe. Let's try to be at
;; least a little more discerning.
(setq gnutls-verify-error (not (getenv "INSECURE"))
      gnutls-algorithm-priority
      (when (boundp 'libgnutls-version)
        (concat "SECURE128:+SECURE192:-VERS-ALL"
                (if (and (not IS-WINDOWS)
                         (not (version< emacs-version "26.3"))
                         (>= libgnutls-version 30605))
                    ":+VERS-TLS1.3")
                ":+VERS-TLS1.2"))
      ;; `gnutls-min-prime-bits' is set based on recommendations from
      ;; https://www.keylength.com/en/4/
      gnutls-min-prime-bits 3072
      tls-checktrust gnutls-verify-error
      ;; Emacs is built with `gnutls' by default, so `tls-program' would not be
      ;; used in that case. Otherwise, people have reasons to not go with
      ;; `gnutls', we use `openssl' instead. For more details, see
      ;; https://redd.it/8sykl1
      tls-program '("openssl s_client -connect %h:%p -CAfile %t -nbio -no_ssl3 -no_tls1 -no_tls1_1 -ign_eof"
                    "gnutls-cli -p %p --dh-bits=3072 --ocsp --x509cafile=%t \
--strict-tofu --priority='SECURE192:+SECURE128:-VERS-ALL:+VERS-TLS1.2:+VERS-TLS1.3' %h"
                    ;; compatibility fallbacks
                    "gnutls-cli -p %p %h"))

;; Feature `url-http' is a library for making HTTP requests.
(with-eval-after-load 'url-http
  (eval-when-compile
    (require 'url-http))

  (radian-defadvice radian--no-query-on-http-kill
      (buffer)
    :filter-return #'url-http
    "Disable query-on-exit for all network connections.
This prevents Emacs shutdown from being interrupted just because
there is a pending network request."
    (prog1 buffer
      (set-process-query-on-exit-flag
       (get-buffer-process buffer) nil))))

;;; Set up package management
;;;; straight.el

;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
      '(;; Packages registered in this file.
        (radian . "radian.el")
        ;; Packages registered in the local init-file during hooks.
        (radian-local . "radian-local.el")
        ;; Packages registered interactively.
        (nil . "default.el")))

;; Pretend to dynamically bind `straight-current-profile' to `radian'
;; over the init-file. We do this to avoid having straight.el
;; configuration mentioned in the top-level init-file.

(radian-defhook radian--reset-straight-current-profile ()
  radian--finalize-init-hook
  "Reset `straight-current-profile' to nil.
This function is used on `radian--finalize-init-hook' to emulate
binding the variable dynamically over the entire init-file."
  (setq straight-current-profile nil))

(setq straight-current-profile 'radian)

(setq straight-repository-branch "develop"
      ;; Since byte-code is rarely compatible across different versions of
      ;; Emacs, it's best we build them in separate directories, per emacs
      ;; version.
      straight-build-dir (format "build-%s" emacs-version)
      ;; Before switching to straight, `user-emacs-directory' would average out at
      ;; around 100mb with half Radian's modules at ~230 packages. Afterwards, at
      ;; around 1gb. With shallow cloning, that is reduced to ~400mb. This has
      ;; no affect on packages that are pinned, however (run 'radian purge' to
      ;; compact those after-the-fact). Some packages break when shallow cloned
      ;; (like magit and org), but we'll deal with that elsewhere.
      straight-vc-git-default-clone-depth 1)

;; If watchexec and Python are installed, use file watchers to detect
;; package modifications. This saves time at startup. Otherwise, use
;; the ever-reliable find(1).
(if (and (executable-find "watchexec")
         (executable-find "python3"))
    (setq straight-check-for-modifications '(watch-files find-when-checking))
  (setq straight-check-for-modifications
        '(find-at-startup find-when-checking)))

;; Clear out recipe overrides (in case of re-init).
(setq straight-recipe-overrides nil)

(radian--run-hook before-straight)

;; Bootstrap the package manager, straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Leaf

;; Package `leaf' provides a handy macro by the same name which
;; is essentially a wrapper around `with-eval-after-load' with a lot
;; of handy syntactic sugar and useful features.

;; For print macroexpand.
(defmacro pexm (form)
  "Output expanded form of given FORM."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))

(eval-and-compile
  (straight-use-package 'leaf)
  (straight-use-package '(leaf-keywords :fork "meziberry/leaf-keywords"))

  ;; Run leaf command to load the package of leaf.
  (leaf leaf-keywords
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)
    ;; Let leaf to use straight by default.
    (setq leaf-system-defaults (append '(:straight t) leaf-system-defaults))

    ;; blackout
    (leaf blackout))

  ;; For not using straight.el package config.
  (defmacro leaf! (name &rest args)
    "Like `leaf', but with `straight' disabled.
NAME and ARGS are as in `leaf'."
    (declare (indent defun))
    `(leaf ,name
       :straight nil
       ,@args)))

;; gcmh-mode
(leaf gcmh :blackout t)

;;; Configure ~/.emacs.d paths

;; Package `no-littering' changes the default paths for lots of
;; different packages, with the net result that the ~/.emacs.d folder
;; is much more clean and organized.
(leaf no-littering :require t)

;;; Prevent Emacs-provided Org from being loaded

;; Our real configuration for Org comes much later. Doing this now
;; means that if any packages that are installed in the meantime
;; depend on Org, they will not accidentally cause the Emacs-provided
;; (outdated and duplicated) version of Org to be loaded before the
;; real one is registered.
;;
;; Use my mirror of Org because the upstream has *shockingly*
;; atrocious uptime (namely, the entire service will just go down for
;; more than a day at a time on a regular basis). Unacceptable because
;; it keeps breaking Radian CI.
(straight-use-package
 '(org :host github :repo "emacs-straight/org-mode" :local-repo "org"))

;;; el-patch
(leaf el-patch)

;; Only needed at compile time, thanks to Jon
;; <https://github.com/raxod502/el-patch/pull/11>.
(eval-when-compile
  (require 'el-patch))

;;; Keybindings

(leaf key-chord :require t
  :config
  (radian--advice-silence-messages #'key-chord-mode +1)
  (setq key-chord-two-keys-delay 0.2))

(defvar radian-keymap (make-sparse-keymap)
  "Keymap for Radian commands that should be put under a prefix.
This keymap is bound under \\[radian-keymap].")

(leaf-key "M-P" radian-keymap)

(defmacro radian-bind-key (key-name command)
  "Bind a key in `radian-keymap'.
KEY-NAME, COMMAND, and PREDICATE are as in `bind-key'."
  `(leaf-key ,key-name ,command 'radian-keymap))

(defun radian-join-keys (&rest keys)
  "Join key sequences KEYS. Empty strings and nils are discarded.
\(radian--join-keys \"\\[radian-keymap] e\" \"e i\")
  => \"\\[radian-keymap] e e i\"
\(radian--join-keys \"\\[radian-keymap]\" \"\" \"e i\")
  => \"\\[radian-keymap] e i\""
  (string-join (remove "" (mapcar #'string-trim (remove nil keys))) " "))

(radian-defadvice radian--quoted-insert-allow-quit (quoted-insert &rest args)
  :around #'quoted-insert
  "Allow quitting out of \\[quoted-insert] with \\[keyboard-quit]."
  (radian-flet ((defun insert-and-inherit (&rest args)
                  (dolist (arg args)
                    (when (equal arg ?\C-g)
                      (signal 'quit nil)))
                  (apply insert-and-inherit args)))
    (apply quoted-insert args)))

;; Package `which-key' displays the key bindings and associated
;; commands following the currently-entered key prefix in a popup.
(leaf which-key
  :config

  ;; We configure it so that `which-key' is triggered by typing C-h
  ;; during a key sequence (the usual way to show bindings). See
  ;; <https://github.com/justbur/emacs-which-key#manual-activation>.
  ;; (setq which-key-show-early-on-C-h t)
  ;; (setq which-key-idle-delay most-positive-fixnum)
  ;; (setq which-key-idle-secondary-delay 1e-100)

  (which-key-mode +1)

  :blackout t)

;;; Environment
;;;; Environment variables

;; Unix tools look for HOME, but this is normally not defined on Windows.
(when (and IS-WINDOWS (null (getenv "HOME")))
  (setenv "HOME" (getenv "USERPROFILE")))

(defvar radian--env-setup-p nil
  "Non-nil if `radian-env-setup' has completed at least once.")

(defun radian-env-setup (&optional again)
  "Load ~/.profile and set environment variables exported therein.
Only do this once, unless AGAIN is non-nil."
  (interactive (list 'again))
  ;; No need to worry about race conditions because Elisp isn't
  ;; concurrent (yet).
  (unless (and radian--env-setup-p (not again))
    (let (;; Current directory may not exist in certain horrifying
          ;; circumstances (yes, this has happened in practice).
          (default-directory "/")
          (profile-file "~/.profile")
          (buf-name " *radian-env-output*"))
      (when (and profile-file
                 (file-exists-p profile-file)
                 (executable-find "python3"))
        (ignore-errors (kill-buffer buf-name))
        (with-current-buffer (get-buffer-create buf-name)
          (let* ((python-script
                  (expand-file-name "scripts/print_env.py" radian-directory))
                 (delimiter (radian--random-string))
                 (sh-script (format ". %s && %s %s"
                                    (shell-quote-argument
                                     (expand-file-name profile-file))
                                    (shell-quote-argument python-script)
                                    (shell-quote-argument delimiter)))
                 (return (call-process "sh" nil t nil "-c" sh-script))
                 (found-delimiter
                  (progn
                    (goto-char (point-min))
                    (search-forward delimiter nil 'noerror))))
            (if (and (= 0 return) found-delimiter)
                (let* ((results (split-string
                                 (buffer-string) (regexp-quote delimiter)))
                       (results (cl-subseq results 1 (1- (length results)))))
                  (if (cl-evenp (length results))
                      (progn
                        (cl-loop for (var value) on results by #'cddr do
                                 (setenv var value)
                                 (when (string= var "PATH")
                                   (setq exec-path (append
                                                    (parse-colon-path value)
                                                    (list exec-directory)))))
                        (setq radian--env-setup-p t))
                    (message
                     "Loading %s produced malformed result; see buffer %S"
                     profile-file
                     buf-name)))
              (message "Failed to load %s; see buffer %S"
                       profile-file
                       buf-name))))))))

(defvar radian--env-setup-timer
  (run-at-time 1 nil #'radian-env-setup)
  "Timer used to run `radian-env-setup'.
We (mostly) don't need environment variables to be set correctly
during init, so deferring their processing saves some time at
startup.")

;;;; Clipboard integration
;; On macOS, clipboard integration works out of the box in windowed
;; mode but not terminal mode. The following code to fix it was
;; originally based on [1], and then modified based on [2].
;;
;; [1]: https://gist.github.com/the-kenny/267162
;; [2]: https://emacs.stackexchange.com/q/26471/12534
(when IS-MAC
  (unless (display-graphic-p)

    (defvar radian--clipboard-last-copy nil
      "The last text that was copied to the system clipboard.
This is used to prevent duplicate entries in the kill ring.")

    (eval-and-compile
      (defun radian--clipboard-paste ()
        "Return the contents of the macOS clipboard, as a string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Command pbpaste returns the clipboard contents as a
               ;; string.
               (text (shell-command-to-string "pbpaste")))
          ;; If this function returns nil then the system clipboard is
          ;; ignored and the first element in the kill ring (which, if
          ;; the system clipboard has not been modified since the last
          ;; kill, will be the same) is used instead. Including this
          ;; `unless' clause prevents you from getting the same text
          ;; yanked the first time you run `yank-pop'.
          (unless (string= text radian--clipboard-last-copy)
            text)))

      (defun radian--clipboard-copy (text)
        "Set the contents of the macOS clipboard to given TEXT string."
        (let* (;; Setting `default-directory' to a directory that is
               ;; sure to exist means that this code won't error out
               ;; when the directory for the current buffer does not
               ;; exist.
               (default-directory "/")
               ;; Setting `process-connection-type' makes Emacs use a pipe to
               ;; communicate with pbcopy, rather than a pty (which is
               ;; overkill).
               (process-connection-type nil)
               ;; The nil argument tells Emacs to discard stdout and
               ;; stderr. Note, we aren't using `call-process' here
               ;; because we want this command to be asynchronous.
               ;;
               ;; Command pbcopy writes stdin to the clipboard until it
               ;; receives EOF.
               (proc (start-process "pbcopy" nil "pbcopy")))
          (process-send-string proc text)
          (process-send-eof proc))
        (setq radian--clipboard-last-copy text)))

    (setq interprogram-paste-function #'radian--clipboard-paste)
    (setq interprogram-cut-function #'radian--clipboard-copy)))

;; If you have something on the system clipboard, and then kill
;; something in Emacs, then by default whatever you had on the system
;; clipboard is gone and there is no way to get it back. Setting the
;; following option makes it so that when you kill something in Emacs,
;; whatever was previously on the system clipboard is pushed into the
;; kill ring. This way, you can paste it with `yank-pop'.
(setq save-interprogram-paste-before-kill t)

(radian-defadvice radian--advice-gui-get-selection-quietly (func &rest args)
  :around #'gui-selection-value
  "Disable an annoying message emitted when Emacs can't yank something.
In particular, if you have an image on your system clipboard and
you either yank or kill (as `save-interprogram-paste-before-kill'
means Emacs will try to put the system clipboard contents into
the kill ring when you kill something new), you'll get the
message 'gui-get-selection: (error \"Selection owner couldn't
convert\" UTF8_STRING)'. Disable that."
  (radian--with-silent-message "Selection owner couldn't convert"
    (apply func args)))

;;;; Mouse integration

;; Scrolling is way too fast on macOS with Emacs 27 and on Linux in
;; general. Decreasing the number of lines we scroll per mouse event
;; improves the situation. Normally, holding shift allows this slower
;; scrolling; instead, we make it so that holding shift accelerates
;; the scrolling.
(setq mouse-wheel-scroll-amount
      '(1 ((shift) . 5) ((control))))

;; Mouse integration works out of the box in windowed mode but not
;; terminal mode. The following code to fix it was based on
;; <https://stackoverflow.com/a/8859057/3538165>.
(unless (display-graphic-p)

  ;; Enable basic mouse support (click and drag).
  (xterm-mouse-mode t)

  ;; Note that the reason for the next two functions is that
  ;; `scroll-down' and `scroll-up' scroll by a "near full screen"
  ;; by default, whereas we want a single line.

  (eval-and-compile
    (defun radian-scroll-down ()
      "Scroll down one line."
      (interactive)
      (scroll-down 1))

    (defun radian-scroll-up ()
      "Scroll up one line."
      (interactive)
      (scroll-up 1)))

  ;; Enable scrolling with the mouse wheel.
  (leaf-key "<mouse-4>" #'radian-scroll-down)
  (leaf-key "<mouse-5>" #'radian-scroll-up))

;;; Candidate selection

;; Allow doing a command that requires candidate-selection when you
;; are already in the middle of candidate-selection. Sometimes it's
;; handy!
(setq enable-recursive-minibuffers t)

(radian-defadvice radian--advice-eval-expression-save-garbage
    (func prompt &optional initial-contents keymap read &rest args)
  :around #'read-from-minibuffer
  "Save user input in history even if it's not a valid sexp.
We do this by forcing `read-from-minibuffer' to always be called
with a nil value for READ, and then handling the effects of READ
ourselves."
  (let ((input (apply func prompt initial-contents keymap nil args)))
    (when read
      ;; This is based on string_to_object in minibuf.c.
      (let ((result (read-from-string input)))
        (unless (string-match-p
                 "\\`[ \t\n]*\\'" (substring input (cdr result)))
          (signal
           'invalid-read-syntax
           '("Trailing garbage following expression")))
        (setq input (car result))))
    input))

;; Package `selectrum' is an incremental completion and narrowing
;; framework. Like Ivy and Helm, which it improves on, Selectrum
;; provides a user interface for choosing from a list of options by
;; typing a query to narrow the list, and then selecting one of the
;; remaining candidates. This offers a significant improvement over
;; the default Emacs interface for candidate selection.
(leaf selectrum
  :straight (selectrum :host github :repo "raxod502/selectrum")
  :init

  ;; This doesn't actually load Selectrum.
  (selectrum-mode +1))

;; Package `prescient' is a library for intelligent sorting and
;; filtering in various contexts.
(leaf prescient
  :require t
  :config

  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1)

  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (setq prescient-history-length 1000))

;; Package `selectrum-prescient' provides intelligent sorting and
;; filtering for candidates in Selectrum menus.
(leaf selectrum-prescient
  :straight (selectrum-prescient :host github :repo "raxod502/prescient.el"
                   :files ("selectrum-prescient.el"))
  :after selectrum
  :config

  (selectrum-prescient-mode +1))

;;; Window management

;; Prevent accidental usage of `list-buffers'.
(leaf-key "C-x C-b" #'switch-to-buffer)
(leaf-key "C-x b"   #'list-buffers)

(declare-function minibuffer-keyboard-quit "delsel")
(radian-defadvice radian--advice-keyboard-quit-minibuffer-first
    (keyboard-quit)
  :around #'keyboard-quit
  "Cause \\[keyboard-quit] to exit the minibuffer, if it is active.
Normally, \\[keyboard-quit] will just act in the current buffer.
This advice modifies the behavior so that it will instead exit an
active minibuffer, even if the minibuffer is not selected."
  (if-let ((minibuffer (active-minibuffer-window)))
      (with-current-buffer (window-buffer minibuffer)
        (minibuffer-keyboard-quit))
    (funcall keyboard-quit)))

(radian-defadvice radian--advice-kill-buffer-maybe-kill-window
    (func &optional buffer-or-name kill-window-too)
  :around #'kill-buffer
  "Make it so \\[universal-argument] \\[kill-buffer] kills the window too."
  (interactive
   (lambda (spec)
     (append (or (advice-eval-interactive-spec spec) '(nil))
             current-prefix-arg)))
  (if kill-window-too
      (with-current-buffer buffer-or-name
        (kill-buffer-and-window))
    (funcall func buffer-or-name)))

;; Package `swsw' provides lightway to navigate windows.
(leaf swsw
  :straight (swsw :repo "https://git.sr.ht/~dsemy/swsw")
  :chord (",," . swsw-select)
  :custom
  (swsw-id-chars . '(?a ?o ?e ?u ?h ?t ?n ?s))
  (swsw-scope . 'visible)
  :config
  (swsw-mode +1)
  (swsw-mode-line-conditional-display-function t)
  :blackout t)

;; rebind `delete-window'
(global-set-key (kbd "C-x C-0") #'delete-window)

; Feature `winner' provides an undo/redo stack for window
;; configurations, with undo and redo being C-c left and C-c right,
;; respectively. (Actually "redo" doesn't revert a single undo, but
;; rather a whole sequence of them.) For instance, you can use C-x 1
;; to focus on a particular window, then return to your previous
;; layout with C-c left.
(leaf! winner
  :config

  (winner-mode +1))

;; Feature `ibuffer' provides a more modern replacement for the
;; `list-buffers' command.
(leaf! ibuffer
  :bind (([remap list-buffers] . ibuffer)))

;;; Finding files

;; Follow symlinks when opening files. This has the concrete impact,
;; for instance, that when you edit init.el with M-P e e i and then
;; later do C-x C-f, you will be in the Radian repository instead of
;; your home directory.
(setq find-file-visit-truename t)

;; Disable the warning "X and Y are the same file" which normally
;; appears when you visit a symlinked file by the same name. (Doing
;; this isn't dangerous, as it will just redirect you to the existing
;; buffer.)
(setq find-file-suppress-same-file-warnings t)

;; Feature `saveplace' provides a minor mode for remembering the
;; location of point in each file you visit, and returning it there
;; when you find the file again.
(leaf! saveplace
  :config
  (save-place-mode +1)

  (radian-defadvice radian--advice-save-place-quickly-and-silently
      (func &rest args)
    :around #'save-place-alist-to-file
    "Make `save-place' save more quickly and silently."
    (radian--with-silent-write
      (cl-letf (((symbol-function #'pp) #'prin1))
        (apply func args)))))

;; Package `projectile' keeps track of a "project" list, which is
;; automatically added to as you visit Git repositories, Node.js
;; projects, etc. It then provides commands for quickly navigating
;; between and within these projects.
(leaf projectile
  :defvar projectile-completion-system projectile-switch-project-action
  projectile-command-map
  :init
  (defun radian--projectile-indexing-method-p (method)
    "Non-nil if METHOD is a safe value for `projectile-indexing-method'."
    (memq method '(native alien)))

  (run-with-idle-timer 1 nil #'require 'projectile)

  :bind*   ("C-c p" . projectile-command-map)
  :defer-config

  ;; Use Selectrum (via `completing-read') for Projectile instead of
  ;; IDO.
  (setq projectile-completion-system 'default)

  ;; When switching projects, give the option to choose what to do.
  ;; This is a way better interface than having to remember ahead of
  ;; time to use a prefix argument on `projectile-switch-project'
  ;; (because, and please be honest here, when was the last time you
  ;; actually remembered to do that?).
  (setq projectile-switch-project-action 'projectile-commander)

  ;; Enable the mode again now that we have all the supporting hooks
  ;; and stuff defined.
  (projectile-mode +1)

  (put 'projectile-indexing-method 'safe-local-variable
       #'radian--projectile-indexing-method-p)

  ;; Can't bind M-r because some genius bound ESC. *Never* bind ESC!
  (dolist (key '("C-r" "R"))
    `(leaf-key ,key #'projectile-replace-regexp projectile-command-map))

  :blackout t)

;; `project.el' built-in package.
(leaf! project
  :init
  (setq project-switch-commands
        '((?f "Find file" project-find-file)
          (?g "Find regexp" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?q "Query replace" project-query-replace-regexp)
          (?v "VC-Dir" project-vc-dir)
          (?e "Eshell" project-eshell)))
  :blackout t)

(defvar radian--dirs-to-delete nil
  "List of directories to try to delete when killing buffer.
This is used to implement the neat feature where if you kill a
new buffer without saving it, then Radian will prompt to see if
you want to also delete the parent directories that were
automatically created.")

(defun radian--advice-find-file-create-directories
    (find-file filename &rest args)
  "Automatically create and delete parent directories of new files.
This advice automatically creates the parent directory (or directories) of
the file being visited, if necessary. It also sets a buffer-local
variable so that the user will be prompted to delete the newly
created directories if they kill the buffer without saving it.

This advice has no effect for remote files.

This is an `:around' advice for `find-file' and similar
functions.

FIND-FILE is the original `find-file'; FILENAME and ARGS are its
arguments."
  (if (file-remote-p filename)
      (apply find-file filename args)
    (let ((orig-filename filename)
          ;; For relative paths where none of the named parent
          ;; directories exist, we might get a nil from
          ;; `file-name-directory' below, which would be bad. Thus we
          ;; expand the path fully.
          (filename (expand-file-name filename))
          ;; The variable `dirs-to-delete' is a list of the
          ;; directories that will be automatically created by
          ;; `make-directory'. We will want to offer to delete these
          ;; directories if the user kills the buffer without saving
          ;; it.
          (dirs-to-delete ()))
      ;; If the file already exists, we don't need to worry about
      ;; creating any directories.
      (unless (file-exists-p filename)
        ;; It's easy to figure out how to invoke `make-directory',
        ;; because it will automatically create all parent
        ;; directories. We just need to ask for the directory
        ;; immediately containing the file to be created.
        (let* ((dir-to-create (file-name-directory filename))
               ;; However, to find the exact set of directories that
               ;; might need to be deleted afterward, we need to
               ;; iterate upward through the directory tree until we
               ;; find a directory that already exists, starting at
               ;; the directory containing the new file.
               (current-dir dir-to-create))
          ;; If the directory containing the new file already exists,
          ;; nothing needs to be created, and therefore nothing needs
          ;; to be destroyed, either.
          (while (not (file-exists-p current-dir))
            ;; Otherwise, we'll add that directory onto the list of
            ;; directories that are going to be created.
            (push current-dir dirs-to-delete)
            ;; Now we iterate upwards one directory. The
            ;; `directory-file-name' function removes the trailing
            ;; slash of the current directory, so that it is viewed as
            ;; a file, and then the `file-name-directory' function
            ;; returns the directory component in that path (which
            ;; means the parent directory).
            (setq current-dir (file-name-directory
                               (directory-file-name current-dir))))
          ;; Only bother trying to create a directory if one does not
          ;; already exist.
          (unless (file-exists-p dir-to-create)
            ;; Make the necessary directory and its parents.
            (make-directory dir-to-create 'parents))))
      ;; Call the original `find-file', now that the directory
      ;; containing the file to found exists. We make sure to preserve
      ;; the return value, so as not to mess up any commands relying
      ;; on it.
      (prog1 (apply find-file orig-filename args)
        ;; If there are directories we want to offer to delete later,
        ;; we have more to do.
        (when dirs-to-delete
          ;; Since we already called `find-file', we're now in the
          ;; buffer for the new file. That means we can transfer the
          ;; list of directories to possibly delete later into a
          ;; buffer-local variable. But we pushed new entries onto the
          ;; beginning of `dirs-to-delete', so now we have to reverse
          ;; it (in order to later offer to delete directories from
          ;; innermost to outermost).
          (setq-local radian--dirs-to-delete (reverse dirs-to-delete))
          ;; Now we add a buffer-local hook to offer to delete those
          ;; directories when the buffer is killed, but only if it's
          ;; appropriate to do so (for instance, only if the
          ;; directories still exist and the file still doesn't
          ;; exist).
          (add-hook 'kill-buffer-hook
                    #'radian--kill-buffer-delete-directory-if-appropriate
                    'append 'local)
          ;; The above hook removes itself when it is run, but that
          ;; will only happen when the buffer is killed (which might
          ;; never happen). Just for cleanliness, we automatically
          ;; remove it when the buffer is saved. This hook also
          ;; removes itself when run, in addition to removing the
          ;; above hook.
          (add-hook 'after-save-hook
                    #'radian--remove-kill-buffer-delete-directory-hook
                    'append 'local))))))

(defun radian--kill-buffer-delete-directory-if-appropriate ()
  "Delete parent directories if appropriate.
This is a function for `kill-buffer-hook'. If
`radian--advice-find-file-create-directories' created the
directory containing the file for the current buffer
automatically, then offer to delete it. Otherwise, do nothing.
Also clean up related hooks."
  (when (and
         ;; Stop if the local variables have been killed.
         (boundp 'radian--dirs-to-delete)
         ;; Stop if there aren't any directories to delete (shouldn't
         ;; happen).
         radian--dirs-to-delete
         ;; Stop if `radian--dirs-to-delete' somehow got set to
         ;; something other than a list (shouldn't happen).
         (listp radian--dirs-to-delete)
         ;; Stop if the current buffer doesn't represent a
         ;; file (shouldn't happen).
         buffer-file-name
         ;; Stop if the buffer has been saved, so that the file
         ;; actually exists now. This might happen if the buffer were
         ;; saved without `after-save-hook' running, or if the
         ;; `find-file'-like function called was `write-file'.
         (not (file-exists-p buffer-file-name)))
    (cl-dolist (dir-to-delete radian--dirs-to-delete)
      ;; Ignore any directories that no longer exist or are malformed.
      ;; We don't return immediately if there's a nonexistent
      ;; directory, because it might still be useful to offer to
      ;; delete other (parent) directories that should be deleted. But
      ;; this is an edge case.
      (when (and (stringp dir-to-delete)
                 (file-exists-p dir-to-delete))
        ;; Only delete a directory if the user is OK with it.
        (if (y-or-n-p (format "Also delete directory `%s'? "
                              ;; The `directory-file-name' function
                              ;; removes the trailing slash.
                              (directory-file-name dir-to-delete)))
            (delete-directory dir-to-delete)
          ;; If the user doesn't want to delete a directory, then they
          ;; obviously don't want to delete any of its parent
          ;; directories, either.
          (cl-return)))))
  ;; It shouldn't be necessary to remove this hook, since the buffer
  ;; is getting killed anyway, but just in case...
  (radian--remove-kill-buffer-delete-directory-hook))

(defun radian--remove-kill-buffer-delete-directory-hook ()
  "Clean up directory-deletion hooks, if necessary.
This is a function for `after-save-hook'. Remove
`radian--kill-buffer-delete-directory-if-appropriate' from
`kill-buffer-hook', and also remove this function from
`after-save-hook'."
  (remove-hook 'kill-buffer-hook
               #'radian--kill-buffer-delete-directory-if-appropriate
               'local)
  (remove-hook 'after-save-hook
               #'radian--remove-kill-buffer-delete-directory-hook
               'local))

(dolist (fun '(find-file           ; C-x C-f
               find-alternate-file ; C-x C-v
               write-file          ; C-x C-w
               ))
  (advice-add fun :around #'radian--advice-find-file-create-directories))

(defmacro radian-register-dotfile
    (filename &optional keybinding pretty-filename)
  "Establish functions and keybindings to open a dotfile.

The FILENAME should be a path relative to the user's home
directory. Two interactive functions are created: one to find the
file in the current window, and one to find it in another window.

If KEYBINDING is non-nil, the first function is bound to that key
sequence after it is prefixed by \"\\[radian-keymap] e\", and the
second function is bound to the same key sequence, but prefixed
instead by \"\\[radian-keymap] o\".

This is best demonstrated by example. Suppose FILENAME is
\".emacs.d/init.el\" and KEYBINDING is \"e i\". Then
`radian-register-dotfile' will create the interactive functions
`radian-find-init-el' and `radian-find-init-el-other-window', and
it will bind them to the key sequences \"\\[radian-keymap] e e
i\" and \"\\[radian-keymap] o e i\" respectively.

If PRETTY-FILENAME, a string, is non-nil, then it will be used in
place of \"init-el\" in this example. Otherwise, that string will
be generated automatically from the basename of FILENAME.

To pass something other than a literal string as FILENAME,
unquote it using a comma."
  (when (and (listp filename) (eq (car filename) '\,))
    (setq filename (eval (cadr filename))))
  (let* ((bare-filename (replace-regexp-in-string ".*/" "" filename))
         (full-filename (expand-file-name filename "~"))
         (defun-name (intern
                      (replace-regexp-in-string
                       "-+"
                       "-"
                       (concat
                        "radian-find-"
                        (or pretty-filename
                            (replace-regexp-in-string
                             "[^a-z0-9]" "-"
                             (downcase
                              bare-filename)))))))
         (defun-other-window-name
           (intern
            (concat (symbol-name defun-name)
                    "-other-window")))
         (docstring (format "Edit file %s."
                            full-filename))
         (docstring-other-window
          (format "Edit file %s, in another window."
                  full-filename))
         (defun-form `(defun ,defun-name ()
                        ,docstring
                        (interactive)
                        (when (or (file-exists-p ,full-filename)
                                  (yes-or-no-p
                                   ,(format
                                     "Does not exist, really visit %s? "
                                     (file-name-nondirectory
                                      full-filename))))
                          (find-file ,full-filename))))
         (defun-other-window-form
           `(defun ,defun-other-window-name ()
              ,docstring-other-window
              (interactive)
              (when (or (file-exists-p ,full-filename)
                        (yes-or-no-p
                         ,(format
                           "Does not exist, really visit %s? "
                           (file-name-nondirectory
                            full-filename))))
                (find-file-other-window ,full-filename))))
         (full-keybinding
          (when keybinding
            (radian-join-keys "e" keybinding)))
         (full-other-window-keybinding
          (radian-join-keys "o" keybinding)))
    `(progn
       ,defun-form
       ,defun-other-window-form
       ,@(when full-keybinding
           `((radian-bind-key ,full-keybinding #',defun-name)))
       ,@(when full-other-window-keybinding
           `((radian-bind-key
              ,full-other-window-keybinding
              #',defun-other-window-name)))
       ;; Return the symbols for the two functions defined.
       (list ',defun-name ',defun-other-window-name))))

;; Now we register shortcuts to files relevant to Radian.

(radian-register-dotfile ,radian-directory "r a" "radian-repo")

;; Emacs
(radian-register-dotfile
 ,(expand-file-name "init.el" user-emacs-directory)
 "e i")
(radian-register-dotfile
 ,(expand-file-name "early-init.el" user-emacs-directory)
 "e e")
(radian-register-dotfile
 ,(expand-file-name "emacs/radian.el" radian-directory)
 "e r")
(radian-register-dotfile
 ,(expand-file-name "straight/versions/radian.el" user-emacs-directory)
 "e v" "radian-versions-el")
(radian-register-dotfile
 ,(expand-file-name "init.local.el" user-emacs-directory) "e l")
(radian-register-dotfile
 ,(expand-file-name "straight/versions/radian-local.el" user-emacs-directory)
 "e V" "radian-local-versions-el")

;; Git
(radian-register-dotfile ".gitconfig" "g c")
(radian-register-dotfile ".gitexclude" "g e")
(radian-register-dotfile ".gitconfig.local" "g l")

;; Shell
(radian-register-dotfile ".profile" "p r")
(radian-register-dotfile ".profile.local" "p l")

;; Tmux
(radian-register-dotfile ".tmux.conf" "t c")
(radian-register-dotfile ".tmux.local.conf" "t l")

;; Zsh
(radian-register-dotfile ".zshrc" "z r")
(radian-register-dotfile ".zshrc.local" "z l")

;; Feature `auth-source' reads and writes secrets from files like
;; ~/.netrc for TRAMP and related packages, so for example you can
;; avoid having to type in a particular host's password every time.
(leaf! auth-source
  :config

  (defvar radian--auth-source-blacklist-file
    (no-littering-expand-var-file-name "auth-source/blacklist.el")
    "File to store `auth-source' user blacklist.
The contents are a list of MD5 hashes, one for each potential
password that the user has decided not to save.")

  (radian-defadvice radian--advice-auth-source-persist-blacklist
      (func file add)
    :around #'auth-source-netrc-saver
    "Allow user to permanently disable prompt to save credentials."
    (let* ((key (format "%s %s" file (rfc2104-hash 'md5 64 16 file add)))
           (blacklist
            (ignore-errors
              (with-temp-buffer
                (insert-file-contents radian--auth-source-blacklist-file)
                (read (current-buffer))))))
      (unless (listp blacklist)
        (setq blacklist nil))
      (if (member key blacklist)
          ?n
        (radian-flet ((defun auth-source-read-char-choice (prompt choices)
                        (let ((choice (funcall auth-source-read-char-choice
                                               prompt choices)))
                          (when (= choice ?N)
                            (push key blacklist)
                            (make-directory
                             (file-name-directory
                              radian--auth-source-blacklist-file)
                             'parents)
                            (with-temp-file radian--auth-source-blacklist-file
                              (print blacklist (current-buffer)))
                            (setq choice ?n))
                          choice)))
          (funcall func file add))))))

;;; Saving files

;; Don't make backup files.
(setq make-backup-files nil)

;; Don't make autosave files.
(setq auto-save-default nil)

;; Don't make lockfiles.
(setq create-lockfiles nil)

(defun radian-set-executable-permission (allowed)
  "Enable or disable executable permission on the current file.
If ALLOWED is non-nil, enable permission; otherwise, disable
permission."
  (interactive (list (not current-prefix-arg)))
  (unless buffer-file-name
    (user-error "This buffer is not visiting a file"))
  (with-demoted-errors "Could not set permissions: %S"
    (set-file-modes buffer-file-name (file-modes-symbolic-to-number
                                      (if allowed
                                          "+x"
                                        "-x")
                                      (file-modes buffer-file-name)))
    (message "Executable permission %s"
             (if allowed "enabled" "disabled"))))

(leaf-key* "s-x" #'radian-set-executable-permission)

;;; Editing
;;;; Text formatting

(add-to-list 'safe-local-variable-values '(auto-fill-function . nil))

(add-to-list 'safe-local-eval-forms '(visual-line-mode +1))

(blackout 'visual-line-mode)

;; When region is active, make `capitalize-word' and friends act on
;; it.
(leaf-key "M-c" #'capitalize-dwim)
(leaf-key "M-l" #'downcase-dwim)
(leaf-key "M-u" #'upcase-dwim)

(defun radian-reverse-region-characters (beg end)
  "Reverse the characters in the region from BEG to END.
Interactively, reverse the characters in the current region."
  (interactive "*r")
  (insert
   (reverse
    (delete-and-extract-region
     beg end))))

;; When filling paragraphs, assume that sentences end with one space
;; rather than two.
(setq sentence-end-double-space nil)

;; Trigger auto-fill after punctutation characters, not just
;; whitespace.
(mapc
 (lambda (c)
   (set-char-table-range auto-fill-chars c t))
 "!-=+]};:'\",.?")

;; We could maybe use the variable `comment-auto-fill-only-comments'
;; for this, but I wrote this code before I knew about it. Also, I'm
;; not sure how well it handles the edge cases for docstrings and
;; such.
(eval-when! (version<= "26" emacs-version)
  (radian-defadvice radian--advice-auto-fill-only-text (func &rest args)
    :around #'internal-auto-fill
    "Only perform auto-fill in text, comments, or docstrings."
    (cl-block nil
      ;; Don't auto-fill on the first line of a docstring, since it
      ;; shouldn't be wrapped into the body.
      (when (and (derived-mode-p #'emacs-lisp-mode)
                 (eq (get-text-property (point) 'face) 'font-lock-doc-face)
                 (save-excursion
                   (beginning-of-line)
                   (looking-at-p "[[:space:]]*\"")))
        (cl-return))
      (when (and (derived-mode-p 'text-mode)
                 (not (derived-mode-p 'yaml-mode)))
        (apply func args)
        (cl-return))
      ;; Inspired by <https://emacs.stackexchange.com/a/14716/12534>.
      (when-let ((faces (save-excursion
                          ;; In `web-mode', the end of the line isn't
                          ;; fontified, so we have to step backwards
                          ;; by one character before checking the
                          ;; properties.
                          (ignore-errors
                            (backward-char))
                          (get-text-property (point) 'face))))
        (unless (listp faces)
          (setq faces (list faces)))
        (when (cl-some
               (lambda (face)
                 (memq face '(font-lock-comment-face
                              font-lock-comment-delimiter-face
                              font-lock-doc-face
                              web-mode-javascript-comment-face)))
               faces)
          ;; Fill Elisp docstrings to the appropriate column. Why
          ;; docstrings are filled to a different column, I don't know.
          (let ((fill-column (if (and
                                  (derived-mode-p #'emacs-lisp-mode)
                                  (memq 'font-lock-doc-face faces))
                                 emacs-lisp-docstring-fill-column
                               fill-column)))
            (apply func args)))))))

(blackout 'auto-fill-mode)

(defun radian--do-auto-fill ()
  "Replacement for `do-auto-fill' that respects `normal-auto-fill-function'.
The reason we need this is that in order to enable auto-fill
globally, we are supposed to set the default value of variable
`auto-fill-function'. However, some major modes set
`normal-auto-fill-function' (itself normally set to
`do-auto-fill', which is what we generally set the default value
of variable `auto-fill-function' to), expecting `auto-fill-mode'
to be enabled afterwards (which copies the value of
`normal-auto-fill-function' into variable `auto-fill-function').
However, since we enable auto-fill globally by means of setting
variable `auto-fill-function' directly, this setting gets lost.
The workaround is to set variable `auto-fill-function' globally
to a function which looks up the value of
`normal-auto-fill-function' \(generally just `do-auto-fill') and
calls that. This is a slight inversion of the usual flow of
control and might make you slightly uncomfortable, but we'll just
have to live with it :3"
  (funcall normal-auto-fill-function))

;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Turning-on-auto_002dfill-by-default.html
(setq-default auto-fill-function #'radian--do-auto-fill)

(define-minor-mode radian-fix-whitespace-mode
  "Minor mode to automatically fix whitespace on save.
If enabled, then saving the buffer deletes all trailing
whitespace and ensures that the file ends with exactly one
newline."
  nil nil nil
  (if radian-fix-whitespace-mode
      (progn
        (setq require-final-newline t)
        (add-hook 'before-save-hook #'delete-trailing-whitespace nil 'local))
    (setq require-final-newline nil)
    (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local)))

(define-globalized-minor-mode radian-fix-whitespace-global-mode
  radian-fix-whitespace-mode radian-fix-whitespace-mode)

(radian-fix-whitespace-global-mode +1)

(put 'radian-fix-whitespace-mode 'safe-local-variable #'booleanp)

;; Feature `newcomment' provides commands for commenting an
;; uncommenting code, and editing comments.
(leaf! newcomment
  :bind (([remap default-indent-new-line] . radian-continue-comment))
  :config

  (defun radian-continue-comment ()
    "Continue current comment, preserving trailing whitespace.
This differs from `default-indent-new-line' in the following way:

If you have a comment like \";; Some text\" with point at the end
of the line, then running `default-indent-new-line' will get you
a new line with \";; \", but running it again will get you a line
with only \";;\" (no trailing whitespace). This is annoying for
inserting a new paragraph in a comment. With this command, the
two inserted lines are the same."
    (interactive)
    ;; `default-indent-new-line' uses `delete-horizontal-space'
    ;; because in auto-filling we want to avoid the space character at
    ;; the end of the line from being put at the beginning of the next
    ;; line. But when continuing a comment it's not desired.
    (cl-letf (((symbol-function #'delete-horizontal-space) #'ignore))
      (default-indent-new-line))))

;; Feature `whitespace' provides a minor mode for highlighting
;; whitespace in various special ways.
(leaf! whitespace
  :init

  (define-minor-mode radian-highlight-long-lines-mode
    "Minor mode for highlighting long lines."
    nil nil nil
    (if radian-highlight-long-lines-mode
        (progn
          (setq-local whitespace-style '(face lines-tail))
          (setq-local whitespace-line-column 79)
          (whitespace-mode +1))
      (whitespace-mode -1)
      (kill-local-variable 'whitespace-style)
      (kill-local-variable 'whitespace-line-column)))

  :blackout t)

;; Feature `outline' provides major and minor modes for collapsing
;; sections of a buffer into an outline-like format.
(leaf! outline
  :config

  (define-globalized-minor-mode global-outline-minor-mode
    outline-minor-mode outline-minor-mode)

  (global-outline-minor-mode +1)

  :blackout outline-minor-mode)

;;;; Kill and yank
(radian-defadvice radian--advice-stop-kill-at-whitespace
    (kill-line &rest args)
  :around #'kill-line
  "Prevent `kill-line' from killing through whitespace to a newline.
This affects the case where you press \\[kill-line] when point is
followed by some whitespace and then a newline. Without this
advice, \\[kill-line] will kill both the whitespace and the
newline, which is inconsistent with its behavior when the
whitespace is replaced with non-whitespace. With this advice,
\\[kill-line] will kill just the whitespace, and another
invocation will kill the newline."
  (let ((show-trailing-whitespace t))
    (apply kill-line args)))

;; Eliminate duplicates in the kill ring. That is, if you kill the
;; same thing twice, you won't have to use M-y twice to get past it to
;; older entries in the kill ring.
(setq kill-do-not-save-duplicates t)

(radian-defadvice radian--advice-disallow-password-copying (func &rest args)
  :around #'read-passwd
  "Don't allow copying a password to the kill ring."
  (cl-letf (((symbol-function #'kill-new) #'ignore)
            ((symbol-function #'kill-append) #'ignore))
    (apply func args)))

;; Feature `delsel' provides an alternative behavior for certain
;; actions when you have a selection active. Namely: if you start
;; typing when you have something selected, then the selection will be
;; deleted; and if you press DEL while you have something selected, it
;; will be deleted rather than killed. (Otherwise, in both cases the
;; selection is deselected and the normal function of the key is
;; performed.)
(leaf! delsel :config (delete-selection-mode -1))

;;;; Undo/redo

;; Feature `warnings' allows us to enable and disable warnings.
(leaf! warnings
  :require t
  :config

  ;; Ignore the warning we get when a huge buffer is reverted and the
  ;; undo information is too large to be recorded.
  (add-to-list 'warning-suppress-log-types '(undo discard-info)))

;; Package `undo-tree' replaces the default Emacs undo system, which
;; is poorly designed and hard to use, with a much more powerful
;; tree-based system. In basic usage, you don't even have to think
;; about the tree, because it acts like a conventional undo/redo
;; system. Bindings are C-/, M-/, and C-x u.

(leaf undo-tree
  :require t
  :bind (;; By default, `undo' (and by extension `undo-tree-undo') is
         ;; bound to C-_ and C-/, and `undo-tree-redo' is bound to
         ;; M-_. It's logical to also bind M-/ to `undo-tree-redo'.
         ;; This overrides the default binding of M-/, which is to
         ;; `dabbrev-expand'.
         :undo-tree-map
         ("M-/" . undo-tree-redo))
  :config

  (global-undo-tree-mode +1)

  (radian-defadvice radian--advice-suppress-undo-tree-buffer-modified-message
      (undo-tree-load-history &rest args)
    :around #'undo-tree-load-history
    "Suppress the annoying message saying undo history could not be loaded.
Normally, this message is printed when undo history could not be
loaded since the file was changed outside of Emacs."
    (let ((inhibit-message t))
      (apply undo-tree-load-history args)))

  ;; Disable undo-in-region. It sounds like a cool feature, but
  ;; unfortunately the implementation is very buggy and usually causes
  ;; you to lose your undo history if you use it by accident.
  (setq undo-tree-enable-undo-in-region nil)

  :blackout t)

;;;; Navigation

;; Feature `bookmark' provides a way to mark places in a buffer. I
;; don't use it, but some other packages do.
(leaf! bookmark
  :config

  (dolist (func '(bookmark-load bookmark-write-file))
    (advice-add func :around #'radian--advice-silence-messages)))

;; Package `ctrlf' provides a replacement for `isearch' that is more
;; similar to the tried-and-true text search interfaces in web
;; browsers and other programs (think of what happens when you type
;; ctrl+F).
(leaf ctrlf
  :straight (ctrlf :host github :repo "raxod502/ctrlf")
  :init

  (ctrlf-mode +1))

;; Feature `fileloop' provides the underlying machinery used to do
;; operations on multiple files, such as find-and-replace.
(eval-when! (version<= "27" emacs-version)
  (leaf! fileloop
    :config

    (radian-defadvice radian--advice-fileloop-find-all-matches
        (func &rest args)
      :around #'fileloop-initialize-replace
      "Fix a bug in `fileloop' that causes it to miss matches.
In particular, without this advice, doing a find-and-replace in
multiple files will miss any match that occurs earlier in a
visited file than point happens to be currently in that
buffer."
      (radian-flet ((defun perform-replace (&rest args)
                      (apply perform-replace
                             (append args (list (point-min) (point-max))))))
        (apply func args)))))

;; Package `visual-regexp' provides an alternate version of
;; `query-replace' which highlights matches and replacements as you
;; type.
(leaf visual-regexp
  :bind (([remap query-replace] . vr/query-replace)))

;; Package `visual-regexp-steroids' allows `visual-regexp' to use
;; regexp engines other than Emacs'; for example, Python or Perl
;; regexps.
(leaf visual-regexp-steroids
  :require t
  :after visual-regexp
  :bind (([remap query-replace-regexp] . radian-query-replace-literal))
  :defvar vr/engine
  :config

  ;; Use Emacs-style regular expressions by default, instead of
  ;; Python-style.
  (setq vr/engine 'emacs)

  (defun radian-query-replace-literal ()
    "Do a literal query-replace using `visual-regexp'."
    (interactive)
    (let ((vr/engine 'emacs-plain))
      (call-interactively #'vr/query-replace))))

;; Package `imeng-list' use the side buffer to display imenu
(leaf imenu-list
  :defvar imenu-list-focus-after-activation imenu-list-auto-resize
  :bind ("C-c i" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize t))

;;;; `cua' rectangle edit
(leaf! cua
  :init (cua-selection-mode t)
  ;; disable `delete-selection-mode'
  :custom (cua-delete-selection . nil))

;;; ediff
(after! ediff
  (setq ediff-diff-options "-w" ; turn off whitespace checking
        ediff-split-window-function #'split-window-horizontally
        ediff-window-setup-function #'ediff-setup-windows-plain)
  (defvar radian--ediff-saved-wconf nil)
  ;; Restore window config after quitting ediff
  (add-hook! 'ediff-before-setup-hook
    (defun radian-ediff-save-wconf-h ()
      (setq radian--ediff-saved-wconf (current-window-configuration))))
  (add-hook! '(ediff-quit-hook ediff-suspend-hook) :append
    (defun radian-ediff-restore-wconf-h ()
      (when (window-configuration-p radian--ediff-saved-wconf)
        (set-window-configuration radian--ediff-saved-wconf)))))

;;; Electricity: automatic things
;;;; Autorevert

;; On macOS, Emacs has a nice keybinding to revert the current buffer.
;; On other platforms such a binding is missing; we re-add it here.


;; Feature `autorevert' allows the use of file-watchers or polling in
;; order to detect when the file visited by a buffer has changed, and
;; optionally reverting the buffer to match the file (unless it has
;; unsaved changes).
(leaf! autorevert
  :init
  (run-with-idle-timer 2 nil #'require 'autorevert)
  (defun radian--autorevert-silence ()
    "Silence messages from `auto-revert-mode' in the current buffer."
    (setq-local auto-revert-verbose nil))

  :config

  ;; Turn the delay on auto-reloading from 5 seconds down to 1 second.
  ;; We have to do this before turning on `auto-revert-mode' for the
  ;; change to take effect. (Note that if we set this variable using
  ;; `customize-set-variable', all it does is toggle the mode off and
  ;; on again to make the change take effect, so that way is dumb.)
  (setq auto-revert-interval 1)

  (global-auto-revert-mode +1)

  ;; Auto-revert all buffers, not only file-visiting buffers. The
  ;; docstring warns about potential performance problems but this
  ;; should not be an issue since we only revert visible buffers.
  (setq global-auto-revert-non-file-buffers t)

  ;; Since we automatically revert all visible buffers after one
  ;; second, there's no point in asking the user whether or not they
  ;; want to do it when they find a file. This disables that prompt.
  (setq revert-without-query '(".*"))

  (defun radian-autorevert-inhibit-p (buffer)
    "Return non-nil if autorevert should be inhibited for BUFFER."
    (or (null (get-buffer-window))
        (with-current-buffer buffer
          (or (null buffer-file-name)
              (file-remote-p buffer-file-name)))))

  (eval-if! (version< emacs-version "27")
      (radian-defadvice radian--autorevert-only-visible
          (auto-revert-buffers &rest args)
        :around #'auto-revert-buffers
        "Inhibit `autorevert' for buffers not displayed in any window."
        (radian-flet ((defun buffer-list (&rest args)
                        (cl-remove-if
                         #'radian-autorevert-inhibit-p
                         (apply buffer-list args))))
          (apply auto-revert-buffers args)))
    (radian-defadvice radian--autorevert-only-visible (bufs)
      :filter-return #'auto-revert--polled-buffers
      "Inhibit `autorevert' for buffers not displayed in any window."
      (cl-remove-if #'radian-autorevert-inhibit-p bufs)))

  :blackout auto-revert-mode)

;;;; Automatic delimiter pairing

;; Package `smartparens' provides an API for manipulating paired
;; delimiters of many different types, as well as interactive commands
;; and keybindings for operating on paired delimiters at the
;; s-expression level. It provides a Paredit compatibility layer.
(leaf smartparens
  :require t
  :defun org-agenda-mode json-mode protobuf-mode clojure-mode
  ;; markdown-mode lsp
  ;; radian--advice-lsp-mode-silence company-explicit-action-p company--should-begin
  ;; company--should-continue global-company-mode
  :config

  ;; Load the default pair definitions for Smartparens.
  (require 'smartparens-config)

  ;; Enable Smartparens functionality in all buffers.
  (smartparens-global-mode +1)

  ;; When in Paredit emulation mode, Smartparens binds M-( to wrap the
  ;; following s-expression in round parentheses. By analogy, we
  ;; should bind M-[ to wrap the following s-expression in square
  ;; brackets. However, this breaks escape sequences in the terminal,
  ;; so it may be controversial upstream. We only enable the
  ;; keybinding in windowed mode.
  (when (display-graphic-p)
    (setf (map-elt sp-paredit-bindings "M-[") #'sp-wrap-square))

  ;; Set up keybindings for s-expression navigation and manipulation
  ;; in the style of Paredit.
  (sp-use-paredit-bindings)

  ;; Highlight matching delimiters.
  (show-smartparens-global-mode +1)

  ;; Prevent all transient highlighting of inserted pairs.
  (setq sp-highlight-pair-overlay nil)
  (setq sp-highlight-wrap-overlay nil)
  (setq sp-highlight-wrap-tag-overlay nil)

  ;; Don't disable autoskip when point moves backwards. (This lets you
  ;; open a sexp, type some things, delete some things, etc., and then
  ;; type over the closing delimiter as long as you didn't leave the
  ;; sexp entirely.)
  (setq sp-cancel-autoskip-on-backward-movement nil)

  ;; Disable Smartparens in Org-related modes, since the keybindings
  ;; conflict.

  (leaf! org
    :config

    (add-to-list 'sp-ignore-modes-list #'org-mode))

  (leaf! org-agenda
    :config

    (add-to-list 'sp-ignore-modes-list #'org-agenda-mode))

  ;; Make C-k kill the sexp following point in Lisp modes, instead of
  ;; just the current line.
  (leaf-key [remap kill-line] #'sp-kill-hybrid-sexp 'smartparens-mode-map)

  (defun radian--smartparens-indent-new-pair (&rest _)
    "Insert an extra newline after point, and reindent."
    (newline)
    (indent-according-to-mode)
    (forward-line -1)
    (indent-according-to-mode))

  ;; The following is a really absurdly stupid hack that I can barely
  ;; stand to look at. It needs to be fixed.
  ;;
  ;; Nevertheless, I can't live without the feature it provides (which
  ;; should really come out of the box IMO): when pressing RET after
  ;; inserting a pair, add an extra newline and indent. See
  ;; <https://github.com/Fuco1/smartparens/issues/80#issuecomment-18910312>.

  (defun radian--smartparens-pair-setup (mode delim)
    "In major mode MODE, set up DELIM with newline-and-indent."
    (sp-local-pair mode delim nil :post-handlers
                   '((radian--smartparens-indent-new-pair "RET")
                     (radian--smartparens-indent-new-pair "<return>"))))

  (radian--smartparens-pair-setup #'prog-mode "(")
  (radian--smartparens-pair-setup #'prog-mode "[")
  (radian--smartparens-pair-setup #'prog-mode "{")
  (radian--smartparens-pair-setup #'python-mode "\"\"\"")
  (radian--smartparens-pair-setup #'latex-mode "\\[")
  (radian--smartparens-pair-setup #'markdown-mode "```")
  (radian--smartparens-pair-setup #'css-mode "{")

  ;; It's unclear to me why any of this is needed.
  (radian--smartparens-pair-setup #'json-mode "[")
  (radian--smartparens-pair-setup #'json-mode "{")
  (radian--smartparens-pair-setup #'tex-mode "{")

  ;; Deal with `protobuf-mode' not using `define-minor-mode'.
  (radian--smartparens-pair-setup #'protobuf-mode "{")

  ;; Work around https://github.com/Fuco1/smartparens/issues/783.
  (setq sp-escape-quotes-after-insert nil)

  ;; Quiet some silly messages.
  (dolist (key '(:unmatched-expression :no-matching-tag))
    (setf (cdr (assq key sp-message-alist)) nil))

  :blackout t)


;;;; Code reformatting

;; Package `apheleia' implements a sophisticated algorithm for
;; applying code formatters asynchronously on save without moving
;; point or modifying the scroll position.
(leaf apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :init

  (apheleia-global-mode +1)

  (radian-defadvice radian--save-buffer-reformat-maybe (func &optional arg)
    :around #'save-buffer
    "Make it so \\[save-buffer] with prefix arg inhibits reformatting."
    (let ((apheleia-mode (and apheleia-mode (member arg '(nil 1)))))
      (funcall func)))

  ;; We need to do this both before and after Apheleia is loaded
  ;; because the autoloading is set up such that the minor mode
  ;; definition is evaluated twice.
  (blackout 'apheleia-mode)

  :blackout t)

;;;; Snippet expansion

;; Feature `abbrev' provides functionality for expanding user-defined
;; abbreviations. We prefer to use `yasnippet' instead, though.
(leaf! abbrev :blackout t)

;; Package `yasnippet' allows the expansion of user-defined
;; abbreviations into fillable templates. The only reason we have it
;; here is because it gets pulled in by LSP, and we need to unbreak
;; some stuff.
(leaf yasnippet
  :defvar yas-keymap yas-verbosity company-backends lsp-restart
  :bind (yas-minor-mode-map
        ;; Disable TAB from expanding snippets, as I don't use it and
         ;; it's annoying.
         ("TAB" . nil)
         ("<tab>" . nil))
  :after company
  :config

  ;; Reduce verbosity. The default value is 3. Bumping it down to 2
  ;; eliminates a message about successful snippet lazy-loading setup
  ;; on every(!) Emacs init. Errors should still be shown.
  (setq yas-verbosity 2)

  ;; Make it so that Company's keymap overrides Yasnippet's keymap
  ;; when a snippet is active. This way, you can TAB to complete a
  ;; suggestion for the current field in a snippet, and then TAB to
  ;; move to the next field. Plus, C-g will dismiss the Company
  ;; completions menu rather than cancelling the snippet and moving
  ;; the cursor while leaving the completions menu on-screen in the
  ;; same location.
  (leaf! company
    :defun yas--make-control-overlay radian--yasnippet-normalize-event
    :config

    ;; This function translates the "event types" I get from
    ;; `map-keymap' into things that I can pass to `lookup-key' and
    ;; `define-key'. It's a hack, and I'd like to find a built-in
    ;; function that accomplishes the same thing while taking care of
    ;; any edge cases I might have missed in this ad-hoc solution.
    (defun radian--yasnippet-normalize-event (event)
      "This function is a complete hack, do not use.
But in principle, it translates what we get from `map-keymap'
into what `lookup-key' and `define-key' want."
      (if (vectorp event)
          event
        (vector event)))

    ;; Here we define a hybrid keymap that delegates first to
    ;; `company-active-map' and then to `yas-keymap'.
    (defvar radian--yasnippet-then-company-keymap
      ;; It starts out as a copy of `yas-keymap', and then we
      ;; merge in all of the bindings from `company-active-map'.
      (let ((keymap (copy-keymap yas-keymap)))
        (map-keymap
         (lambda (event company-cmd)
           (let* ((event (radian--yasnippet-normalize-event event))
                  (yas-cmd (lookup-key yas-keymap event)))
             ;; Here we use an extended menu item with the
             ;; `:filter' option, which allows us to dynamically
             ;; decide which command we want to run when a key is
             ;; pressed.
             (define-key keymap event
               `(menu-item
                 nil ,company-cmd :filter
                 (lambda (cmd)
                   ;; There doesn't seem to be any obvious
                   ;; function from Company to tell whether or not
                   ;; a completion is in progress (à la
                   ;; `company-explicit-action-p'), so I just
                   ;; check whether or not `company-my-keymap' is
                   ;; defined, which seems to be good enough.
                   (if company-my-keymap
                       ',company-cmd
                     ',yas-cmd))))))
         company-active-map)
        keymap)
      "Keymap which delegates to both `company-active-map' and `yas-keymap'.
The bindings in `company-active-map' only apply if Company is
currently active.")

    (radian-defadvice radian--advice-company-overrides-yasnippet
        (yas--make-control-overlay &rest args)
      :around #'yas--make-control-overlay
      "Allow `company' keybindings to override those of `yasnippet'."
      ;; The function `yas--make-control-overlay' uses the current
      ;; value of `yas-keymap' to build the Yasnippet overlay, so to
      ;; override the Yasnippet keymap we only need to dynamically
      ;; rebind `yas-keymap' for the duration of that function.
      (let ((yas-keymap radian--yasnippet-then-company-keymap))
        (apply yas--make-control-overlay args))))

  :blackout yas-minor-mode)

;;; IDE features
;;;; Virtual environments

;;;; rainbow-delimiters can colorize delimiter.
(leaf rainbow-delimiters
  :defvar rainbow-delimiters-max-face-count
  :hook ((prog-mode-hook text-mode-hook) . rainbow-delimiters-mode)
  :init
  (leaf! cc-mode :hook (c-mode-common-hook . rainbow-delimiters-mode))
  :config
  (setq rainbow-delimiters-max-face-count 3))

;;;; hl-todo
(leaf hl-todo
  :hook (prog-mode-hook . hl-todo-mode)
  :defvar hl-todo-highlight-punctuation hl-todo-keyword-faces
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold))))

;;;; Language servers

;; Package `lsp-mode' is an Emacs client for the Language Server
;; Protocol <https://langserver.org/>. It is where we get all of our
;; information for completions, definition location, documentation,
;; and so on.
(leaf lsp-mode
  :require t
  :init

  (defcustom radian-lsp-disable nil
    "If non-nil, then LSP is not allowed to be enabled.
For use in file-local variables."
    :type 'boolean
    :safe #'booleanp)

 (radian-defhook radian--lsp-enable ()
    after-change-major-mode-hook
    "Enable `lsp-mode' for most programming modes.
Do this on `after-change-major-mode-hook' instead of
`prog-mode-hook' and `text-mode-hook' because we want to make
sure regular mode hooks get a chance to run first, for example to
set LSP configuration (see `lsp-python-ms')."
    (when (derived-mode-p #'prog-mode #'text-mode)
      (unless (or radian-lsp-disable
                  (null buffer-file-name)
                  (derived-mode-p
                   ;; `lsp-mode' doesn't support Elisp, so let's avoid
                   ;; triggering the autoload just for checking that, yes,
                   ;; there's nothing to do for the *scratch* buffer.
                   #'emacs-lisp-mode
                   ;; Disable for modes that we currently use a specialized
                   ;; framework for, until they are phased out in favor of
                   ;; LSP.
                   #'clojure-mode
                   #'ruby-mode))
        (lsp)
        ;; (setq xref-backend-functions (remq 'lsp--xref-backend xref-backend-functions))
        )))

  :config

  ;; We want to make sure the PATH is set up correctly by now, since
  ;; otherwise we might not be able to find the LSP server binaries.
  (radian-env-setup)

  ;; As per <https://github.com/emacs-lsp/lsp-mode#performance>.
  (setq read-process-output-max (* 1024 1024))

  (defun radian--advice-lsp-mode-silence (format &rest args)
    "Silence needless diagnostic messages from `lsp-mode'.

This is a `:before-until' advice for several `lsp-mode' logging
functions."
    (or
     (string-match-p "No LSP server for %s" format)
     (string-match-p "Connected to %s" format)
     (string-match-p "Unable to calculate the languageId" format)
     (string-match-p
      "There are no language servers supporting current mode" format)
     ;; Errors we get from gopls for no good reason (I can't figure
     ;; out why). They don't impair functionality.
     (and (stringp (car args))
          (or (string-match-p "^no object for ident .+$" (car args))
              (string-match-p "^no identifier found$" (car args))))))

  (dolist (fun '(lsp-warn lsp--warn lsp--info lsp--error))
    (advice-add fun :before-until #'radian--advice-lsp-mode-silence))

  ;; If we don't disable this, we get a warning about YASnippet not
  ;; being available, even though it is. I don't use YASnippet anyway,
  ;; so don't bother with it.
  (setq lsp-enable-snippet nil)

  (radian-defadvice radian--lsp-run-from-node-modules (command)
    :filter-return #'lsp-resolve-final-function
    "Find LSP executables inside node_modules/.bin if present."
    (cl-block nil
      (prog1 command
        (when-let ((project-dir
                    (locate-dominating-file default-directory "node_modules"))
                   (binary
                    (radian--path-join
                     project-dir "node_modules" ".bin" (car command))))
          (when (file-executable-p binary)
            (cl-return (cons binary (cdr command))))))))

  (radian-defhook radian--lsp-teardown ()
    kill-emacs-hook
    "Ignore the LSP server getting killed.
If we don't do this, then when killing Emacs we may be prompted
with whether we want to restart the LSP server that has just been
killed (which happens during Emacs shutdown)."
    (setq lsp-restart nil))

  ;; Looks like `lsp-mode' doesn't know about LaTeX yet.
  (add-to-list 'lsp-language-id-configuration '(latex-mode . "latex"))

  ;; Also, it has a bunch of regexps which are completely wrong.
  (setq lsp-language-id-configuration
        (mapcar
         (lambda (link)
           (if (and (stringp (car link))
                    (string-match "\\`\\.\\*\\.\\(.+\\)\\'" (car link)))
               (cons
                (format "\\.%s\\'" (match-string 1 (car link))) (cdr link))
             link))
         lsp-language-id-configuration))

  ;; Disable LSP reformatting your code as you type. We use Apheleia
  ;; for that instead.
  (setq lsp-enable-on-type-formatting nil)

  :blackout " LSP")

;;;; Indentation

;; Don't use tabs for indentation. Use only spaces. Otherwise,
;; whenever the indent level does not equal the tab width (e.g. in
;; Emacs Lisp code, the indent level is 2 and the tab width is 8),
;; *both* tabs and spaces will be used for indentation. Disgusting.
(setq-default indent-tabs-mode nil)

(defun radian-indent-defun ()
  "Indent the surrounding defun."
  (interactive)
  (save-excursion
    (when (beginning-of-defun)
      (let ((beginning (point)))
        (end-of-defun)
        (let ((end (point)))
          (let ((inhibit-message t)
                (message-log-max nil))
            (indent-region beginning end)))))))

(leaf-key* "C-x TAB" #'radian-indent-defun)

(radian-defadvice radian--advice-indent-region-quietly (func &rest args)
  :around #'indent-region
  "Make `indent-region' shut up about its progress."
  (radian--with-silent-message "Indenting region"
    (apply func args)))

;;;; Autocompletion

;; Package `company' provides an in-buffer autocompletion framework.
;; It allows for packages to define backends that supply completion
;; candidates, as well as optional documentation and source code. Then
;; Company allows for multiple frontends to display the candidates,
;; such as a tooltip menu. Company stands for "Complete Anything".
(leaf company
  :require t
  ;; :defvar company-idle-delay company-minimum-prefix-length company-tooltip-limit
  ;; company-tooltip-minimum company-frontends company-show-numbers company-require-match
  ;; company-dabbrev-other-buffers company-dabbrev-ignore-case company-dabbrev-downcase
  ;; company-tooltip-align-annotations company-active-map
 :init

  (defvar radian--company-backends-global
    '(company-capf
      company-files
      (company-dabbrev-code company-keywords)
      company-dabbrev)
    "Values for `company-backends' used everywhere.
If `company-backends' is overridden by Radian, then these
backends will still be included.")

  :bind (;; Remap the standard Emacs keybindings for invoking
         ;; completion to instead use Company. You might think this
         ;; could be put in the `:bind*' declaration below, but it
         ;; seems that `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)

         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.

         (company-active-map

          ;; Make TAB always complete the current selection, instead of
          ;; only completing a common prefix.
          ("<tab>" . company-complete-selection)
          ("TAB" . company-complete-selection)

          ;; When was the last time you used the C-s binding for
          ;; searching candidates? It conflicts with buffer search,
          ;; anyway. Same for the scroll commands.
          ("C-s" . nil)
          ([remap scroll-down-command] . nil)
          ([remap scroll-up-command] . nil)

          ;; The following are keybindings that only take effect if the
          ;; user has explicitly interacted with Company. Note that
          ;; `:map' from above is "sticky", and applies also below: see
          ;; https://github.com/jwiegley/use-package/issues/334#issuecomment-349473819.
         ;; Make RET don't trigger a completion
         ("<return>" . nil)
         ("RET" . nil)

          ;; We then make <up> and <down> abort the completions menu
          ;; unless the user has interacted explicitly. Note that we
          ;; use `company-select-previous' instead of
          ;; `company-select-previous-or-abort'. I think the former
          ;; makes more sense since the general idea of this `company'
          ;; configuration is to decide whether or not to steal
          ;; keypresses based on whether the user has explicitly
          ;; interacted with `company', not based on the number of
          ;; candidates.
          ;;
          ;; Note that M-p and M-n work regardless of whether explicit
          ;; interaction has happened yet, and note also that M-TAB
          ;; when the completions menu is open counts as an
          ;; interaction.
          ("<up>" . company-select-previous)
          ("<down>" . company-select-next)))

  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. We
          ;; already remapped those bindings to `company-manual-begin'
          ;; above. Here we make sure that they definitely invoke
          ;; `company-manual-begin' even if a minor mode binds M-TAB
          ;; directly.
          ("M-TAB" . company-manual-begin))

  :config
  ;; Make RET trigger a completion if and only if the user has
  ;; explicitly interacted with Company, instead of always
  ;; doing so.
  ;; (bind-keys :map company-active-map
  ;;            :filter company-explicit-action-p
  ;;            ("<return>" . company-complete-selection)
  ;;            ("RET" . company-complete-selection))

  ;; Make completions display twice as soon.
  (setq company-idle-delay 0.15)

  ;; Make completions display when you have only typed one character,
  ;; instead of three.
  (setq company-minimum-prefix-length 1)

  ;; Always display the entire suggestion list onscreen, placing it
  ;; above the cursor if necessary.
  (setq company-tooltip-minimum company-tooltip-limit)

  ;; Always display suggestions in the tooltip, even if there is only
  ;; one. Also, don't display metadata in the echo area. (This
  ;; conflicts with ElDoc.)
  (setq company-frontends '(company-pseudo-tooltip-frontend))

  ;; Show quick-reference numbers in the tooltip. (Select a completion
  ;; with M-1 through M-0.)
  (setq company-show-numbers t)

  ;; Prevent non-matching input (which will dismiss the completions
  ;; menu), but only if the user interacts explicitly with Company.
  (setq company-require-match #'company-explicit-action-p)

  ;; Only search the current buffer to get suggestions for
  ;; `company-dabbrev' (a backend that creates suggestions from text
  ;; found in your buffers). This prevents Company from causing lag
  ;; once you have a lot of buffers open.
  (setq company-dabbrev-other-buffers nil)

  ;; Make the `company-dabbrev' backend fully case-sensitive, to
  ;; improve the UX when working with domain-specific words that have
  ;; particular casing.
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-downcase nil)

  ;; When candidates in the autocompletion tooltip have additional
  ;; metadata, like a type signature, align that information to the
  ;; right-hand side. This usually makes it look neater.
  (setq company-tooltip-align-annotations t)

  (defvar-local radian--company-buffer-modified-counter nil
    "Last return value of `buffer-chars-modified-tick'.
Used to ensure that Company only initiates a completion when the
buffer is modified.")

  (radian-defadvice radian--advice-company-complete-on-change ()
    :override #'company--should-begin
    "Make Company trigger a completion when the buffer is modified.
This is in contrast to the default behavior, which is to trigger
a completion when one of a whitelisted set of commands is used.
One specific improvement this brings about is that you get
completions automatically when backspacing into a symbol."
    (let ((tick (buffer-chars-modified-tick)))
      (unless (equal tick radian--company-buffer-modified-counter)
        ;; Only trigger completion if previous counter value was
        ;; non-nil (i.e., don't trigger completion just as we're
        ;; jumping to a buffer for the first time).
        (prog1 (and radian--company-buffer-modified-counter
                    (not (and (symbolp this-command)
                              (string-match-p
                               "^\\(company-\\|undo-\\|undo$\\)"
                               (symbol-name this-command)))))
          (setq radian--company-buffer-modified-counter tick)))))

  (radian-defadvice radian--advice-company-update-buffer-modified-counter ()
    :after #'company--should-continue
    "Make sure `radian--company-buffer-modified-counter' is up to date.
If we don't do this on `company--should-continue' as well as
`company--should-begin', then we may end up in a situation where
autocomplete triggers when it shouldn't. Specifically suppose we
delete a char from a symbol, triggering autocompletion, then type
it back, but there is more than one candidate so the menu stays
onscreen. Without this advice, saving the buffer will cause the
menu to disappear and then come back after `company-idle-delay'."
    (setq radian--company-buffer-modified-counter
          (buffer-chars-modified-tick)))

  (global-company-mode +1)

  (add-hook! '(shell-mode-hook) (company-mode -1))

  :blackout t)

;;;; TabNine
(leaf company-tabnine
  :defun company-tabnine
  :config
  (setq company-show-numbers t)
  (add-to-list 'company-backends #'company-tabnine))

;;;; company-lsp
;; Package `company-lsp' provides a Company backend for `lsp-mode'.
;; It's configured automatically by `lsp-mode'.
(leaf company-lsp
  :disabled t
  :init

  (leaf! lsp
    :config

    (radian-defadvice radian--company-lsp-setup (&rest _)
      :after #'lsp
      "Disable `company-prescient' sorting by length in some contexts.
Specifically, disable sorting by length if the LSP Company
backend returns fuzzy-matched candidates, which implies that the
backend has already sorted the candidates into a reasonable
order."
      (setq-local company-prescient-sort-length-enable
                  (cl-dolist (w lsp--buffer-workspaces)
                    (when (thread-first w
                            (lsp--workspace-client)
                            (lsp--client-server-id)
                            (memq '(jsts-ls mspyls bash-ls texlab ts-ls))
                            (not))
                      (cl-return t)))))))


;;;; Definition location

;; Package `dumb-jump' provides a mechanism to jump to the definitions
;; of functions, variables, etc. in a variety of programming
;; languages. The advantage of `dumb-jump' is that it doesn't try to
;; be clever, so it "just works" instantly for dozens of languages
;; with zero configuration.
(leaf dumb-jump
  :init
  (radian-defhook dumb-jump-enable ()
    after-change-major-mode-hook
    "enable `dumb-jump'"
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate nil t))
  :bind* (("C-M-d" . xref-find-references)))

;;;; Display contextual metadata

;; Feature `eldoc' provides a minor mode (enabled by default in Emacs
;; 25) which allows function signatures or other metadata to be
;; displayed in the echo area.
(leaf! eldoc
  :require t
  :config

  ;; For Emacs 26 and below, `eldoc--message' is not defined. For
  ;; Emacs 27 and above, `eldoc-message' is obsolete.
  (with-no-warnings
    (radian-defadvice radian--advice-eldoc-no-trample (func &rest args)
      :around #'eldoc-print-current-symbol-info
      "Prevent `eldoc' from trampling on existing messages."
      (radian-flet ((defun eldoc-message (&optional string)
                      (if string
                          (funcall eldoc-message string)
                        (setq eldoc-last-message nil)))
                    (defun eldoc--message (&optional string)
                      (if string
                          (funcall eldoc--message string)
                        (setq eldoc-last-message nil))))
        (apply func args))))

  ;; Always truncate ElDoc messages to one line. This prevents the
  ;; echo area from resizing itself unexpectedly when point is on a
  ;; variable with a multiline docstring.
  (setq eldoc-echo-area-use-multiline-p nil)

  ;; Original code from
  ;; https://github.com/PythonNut/emacs-config/blob/1a92a1ff1d563fa6a9d7281bbcaf85059c0c40d4/modules/config-intel.el#L130-L137,
  ;; thanks!

  (radian-defadvice radian--advice-eldoc-better-display-message-p (&rest _)
    :override #'eldoc--message-command-p
    "Make ElDoc smarter about when to display its messages.
By default ElDoc has a customizable whitelist of commands that it
will display its messages after. The idea of this is to not
trample on messages that other commands may have printed.
However, this is a hopeless endeavour because there are a
virtually unlimited number of commands that don't conflict with
ElDoc. A better approach is to simply check to see if a message
was printed, and only have ElDoc display if one wasn't."
    (member (current-message) (list nil eldoc-last-message)))

  :blackout t)

;;;; Syntax checking and code linting

;; Package `flycheck' provides a framework for in-buffer error and
;; warning highlighting. We kind of don't use it because we use
;; `lsp-ui' instead, but internally `lsp-ui' actually hacks Flycheck
;; to behave differently, so it is a dependency. We just don't enable
;; Flycheck anywhere else and rely on `lsp-ui' to handle things when
;; appropriate. However, interestingly, Flycheck is not marked as a
;; dependency of `lsp-ui', hence this declaration.
(leaf flycheck
  :config

  ;; For use with `lsp-ui'.
  (radian-bind-key "p" #'flycheck-previous-error)
  (radian-bind-key "n" #'flycheck-next-error)

  :blackout t)

;; Package `lsp-ui' provides a pretty UI for showing diagnostic
;; messages from LSP in the buffer using overlays. It's configured
;; automatically by `lsp-mode'.
(leaf lsp-ui
  :bind (("C-c f" . lsp-ui-sideline-apply-code-actions))
  :config

  (radian-defadvice radian--advice-lsp-ui-apply-single-fix
      (orig-fun &rest args)
    :around #'lsp-ui-sideline-apply-code-actions
    "Apply code fix immediately if only one is possible."
    (radian-flet ((defun completing-read (prompt collection &rest args)
                    (if (= (safe-length collection) 1)
                        (car collection)
                      (apply completing-read prompt collection args))))
      (apply orig-fun args)))

  (leaf! lsp-mode
    :defvar lsp-eldoc-enable-hover
    :defer-config

    ;; With `lsp-ui', there's no need for the ElDoc integration
    ;; provided by `lsp-mode', and in fact for Bash it is very
    ;; annoying since all the hover information is multiline.
    (setq lsp-eldoc-enable-hover nil)))

;; Feature `lsp-ui-doc' from package `lsp-ui' displays documentation
;; in a child frame when point is on a symbol.
(leaf! lsp-ui-doc
  :defvar lsp-ui-doc-frame-parameters
  :defun lsp-ui-doc--render-buffer
  :defer-config

  ;; https://github.com/emacs-lsp/lsp-ui/issues/414
  (add-to-list 'lsp-ui-doc-frame-parameters '(no-accept-focus . t))

  (radian-defadvice radian--advice-lsp-ui-doc-allow-multiline (func &rest args)
    :around #'lsp-ui-doc--render-buffer
    "Prevent `lsp-ui-doc' from removing newlines from documentation."
    (radian-flet ((defun replace-regexp-in-string
                      (regexp rep string &rest args)
                    (if (equal regexp "`\\([\n]+\\)")
                        string
                      (apply replace-regexp-in-string
                             regexp rep string args))))
      (apply func args))))

;;; Language support
;;;; Lisp languages

;; Feature `lisp-mode' provides a base major mode for Lisp languages,
;; and supporting functions for dealing with Lisp code.
(leaf! lisp-mode
  :init

  (add-to-list 'safe-local-variable-values
               '(lisp-indent-function . common-lisp-indent-function)))

;;;; AppleScript
;; https://developer.apple.com/library/content/documentation/AppleScript/Conceptual/AppleScriptLangGuide/introduction/ASLR_intro.html
(leaf apples-mode
  :mode "\\.\\(applescri\\|sc\\)pt\\'")

;;;; C, C++, Objective-C, Java
;; https://en.wikipedia.org/wiki/C_(programming_language)
;; https://en.wikipedia.org/wiki/C%2B%2B
;; https://en.wikipedia.org/wiki/Objective-C
;; https://en.wikipedia.org/wiki/Java_(programming_language)

;; Feature `cc-mode' provides major modes for C, C++, Objective-C, and
;; Java.
(leaf! cc-mode
  :defun c-update-modeline
  :defvar c-default-style
  :defer-config

  (radian-defadvice radian--advice-inhibit-c-submode-indicators (&rest _)
    :override #'c-update-modeline
    "Unconditionally inhibit CC submode indicators in the mode lighter.")

  ;; Switch to a better indentation-and-braces style. This turns the
  ;; following code:
  ;;
  ;; if (condition)
  ;;   {
  ;;     statement;
  ;;   }
  ;;
  ;; Into this:
  ;;
  ;; if (condition)
  ;; {
  ;;   statement;
  ;; }
  ;;
  ;; We do this by defining a custom style that is based on BSD, and
  ;; then overriding the indentation (which is set to 8 spaces by
  ;; default). This style is only used for languages which do not have
  ;; a more specific style set in `c-default-style'.
  (c-add-style "radian-bsd"
               '("bsd"
                 (c-basic-offset . 2)))
  (setf (map-elt c-default-style 'other) "radian-bsd")

  (put 'c-default-style 'safe-local-variable #'stringp))

;;;; Clojure
;; https://clojure.org/
;; Package `clojure-mode' provides a major mode for Clojure.
(leaf clojure-mode)

;;;; Go
;; https://golang.org/

;; Package `go-mode' provides a major mode for Go.
(leaf go-mode
  :defun go--backward-irrelevant radian--go-beginning-of-defun radian--go-end-of-defun
  :defer-config

  (defvar radian--go-defun-regexp
    "^\\(const\\|func\\|import\\|interface\\|package\\|type\\|var\\)"
    "Regexp matching top-level declarations in Go.")

  (defun radian--go-beginning-of-defun (&optional arg)
    "Move to beginning of current or previous top-level declaration."
    (cond
     ((null arg)
      (cl-block nil
        (while t
          (re-search-backward radian--go-defun-regexp nil 'noerror)
          (when (or (bobp)
                    (eq (get-text-property (point) 'face)
                        'font-lock-keyword-face))
            (cl-return)))))
     ((> arg 0)
      (dotimes (_ arg)
        (radian--go-beginning-of-defun)))
     ((< arg 0)
      ;; Yuck -- but we need to implement this, otherwise
      ;; `end-of-defun' just does the wrong thing :/
      (dotimes (_ (- arg))
        (radian--go-beginning-of-defun)
        (radian--go-end-of-defun)
        (radian--go-end-of-defun))
      (radian--go-beginning-of-defun))))

  (defun radian--go-end-of-defun ()
    "Move to end of current or previous top-level declaration.
Only works if `radian--go-beginning-of-defun' was just called
previously."
    (dotimes (_ 2)
      (cl-block nil
        (while t
          (re-search-forward radian--go-defun-regexp nil 'noerror)
          (when (or (eobp)
                    (save-excursion
                      (beginning-of-line)
                      (eq (get-text-property (point) 'face)
                          'font-lock-keyword-face)))
            (cl-return)))))
    (beginning-of-line)
    (go--backward-irrelevant 'stop-at-string)
    (forward-line))

  (radian-defhook radian--go-defun-setup ()
    go-mode-hook
    "Set up \\[beginning-of-defun] and \\[end-of-defun] correctly.
See <https://github.com/dominikh/go-mode.el/issues/232>."
    (setq-local beginning-of-defun-function #'radian--go-beginning-of-defun)
    (setq-local end-of-defun-function #'radian--go-end-of-defun))

  (leaf! lsp-ui
    :defvar lsp-ui-sideline--code-actions
    :defun lsp-ui-sideline--code-actions
    :config

    (radian-defadvice radian--advice-lsp-ui-organize-imports-more-cleanly
        (func actions &rest args)
      :around #'lsp-ui-sideline--code-actions
      "Clean up the \"Organize Imports\" code actions for Go.
Firstly, don't display \"Organize Imports\" or \"Organize All
Imports\" in the sideline, as gopls sometimes reports these code
actions when the indentation is wrong (rather than when imports
need to be changed). Secondly, filter out \"Organize All
Imports\" internally, so that applying a code action will default
to \"Organize Imports\" instead of prompting you to decide
between that and \"Organize All Imports\" (which does the same
thing as far as I can tell)."
      (let ((actions-to-keep nil)
            (actions-to-render nil))
        (dolist (action actions)
          (unless (equal "Organize All Imports" (gethash "title" action))
            (push action actions-to-keep)
            (unless (equal "Organize Imports" (gethash "title" action))
              (push action actions-to-render))))
        (setq actions-to-keep (nreverse actions-to-keep))
        (setq actions-to-render (nreverse actions-to-render))
        (when actions-to-render
          (apply func actions-to-render args))
        (setq lsp-ui-sideline--code-actions actions-to-keep)))))

;;;; Haskell
;; https://www.haskell.org/

;; Package `haskell-mode' provides a major mode and REPL integration
;; for Haskell.
(leaf haskell-mode
  :config

  ;; Enable REPL integration.
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)

  (radian-defadvice radian--advice-haskell-fix-back-to-indentation
      (back-to-indentation)
    :around #'back-to-indentation
    "Fix `back-to-indentation' in `literate-haskell-mode'.
Otherwise, it just moves point to column 0, which is wrong.

This works around an upstream bug; see
<https://github.com/haskell/haskell-mode/issues/1594>."
    (if (derived-mode-p 'literate-haskell-mode)
        (progn
          (beginning-of-line 1)
          (when-let ((c (char-after)))
            (when (= c ? )
              (forward-char)))
          (skip-syntax-forward " " (line-end-position))
          (backward-prefix-chars))
      (funcall back-to-indentation))))

;; Feature `haskell' from package `haskell-mode' is a meta-feature
;; which includes many other features from the package, and also for
;; some reason is where `interactive-haskell-mode' is defined.
(leaf! haskell
  :defvar interactive-haskell-mode-map
  :defer-config

  ;; Prevent this binding from overriding the alternative binding from
  ;; LSP that we actually want to use.
  (define-key interactive-haskell-mode-map "\M-." nil)

  :blackout interactive-haskell-mode)

;; Feature `haskell-customize' from package `haskell-mode' defines the
;; user options for the package.
(leaf! haskell-customize
  :config

  ;; Disable in-buffer underlining of errors and warnings, since we
  ;; already have them from `lsp-ui'.
  (setq haskell-process-show-overlays nil))

;; Package `lsp-haskell' configures the HIE Haskell language server
;; for use with `lsp-mode'.
(leaf lsp-haskell
  :after (:all lsp-mode haskell-mode))

;;;; Lua
;; <http://www.lua.org/>

;; Package `lua-mode' provides a major mode for Lua code.
(leaf lua-mode)

;;;; Makefile

;; Feature `make-mode' provides major modes for editing Makefiles.
(leaf! make-mode
  :blackout ((makefile-automake-mode . "Makefile")
             (makefile-gmake-mode . "Makefile")
             (makefile-makepp-mode . "Makefile")
             (makefile-bsdmake-mode . "Makefile")
             (makefile-imake-mode . "Makefile")))

;;;; Markdown
;; https://daringfireball.net/projects/markdown/

;; Package `markdown-mode' provides a major mode for Markdown.
(leaf markdown-mode
  :defun markdown-table-at-point-p markdown-table-forward-cell markdown-list-item-at-point-p
  markdown-demote-list-item markdown-table-backward-cell markdown-promote-list-item markdown-match-generic-metadata
  :mode (;; Extension used by Hugo.
         ("\\.mmark\\'" . markdown-mode))

  :bind (;; C-c C-s p is a really dumb binding, we prefer C-c C-s C-p.
         ;; Same for C-c C-s q.
         (markdown-mode-style-map
          ("C-p" . markdown-insert-pre)
          ("C-q" . markdown-insert-blockquote))
         (markdown-mode-map
          ("TAB" . radian-markdown-tab)
          ;; Try to override all the bindings in
          ;; `markdown-mode-map'...
          ("<S-iso-lefttab>" . radian-markdown-shifttab)
          ("<S-tab>" . radian-markdown-shifttab)
          ("<backtab>" . radian-markdown-shifttab)))
  :defer-config

  (defun radian-markdown-tab ()
    "Do something reasonable when the user presses TAB.
This means moving forward a table cell, indenting a list item, or
performing normal indentation."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-forward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-demote-list-item))
     (t
      ;; Ew. But `markdown-indent-line' checks to see if
      ;; `this-command' is `markdown-cycle' before doing something
      ;; useful, so we have to.
      (let ((this-command 'markdown-cycle))
        (indent-for-tab-command)))))

  (defun radian-markdown-shifttab ()
    "Do something reasonable when the user presses S-TAB.
This means moving backward a table cell or unindenting a list
item."
    (interactive)
    (cond
     ((markdown-table-at-point-p)
      (markdown-table-backward-cell))
     ((markdown-list-item-at-point-p)
      (markdown-promote-list-item))))

  (radian-defadvice radian--disable-markdown-metadata-fontification (&rest _)
    :override #'markdown-match-generic-metadata
    "Prevent fontification of YAML metadata blocks in `markdown-mode'.
This prevents a mis-feature wherein if the first line of a
Markdown document has a colon in it, then it's distractingly and
usually wrongly fontified as a metadata block. See
https://github.com/jrblevin/markdown-mode/issues/328."
    (prog1 nil (goto-char (point-max)))))

;;;; Protobuf

;; Package `protobuf-mode' provides a major mode for Protobuf.
(leaf protobuf-mode)

;;;; Python
;; https://www.python.org/

;; Feature `python' provides a major mode for Python.
(leaf! python
  :config

  ;; The only consistent style.
  (setq python-fill-docstring-style 'django)

  (radian-defhook radian--python-fix-outline-mode-config ()
    python-mode-hook
    "Prevent `python-mode' from overriding `outline-minor-mode' config.
If this hook is not used, then `python-mode' will override even a
file-local setting of e.g. `outline-regexp' with its own setting."
    (kill-local-variable 'outline-regexp)
    (kill-local-variable 'outline-level)
    (kill-local-variable 'outline-heading-end-regexp))

  (radian-defhook radian--python-no-reindent-on-colon ()
    python-mode-hook
    "Don't reindent on typing a colon.
See https://emacs.stackexchange.com/a/3338/12534."
    (setq electric-indent-chars (delq ?: electric-indent-chars)))

  ;; Default to Python 3. Prefer the versioned Python binaries since
  ;; some systems stupidly make the unversioned one point at Python 2.
  (cond
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python")))

  (radian-defhook radian--python-use-correct-executable ()
    python-mode-hook
    "Use correct executables for Python tooling."
    (save-excursion
      (save-match-data
        (when (or (looking-at "#!/usr/bin/env \\(python[^ \n]+\\)")
                  (looking-at "#!\\([^ \n]+/python[^ \n]+\\)"))
          (setq-local
           python-shell-interpreter
           (substring-no-properties (match-string 1))))))
    (with-no-warnings
      (setq-local
       lsp-python-ms-python-executable-cmd
       python-shell-interpreter)))

  ;; I honestly don't understand why people like their packages to
  ;; spew so many messages.
  (setq python-indent-guess-indent-offset-verbose nil)

  (defun radian--python-find-virtualenv ()
    "Find a virtualenv corresponding to the current buffer.
Return either a string or nil."
    (cl-block nil
      (when (and (executable-find "poetry")
                 (locate-dominating-file default-directory "pyproject.toml"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process
                      "poetry" nil '(t nil) nil "run" "which" "python"))
            (goto-char (point-min))
            (when (looking-at "\\(.+\\)/bin/python\n")
              (let ((venv (match-string 1)))
                (when (file-directory-p venv)
                  (cl-return venv)))))))
      (when (and (executable-find "pipenv")
                 (locate-dominating-file default-directory "Pipfile"))
        (with-temp-buffer
          ;; May create virtualenv, but whatever.
          (when (= 0 (call-process "pipenv" nil '(t nil) nil "--venv"))
            (goto-char (point-min))
            (let ((venv (string-trim (buffer-string))))
              (when (file-directory-p venv)
                (cl-return venv)))))))))
;; lsp for python
(leaf lsp-pyright
  :after lsp-mode python
  :require t)

;;;; Shell
;; http://pubs.opengroup.org/onlinepubs/9699919799/utilities/sh.html
;; https://www.gnu.org/software/bash/
;; http://www.zsh.org/

(leaf! sh-script
  :defvar sh-shell
  :defer-config

  (dolist (func '(sh-set-shell sh-make-vars-local))
    (advice-add func :around #'radian--advice-silence-messages))

  (radian-defhook radian--sh-prettify-mode-line ()
    sh-mode-hook
    "Instead of \"Shell[bash]\", display mode name as \"Bash\"."
    ;; Only do this for `sh-mode', not derived modes such as
    ;; `pkgbuild-mode'.
    (setq mode-line-process nil)
    (when (eq major-mode 'sh-mode)
      (setq mode-name (capitalize (symbol-name sh-shell)))))

  (leaf! lsp-bash
    :defer-config

    ;; Only activate the Bash LSP server in Bash code, not all shell
    ;; script code. It's not very helpful to get Bash syntax errors
    ;; while editing Zsh code.
    (radian-protect-macros
      (setf (lsp--client-activation-fn (gethash 'bash-ls lsp-clients))
            (lambda (&rest _)
              (memq sh-shell '(sh bash)))))))

;;;; Web
;; https://developer.mozilla.org/en-US/docs/web/HTML
;; https://developer.mozilla.org/en-US/docs/Web/CSS
;; https://developer.mozilla.org/en-US/docs/Web/JavaScript

;; Feature `js' provides a major mode `js-mode' for JavaScript. We
;; don't use it (because `web-mode' is better), but we still configure
;; some of its variables because `json-mode' uses them.
(leaf! js
  :config

  ;; Default is 4, and nobody should indent JSON with four spaces.
  (setq js-indent-level 2))

;; Package `web-mode' provides a major mode for HTML, CSS, JavaScript,
;; and every conceivable thing adjacent (TypeScript, JSX, TSX, PSP,
;; ASP, Handlebars, etc.) all at once.
(leaf web-mode
  :defvar web-mode-markup-indent-offset web-mode-code-indent-offset web-mode-css-indent-offset
  web-mode-enable-auto-closing web-mode-auto-close-style web-mode-enable-auto-quoting
  web-mode-enable-auto-indentation web-mode-content-types-alist web-mode-comment-formats
  web-mode-content-type web-mode-fontification-off web-mode-scan-beg web-mode-scan-end
  web-mode-script-padding web-mode-style-padding
  ;; Unfortunately `web-mode' does not come with `auto-mode-alist'
  ;; autoloads. We have to establish them manually. This list comes
  ;; from the official website at <http://web-mode.org/> as of
  ;; 2018-07-09.
  :mode (("\\.phtml\\'" . web-mode)
         ("\\.tpl\\.php\\'" . web-mode)
         ("\\.[agj]sp\\'" . web-mode)
         ("\\.as[cp]x\\'" . web-mode)
         ("\\.erb\\'" . web-mode)
         ("\\.mustache\\'" . web-mode)
         ("\\.djhtml\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ;; My additions.
         ("\\.ejs\\'" . web-mode)
         ("\\.jsx?\\'" . web-mode)
         ("\\.tsx?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  ;; Use `web-mode' rather than `js-mode' for scripts.
  :interpreter (("js" . web-mode)
                ("node" . web-mode))
  :defer-config

  ;; Indent by two spaces by default. Compatibility with Prettier.
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)

  ;; Not sure why anyone would want 1 space indent for inline scripts
  ;; and CSS. Set it to 2 for consistency.
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)

  ;; Autocomplete </ instantly.
  (setq web-mode-enable-auto-closing t)

  ;; Insert matching tags automatically. Why this is "mode 2", I have
  ;; not the slightest idea.
  (setq web-mode-auto-close-style 2)

  ;; Don't insert quotes automatically. It messes with JSX.
  (setq web-mode-enable-auto-quoting nil)

  ;; Disable `web-mode' automatically reindenting a bunch of
  ;; surrounding code when you paste anything. It's real annoying if
  ;; it happens to not know how to indent your code correctly.
  (setq web-mode-enable-auto-indentation nil)

  ;; When using `web-mode' to edit JavaScript files, support JSX tags.
  (add-to-list 'web-mode-content-types-alist
               '("jsx" . "\\.js[x]?\\'"))

  ;; Create line comments instead of block comments by default in
  ;; JavaScript. See <https://github.com/fxbois/web-mode/issues/619>.
  (let ((types '("javascript" "jsx")))
    (setq web-mode-comment-formats
          (cl-remove-if (lambda (item)
                          (member (car item) types))
                        web-mode-comment-formats))
    (dolist (type types)
      (push (cons type "//") web-mode-comment-formats)))

  (radian-defhook radian--web-js-fix-comments ()
    web-mode-hook
    "Fix comment handling in `web-mode' for JavaScript.
Note that this somewhat breaks HTML comments, but it's good
enough for the moment."

    ;; For some reason the default is to insert HTML comments even
    ;; in JavaScript.
    (setq-local comment-start "//")
    (setq-local comment-end "")

    ;; Needed since otherwise the default value generated by
    ;; `comment-normalize-vars' will key off the syntax and think
    ;; that a single "/" starts a comment, which completely borks
    ;; auto-fill.
    (setq-local comment-start-skip "// *"))

  (leaf! apheleia
    :config

    (radian-defhook radian--web-highlight-after-formatting ()
      apheleia-post-format-hook
      "Make sure syntax highlighting works with Apheleia.
The problem is that `web-mode' doesn't do highlighting correctly
in the face of arbitrary buffer modifications, and kind of hacks
around the problem by hardcoding a special case for yanking based
on the value of `this-command'. So, when buffer modifications
happen in an unexpected (to `web-mode') way, we have to manually
poke it. Otherwise the modified text remains unfontified."
      (setq web-mode-fontification-off nil)
      (when (and web-mode-scan-beg web-mode-scan-end global-font-lock-mode)
        (save-excursion
          (font-lock-fontify-region web-mode-scan-beg web-mode-scan-end))))))

;;; Configuration file formats

;; Package `apache-mode' provides a major mode for .htaccess and
;; similar files.
(leaf apache-mode)

;; Package `crontab-mode' provides a major mode for crontab files.
(leaf crontab-mode)

;; Package `dockerfile-mode' provides a major mode for Dockerfiles.
(leaf dockerfile-mode)

;; Package `gitconfig-mode' provides a major mode for .gitconfig and
;; .gitmodules files.
(leaf gitconfig-mode :mode "\\.gitconfig.*")

;; Package `gitignore-mode' provides a major mode for .gitignore
;; files.
(leaf gitignore-mode)

;; Package `json-mode' provides a major mode for JSON.
(leaf json-mode
  :init/el-patch

  (defconst json-mode-standard-file-ext '(".json" ".jsonld")
    "List of JSON file extensions.")

  (defsubst json-mode--update-auto-mode (filenames)
    "Update the `json-mode' entry of `auto-mode-alist'.

FILENAMES should be a list of file as string.
Return the new `auto-mode-alist' entry"
    (let* ((new-regexp
            (rx-to-string
             `(seq (eval
                    (cons 'or
                          (append json-mode-standard-file-ext
                                  ',filenames)))
                   eot)))
           (new-entry (cons new-regexp 'json-mode))
           (old-entry (when (boundp 'json-mode--auto-mode-entry)
                        json-mode--auto-mode-entry)))
      (setq auto-mode-alist (delete old-entry auto-mode-alist))
      (add-to-list 'auto-mode-alist new-entry)
      new-entry))

  (defcustom json-mode-auto-mode-list '(".babelrc" ".bowerrc" "composer.lock")
    "List of filename as string to pass for the JSON entry of
`auto-mode-alist'.

Note however that custom `json-mode' entries in `auto-mode-alist'
won’t be affected."
    :group 'json-mode
    :type '(repeat string)
    :set (lambda (symbol value)
           "Update SYMBOL with a new regexp made from VALUE.

This function calls `json-mode--update-auto-mode' to change the
`json-mode--auto-mode-entry' entry in `auto-mode-alist'."
           (set-default symbol value)
           (setq json-mode--auto-mode-entry
                 (json-mode--update-auto-mode value))))

  (defvar json-mode--auto-mode-entry
    (json-mode--update-auto-mode json-mode-auto-mode-list)
    "Regexp generated from the `json-mode-auto-mode-list'.")

  :config

  (radian-defhook radian--fix-json-indentation ()
    json-mode-hook
    "Set the tab width to 2 for JSON."
    (setq-local tab-width 2)))

;; Package `pip-requirements' provides a major mode for
;; requirements.txt files used by Pip.
(leaf pip-requirements

  ;; The default mode lighter is "pip-require". Ew.
  :blackout "Requirements")

;; Package `pkgbuild-mode' provides a major mode for PKGBUILD files
;; used by Arch Linux and derivatives.
(leaf pkgbuild-mode)

;; Package `ssh-config-mode' provides major modes for files in ~/.ssh.
(leaf ssh-config-mode
  :blackout "SSH-Config")

;; Package `terraform-mode' provides major modes for Terraform
;; configuration files.
(leaf terraform-mode)

;; Package `toml-mode' provides a major mode for TOML.
(leaf toml-mode
  :mode "Pipfile\\'"
  ;; Correct the capitalization from "Toml" to "TOML".
  :blackout "TOML")

;; Package `yaml-mode' provides a major mode for YAML.
(leaf yaml-mode)

;;; Introspection
;;;; Help

;; Feature `help' powers the *Help* buffer and related functionality.
(leaf! help
  :bind (help-map
         ("M-k" . radian-describe-keymap))
  :config

  (radian-defadvice radian--advice-help-inhibit-hints (&rest _)
    :override #'help-window-display-message
    "Inhibit the \"Type q in help window to delete it\" hints.
Normally these are printed in the echo area whenever you open a
help buffer.")

  (radian-defadvice radian--advice-help-disable-revert-prompt
      (help-mode-revert-buffer ignore-auto _noconfirm)
    :around #'help-mode-revert-buffer
    "Don't ask for confirmation before reverting help buffers.
\(Reverting is done by pressing \\<help-mode-map>\\[revert-buffer].)"
    (funcall help-mode-revert-buffer ignore-auto 'noconfirm))

  (defun radian-describe-keymap (keymap)
    "Display the bindings defined by KEYMAP, a symbol or keymap.
Interactively, select a keymap from the list of all defined
keymaps."
    (interactive
     (list
      (intern
       (completing-read
        "Keymap: " obarray
        (lambda (m)
          (and (boundp m)
               (keymapp (symbol-value m))))
        'require-match
        nil nil (thing-at-point 'symbol)))))
    (with-help-window (help-buffer)
      (with-current-buffer (help-buffer)
        (insert (format "Keymap `%S' defines the following bindings:" keymap)
                "\n\n"
                (substitute-command-keys (format "\\{%S}" keymap))))))

  (radian-defhook radian--xref-help-setup ()
    help-mode-hook
    "Make xref look up Elisp symbols in help buffers.
Otherwise, it will try to find a TAGS file using etags, which is
unhelpful."
    (add-hook 'xref-backend-functions #'elisp--xref-backend nil 'local)))

;; Package `helpful' provides a complete replacement for the built-in
;; Emacs help facility which provides much more contextual information
;; in a better format.
(leaf helpful
  :defvar radian-universal-keyboard-quit-mode
  :defun radian-universal-keyboard-quit-mode helpful--library-path
  :bind (;; Remap standard commands.
         ([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-key]      . helpful-key)

         ;; Suggested bindings from the documentation at
         ;; https://github.com/Wilfred/helpful.

         (help-map
          ("F" . helpful-function)
          ("M-f" . helpful-macro)
          ("C" . helpful-command))

         (global-map
          ("C-c C-d" . helpful-at-point)))

  :config

  ;; Make it so you can quit out of `helpful-key' with C-g, like for
  ;; every other command. Put this in a minor mode so it can be
  ;; disabled.
  (define-minor-mode radian-universal-keyboard-quit-mode
    "Minor mode for making C-g work in `helpful-key'."
    :global t
    (if radian-universal-keyboard-quit-mode
        (radian-defadvice radian--advice-helpful-key-allow-keyboard-quit
            (&rest _)
          :before #'helpful-key
          "Make C-g work in `helpful-key'."
          ;; The docstring of `add-function' says that if we make our
          ;; advice interactive and the interactive spec is *not* a
          ;; function, then it overrides the original function's
          ;; interactive spec.
          (interactive
           (list
            (let ((ret (read-key-sequence "Press key: ")))
              (when (equal ret "\^G")
                (signal 'quit nil))
              ret))))
      (advice-remove
       #'helpful-key #'radian--advice-helpful-key-allow-keyboard-quit)))

  (radian-universal-keyboard-quit-mode +1)

  (radian-defadvice radian--advice-helpful-clone-emacs-source (library-name)
    :before #'helpful--library-path
    "Prompt user to clone Emacs source code when looking up functions.
Otherwise, it only happens when looking up variables, for some
bizarre reason."
    (when (member (file-name-extension library-name) '("c" "rs"))
      (radian-clone-emacs-source-maybe))))

;;;; Custom

;; Feature `cus-edit' powers Customize buffers and related
;; functionality.
(leaf! cus-edit
  :config

  ;; Don't show the search box in Custom.
  (setq custom-search-field nil))

;;;; Emacs Lisp development

;; Feature `elisp-mode' provides the major mode for Emacs Lisp. Very
;; important! It also provides the major mode for the *scratch*
;; buffer, which is very similar but slightly different. Not as
;; important.
(leaf! elisp-mode
  :config

  ;; Note that this function is actually defined in `elisp-mode'
  ;; because screw modularity.
  (radian-defadvice radian--advice-company-elisp-use-helpful
      (func &rest args)
    :around #'elisp--company-doc-buffer
    "Cause `company' to use Helpful to show Elisp documentation."
    (cl-letf (((symbol-function #'describe-function) #'helpful-function)
              ((symbol-function #'describe-variable) #'helpful-variable)
              ((symbol-function #'help-buffer) #'current-buffer))
      (apply func args)))

  (radian-defadvice radian--advice-fill-elisp-docstrings-correctly (&rest _)
    :before-until #'fill-context-prefix
    "Prevent `auto-fill-mode' from adding indentation to Elisp docstrings."
    (when (and (derived-mode-p #'emacs-lisp-mode)
               (eq (get-text-property (point) 'face) 'font-lock-doc-face))
      ""))

  ;; The default mode lighter has a space instead of a hyphen.
  ;; Disgusting!
  :blackout ((lisp-interaction-mode . "Lisp-Interaction")
             (emacs-lisp-mode . `("ELisp"
                                  (lexical-binding
                                   ""
                                   (:propertize
                                    "/d" face warning))))))

(defun radian-reload-init ()
  "Reload the init-file."
  (interactive)
  (message "Reloading init-file...")
  ;; Total hack here. I have no idea why it's needed. But, probably
  ;; due to some kind of disgusting Gilardi scenario, if we don't
  ;; explicitly load it here, the autoloading does not quite suffice
  ;; to make everything work out. Specifically, if we byte-compile the
  ;; init-file, start up using that init-file, then make a
  ;; modification to the init-file and reload using
  ;; `radian-reload-init', then all the `leaf' declarations
  ;; fail to recognize `:straight' as a supported keyword, strongly
  ;; suggesting there is some kind of eager macroexpansion that fails
  ;; because straight.el has not yet installed the `leaf'
  ;; integration. I would have thought that putting a `require'
  ;; statement inside `eval-when-compile' (or even a bare `require')
  ;; after we request `leaf' from straight.el would solve the
  ;; problem, but unfortunately it does not. As such, the hack.
  (require 'leaf)
  (load user-init-file nil 'nomessage)
  (message "Reloading init-file...done"))

(radian-bind-key "r" #'radian-reload-init)

(defun radian-eval-buffer-or-region (&optional start end)
  "Evaluate the current region, or the whole buffer if no region is active.
In Lisp code, START and END denote the region to be evaluated;
they default to `point-min' and `point-max' respectively.

If evaluating a buffer visiting this file, then delegate instead
to `radian-reload-init'."
  (interactive)
  (if (and buffer-file-name
           (member (file-truename buffer-file-name)
                   (list
                    (when (bound-and-true-p early-init-file)
                      (file-truename early-init-file))
                    (file-truename user-init-file)
                    (file-truename radian-lib-file)
                    (file-truename radian-local-init-file)))
           (not (region-active-p)))
      (radian-reload-init)
    (let ((name nil))
      (if (region-active-p)
          (progn
            (setq start (region-beginning))
            (setq end (region-end))
            (setq name "region"))
        (setq start (point-min))
        (setq end (point-max))
        (setq name (buffer-name)))
      (let ((load-file-name (buffer-file-name)))
        (message "Evaluating %s..." name)
        (eval-region start end)
        (message "Evaluating %s...done" name)))))

;; This keybinding is used for evaluating a buffer of Clojure code in
;; CIDER, and for evaluating a buffer of Scheme code in Geiser.
(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key map "\C-c\C-k" #'radian-eval-buffer-or-region))

(defun radian-find-symbol (&optional symbol)
  "Same as `xref-find-definitions' but only for Elisp symbols.
SYMBOL is as in `xref-find-definitions'."
  (interactive)
  (let ((xref-backend-functions '(elisp--xref-backend))
        ;; Make this command behave the same as `find-function' and
        ;; `find-variable', i.e. always prompt for an identifier,
        ;; defaulting to the one at point.
        (xref-prompt-for-identifier t))
    ;; for slience unused warnings, when byte-compile.
    (eval-when-compile (require 'xref))
    (if symbol
        (xref-find-definitions symbol)
      (call-interactively 'xref-find-definitions))))

;; By default, C-h f, C-h v, and C-h o are bound to
;; `describe-function', `describe-variable', and `describe-symbol'
;; respectively. By analogy, C-h C-f, C-h C-v, and C-h C-o should be
;; bound as follows. (There's no `find-symbol' function by default for
;; some reason; note that `xref-find-definitions' is not a replacement
;; because it is major-mode dependent.) By further analogy, we should
;; bind `find-library'.
(leaf-key "C-h C-f" #'find-function)
(leaf-key "C-h C-v" #'find-variable)
(leaf-key "C-h C-o" #'radian-find-symbol)
(leaf-key "C-h C-l" #'find-library)

;; Let's establish a standard location for the Emacs source code.
(setq source-directory (expand-file-name "src" user-emacs-directory))

;; This is initialized to nil by `find-func' if the source is not
;; cloned when the library is loaded.
(setq find-function-C-source-directory
      (expand-file-name "src" source-directory))

(defun radian-clone-emacs-source-maybe ()
  "Prompt user to clone Emacs source repository if needed."
  (when (and (not (file-directory-p source-directory))
             (not (get-buffer "*clone-emacs-src*"))
             (yes-or-no-p "Clone Emacs source repository? "))
    (make-directory (file-name-directory source-directory) 'parents)
    (let ((compilation-buffer-name-function
           (lambda (&rest _)
             "*clone-emacs-src*")))
      (save-current-buffer
        (compile
         (format
          "git clone https://github.com/emacs-mirror/emacs.git %s"
          (shell-quote-argument source-directory)))))))

;; Feature `find-func' provides the ability for you to locate the
;; definitions of Emacs Lisp functions and variables.
(leaf! find-func
  :defun find-function-C-source
  :config

  (radian-defadvice radian--advice-find-func-clone-emacs-source (&rest _)
    :before #'find-function-C-source
    "Clone Emacs source if needed to view definition."
    (radian-clone-emacs-source-maybe)))

;; Package `macrostep' provides a facility for interactively expanding
;; Elisp macros.
(leaf macrostep
  :bind (("C-c e" . macrostep-expand)))


;;;;; Emacs Lisp byte-compilation

;; Feature `bytecomp' handles byte-compilation of Emacs Lisp code.
(leaf! bytecomp
  :config

  ;; Eliminate two warnings that are essentially useless for me. The
  ;; `make-local' warning gets triggered every time you call
  ;; `define-minor-mode' inside of `leaf', and the `noruntime'
  ;; warning gets triggered basically all the time for everything.
  (setq byte-compile-warnings '(not make-local noruntime))

  (defun radian-batch-byte-compile ()
    "Byte-compile radian.el. For usage in batch mode."
    (byte-compile-file radian-lib-file))

  (defun radian-byte-compile (&optional report-progress)
    "Byte-compile radian.el. For interactive usage.
REPORT-PROGRESS non-nil (or interactively) means to print more
messages."
    (interactive (list 'report-progress))
    (cl-block nil
      (unless (file-newer-than-file-p
               radian-lib-file
               (concat radian-lib-file "c"))
        (when report-progress
          (message "Byte-compiled configuration already up to date"))
        (cl-return))
      (when report-progress
        (message "Byte-compiling updated configuration..."))
      (ignore-errors
        (kill-buffer " *radian-byte-compile*"))
      (let ((default-directory radian-directory))
        (radian-env-setup)
        (make-process
         :name "radian-byte-compile"
         :buffer " *radian-byte-compile*"
         :command '("make" "compile")
         :noquery t
         :sentinel
         (lambda (proc _event)
           (unless (process-live-p proc)
             (with-current-buffer (process-buffer proc)
               (if (= 0 (process-exit-status proc))
                   (progn
                     (insert "Byte-compilation completed successfully!\n")
                     (message
                      (if report-progress
                          "Byte-compiling updated configuration...done"
                        "Byte-compiled updated configuration")))
                 (save-match-data
                   (save-excursion
                     (goto-char (point-min))
                     (when (looking-at "In toplevel form:")
                       (forward-line))
                     (when (looking-at "radian\\.el:[0-9]+:[0-9]+:Warning: ")
                       (goto-char (match-end 0)))
                     (message "Failed to byte-compile")
                     (if (looking-at ".+")
                         (progn
                           (message "%s" (match-string 0))
                           (forward-line)
                           (while (looking-at ".+")
                             (message "%s" (match-string 0))
                             (forward-line)))
                       (message " (no output)"))))))))))))

  :blackout (emacs-lisp-compilation-mode . "Byte-Compile"))

;;;;; Emacs Lisp linting
;; Feature `checkdoc' provides some tools for validating Elisp
;; docstrings against common conventions.
(leaf! checkdoc
  :init
  ;; Not sure why this isn't included by default.
  (put 'checkdoc-package-keywords-flag 'safe-local-variable #'booleanp))

;; Package `elisp-lint', not installed, provides a linting framework
;; for Elisp code. We use `with-eval-after-load' because `leaf'
;; is configured to try to `require' features during byte-compilation.
(with-eval-after-load 'elisp-lint
  :init
  ;; From the package. We need this because some packages set this as
  ;; a file-local variable, but we don't install the package so Emacs
  ;; doesn't know the variable is safe.
  (put 'elisp-lint-indent-specs 'safe-local-variable #'listp))

;; Package `package-lint' provides a command that lets you check for
;; common package.el packaging problems in your packages.
(leaf package-lint)

;;; Applications
;;;; Organization
;; Use `use-feature' here because we already installed Org earlier.
(leaf! org
  :defvar org-insert-heading-respect-content org-startup-folded org-enforce-todo-dependencies
  org-special-ctrl-a/e org-special-ctrl-k org-highlight-sparse-tree-matches
  :defun org-insert-heading
  :chord (",c" . org-capture)
  :bind (org-mode-map
         ;; See discussion of this function below.
         ("C-M-RET" . radian-org-insert-heading-at-point)
         ("C-M-<return>" . radian-org-insert-heading-at-point))
  :config

  ;; If you try to insert a heading in the middle of an entry, don't
  ;; split it in half, but instead insert the new heading after the
  ;; end of the current entry.
  (setq org-insert-heading-respect-content t)
  ;; But add a new function for recovering the old behavior (see
  ;; `:bind' above).
  (defun radian-org-insert-heading-at-point ()
    "Insert heading without respecting content.
This runs `org-insert-heading' with
`org-insert-heading-respect-content' bound to nil."
    (interactive)
    (let ((org-insert-heading-respect-content nil))
      (org-insert-heading)))
  ;; Show headlines but not content by default.
  (setq org-startup-folded 'content)
  ;; Make it possible to dim or hide blocked tasks in the agenda view.
  (setq org-enforce-todo-dependencies t)
  ;; Make C-a, C-e, and C-k smarter with regard to headline tags.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)
  (put 'org-tags-exclude-from-inheritance 'safe-local-variable
       #'radian--list-of-strings-p)
  ;; When you create a sparse tree and `org-indent-mode' is enabled,
  ;; the highlighting destroys the invisibility added by
  ;; `org-indent-mode'. Therefore, don't highlight when creating a
  ;; sparse tree.
  (setq org-highlight-sparse-tree-matches nil))
;; Feature `org-indent' provides an alternative view for Org files in
;; which sub-headings are indented.
(leaf! org-indent
  :defun org-indent-mode
  :init

  (add-hook 'org-mode-hook #'org-indent-mode))
;; Feature `org-agenda' from package `org' provides the agenda view
;; functionality, which allows for collating TODO items from your Org
;; files into a single buffer.
(leaf! org-agenda
  :defvar org-directory
  :defun org-agenda-set-mode-name
  :config

  (radian-defadvice radian--advice-org-agenda-default-directory
      (org-agenda &rest args)
    :around #'org-agenda
    "If `org-directory' exists, set `default-directory' to it in the agenda.
This makes the behavior of `find-file' more reasonable."
    (let ((default-directory (if (file-exists-p org-directory)
                                 org-directory
                               default-directory)))
      (apply org-agenda args)))

  (radian-defadvice radian--advice-blackout-org-agenda
      (&rest _)
    :override #'org-agenda-set-mode-name
    "Override the `org-agenda' mode lighter to just \"Org-Agenda\"."
    "Org-Agenda")

  (radian-defhook radian--org-agenda-setup ()
    org-agenda-mode-hook
    "Disable `visual-line-mode' locally."
    ;; See https://superuser.com/a/531670/326239.
    (visual-line-mode -1)
    (let ((inhibit-message t)
          (message-log-max nil))
      ;; I'm not exactly sure why this is necessary. More research is
      ;; needed.
      (toggle-truncate-lines +1)))

  ;; Hide blocked tasks in the agenda view.
  (setq org-agenda-dim-blocked-tasks 'invisible))
;; Feature `org-clock' from package `org' provides the task clocking
;; functionality.
(leaf! org-clock
  ;; We have to autoload these functions in order for the below code
  ;; that enables clock persistence without slowing down startup to
  ;; work.
  :commands (org-clock-load org-clock-save)
  :init

  ;; Allow clock data to be saved persistently.
  (setq org-clock-persist t)

  ;; Actually enable clock persistence. This is taken from
  ;; `org-clock-persistence-insinuate', but we can't use that function
  ;; since it causes both `org' and `org-clock' to be loaded for no
  ;; good reason.
  (add-hook 'org-mode-hook 'org-clock-load)
  (radian-defhook radian--org-clock-save ()
    kill-emacs-hook
    "Run `org-clock-save', but only if Org has been loaded.
Using this on `kill-emacs-hook' instead of `org-clock-save'
prevents a delay on killing Emacs when Org was not yet loaded."
    (when (featurep 'org)
      (org-clock-save)))

  (defun radian--advice-org-clock-load-automatically (&rest _)
    "Run `org-clock-load'.
This is a `:before' advice for various Org functions which might
be invoked before `org-mode-hook' is run."
    (org-clock-load))

  :config

  (advice-add #'org-clock-load :around #'radian--advice-silence-messages)

  (dolist (fun '(org-clock-in
                 org-clock-out
                 org-clock-in-last
                 org-clock-goto
                 org-clock-cancel))
    (advice-add fun :before #'radian--advice-org-clock-load-automatically)))

;;;; Filesystem management

;; When deleting a file interactively, move it to the trash instead.
(setq delete-by-moving-to-trash t)

;; Package `osx-trash' provides functionality that allows Emacs to
;; place files in the trash on macOS.
(leaf osx-trash
  :commands (osx-trash-move-file-to-trash)
  :init/el-patch

  (defun osx-trash-setup ()
    "Provide trash support for OS X.

Provide `system-move-file-to-trash' as an alias for
`osx-trash-move-file-to-trash'.

Note that you still need to set `delete-by-moving-to-trash' to a
non-nil value to enable trashing for file operations."
    (when (and (eq system-type 'darwin)
               (not (fboundp 'system-move-file-to-trash)))
      (defalias 'system-move-file-to-trash
        'osx-trash-move-file-to-trash)))

  (osx-trash-setup))

;;;;; Dired

;; Dired has some trouble parsing out filenames that have e.g. leading
;; spaces, unless the ls program used has support for Dired. GNU ls
;; has this support, so if it is available we tell Dired (and the
;; `list-directory' command, not that it sees much use) to use it.
;;
;; This is in an advice so that we can defer the PATH search until
;; necessary.
(radian-defadvice radian--use-gls-for-list-directory (&rest _)
  :before #'list-directory
  "Make Dired use GNU ls, if it is available."
  (when (executable-find "gls")
    (setq insert-directory-program "gls"))
  ;; Only do the check once, for efficiency.
  (advice-remove #'list-directory #'radian--use-gls-for-list-directory))

;; Feature `dired' provides a simplistic filesystem manager in Emacs.
(leaf! dired
  :defvar dired-use-ls-dired dired-clean-confirm-killing-deleted-buffers
  dired-auto-revert-buffer dired-omit-verbose
  :defun dired-rename-file dired-insert-directory
  :bind (dired-mode-map
              ;; This binding is way nicer than ^. It's inspired by
              ;; Sunrise Commander.
              ("J" . dired-up-directory))
  :bind* (("C-x w" . radian-rename-current-file))
  :defer-config

  (defun radian-rename-current-file (newname)
    "Rename file visited by current buffer to NEWNAME.
Interactively, prompt the user for the target filename, with
completion.

If NEWNAME is a directory then extend it with the basename of
`buffer-file-name'. Make parent directories automatically."
    (interactive
     (progn
       (unless buffer-file-name
         (user-error "Current buffer is not visiting a file"))
       (let ((newname (read-file-name "Rename to: " nil buffer-file-name)))
         (when (equal (file-truename newname)
                      (file-truename buffer-file-name))
           (user-error "%s" "Can't rename a file to itself"))
         (list newname))))
    (unless buffer-file-name
      (error "Current buffer is not visiting a file"))
    (when (equal (file-truename newname)
                 (file-truename buffer-file-name))
      (error "%s: %s" "Can't rename a file to itself" newname))
    (when (equal newname (file-name-as-directory newname))
      (setq newname
            (concat newname (file-name-nondirectory buffer-file-name))))
    (make-directory (file-name-directory newname) 'parents)
    ;; Passing integer as OK-IF-ALREADY-EXISTS means prompt for
    ;; confirmation before overwriting. Why? Who can say...
    (dired-rename-file buffer-file-name newname 0))

  (radian-defadvice radian--advice-dired-check-for-ls-dired (&rest _)
    :before #'dired-insert-directory
    "Check if ls --dired is supported ahead of time, and silently.

This advice prevents Dired from printing a message if your ls
does not support the --dired option. (We do this by performing
the check ourselves, and refraining from printing a message in
the problematic case.)"
    (when (eq dired-use-ls-dired 'unspecified)
      (setq dired-use-ls-dired
            (eq 0 (call-process insert-directory-program
                                nil nil nil "--dired")))))

  (add-hook 'dired-mode-hook #'radian--autorevert-silence)

  ;; Disable the prompt about whether I want to kill the Dired buffer
  ;; for a deleted directory. Of course I do! It's just a Dired
  ;; buffer, after all. Note that this variable, for reasons unknown
  ;; to me, is defined in `dired-x', but only affects the behavior of
  ;; functions defined in `dired'.
  (setq dired-clean-confirm-killing-deleted-buffers nil)

  ;; Instantly revert Dired buffers on re-visiting them, with no
  ;; message. (A message is shown if insta-revert is either disabled
  ;; or determined dynamically by setting this variable to a
  ;; function.)
  (setq dired-auto-revert-buffer t))

(leaf! dired-x
  :defun dired-guess-default
  :bind (;; Bindings for jumping to the current directory in Dired.
         ("C-x C-j" . dired-jump)
         ("C-x 4 C-j" . dired-jump-other-window))
  :config

  ;; Prevent annoying "Omitted N lines" messages when auto-reverting.
  (setq dired-omit-verbose nil)

  (when IS-MAC
    (radian-defadvice radian--advice-dired-guess-open-on-macos
        (&rest _)
      :override #'dired-guess-default
      "Cause Dired's '!' command to use open(1).
This advice is only activated on macOS, where it is helpful since
most of the Linux utilities in `dired-guess-shell-alist-default'
are probably not going to be installed."
      "open")))

;;;; Terminal emulator

;; Feature `term' provides a workable, though slow, terminal emulator
;; within Emacs.
(leaf! term
  :bind (;; Allow usage of more commands from within the terminal.
         term-raw-map
         ("M-x" . execute-extended-command)
         ("C-h" . help-command)))

;;;; Version control

;; Feature `vc-hooks' provides hooks for the Emacs VC package. We
;; don't use VC, because Magit is superior in pretty much every way.
(leaf! vc-hooks
  :config

  ;; Disable VC. This improves performance and disables some annoying
  ;; warning messages and prompts, especially regarding symlinks. See
  ;; https://stackoverflow.com/a/6190338/3538165.
  (setq vc-handled-backends nil))

;; Feature `smerge-mode' provides an interactive mode for visualizing
;; and resolving Git merge conflicts.
(leaf! smerge-mode :blackout t)
;; Package `transient' is the interface used by Magit to display
;; popups.
(leaf transient
  :defun transient-bind-q-to-quit
  :defer-config

  ;; Allow using `q' to quit out of popups, in addition to `C-g'. See
  ;; <https://magit.vc/manual/transient.html#Why-does-q-not-quit-popups-anymore_003f>
  ;; for discussion.
  (transient-bind-q-to-quit))

;; Package `magit' provides a full graphical interface for Git within
;; Emacs.
(leaf magit
  :bind (;; This is the primary entry point for Magit. Binding to C-x
         ;; g is recommended in the manual:
         ;; https://magit.vc/manual/magit.html#Getting-Started
         ("C-x g" . magit-status)
         ;; Alternate transient entry point; binding recommended in
         ;; <https://magit.vc/manual/magit.html#Transient-Commands>.
         ("C-x M-g" . magit-dispatch)
         ;; Completing the trio of bindings in `magit-file-mode-map'.
         ("C-c M-g" . magit-file-dispatch))

  :defvar magit-credential-cache-daemon-socket magit-credential-cache-daemon-process
  magit-save-repository-buffers emacsql-sqlite-executable magit-git-executable
  :defun magit-maybe-start-credential-cache-daemon magit-diff-visit-file--setup

  :init

  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))

  :config/el-patch

  ;; Prevent Emacs asking if we're sure we want to exit, if a
  ;; Magit-spawned git-credential-cache process is running.
  (defun magit-maybe-start-credential-cache-daemon ()
    "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
    (unless (or (not magit-credential-cache-daemon-socket)
                (process-live-p magit-credential-cache-daemon-process)
                (memq magit-credential-cache-daemon-process
                      (list-system-processes)))
      (setq magit-credential-cache-daemon-process
            (or (--first (let* ((attr (process-attributes it))
                                (comm (cdr (assq 'comm attr)))
                                (user (cdr (assq 'user attr))))
                           (and (string= comm "git-credential-cache--daemon")
                                (string= user user-login-name)))
                         (list-system-processes))
                (condition-case nil
                    (el-patch-wrap 2
                      (with-current-buffer
                          (get-buffer-create " *git-credential-cache--daemon*")
                        (start-process "git-credential-cache--daemon"
                                       (el-patch-swap
                                         " *git-credential-cache--daemon*"
                                         (current-buffer))
                                       magit-git-executable
                                       "credential-cache--daemon"
                                       magit-credential-cache-daemon-socket)
                        (el-patch-add
                          (set-process-query-on-exit-flag
                           (get-buffer-process (current-buffer)) nil))))
                  ;; Some Git implementations (e.g. Windows) won't have
                  ;; this program; if we fail the first time, stop trying.
                  ((debug error)
                   (remove-hook
                    'magit-credential-hook
                    #'magit-maybe-start-credential-cache-daemon)))))))

  :config

  ;; The default location for git-credential-cache is in
  ;; ~/.config/git/credential. However, if ~/.git-credential-cache/
  ;; exists, then it is used instead. Magit seems to be hardcoded to
  ;; use the latter, so here we override it to have more correct
  ;; behavior.
  (unless (file-exists-p "~/.git-credential-cache/")
    (let* ((xdg-config-home (or (getenv "XDG_CONFIG_HOME")
                                (expand-file-name "~/.config/")))
           (socket (expand-file-name "git/credential/socket" xdg-config-home)))
      (setq magit-credential-cache-daemon-socket socket)))

  ;; Don't try to save unsaved buffers when using Magit. We know
  ;; perfectly well that we need to save our buffers if we want Magit
  ;; to see them.
  (setq magit-save-repository-buffers nil)

  (transient-append-suffix
    'magit-merge "-n"
    '("-u" "Allow unrelated" "--allow-unrelated-histories"))

  (transient-append-suffix 'magit-pull "-r"
    '("-a" "Autostash" "--autostash"))

  (transient-append-suffix 'magit-fetch "-t"
    '("-u" "Unshallow" "--unshallow")))

;; Feature `magit-diff' from package `magit' handles all the stuff
;; related to interactive Git diffs.
(leaf! magit-diff
  :config

  (radian-defadvice radian--magit-diff-revert-before-smerge (buf _pos)
    :before #'magit-diff-visit-file--setup
    "Before calling `smerge-start-session', try to revert buffer.
This is necessary because it's possible that the file being
visited has changed on disk (due to merge conflict, for example)
but it was already visited, and hasn't been autoreverted yet
(because it hasn't been visible in a window, for example). But
`smerge-start-session', which is called by Magit while jumping
you to the file, will not wait for an autorevert. It will just
see that there aren't any conflict markers in the file and
disable itself. Sad."
    (with-current-buffer buf
      (auto-revert-handler))))

;; Feature `git-commit' from package `magit' provides the commit
;; message editing capabilities of Magit.
(leaf! git-commit
  :config

  ;; Max length for commit message summary is 50 characters as per
  ;; https://chris.beams.io/posts/git-commit/.
  (setq git-commit-summary-max-length 50))

;; Package `magit-todo'
(leaf magit-todos
  :commands magit-todos-mode magit-todos-list
  :after hl-todo)

;; Package `emacsql-sqlite' is a dependency of Forge which is used to
;; interact with the SQLite database that Forge uses to keep track of
;; information about pull requests.
(leaf! emacsql-sqlite
  :defun emacsql-sqlite-ensure-binary
  :init

  ;; Put the EmacSQL binary in the repository, not the build dir. That
  ;; way we don't have to recompile it every time packages get rebuilt
  ;; by straight.el. See
  ;; <https://github.com/raxod502/straight.el/issues/274> for not
  ;; having to use the internal function `straight--dir'.
  (setq emacsql-sqlite-data-root (straight--repos-dir "emacsql"))

  :config

  (radian-defadvice radian--advice-emacsql-no-compile-during-compile
      (&rest _)
    :before-until #'emacsql-sqlite-ensure-binary
    "Prevent EmacSQL from trying to compile stuff during byte-compilation.
This is a problem because Forge tries to get EmacSQL to compile
its binary at load time, which is bad (you should never do
anything significant at package load time) since it breaks CI."
    byte-compile-current-file))

;; Package `forge' provides a GitHub/GitLab/etc. interface directly
;; within Magit.
(leaf forge)

;; Feature `forge-core' from package `forge' implements the core
;; functionality.
(leaf! forge-core
  :defun forge-get-repository emacsql-sqlite-compile
  :config

  (radian-defadvice radian--forge-get-repository-lazily (&rest _)
    :before-while #'forge-get-repository
    "Make `forge-get-repository' return nil if the binary isn't built yet.
This prevents having EmacSQL try to build its binary (which may
be annoying, inconvenient, or impossible depending on the
situation) just because you tried to do literally anything with
Magit."
    (file-executable-p emacsql-sqlite-executable))

  (radian-defadvice radian--forge-build-binary-lazily (&rest _)
    :before #'forge-dispatch
    "Make `forge-dispatch' build the binary if necessary.
Normally, the binary gets built as soon as Forge is loaded, which
is terrible UX. We disable that above, so we now have to manually
make sure it does get built when we actually issue a Forge
command."
    (unless (file-executable-p emacsql-sqlite-executable)
      (emacsql-sqlite-compile 2))))

;; Package `git-gutter' adds a column to the left-hand side of each
;; window, showing which lines have been added, removed, or modified
;; since the last Git commit.
(leaf git-gutter
  :require t
  :commands (git-gutter:previous-hunk
             git-gutter:next-hunk
             radian-git-gutter:beginning-of-hunk
             git-gutter:end-of-hunk
             git-gutter:revert-hunk)
  :init

  (radian-bind-key "v p" #'git-gutter:previous-hunk)
  (radian-bind-key "v n" #'git-gutter:next-hunk)
  (radian-bind-key "v a" #'radian-git-gutter:beginning-of-hunk)
  (radian-bind-key "v e" #'git-gutter:end-of-hunk)
  (radian-bind-key "v k" #'git-gutter:revert-hunk)

  ;; Disable in Org mode, as per
  ;; <https://github.com/syl20bnr/spacemacs/issues/10555> and
  ;; <https://github.com/syohex/emacs-git-gutter/issues/24>.
  ;; Apparently, the mode-enabling function for global minor modes
  ;; gets called for new buffers while they are still in
  ;; `fundamental-mode', before a major mode has been assigned. I
  ;; don't know why this is the case, but adding `fundamental-mode'
  ;; here fixes the issue.
  (setq git-gutter:disabled-modes '(fundamental-mode org-mode))

  ;; (radian-defhook radian--git-gutter-load ()
  ;;   find-file-hook
  ;;   "Load `git-gutter' when initially finding a file."
  ;;   (require 'git-gutter)
  ;;   (remove-hook 'find-file-hook #'radian--git-gutter-load))

  ;; :defvar git-gutter:ask-p git-gutter:diffinfos git-gutter-mode
  ;; :defun git-gutter:awhen git-gutter:search-here-diffinfo git-gutter-hunk-start-line git-gutter:post-command-hook
  ;; fringe-helper-define fringe-helper-insert-region git-gutter-fr:view-diff-infos
  :config

  ;; Don't prompt when reverting hunk.
  (setq git-gutter:ask-p nil)

  (global-git-gutter-mode +1)

  (defun radian-git-gutter:beginning-of-hunk ()
    "Move to beginning of current diff hunk."
    (interactive)
    (git-gutter:awhen (git-gutter:search-here-diffinfo git-gutter:diffinfos)
      (let ((lines (- (git-gutter-hunk-start-line it) (line-number-at-pos))))
        ;; This will move backwards since lines will be negative.
        (forward-line lines))))

  ;; Shuffle around all the hooks. `git-gutter' puts itself on a bunch
  ;; of different things, but not exactly the right things. Remove all
  ;; its meddling, and then do the right thing (run on window or
  ;; buffer switch after a top-level command, after a buffer revert,
  ;; and after Apheleia runs).

  (remove-hook 'post-command-hook #'git-gutter:post-command-hook)
  (ad-deactivate #'quit-window)
  (ad-deactivate #'switch-to-buffer)

  (defvar radian--git-gutter-last-buffer-and-window nil
    "Cons of current buffer and selected window before last command.
This is used to detect when the current buffer or selected window
changes, which means that `git-gutter' needs to be re-run.")

  (radian-defhook radian--git-gutter-on-buffer-or-window-change ()
    post-command-hook
    "Update `git-gutter' when current buffer or selected window changes."
    (let ((new (cons (current-buffer) (selected-window))))
      (unless (equal new radian--git-gutter-last-buffer-and-window)
        (setq radian--git-gutter-last-buffer-and-window new)
        ;; Sometimes the current buffer has not gotten updated yet
        ;; after switching window, for example after `quit-window'.
        (with-current-buffer (window-buffer)
          (when git-gutter-mode
            (when buffer-file-name
              (unless (file-remote-p buffer-file-name)
                (git-gutter))))))))

  (leaf! autorevert
    :config

    (radian-defhook radian--git-gutter-after-autorevert ()
      after-revert-hook
      "Update `git-gutter' after the buffer is autoreverted."
      (when git-gutter-mode
        (git-gutter))))

  (leaf! apheleia
    :config

    (radian-defhook radian--git-gutter-after-apheleia ()
      apheleia-post-format-hook
      "Update `git-gutter' after Apheleia formats the buffer."
      (when git-gutter-mode
        (git-gutter))))

  :blackout git-gutter-mode)

;; Package `git-gutter-fringe' integrates with `git-gutter' to make
;; the gutter display use the window fringe rather than a column of
;; text.
;;
;; Note that we only even put the package on the load path if
;; `git-gutter-fringe' fringe is defined. The function might not be
;; defined if Emacs was not built with X/Cocoa support, and if that's
;; the case, then loading it will cause errors (and besides that, will
;; break `git-gutter' since the fringe stuff is not available).
;; However, we do need to load the package in order to byte-compile
;; this configuration. That's okay since it's only done in a
;; subprocess (so it won't break `git-gutter') but we still need to
;; fix the errors in that case. Hence the `eval-when-compile'.
(straight-register-package 'git-gutter-fringe)
(when (fboundp 'define-fringe-bitmap)
  (eval-when-compile
    (unless (fboundp 'define-fringe-bitmap)
      (fset 'define-fringe-bitmap #'ignore))
    (unless (boundp 'overflow-newline-into-fringe)
      (setq overflow-newline-into-fringe t)))
  (leaf git-gutter-fringe
    :require t
    :after git-gutter
    :init

    (leaf! git-gutter
      :config

      ;; This function is only available when Emacs is built with
      ;; X/Cocoa support, see e.g.
      ;; <https://github.com/pft/mingus/issues/5>. If we try to
      ;; load/configure `git-gutter-fringe' without it, we run into
      ;; trouble.
      (when (fboundp 'define-fringe-bitmap)
        (require 'git-gutter-fringe)))

    :config

    (fringe-helper-define 'radian--git-gutter-blank nil
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........"
      "........")

    (radian-defadvice radian--advice-git-gutter-remove-bitmaps
        (func &rest args)
      :around #'git-gutter-fr:view-diff-infos
      "Disable the cutesy bitmap pluses and minuses from `git-gutter-fringe'.
Instead, display simply a flat colored region in the fringe."
      (radian-flet ((defun fringe-helper-insert-region
                        (beg end _bitmap &rest args)
                      (apply fringe-helper-insert-region
                             beg end 'radian--git-gutter-blank args)))
        (apply func args)))))

;; Package `rg' just provides an interactive command `rg' to run the
;; search tool of the same name.
(leaf rg
  :defun rg-run rg-read-pattern rg-default-alias rg-project-root
  :bind ("C-c k" . radian-rg)
  :config

  (defun radian-rg (&optional only-current-type)
    "Search for string in current project.
With ONLY-CURRENT-TYPE non-nil, or interactively with prefix
argument, search only in files matching current type."
    (interactive "P")
    (rg-run (rg-read-pattern nil)
            (if only-current-type (car (rg-default-alias)) "*")
            (rg-project-root buffer-file-name))))

;;;; Internet applications

;; Feature `browse-url' provides commands for opening URLs in
;; browsers.
(leaf! browse-url
  :defun magit-mode
  :bind ("C-c C-o" . browse-url-at-point)
  :init

  (defun radian--browse-url-predicate ()
    "Return non-nil if \\[browse-url-at-point] should be rebound."
    ;; All of these major modes provide more featureful bindings for
    ;; C-c C-o than `browse-url-at-point'.
    (not (derived-mode-p
          #'markdown-mode #'org-mode #'org-agenda-mode #'magit-mode)))

  ;; (bind-key "C-c C-o" #'browse-url-at-point global-map
  ;;           #'radian--browse-url-predicate)
  )

;;; Startup

;; Disable the *About GNU Emacs* buffer at startup, and go straight
;; for the scratch buffer.
(setq inhibit-startup-screen t)

;; Contrary to what many Emacs users have in their configs, you really don't
;; need more than this to make UTF-8 the default coding system:
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))       ; pretty
(prefer-coding-system 'utf-8)            ; pretty
(setq locale-coding-system 'utf-8)       ; please
;; The clipboard's on Windows could be in a wider (or thinner) encoding than
;; utf-8 (likely UTF-16), so let Emacs/the OS decide what encoding to use there.
(unless IS-WINDOWS
  (setq selection-coding-system 'utf-8)) ; with sugar on top

;; Disable warnings from legacy advice system. They aren't useful, and what can
;; we do about them, besides changing packages upstream?
(setq ad-redefinition-action 'accept)

;; Make `apropos' et co search more extensively. They're more useful this way.
(setq apropos-do-all t)

;; A second, case-insensitive pass over `auto-mode-alist' is time wasted, and
;; indicates misconfiguration (or that the user needs to stop relying on case
;; insensitivity).
(setq auto-mode-case-fold nil)

;; Reduce *Message* noise at startup. An empty scratch buffer (or the dashboard)
;; is more than enough.
(setq inhibit-startup-message t
      inhibit-default-init t
      ;; Shave seconds off startup time by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode', which
      ;; pull in a ton of packages.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Get rid of "For information about GNU Emacs..." message at startup, unless
;; we're in a daemon session where it'll say "Starting Emacs daemon." instead,
;; which isn't so bad.
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1.0)

;; Emacs stores `authinfo' in $HOME and in plain-text. Let's not do that, mkay?
;; This file stores usernames, passwords, and other such treasures for the
;; aspiring malicious third party.
;; (setq auth-sources (list "~/.authinfo.gpg"))

;; HACK Stop sessions from littering the user directory
(defadvice! radian--use-cache-dir-a (session-id)
  :override #'emacs-session-filename
  (concat radian-var-dir "emacs-session." session-id))

;;
;;; Native Compilation support (http://akrl.sdf.org/gccemacs.html)

;; Don't store eln files in ~/.emacs.d/eln-cache (they are likely to be purged
;; when upgrading Radian).
(when (boundp 'comp-eln-load-path)
  (add-to-list 'comp-eln-load-path (concat radian-var-dir "eln/")))

(after! comp
  ;; HACK Disable native-compilation for some troublesome packages
  (dolist (entry (list (concat "\\`" (regexp-quote radian-local-dir) ".*/evil-collection-vterm\\.el\\'")
                       (concat "\\`" (regexp-quote radian-autoloads-file) "'")))
    (add-to-list 'comp-deferred-compilation-black-list entry)))

(defun radian-display-benchmark-h (&optional return-p)
  "Display a benchmark including number of packages and modules loaded.

If RETURN-P, return the message as a string instead of displaying it."
  (funcall (if return-p #'format #'message)
           "Startup in %.03fs"
           (float-time (time-subtract (current-time) before-init-time))))

;; Bootstrap our GC manager
(add-hook 'after-init-hook #'gcmh-mode)
;; Bootstrap the interactive session
(add-hook 'window-setup-hook #'radian-display-benchmark-h)

;;; Shutdown

(defun radian-really-kill-emacs ()
  "Kill Emacs immediately, bypassing `kill-emacs-hook'."
  (interactive)
  (let ((kill-emacs-hook nil))
    (kill-emacs)))

;; Package `restart-emacs' provides an easy way to restart Emacs from
;; inside of Emacs, both in the terminal and in windowed mode.
(leaf restart-emacs
  :commands (radian-new-emacs)
  :defun restart-emacs--ensure-can-restart restart-emacs--guess-startup-directory
  restart-emacs--frame-restore-args restart-emacs--launch-other-emacs

  :init

  (defvar radian--restart-in-progress nil
    "Used to prevent infinite recursion.
This is non-nil if `radian--advice-kill-emacs-dispatch' has called
`restart-emacs'.")

  (defvar radian--restart-emacs-eager-hook-functions
    ;; This list contains hooks that I determined via profiling to be
    ;; slow (double-digit milliseconds).
    '(prescient--save
      radian--org-clock-save
      save-place-kill-emacs-hook)
    "List of functions on `kill-emacs-hook' which can be run eagerly.
If actually present on `kill-emacs-hook', then these functions
are run immediately on `save-buffers-kill-emacs'. This means that
Emacs shutdown appears to be slightly faster.

Functions can only be added here if it is okay to run them even
when shutting down Emacs is canceled. However, it is fine to put
functions here that aren't actually present on `kill-emacs-hook'.")

  (defvar radian--restart-emacs-eager-hook-functions-run nil
    "List of functions on `kill-emacs-hook' which have been run eagerly.
The global value of this variable is irrelevant; it is always
bound dynamically before being used.")

  (autoload #'restart-emacs--translate-prefix-to-args "restart-emacs")

  (radian-defadvice radian--advice-kill-emacs-dispatch
      (save-buffers-kill-emacs &optional arg)
    :around #'save-buffers-kill-emacs
    "Allow restarting Emacs or starting a new session on shutdown."
    (if radian--restart-in-progress
        (funcall save-buffers-kill-emacs arg)
      (let ((radian--restart-in-progress t)
            ;; Don't mutate the global value.
            (radian--restart-emacs-eager-hook-functions-run nil)
            (prompt (concat "Really exit (or restart, or start new, or kill) "
                            "Emacs? (y/n/r/e/k) "))
            (key nil))
        (dolist (func radian--restart-emacs-eager-hook-functions)
          ;; Run eager hook functions asynchronously while waiting for
          ;; user input. Use a separate idle timer for each function
          ;; because the order shouldn't be important, and because
          ;; that way if we don't actually restart then we can cancel
          ;; out faster (we don't have to wait for all the eager hook
          ;; functions to run).
          (run-with-idle-timer
           0 nil
           (lambda ()
             (when (and radian--restart-in-progress
                        (memq func kill-emacs-hook))
               (funcall func)
               ;; Thank goodness Elisp is single-threaded.
               (push func radian--restart-emacs-eager-hook-functions-run)))))
        (while (null key)
          (let ((cursor-in-echo-area t))
            (when minibuffer-auto-raise
              (raise-frame (window-frame (minibuffer-window))))
            (setq key
                  (read-key (propertize prompt
                                        'face 'minibuffer-prompt)))
            ;; No need to re-run the hooks that we already ran
            ;; eagerly. (This is the whole point of those
            ;; shenanigans.)
            (let ((kill-emacs-hook
                   (cl-remove-if
                    (lambda (func)
                      (memq
                       func
                       radian--restart-emacs-eager-hook-functions-run))
                    kill-emacs-hook)))
              (pcase key
                ((or ?y ?Y) (funcall save-buffers-kill-emacs arg))
                ((or ?n ?N))
                ((or ?r ?R)
                 (restart-emacs arg))
                ((or ?e ?E)
                 (radian-new-emacs
                  (restart-emacs--translate-prefix-to-args arg)))
                ((or ?k ?K) (radian-really-kill-emacs))
                (?\C-g (signal 'quit nil))
                (_ (setq key nil))))))
        (message "%s%c" prompt key))))

  :config/el-patch

  (defun (el-patch-swap restart-emacs radian-new-emacs)
      (&optional args)
    (el-patch-concat
      (el-patch-swap
        "Restart Emacs."
        "Start a new Emacs session without killing the current one.")
      "

When called interactively ARGS is interpreted as follows

- with a single `universal-argument' (`C-u') Emacs is "
      (el-patch-swap "restarted" "started")
      "
  with `--debug-init' flag
- with two `universal-argument' (`C-u') Emacs is "
      (el-patch-swap "restarted" "started")
      " with
  `-Q' flag
- with three `universal-argument' (`C-u') the user prompted for
  the arguments

When called non-interactively ARGS should be a list of arguments
with which Emacs should be "
      (el-patch-swap "restarted" "started")
      ".")
    (interactive "P")
    ;; Do not trigger a restart unless we are sure, we can restart emacs
    (restart-emacs--ensure-can-restart)
    ;; We need the new emacs to be spawned after all kill-emacs-hooks
    ;; have been processed and there is nothing interesting left
    (let* ((default-directory (restart-emacs--guess-startup-directory))
           (translated-args (if (called-interactively-p 'any)
                                (restart-emacs--translate-prefix-to-args args)
                              args))
           (restart-args (append translated-args
                                 ;; When Emacs is started with a -Q
                                 ;; restart-emacs's autoloads would
                                 ;; not be present causing the the
                                 ;; --restart-emacs-desktop argument
                                 ;; to be unhandled
                                 (unless (member "-Q" translated-args)
                                   (restart-emacs--frame-restore-args))))
           (el-patch-remove
             (kill-emacs-hook
              (append kill-emacs-hook
                      (list (apply-partially
                             #'restart-emacs--launch-other-emacs
                             restart-args))))))
      (el-patch-swap
        (save-buffers-kill-emacs)
        (restart-emacs--launch-other-emacs restart-args)))))

;;; Miscellaneous

;; Typing yes/no is obnoxious when y/n will do
(advice-add #'yes-or-no-p :override #'y-or-n-p)

;; EN date format
(setq system-time-locale "C")

;; Enable all disabled commands.
(setq disabled-command-function nil)

;; Disable warnings from obsolete advice system. They don't provide
;; useful diagnostic information and often they can't be fixed except
;; by changing packages upstream.
(setq ad-redefinition-action 'accept)

;;; Appearance

;; Make the initial frame maximized, unless using the Mac port in
;; which case this does horrifying things that prevent you from
;; resizing the frame.
(unless (boundp 'mac-option-modifier)
  (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

;; Allow you to resize frames however you want, not just in whole
;; columns. "The 80s called, they want their user interface back"
(setq frame-resize-pixelwise t)

(defcustom radian-font nil
  "Default font, as a string. Nil means use the default.
This is passed to `set-frame-font'."
  :type '(choice string (const :tag "Default" nil)))

(defcustom radian-font-size nil
  "Default font size, in pixels. Nil means use the default."
  :type '(choice integer (const :tag "Default" nil)))

;; Turn off the alarm bell.
(setq ring-bell-function #'ignore)

;; Display keystrokes in the echo area immediately, not after one
;; second. We can't set the delay to zero because somebody thought it
;; would be a good idea to have that value suppress keystroke display
;; entirely.
(setq echo-keystrokes 1e-6)

;; Unfortunately, `which-key' sets an internal variable at load time
;; based on the value of `echo-keystrokes', and then later overrides
;; `echo-keystrokes' to the value of this internal variable,
;; effectively overwriting our configuration here. Stop that behavior.
(leaf! which-key
  :config

  (setq which-key-echo-keystrokes echo-keystrokes))

;; Don't suggest shorter ways to type commands in M-x, since they
;; don't apply when using Selectrum.
(setq suggest-key-bindings 0)

;; Don't blink the cursor on the opening paren when you insert a
;; closing paren, as we already have superior handling of that from
;; Smartparens.
(setq blink-matching-paren nil)

;; frame name show either a file or a buffer name
(setq frame-title-format
      '("" user-login-name "@" system-name " | "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Rainbow
(leaf rainbow-mode
  :hook ((prog-mode-hook text-mode-hook) . rainbow-mode)
  :config
  (add-hook! '(c-mode-hook c++-mode-hook) (rainbow-mode -1))
  (with-no-warnings
    ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
    ;; @see https://emacs.stackexchange.com/questions/36420
    (defun my-rainbow-colorize-match (color &optional match)
      (let* ((match (or match 0))
             (ov (make-overlay (match-beginning match) (match-end match))))
        (overlay-put ov 'ovrainbow t)
        (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                  "white" "black"))
                                (:background ,color)))))
    (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)
    (defun my-rainbow-clear-overlays ()
      "Clear all rainbow overlays."
      (remove-overlays (point-min) (point-max) 'ovrainbow t))
    (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))
  :blackout t)

;;;; set initial frame appearance
(if (display-graphic-p)
    (appendq! initial-frame-alist
                 '((tool-bar-lines . 0)
                   ;;(width . 100)
                   ;;(height . 30)
                   ;;(left . 50)
                   ;;(top . 50)
                   ;;(undecorated . t)
                   (alpha . (95 . 80))))
  (appendq! initial-frame-alist '((tool-bar-lines . 0) (alpha . (95 . 80)))))
(appendq! default-frame-alist initial-frame-alist)

(setq minibuffer-message-properties '(face minibuffer-prompt))

;; Disable the contextual menu that pops up when you right-click.
(global-set-key (kbd "<C-down-mouse-1>") nil)

;; The menu bar appears in both graphical and tty frames. Kill it.
(menu-bar-mode -1)

(when (display-graphic-p)

  ;; Disable unnecessary graphical elements.
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
  (tool-bar-mode -1)

  (when IS-MAC

    (radian-defhook radian--disable-menu-bar-again-on-macos (_)
      after-make-frame-functions
      "Disable the menu bar again, because macOS is dumb.
On macOS, for some reason you can't disable the menu bar once it
appears, and also `menu-bar-mode' doesn't prevent the menu bar
from appearing when run during early init. So we do a hack and
turn it off again after creating the first frame."
      (menu-bar-mode -1)))

  ;; Prevent the cursor from blinking. Do it two ways: using the minor
  ;; mode only works during regular init, while using the variable
  ;; only works during early init.
  (blink-cursor-mode -1)
  (setq no-blinking-cursor t)

  ;; Set the default font size.
  (when radian-font-size
    (set-face-attribute 'default nil :height radian-font-size))

  ;; Set the default font. No, I have no idea why we have to do it
  ;; this way. Using `set-face-attribute' does not have an effect,
  ;; unlike with the font size.
  (when radian-font
    (add-to-list 'default-frame-alist `(font . ,radian-font)))

  ;; Use the same font for fixed-pitch text as the rest of Emacs (you
  ;; *are* using a monospace font, right?).
  (set-face-attribute 'fixed-pitch nil :family 'unspecified)

  ;; On macOS, set the title bar to match the frame background.
  (when IS-MAC
    (add-to-list 'default-frame-alist '(ns-appearance . dark))
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))))

;;;; Mode line

;; The following code customizes the mode line to something like:
;; [*] radian.el   18% (18,0)     [radian:develop*]  (Emacs-Lisp)

(defun radian-mode-line-buffer-modified-status ()
  "Return a mode line construct indicating buffer modification status.
This is [*] if the buffer has been modified and whitespace
otherwise. (Non-file-visiting buffers are never considered to be
modified.) It is shown in the same color as the buffer name, i.e.
`mode-line-buffer-id'."
  (propertize
   (if (and (buffer-modified-p)
            (buffer-file-name))
       "[*]"
     "   ")
   'face 'mode-line-buffer-id))

;; Normally the buffer name is right-padded with whitespace until it
;; is at least 12 characters. This is a waste of space, so we
;; eliminate the padding here. Check the docstrings for more
;; information.
(setq-default mode-line-buffer-identification
              (propertized-buffer-identification "%b"))

;; Make `mode-line-position' show the column, not just the row.
(column-number-mode +1)

;; https://emacs.stackexchange.com/a/7542/12534
(defun radian--mode-line-align (left right)
  "Render a left/right aligned string for the mode line.
LEFT and RIGHT are strings, and the return value is a string that
displays them left- and right-aligned respectively, separated by
spaces."
  (let ((width (- (window-total-width) (length left))))
    (format (format "%%s%%%ds" width) left right)))

(defcustom radian-mode-line-left
  '(;; Show [*] if the buffer is modified.
    (:eval (radian-mode-line-buffer-modified-status))
    " "
    ;; Show the name of the current buffer.
    mode-line-buffer-identification
    "   "
    ;; Show the row and column of point.
    mode-line-position
    ;; Show the active major and minor modes.
    "  "
    mode-line-modes)
  "Composite mode line construct to be shown left-aligned."
  :type 'sexp)

(defcustom radian-mode-line-right nil
  "Composite mode line construct to be shown right-aligned."
  :type 'sexp)

;; Actually reset the mode line format to show all the things we just
;; defined.
(setq-default mode-line-format
              '(:eval (replace-regexp-in-string
                       "%" "%%"
                       (radian--mode-line-align
                        (format-mode-line radian-mode-line-left)
                        (format-mode-line radian-mode-line-right))
                       'fixedcase 'literal)))

;;; Closing

(radian--run-hook after-init)

;; Prune the build cache for straight.el; this will prevent it from
;; growing too large. Do this after the final hook to prevent packages
;; installed there from being pruned.
(straight-prune-build-cache)

;; Occasionally prune the build directory as well. For similar reasons
;; as above, we need to do this after local configuration.
(unless (bound-and-true-p radian--currently-profiling-p)
  (when (= 0 (random 100))
    (straight-prune-build-directory)))

;; We should only get here if init was successful. If we do,
;; byte-compile this file asynchronously in a subprocess using the
;; Radian Makefile. That way, the next startup will be fast(er).
(run-with-idle-timer 1 nil #'radian-byte-compile)

;;; Modus-theme configuration.
;; FIXME: straight rebuild modus on every startup.
;; (leaf modus-themes
;;   :straight
;;   ;; (modus-themes :host github :repo "protesilaos/modus-themes"
;;   ;; :branch "main")
;;   (modus-themes :local-repo "~/z-emacs.d/straight/repos/modus-themes")
;;   :require t
;;   :custom
;;   ;; Set customization options to values of your choice
;;   (modus-themes-slanted-constructs . t)
;;   (modus-themes-bold-constructs . nil)
;;   (modus-themes-fringes . nil) ; {nil,'subtle,'intense}
;;   (modus-themes-mode-line . '3d) ; {nil,'3d,'moody}
;;   (modus-themes-syntax . nil) ; Lots of options---continue reading the manual
;;   (modus-themes-intense-hl-line . nil)
;;   (modus-themes-paren-match . 'subtle-bold) ; {nil,'subtle-bold,'intense,'intense-bold}
;;   (modus-themes-links . 'neutral-underline) ; Lots of options---continue reading the manual
;;   (modus-themes-no-mixed-fonts . nil)
;;   (modus-themes-prompts . nil) ; {nil,'subtle,'intense}
;;   (modus-themes-completions . nil) ; {nil,'moderate,'opinionated}
;;   (modus-themes-region . 'bg-only-no-extend) ; {nil,'no-extend,'bg-only,'bg-only-no-extend}
;;   (modus-themes-diffs . nil) ; {nil,'desaturated,'fg-only,'bg-only}
;;   (modus-themes-org-blocks . nil) ; {nil,'grayscale,'rainbow}
;;   (modus-themes-headings ; Lots of options
;;    ;; nil (default fallback option—covers all heading levels)
;;    ;; t (default style for a single heading, when the fallback differs)
;;    ;; no-bold
;;    ;; line
;;    ;; line-no-bold
;;    ;; rainbow
;;    ;; rainbow-line
;;    ;; rainbow-line-no-bold
;;    ;; highlight
;;    ;; highlight-no-bold
;;    ;; rainbow-highlight
;;    ;; rainbow-highlight-no-bold
;;    ;; section
;;    ;; section-no-bold
;;    ;; rainbow-section
;;    ;; rainbow-section-no-bold
;;    . '((1 . section)
;;        (2 . section-no-bold)
;;        (3 . rainbow-line)
;;        (t . rainbow-line-no-bold)))
;;   (modus-themes-variable-pitch-headings . nil)
;;   (modus-themes-scale-headings . nil)
;;   (modus-themes-scale-1 . 1.1)
;;   (modus-themes-scale-2 . 1.15)
;;   (modus-themes-scale-3 . 1.21)
;;   (modus-themes-scale-4 . 1.27)
;;   (modus-themes-scale-5 . 1.33))

;;;; change theme and customize face
(defvar radian-theme-list nil
  "Theme sequence of changing. `(THEME-NAME . IS-DARK-THEME)'")

(defun true-color-p ()
  "Return non-nil on displays that support 256 colors."
  (or
   (display-graphic-p)
   (= (tty-display-color-cells) 16777216)))

(defun radian-change-theme ()
  (interactive)
  (let* ((theme-cons (car radian-theme-list))
         (theme (car theme-cons))
         (is-dark (cdr theme-cons)))
    (disable-theme theme)
    (setq radian-theme-list (append (cdr radian-theme-list) (list theme-cons)))
    (cond ((member theme '(modus-vivendi modus-operandi))
           (progn
             (load-theme theme t)
             (run-hooks 'modus-themes-after-load-theme-hook)))
          (t
           (load-theme 'adwaita t)))

    ;; custom face.
    (let ((background-purple (if is-dark "#373b41" "#d6d6d6"))
          (class '((class color) (min-colors 89)))
          (green (if (true-color-p) "lime green" "#87af5f"))
          (orange (if (true-color-p) "tomato" "#d7875f"))
          (purple (if (true-color-p) "orchid" "#d787d7")))
      (custom-theme-set-faces
       theme
       ;; selectrum-face
       `(selectrum-current-candidate   ((,class (:background ,background-purple :weight bold :foreground ,purple))))
       `(selectrum-primary-highlight   ((,class (:foreground ,orange))))
       `(selectrum-secondary-highlight ((,class (:foreground ,green))))

       ;; M-x prompt-face
       '(minibuffer-prompt      ((((class color) (min-colors 89)) (:foreground "magenta"))))
       '(comint-highlight-input ((((class color) (min-colors 89)) (:foreground "yellow green" :bold t)))))
      (enable-theme theme))))

(radian-bind-key "t" #'radian-change-theme)
(setq radian-theme-list '((modus-vivendi . t) (modus-operandi . nil)))
(radian-change-theme)

;; enable recentf-mode
(leaf! recentf
  :hook (after-init-hook . recentf-mode)
  :preface
  ;; slience annoying message on startup.
  (radian--advice-silence-messages #'recentf-mode)
  :custom
  (recentf-max-saved-items . 200)
  ;; Set history-length longer
  :setq-default (history-length . 100))

;; Local Variables:
;; checkdoc-symbol-words: ("top-level")
;; indent-tabs-mode: nil
;; outline-regexp: ";;;+ "
;; sentence-end-double-space: nil
;; End:

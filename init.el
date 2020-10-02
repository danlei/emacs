;;;;; -*- emacs-lisp -*-
;;;;;
;;;;; Emacs configuration (init.el)
;;;;;

(require 'cl)


;;;;
;;;; backports
;;;;

(unless (fboundp 'with-eval-after-load)
  (defmacro with-eval-after-load (file &rest body)
    "Execute BODY after FILE is loaded.
FILE is normally a feature name, but it can also be a file name,
in case that file does not provide any feature."
    (declare (indent 1) (debug t))
    `(eval-after-load ,file (lambda () ,@body))))


;;;;
;;;; environment
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp")

(case system-type
  (windows-nt
   (setenv "CYGWIN" (concat (getenv "CYGWIN") " nodosfilewarning"))
   (mapc (apply-partially 'add-to-list 'exec-path)
         `("C:/Perl/bin"
           "C:/MinGW/bin"
           ,(expand-file-name "~/programme/latex/miktex/bin")
           ,(expand-file-name "~/programme/darcs-2.5.2-win1")
           "C:/cygwin/bin"))
   (setenv "PATH"
           (mapconcat 'identity
                      `("C:/Perl/bin/"
                        "C:/MinGW/bin"
                        ,(expand-file-name "~/programme/latex/miktex/bin")
                        "C:/cygwin/bin"
                        ,(expand-file-name "~/programme/darcs-2.5.2-win1")
                        ,(getenv "PATH"))
                      ";")))
  (gnu/linux
   (setenv "LANG" "de_DE.UTF-8")
   (setenv "LC_MESSAGES" "C")
   (setenv "MANWIDTH" "72")
   (setenv "PERL5LIB" (expand-file-name "~/.perl5/lib/perl5")))
  (darwin
   (setenv "LANG" "de_DE.UTF-8")
   (setenv "LC_MESSAGES" "C")
   (setenv "MANWIDTH" "72")
   (setenv "PERL5LIB" (expand-file-name "~/.perl5/lib/perl5"))
   (setenv "PATH"
           (mapconcat 'identity
                      `(,(expand-file-name "~/bin")
                        "/usr/local/bin"
                        ,(getenv "PATH"))
                      ":")
           (mapc (apply-partially 'add-to-list 'exec-path)
                 `(,(expand-file-name "~/bin")
                   "/usr/local/bin")))))

(setenv "EDITOR" "emacsclient")


;;;;
;;;; packages
;;;;

(when (require 'package nil t)
  (mapc (apply-partially 'add-to-list 'package-archives)
        '(;("marmalade" . "https://marmalade-repo.org/packages/")
          ("melpa" . "https://melpa.org/packages/")))
  (package-initialize)
  (setq package-enable-at-startup nil))


;;;;
;;;; color-theme
;;;;

(if (< emacs-major-version 24)
    (progn
      (add-to-list 'load-path "~/.emacs.d/elisp/color-theme-6.6.0")
      (when (require 'color-theme nil t)
        (color-theme-initialize)
        (and (require 'color-theme-dhl-hober nil t)
             (color-theme-dhl-hober))))
  (mapc (apply-partially 'add-to-list 'custom-theme-load-path)
        '("~/.emacs.d/themes/"
          "~/.emacs.d/themes/leuven-theme/"
          "~/.emacs.d/themes/white-theme/"
          "~/.emacs.d/themes/tango-dhl-theme/"
          "~/.emacs.d/themes/zenburn-emacs/")))

(defun dhl-toggle-theme ()
  (interactive)
  (pcase custom-enabled-themes
    ('(tango-dhl) (disable-theme 'tango-dhl)
                  (load-theme 'zenburn))
    ('(zenburn) (disable-theme 'zenburn)
                (load-theme 'tango-dhl))
    ('() (if (< 6 (string-to-number (format-time-string "%H")) 19)
             (load-theme 'tango-dhl)
           (load-theme 'zenburn)))))


;;;;
;;;; auto-complete
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/auto-complete")
(add-to-list 'load-path "~/.emacs.d/elisp/popup-el")

(when (require 'auto-complete nil t)
  (ac-set-trigger-key "C-M-i"))

(setq ac-use-menu-map t
      ac-auto-start nil                 ; was 2
      ac-auto-show-menu nil             ; also try 0.8
      )

(setq-default ac-sources
  '(ac-source-filename
    ac-source-functions
;   ac-source-yasnippet
    ac-source-variables
    ac-source-symbols
    ac-source-features
    ac-source-abbrev
    ac-source-words-in-same-mode-buffers
    ac-source-words-in-all-buffer
    ac-source-dictionary))


;;;;
;;;; comint
;;;;

(unless (fboundp 'comint-write-input-ring)
  (require 'comint))

(setq comint-prompt-read-only t)

(if (< emacs-major-version 25)            ; fixes strange comint-bol
    (setq comint-use-prompt-regexp t))    ; behavior that messed up
                                          ; indentation on 24

(make-variable-buffer-local 'comint-use-prompt-regexp) ; prevent modes from
                                                       ; messing with this


;;;
;;; input history
;;;

;; cf. https://emacs.stackexchange.com/a/9952/20422
;;     https://emacs.stackexchange.com/a/14299/20422

(defun dhl-enable-comint-history (histfile)
  "Allow saving comint input history to HISTFILE.

Enable saving input history to HISTFILE when `comint-write-input-ring`
is called. Intended for use in mode hooks.

Additionally, save history to HISTFILE on process status change.
(E.g. when the process quits.)"
  (setq comint-input-ring-file-name histfile)
  (comint-read-input-ring 'silent)
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (set-process-sentinel process
        (lambda (process event)
          (comint-write-input-ring)
          (let ((buf (process-buffer process)))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (insert (format "\nProcess %s %s" process event))))))))))

(add-hook 'kill-buffer-hook 'comint-write-input-ring)
(add-hook 'kill-emacs-hook
          (lambda ()
            (mapcar (lambda (buffer)
                      (with-current-buffer buffer
                        (comint-write-input-ring)))
                    (buffer-list))))


;;;;
;;;; quickrun
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/quickrun")
(when (require 'quickrun nil t)
  (global-set-key (kbd "<s-return>") (lambda ()
                                       (interactive)
                                       (save-buffer)
                                       (quickrun)))
  (quickrun-add-command "m4"
    '((:command . "m4")
      (:exec    . ("%c %s")))
    :mode 'm4-mode))


;;;;
;;;; common lisp
;;;;

(put 'iter 'common-lisp-indent-function 0)


;;;
;;; slime
;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/slime" "~/.emacs.d/elisp/slime/contrib"))

(when (require 'slime nil t)
  (slime-setup '(slime-fancy
;                slime-asdf
                 slime-indentation
                 slime-xref-browser
;                slime-js               ; TODO: setup slime-js
                 )))

(setq slime-enable-evaluate-in-emacs t
      slime-net-coding-system 'utf-8-unix
      slime-protocol-version 'ignore)

(setq slime-lisp-implementations
      `((ccl ,@(list (case system-type
                       ((windows-nt cygwin) '("~/build/ccl/wx86cl" "-K utf-8"))
                       (gnu/linux '("~/build/ccl/lx86cl64" "-K utf-8"))
                       (darwin '("ccl64" "-K utf-8")))))
        (clisp ,@(list (case system-type
                         ((cygwin gnu/linux) '("clisp" "-E utf-8" "-modern"))
                         (windows-nt '("~/build/clisp/clisp-2.49/clisp"
                                       "-modern"))
                         (darwin '("/usr/local/bin/clisp" "-modern")))))
        (ecl ,@(list (case system-type
                       ((gnu/linux '("ecl"))))))
        (sbcl ("sbcl")))
      slime-default-lisp 'ccl)

(add-hook 'slime-mode-hook
          (lambda ()
            (dhl-define-keys slime-mode-map
                             '(("C-c s" slime-selector)
                               ("C-j" newline-and-indent)
                               ("TAB" slime-indent-and-complete-symbol)
                               ("C-c C-d c" cltl2-lookup)
                               ("C-c d" slime-documentation)))))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (dhl-define-keys slime-repl-mode-map
                             '(("C-c s" slime-selector)
                               ("C-c C-d c" cltl2-lookup)
                               ("C-c d" slime-documentation)))))

;;; nyef's pathname fix for cygwin
;;; TODO: still needed?
(when (eq system-type 'cygwin)
  ;; FIXME: It turns out that the slime-tramp contrib wraps over this
  ;; to produce the interface I used when first I did this.
  (setq slime-to-lisp-filename-function
        (lambda (filename)
          (replace-regexp-in-string "\n" ""
                                    (shell-command-to-string
                                     (concat "cygpath -m " filename)))))
  (setq slime-from-lisp-filename-function
        (lambda (filename)
          (replace-regexp-in-string "\n" ""
                                    (shell-command-to-string
                                     (concat "cygpath "
                                             (replace-regexp-in-string
                                              "\\\\" "/" filename))))))
  (setq slime-backend (concat "/cygwin" slime-path slime-backend)))


;;;
;;; hyperspec/cltl
;;;

(setq common-lisp-hyperspec-root (expand-file-name "~/doc/HyperSpec/")
      cltl2-root-url (expand-file-name "~/doc/cltl2/"))

(require 'cltl2 nil t)

(setq Info-additional-directory-list
      (list (expand-file-name "~/.emacs.d/hyperspec/")))

(require 'info-look nil t)

(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(defun dhl-hyperspec-index-search (topic)
  "Look up TOPIC in the indices of the HyperSpec."
  (interactive "sSubject to look up: ")
  (info "ansicl")
  (Info-index topic))

(defun dhl-random-hyperspec ()
  "Browse a random HyperSpec entry."
  (interactive)
  (let* ((random-hyperspec-symbol
          (let ((syms '()))
            (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
            (nth (random (length syms)) syms)))
         (random-page (let ((pages (symbol-value random-hyperspec-symbol)))
                        (nth (random (length pages)) pages))))
    (browse-url (concat common-lisp-hyperspec-root "Body/" random-page))))


;;;
;;; ac
;;;

;; (add-to-list 'load-path "~/.emacs.d/elisp/ac-slime")

;; (require 'ac-slime nil t)

;; (eval-after-load 'auto-complete
;;   '(progn
;;      (add-to-list 'ac-modes 'slime-repl-mode)
;;      (add-to-list 'ac-modes 'js2-mode)
;;      (add-to-list 'ac-modes 'js-mode)
;;      (add-hook 'slime-mode-hook 'set-up-slime-ac)
;;      (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)))


;;;;
;;;; lsp
;;;;

(mapc (lambda (library)
        (add-to-list 'load-path (concat "~/.emacs.d/elisp/"
                                        (symbol-name library))))
      '(ht.el dash.el f.el s.el spinner.el markdown-mode lsp-mode lsp-ui))

(when (and (require 'ht nil t)
           (require 'lsp-mode nil t)
           (require 'lsp-ui nil t))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-mode-map (kbd "C-c C-d") 'lsp-describe-thing-at-point)
  (define-key lsp-mode-map (kbd "<s-mouse-1>")
    (lambda (event &optional promote-to-region)
      (interactive "e\np")
      (mouse-set-point event promote-to-region)
      (lsp-find-definition)))
  (define-key lsp-mode-map (kbd "<s-mouse-3>")
    (lambda (event &optional promote-to-region)
      (interactive "e\np")
      (save-excursion
        (mouse-set-point event promote-to-region)
        (lsp-describe-thing-at-point)))))

(setq lsp-enable-snippet nil
      lsp-auto-configure t
      lsp-signature-render-all nil
      lsp-ui-doc-enable nil
      lsp-ui-doc-use-childframe nil
      lsp-ui-doc-position 'top
      lsp-ui-doc-include-signature nil
      lsp-ui-sideline-enable nil
      lsp-ui-flycheck-enable t
      lsp-ui-flycheck-list-position 'right
      lsp-ui-flycheck-live-reporting t
      lsp-ui-peek-enable t
      lsp-ui-peek-list-width 60
      lsp-ui-peek-peek-height 25
      lsp-file-watch-threshold 20000)

;(setf (lsp--client-priority (gethash 'php-ls lsp-clients)) 3)

;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection '("php" "/home/dleidisch/.composer/vendor/felixfbecker/language-server/bin/php-language-server.php"))
;;                   :major-modes '(php-mode)
;;                   :priority 1
;;                   :server-id 'tramp-lsp
;;                   :remote? t))

(when (require 'longlines nil t)
  (defadvice lsp-describe-thing-at-point
      (around dhl-wrap-lsp-description activate)
    "Wrap `lsp-describe-thing-at-point' buffer."
    (with-current-buffer (get-buffer-create "*lsp-help*")
      (longlines-mode -1))
    ad-do-it
    (with-current-buffer (get-buffer "*lsp-help*")
      (longlines-mode 1))))


;;;;
;;;; paredit
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/paredit")

(modify-syntax-entry ?[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?] ")[" lisp-mode-syntax-table)

(require 'eldoc nil t)                   ; TODO: why is this here?

(when (require 'paredit nil t)
  (mapc (lambda (hook) (add-hook hook (lambda () (paredit-mode 1))))
        '(slime-mode-hook
          slime-repl-mode-hook
          emacs-lisp-mode-hook
          lisp-mode-hook
          ielm-mode-hook
          scheme-mode-hook
          inferior-scheme-mode-hook
          inferior-qi-mode-hook
          qi-mode-hook
          clojure-mode-hook
          cider-repl-mode-hook
          geiser-repl-mode-hook
          eval-expression-minibuffer-setup-hook))
  (setq clojure-enable-paredit t)
  (eldoc-add-command 'paredit-backward-delete 'paredit-close-round))

(add-hook 'paredit-mode-hook
          (lambda ()
            (dhl-define-keys paredit-mode-map
                             '((")" paredit-close-parenthesis)
                               ("M-)" paredit-close-parenthesis-and-newline)
                               ("}" paredit-close-curly)
                               ("{" paredit-open-curly)
                               ("M-{" paredit-wrap-curly)
                               ("M-[" paredit-wrap-square)
                               ("M-f" paredit-forward)
                               ("C-M-f" forward-word)
                               ("M-b" paredit-backward)
                               ("C-M-b" backward-word)
                               ("M-u" backward-up-list)
                               ("C-M-u" upcase-word)
                               ("M-ö" down-list)
                               ("M-t" transpose-sexps)
                               ("C-M-t" transpose-words)
                               ("<M-backspace>" paredit-backward-kill-word)
                               ("<C-backspace>" backward-kill-sexp)
                               ("M-k" kill-sexp)
                               ("M-a" beginning-of-defun)
                               ("M-e" end-of-defun)
                               ("C-M-a" backward-sentence)
                               ("C-M-e" forward-sentence)
                               ("M-q" indent-pp-sexp)
                               ("C-M-q" fill-paragraph)))))


;;;;
;;;; clojure
;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/clojure-mode"
        "~/.emacs.d/elisp/dash.el"
        "~/.emacs.d/elisp/epl"
        "~/.emacs.d/elisp/pkg-info.el"
        "~/.emacs.d/elisp/s.el"
        "~/.emacs.d/elisp/seq.el"        ; TODO: included in emacs 25
        "~/.emacs.d/elisp/spinner.el"
        "~/.emacs.d/elisp/cider"))

(when (require 'clojure-mode nil t)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq inferior-lisp-program (case system-type
                                            (windows-nt "cmd /c lein repl")
                                            (t "lein repl")))
              (put 'match 'clojure-indent-function 1))))

;;(require 'icomplete nil t) ; TODO: enhances minibuffer completion

(when (require 'cider nil t)
  (add-to-list 'same-window-buffer-names "*cider*")
  (add-hook 'cider-interaction-mode-hook
            (lambda ()
              (subword-mode)
              (eldoc-mode)))
  (add-hook 'cider-mode-hook
            (lambda ()
              (subword-mode)
              (eldoc-mode)
              (local-set-key (kbd "TAB")
                             'cider-repl-indent-and-complete-symbol))))

(setq cider-history-file "~/.emacs.d/cider-history"
      cider-show-error-buffer t
      cider-auto-select-error-buffer t
      cider-repl-wrap-history t
      cider-repl-use-pretty-printing t
      cider-repl-use-clojure-font-lock t
      cider-prefer-local-resources t
      cider-repl-display-help-banner nil
      cider-use-overlays 'both
      nrepl-log-messages nil)


;;;;
;;;; qi
;;;;

(require 'qi-mode nil t)

(setq inferior-qi-program
      (case system-type
        (windows-nt (concat "C:/Users/dhl/QiII1.06SBCL/sbcl.exe --noinform "
                            "--core c:/Users/dhl/QiII1.06SBCL/Qi.core"))
        (cygwin (concat "~/build/QiII1.06SBCL/sbcl.exe --noinform --core "
                        "c:/cygwin/home/danlei/build/QiII1.06SBCL/Qi.core"))))


;;;;
;;;; scheme
;;;;

(mapc (lambda (entry)
        (destructuring-bind (symbol . indent) entry
          (put symbol 'scheme-indent-function indent)))
      '((let/cc . 1)
        (shift . 1)
        (reset . 0)
        (for . 1)
        (let-keywords . 3)))

(when (or (load "~/.emacs.d/elisp/geiser/build/elisp/geiser-load" t)
          (load "~/.emacs.d/elisp/geiser/elisp/geiser" t))
  (setq geiser-mode-smart-tab-p t
        geiser-guile-load-init-file-p t
        geiser-default-implementation 'guile)
  (add-to-list 'Info-additional-directory-list "~/.emacs.d/elisp/geiser/doc/"))

(defadvice geiser-repl--maybe-send
    (before dhl-geiser-repl--maybe-send-goto-end-of-form activate)
  "Allow RET to send from anywhere in a form unconditionally."
  (end-of-line)
  (end-of-defun))


;;;;
;;;; newlisp
;;;;

;; TODO: upgrade?

(add-to-list 'load-path "~/.emacs.d/elisp/newlisp-mode")

(add-to-list 'auto-mode-alist '("\\.lsp\\'" . newlisp-mode))

(autoload 'newlisp-mode "newlisp"
  "Newlisp editing mode." t)


;;;;
;;;; j-mode
;;;;

;; TODO: messes up color themes and exhausts max-specpdl-size

(add-to-list 'load-path "~/.emacs.d/elisp/j-mode")

;; (autoload 'j-mode "j-mode.el" "Major mode for editing J files" t)
;; (autoload 'j-console "j-console.el" "Inferior J" t)

;; TODO: FIXME
;; (when (and (require 'j-mode nil t)
;;            (require 'j-console nil t))
;;   (set-face-foreground 'j-verb-face (face-foreground 'default))
;;   (set-face-foreground 'j-adverb-face (face-foreground 'default))
;;   (set-face-foreground 'j-conjunction-face (face-foreground 'default))
;;   (set-face-foreground 'j-other-face (face-foreground 'default)))

;; (add-to-list 'auto-mode-alist '("\\.ij[rstp]\\'" . j-mode))

(setq j-console-cmd "~/build/j/j801/bin/jconsole"
      j-help-local-dictionary-url
      "file:///home/dhl/build/j/j801/addons/docs/help/dictionary/")

;; (custom-set-faces
;;  '(j-verb-face ((t (:foreground "Red"))))
;;  '(j-adverb-face ((t (:foreground "Green"))))
;;  '(j-conjunction-face ((t (:foreground "Blue"))))
;;  '(j-other-face ((t (:foreground "Black")))))

;; (put 'j-verb-face 'face-alias 'default)
;; (put 'j-adverb-face 'face-alias 'default)
;; (put 'j-conjuntion-face 'face-alias 'default)
;; (put 'j-other-face 'face-alias 'default)


;;;;
;;;; apl
;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/gnu-apl-mode"
        "~/.emacs.d/elisp/gnu-apl-mode-docs-ibm"))

(setq gnu-apl-use-old-symbols nil)

(when (require 'gnu-apl-mode nil t)
  (dolist (hook '(gnu-apl-mode-hook gnu-apl-interactive-mode-hook))
    (add-hook hook (lambda ()
                     (eldoc-mode)
                     (setq buffer-face-mode-face 'gnu-apl-default)
                     (buffer-face-mode))))
  (add-hook 'gnu-apl-documentation-mode-hook
            (lambda () (view-mode 1)))
  (set-face-attribute 'gnu-apl-default nil
                      :family "DejaVu Sans Mono")
  (add-to-list 'auto-mode-alist '("\\.apl\\'" . gnu-apl-mode)))

(setq gnu-apl-show-keymap-on-startup nil
      gnu-apl-show-tips-on-start nil
      gnu-apl-use-new-native-library t
;     gnu-apl-libemacs-location
;     (expand-file-name "~/.emacs.d/elisp/gnu-apl-mode/native/libemacs.so")
      )

(when (locate-file "gnu-apl-refdocs-apl2" load-path '(".el" ".elc"))
  (makunbound 'gnu-apl--symbol-doc)
  (load "gnu-apl-refdocs-apl2" t))

(defadvice gnu-apl-show-keyboard
    (after dhl-gnu-apl-show-keyboard-advice last () activate)
  "Switch to the GNU APL keyboard window after invocation."
  (other-window 1))


;;;;
;;;; ruby
;;;;

;; TODO: use a proper ruby mode

(add-to-list 'load-path "~/.emacs.d/elisp/inf-ruby")

(autoload 'run-ruby "inf-ruby.el" "Run irb from Emacs." t)
;(setq ruby-program-name "irb")

(when (eq system-type 'windows-nt)
  (setq ruby-program-name "C:/Ruby192/bin/ruby.exe"))

(add-hook
 'ruby-mode-hook
 (lambda ()
   (dhl-define-keys ruby-mode-map
                    '(("C-M-x" ruby-send-definition)
                      ("C-c C-c" dhl-ruby-send-buffer)
                      ("C-c c" dhl-ruby-send-buffer)
                      ("C-c h" dhl-ruby-browse-class-documentation)))))

(defun dhl-ruby-send-buffer ()
  "Send the current buffer to the inferior Ruby process."
  (interactive)
  (ruby-send-region (point-min) (point-max)))

(defvar dhl-ruby-class-documentation-uri
  "http://ruby-doc.org/core-1.9/classes/"
  "The ruby class documentation root URI.")

(defun dhl-ruby-browse-class-documentation (class-name)
  "Browse the ruby class documentation for CLASS-NAME.

CLASS-NAME is queried in the minibuffer, defaulting to
`word-at-point'."
  (interactive
   (let* ((default (word-at-point))
          (input
           (read-from-minibuffer
            (if default
                (format "Browse documentation for class (default %s): " default)
              "Browse documentation for class: "))))
     (list (if (equal input "") default input))))
  (browse-url (concat dhl-ruby-class-documentation-uri class-name ".html")))


;;;;
;;;; python
;;;;

;; TODO: update config for use with bundled python mode --
;;       but this already seems to work

;(add-to-list 'load-path "~/.emacs.d/elisp/python.el")

(require 'python nil t)

;; (setq python-process-kill-without-query t
;;       python-default-version 3)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c d") 'dhl-pydoc)
;           (setq parens-require-spaces nil)
            (eldoc-mode 1)))

(add-hook
 'python-mode-hook
 (lambda ()
   (dolist (mode-map (list python-mode-map
                           inferior-python-mode-map))
     (dhl-define-keys mode-map
                      '(("M-f" python-nav-forward-sexp)
                        ("C-M-f" forward-word)
                        ("M-b" python-nav-backward-sexp)
                        ("C-M-b" backward-word)
                        ("M-u" python-nav-backward-up-list)
                        ("C-M-u" upcase-word)
                        ("M-t" transpose-sexps)
                        ("C-M-t" transpose-words)
                        ("C-M-a" python-nav-backward-defun)
                        ("C-M-e" python-nav-forward-defun)
;                       ("C-M-q" fill-paragraph)
                        ("C-M-i" python-shell-completion-complete-at-point)
                        ("C-c d" dhl-pydoc))))))

(add-hook
 'inferior-python-mode-hook
 (lambda ()
   (dhl-enable-comint-history "~/.emacs.d/python-input-history")))

(setq dhl-python-command
      (if (eq system-type 'windows-nt)
          "C:/Python32/python.exe"
        "python3"))

(setq-default python-shell-interpreter dhl-python-command
              python-shell-interpreter-args "-ui"
;             python-command dhl-python-command
;             python-python-command dhl-python-command
              )

(defadvice python-describe-symbol
    (after dhl-python-describe-symbol-advice last () activate)
  "Switch to the python help window after invocation."
  (other-window 1))

;; modified from http://ubuntuforums.org/showthread.php?t=1363999
(defun dhl-pydoc (word)
  "Launch pydoc on the word at point"
  (interactive
   (list (let* ((word (thing-at-point 'word t))
                (input (read-string
                        (format "pydoc entry%s: "
                                (if word
                                    (format " (default %s)" word)
                                  "")))))
           (if (string= input "")
               (or word (error "No pydoc args given"))
             input))))
  (save-window-excursion
    (shell-command (concat "pydoc3 " word) "*PYDOC*"))
  (view-buffer "*PYDOC*" 'bury-buffer))


;;;;
;;;; perl
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/sepia")

(when (require 'sepia nil t)
  (add-to-list 'auto-mode-alist '("\\.pl\\'" . sepia-mode))
  (add-to-list 'interpreter-mode-alist '("perl" . sepia-mode))
  (add-to-list 'interpreter-mode-alist '("perl5" . sepia-mode))
  (add-to-list 'interpreter-mode-alist '("miniperl" . sepia-mode))

  (setq sepia-perl5lib
        (list (expand-file-name "~/.emacs.d/elisp/sepia/lib")))
; (defalias 'perl-mode 'sepia-mode)
  (if (eq system-type 'windows-nt)
      (setq sepia-program-name "c:/Perl/bin/perl")))

(setq cperl-indent-level 4
      cperl-indent-parens-as-block nil
      cperl-highlight-variables-indiscriminately t)

(put 'cperl-array-face 'face-alias 'font-lock-variable-name-face)
(put 'cperl-hash-face 'face-alias 'font-lock-variable-name-face)

;; (defun dhl-cperl-eldoc-documentation-function ()
;;   "Return a meaningful doc string for `eldoc-mode'."
;;   (car
;;    (let ((cperl-message-on-help-error nil))
;;      (cperl-get-help))))

;; (add-hook 'cperl-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'eldoc-documentation-function)
;;                  'dhl-cperl-eldoc-documentation-function)))


;;;
;;; perl 6
;;;

(add-to-list 'load-path "~/.emacs.d/elisp/perl6-mode")
(add-to-list 'load-path "~/.emacs.d/elisp/inferior-perl6")

(when (require 'perl6-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.p6\\'" . perl6-mode)))

(require 'inferior-perl6 nil t)

(setq inferior-perl6-program
      (case system-type
        (gnu/linux "~/build/rakudobrew/bin/perl6")
        (darwin "/usr/local/bin/perl6")))

(add-hook 'perl6-mode-hook
	  (lambda ()
	    (dhl-define-keys perl6-mode-map
			     '(("C-x C-e" perl6-send-last-sexp)
			       ("C-M-x" perl6-send-defun)
			       ("C-c C-b" perl6-send-buffer)
			       ("C-c C-r" perl6-send-region)
			       ("C-c C-c" perl6-send-sentence)))))


;;;;
;;;; tcl
;;;;

;; TODO: linux, too

(require 'tcl nil t)

(setq tcl-application "tclsh8.6")

(when (eq system-type 'cygwin)
  (setq tcl-application "/cygdrive/c/Tcl/bin/tclsh85.exe"))

(add-hook 'inferior-tcl-mode-hook
          (lambda ()
            (tcl-send-string
             (inferior-tcl-proc) "set ::tcl_interactive 1\n")
            (tcl-send-string
             (inferior-tcl-proc)
             "namespace path {::tcl::mathop ::tcl::mathfunc}\n")))

(add-hook 'tcl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'tcl-eval-defun)
            (local-set-key (kbd "C-c C-r") 'tcl-eval-region)))


;;;;
;;;; maxima
;;;;

;; TODO: make this work properly under linux

(when (eq system-type 'windows-nt)
  (setq maxima-command "C:/Program Files/Maxima-5.25.0/bin/maxima.bat"))

(add-to-list 'load-path "~/.emacs.d/elisp/maxima/emaxima")

(when (require 'maxima nil t)
  (add-to-list 'auto-mode-alist '("\\.max\\'" . maxima-mode))
  (defadvice maxima
      (after dhl-maxima-maybe-insert-semicolon activate)
    "Insert semicolon if needed."
    (use-local-map (copy-keymap comint-mode-map))
    (local-set-key (kbd "RET") 'dhl-maybe-insert-semicolon-and-send-input)))

(require 'maxima-font-lock nil t)

(add-to-list 'load-path "~/.emacs.d/elisp/maxima/imaxima")

(autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t)

(with-eval-after-load "imaxima"         ; TODO
  (defadvice imaxima
      (after dhl-imaxima-maybe-insert-semicolon activate)
    "Insert semicolon if needed."
    (use-local-map (copy-keymap comint-mode-map))
    (local-set-key (kbd "RET") 'dhl-maybe-insert-semicolon-and-send-input)))

(setq imaxima-fnt-size "Large"
;     imaxima-pt-size 12
;     imaxima-scale-factor 1.2
      imaxima-label-color "#dc322f")

(defun dhl-maybe-insert-semicolon-and-send-input ()
  "Insert semicolon if needed and send input."
  (interactive)
  (end-of-line)
  (unless (or (= (point) (comint-line-beginning-position))
              (char-equal (char-before) ?\;))
    (insert ";"))
  (comint-send-input))


;;;;
;;;; octave
;;;;

(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(when (eq system-type 'windows-nt)
  (setq inferior-octave-program "C:/Octave/3.2.4_gcc-4.4.0/bin/octave.exe"))


;;;;
;;;; haskell
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/haskell-mode")

(and (require 'haskell-mode nil t)
     (require 'inf-haskell nil t)
     (require 'haskell-indent nil t))

(require 'ghc-core nil t)

(setq haskell-program-name (concat "ghci "
;                                   "-fglasgow-exts "
;                                   "-XNoMonomorphismRestriction "
;                                   "-XTupleSections "
                                   ))

;(setq inferior-haskell-find-project-root nil)

(setq haskell-font-lock-symbols 'unicode)

(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . literal-haskell-mode))

;; TODO:
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(setq haskell-hoogle-command "hoogle")

(add-hook 'inferior-haskell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h") 'haskell-hoogle)
;           (turn-on-haskell-doc-mode 1)
            ))

(add-hook
 'haskell-mode-hook
 (lambda ()
   (dhl-define-keys haskell-mode-map
                    '(("RET" newline)
                      ("TAB" haskell-indent-cycle)
                      ("C-c =" haskell-indent-insert-equal)
                      ("C-c |" haskell-indent-insert-guard)
                      ("C-c o" haskell-indent-insert-otherwise)
                      ("C-c w" haskell-indent-insert-where)
                      ("C-c ." haskell-indent-align-guards-and-rhs)
;                     ("C-c h" haskell-hoogle)
;                     ("C-c t" inferior-haskell-type)
                      ("C-c i" inferior-haskell-info)
                      ("M-." inferior-haskell-find-definition)))))

(when (eq system-type 'cygwin)
  (defadvice inferior-haskell-load-file
      (around dhl-inferior-haskell-load-file-cygwin-fix)
    "Fix `inferior-haskell-load-file' for Win Haskell/Cygwin Emacs."
    (save-buffer)
    (let ((buffer-file-name (concat "c:/cygwin" buffer-file-name)))
      ad-do-it))
  (ad-activate 'inferior-haskell-load-file))


;;;
;;; ghc-mod
;;;

;; (add-to-list 'load-path "~/.emacs.d/elisp/ghc-mod")

;; (setq ghc-completion-key (kbd "<C-tab>")
;;       ghc-document-key (kbd "C-c d")
;;       ghc-import-key (kbd "C-c m")
;;       ghc-previous-key (kbd "M-p")
;;       ghc-next-key (kbd "M-n")
;;       ghc-help-key (kbd "C-c h")
;;       ghc-insert-key (kbd "C-c t")
;;       ghc-sort-key (kbd "C-c s")
;;       ghc-check-key (kbd "C-x C-s")
;;       ghc-toggle-key (kbd "C-c C-c"))

;; (autoload 'ghc-init "ghc" nil t)

;; (when (require 'ghc nil t)
;;   (add-hook 'haskell-mode-hook
;;             (lambda ()
;;               (ghc-init))))

;; (defadvice ghc-init
;;     (before dhl-ghc-mod-local-completion first () activate)
;;   "Make `ghc-mod' completions buffer local."
;;   (make-local-variable 'ghc-loaded-module)
;;   (make-local-variable 'ghc-merged-keyword))

;; (defadvice ghc-import-module
;;     (before dhl-ghc-mod-reset-modules first () activate)
;;   "Make `ghc-import-module' recognize dropped imports."
;;   (setq ghc-loaded-module nil)
;;   (ghc-comp-init))


;;;;
;;;; f#
;;;;

;; TODO: make this work under linux

(add-to-list 'load-path "~/.emacs.d/elisp/fsharp")

(require 'fsharp nil t)

(setq-default fsharp-indent-offset 2
              fsharp-continuation-offset 1)

(when (eq system-type 'gnu/linux)
  (setq inferior-fsharp-program
        "mono /home/danlei/build/FSharp-2.0.0.0/bin/fsi.exe --readline-"
        fsharp-compiler
        "mono /home/danlei/build/FSharp-2.0.0.0/bin/fsc.exe --resident"))

(when (member system-type '(cygwin windows-nt))
  (setq inferior-fsharp-program "fsi.exe --readline-"
        fsharp-compiler "fsc.exe"))

(add-to-list 'auto-mode-alist '("\\.fs\\'" . fsharp-mode))

(add-hook
 'fsharp-mode-hook
 (lambda ()
   (dhl-define-keys fsharp-mode-map
                    `(("C-c b" fsharp-mark-block)
                      ("C-c r" fsharp-run-executable-file)))))


;;;;
;;;; prolog
;;;;

(require 'prolog nil t)

(setq-default prolog-system 'swi)

(setq prolog-indent-width 4
      prolog-electric-newline-flag nil)

(setq prolog-program-name
      (case system-type
        (windows-nt "C:/Program Files/pl/bin/swipl.exe")
        (cygwin "/usr/bin/pl")
        (gnu/linux '((swi "/usr/bin/swipl")
                     (gnu "/usr/bin/gprolog")
                     (yap "/usr/bin/yap")))
        (darwin "/usr/local/bin/swipl")))

(add-to-list 'prolog-program-switches
             '(swi ("--no-tty" "--traditional" "--quiet")))
(add-to-list 'prolog-program-switches
             '(yap ("-q")))

(when (eq system-type 'windows-nt)
  (add-to-list 'prolog-program-switches
               '(swi ("-f" "C:/Users/dhl/.emacs.d/swipl-init.pl"))))

(add-hook 'prolog-inferior-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-d") 'comint-send-eof)))

(add-hook 'prolog-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-l") 'prolog-consult-file)))

;(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
(add-to-list 'auto-mode-alist '("\\.pro\\'" . prolog-mode))


;;;;
;;;; erlang
;;;;

(eval-when (compile load eval)
  (let ((erlang-path (car (file-expand-wildcards
                           "/usr/local/lib/erlang/lib/tools-*/emacs"))))
    (when erlang-path
      (add-to-list 'load-path erlang-path))))

(when (require 'erlang nil t)
  (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
  (define-key erlang-shell-mode-map (kbd "C-c C-d h") 'dhl-erlang-online-doc))

(setq erlang-root-dir "/usr/local/lib/erlang"
      erlang-electric-commands '()
      inferior-erlang-machine-options '("-sname" "emacs")
      erl-nodename-cache
      (make-symbol
       (concat "emacs@"
               (car (split-string (shell-command-to-string "hostname"))))))

;; make work if not on identifier
(defun dhl-erlang-online-doc ()
  "Look up Erlang identifiers in the online doc."
  (interactive)
  (browse-url
   (concat "http://erlang.org/doc/search/?q="
           (let* ((identifier (erlang-get-identifier-at-point))
                  (default (if (eq (car identifier)
                                   'qualified-function)
                               (concat (nth 1 identifier)
                                       ":"
                                       (nth 2 identifier))
                             (nth 2 identifier))))
             (read-from-minibuffer "Identifier: " default)))))

(defadvice erlang-compile
    (after dhl-switch-to-erlang-buffer last () activate)
  (other-window 1))

(add-hook 'erlang-mode-hook
	  (lambda ()
	    (dhl-define-keys erlang-mode-map
			     '(("M-a" erlang-beginning-of-clause)
			       ("M-e" erlang-end-of-clause)
			       ("C-M-a" erlang-beginning-of-function)
			       ("C-M-e" erlang-end-of-function)
			       ("M-h" erlang-mark-clause)
			       ("C-M-h" erlang-mark-function)
			       ("M-q" erlang-indent-function)
			       ("C-M-q" erlang-fill-paragraph)
             ("C-c C-d h" dhl-erlang-online-doc) ; TODO: overwritten at eshell startup
             ("C-c C-d ?" dhl-erlang-online-doc)))))

(add-to-list 'load-path "~/.emacs.d/elisp/distel/elisp")

(when (require 'distel nil t)
  (distel-setup)
  (define-key erlang-mode-map (kbd "<C-tab>") 'erl-complete)
  (define-key erlang-mode-map (kbd "M-p") 'previous-error)
  (define-key erlang-mode-map (kbd "M-n") 'next-error)
  (define-key erlang-shell-mode-map (kbd "<C-tab>") 'erl-complete)
  (define-key erlang-shell-mode-map (kbd "C-c C-d d") 'erl-fdoc-describe)
  (define-key erl-process-list-mode-map (kbd "p") 'previous-line)
  (define-key erl-process-list-mode-map (kbd "n") 'next-line))

(add-hook 'distel-erlang-mode-hook
	        (lambda ()
            (local-set-key (kbd "C-c C-d h") 'dhl-erlang-online-doc)))

(add-hook 'erl-process-list-mode-hook
	  (lambda ()
      (hl-line-mode 1)))

(defadvice erl-show-fdoc-matches
    (after dhl-switch-to-fdoc-buffer last () activate)
  (other-window 1)
  (view-mode 1))


;;;;
;;;; java
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/javarun")

(when (require 'javarun nil t)
  (when (eq system-type 'windows-nt)
    (setq javarun-java-path "C:/Program Files/Java/jdk1.6.0/bin/"))
  (add-hook 'java-mode-hook
            (lambda ()
              (javarun-mode 1)
              (subword-mode 1))))


;;;;
;;;; groovy
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/groovy-emacs-modes")

(when (require 'groovy-mode nil t)
  (add-to-list 'auto-mode-alist '("\\`Jenkinsfile\\'" . groovy-mode)))

(require 'inf-groovy nil t)

(setq groovy-indent-offset 2)

(add-hook 'inferior-groovy-mode-hook
          (lambda ()
;           (setq comint-prompt-regexp "^groovy:[^>]*> ")
            (local-set-key (kbd "C-a") 'comint-bol)))


;;;;
;;;; nim
;;;;

(mapc (lambda (library)
        (add-to-list 'load-path (concat "~/.emacs.d/elisp/"
                                        (symbol-name library))))
      '(nim-mode emacs-buttercup emacs-deferred emacs-ctable
        emacs-epc flycheck-nimsuggest inim-mode))

(when (and (require 'nim-mode nil t)
           (require 'nim-compile))
  (add-to-list 'auto-mode-alist '("\\.nim\\'" . nim-mode))
  (define-key nim-mode-map (kbd "M-p") 'previous-error)
  (define-key nim-mode-map (kbd "M-n") 'next-error))

(when (require 'inim nil t)
  (define-key inferior-inim-mode-map (kbd "C-a") 'comint-bol))


;;;;
;;;; go
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/go-mode.el")

(when (require 'go-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))


;;;;
;;;; javascript
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/js2-mode")

(setq-default js-indent-level 2)

(add-to-list 'auto-mode-alist '("\\.jsonl?\\'" . javascript-mode))

(when (require 'js2-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.js\\|\\.ts\\'" . js2-mode)))

(setq-default js2-basic-offset 2)

;(setq js2-bounce-indent-p t)


;; TODO: fix slime-js
;(require 'slime-js nil t)

;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (slime-js-minor-mode 1)
;;             (dhl-define-keys slime-js-minor-mode-map
;;                              `(("M-n" next-error)
;;                                ("M-p" previous-error)))))

(add-hook 'js2-mode-hook
          (lambda ()
            (dhl-define-keys js2-mode-map
                             `(("M-n" next-error)
                               ("M-p" previous-error)))))

(add-to-list 'load-path "~/.emacs.d/elisp/jss")
(add-to-list 'load-path "~/.emacs.d/elisp/emacs-websocket")
(require 'jss nil t)

(require 'js-comint nil t)

(setq inferior-js-program-command "js24 -s")

(add-hook 'inferior-js-mode-hook
          (lambda ()
            (ansi-color-for-comint-mode-on)
            (setq comint-process-echoes t)
            (add-to-list
             'comint-preoutput-filter-functions
             (lambda (output)
               (replace-regexp-in-string "\033\\[[0-9]+[GK]" "" output)))))


(require 'nodejs-repl nil t)

(setq nodejs-repl-command "/usr/local/bin/node"
      nodejs-repl-options '("--experimental-repl-await"
                            "--use_strict"))

(defun dhl-nodejs-repl-eval-buffer ()
  (interactive)
  (nodejs-repl--send-string (buffer-string)))

(defun dhl-nodejs-repl-eval-defun ()
  (interactive)
  (nodejs-repl--send-string (thing-at-point 'defun t)))

(defun dhl-nodejs-repl-eval-last-sexp ()
  (interactive)
  (nodejs-repl--send-string (preceding-sexp)))

(defun dhl-nodejs-repl-eval-region (start end)
  (interactive "r")
  (nodejs-repl--send-string (buffer-substring-no-properties start end)))

(add-hook 'js2-mode-hook
          (lambda ()
            (dhl-define-keys js2-mode-map
                             '(("C-c C-b" dhl-nodejs-repl-eval-buffer)
                               ("C-c C-c" dhl-nodejs-repl-eval-defun)
                               ("C-M-x" dhl-nodejs-repl-eval-defun)
                               ("C-c C-e" dhl-nodejs-repl-eval-last-sexp)
                               ("C-c C-r" dhl-nodejs-repl-eval-last-sexp)))))

(add-hook 'js-mode-hook
          (lambda ()
            (dhl-define-keys js-mode-map
                             '(("C-c C-b" dhl-nodejs-repl-eval-buffer)
                               ("C-c C-c" dhl-nodejs-repl-eval-defun)
                               ("C-M-x" dhl-nodejs-repl-eval-defun)
                               ("C-c C-e" dhl-nodejs-repl-eval-last-sexp)
                               ("C-c C-r" dhl-nodejs-repl-eval-last-sexp)))))

(when (require 'js nil t)
  (modify-syntax-entry ?` "\"" js-mode-syntax-table)) ; for ES6 string templates


;;;;
;;;; coffee
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/coffee-mode")

(when (require 'coffee-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.coffee\\'" . coffee-mode))
  (setq coffee-command "coffee")
  (add-to-list ; alternatively, set NODE_NO_READLINE=1
   'comint-preoutput-filter-functions
   (lambda (output)
     (replace-regexp-in-string "\\[[0-9]+[GKJ]" "" output))))


;;;;
;;;; c
;;;;

;(require 'cc-mode nil t)

(setq compilation-window-height 10
;     compilation-read-command nil ;; TODO: breaks nim-compile
      )                            ;;       (nim-compile--assert)

(setq-default c-basic-offset 2)

(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c c") 'compile)
            (local-set-key (kbd "M-p") 'previous-error)
            (local-set-key (kbd "M-n") 'next-error)
            (subword-mode 1)
            (setq comment-start "// "
                  comment-end "")
            (c-set-offset 'case-label '+)
            (c-toggle-electric-state -1)))

(defun dhl-gcc-compile-command (&optional options)
  "Return a gcc command line invocation string with OPTIONS.

Intended to use with the variable `compile-command' on little
test programs that don't need a proper makefile. Extra command
line options may be given in OPTIONS."
  (mapconcat
   'identity
   (list "gcc -std=c99 -pedantic -Wall -o"
         (file-name-sans-extension (file-name-nondirectory buffer-file-name))
         (file-name-nondirectory buffer-file-name)
         options)
   " "))


;;;
;;; lpc
;;;

(define-derived-mode lpc-mode c-mode "LPC")

(modify-syntax-entry ?' "'" lpc-mode-syntax-table)
;(modify-syntax-entry ?# "' 14" lpc-mode-syntax-table) ;; TODO


;;;
;;; pike
;;;

(add-to-list 'auto-mode-alist '("\\.pike\\'" . pike-mode))


;;;;
;;;; auctex
;;;;

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default Tex-master nil)

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)


;;;;
;;;; xml
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/rnc-mode")

(when (require 'rnc-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
  (setq rnc-indent-level 2))

(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsd\\|xsl\\)\\'" . nxml-mode))

(setq nxml-slash-auto-complete-flag t)

(add-hook 'nxml-mode-hook
          (lambda ()
            (dhl-define-keys nxml-mode-map
                             `(("TAB" ,(lambda (n)
                                         (interactive "p")
                                         (indent-for-tab-command)
                                         (nxml-complete)))
                               ("<C-return>"
                                ,(lambda (n)
                                   (interactive "p")
                                   (nxml-complete)
                                   (nxml-balanced-close-start-tag-block)))
                               ("M-ö" nxml-down-element)
                               ("M-u" nxml-backward-up-element)
                               ("M-n" nxml-forward-element)
                               ("M-p" nxml-backward-element)
                               ("C-M-ö" nxml-backward-down-element)
                               ("C-M-n" nxml-up-element)))))


;;;
;;; xquery
;;;

;; TODO: switch to outputbuffer in view mode automatically (maybe use
;; sentinels? or write from scratch?

(add-to-list 'load-path "~/.emacs.d/elisp/xquery-mode")

(when (require 'xquery-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.xqy\\'" . xquery-mode)))

(define-key xquery-mode-map (kbd "C-c C-c") 'dhl-run-xquery)

(setq dhl-xquery-command "basex")

(defun dhl-run-xquery ()
  (interactive)
  (save-buffer)
  (shell-command (concat dhl-xquery-command " " (buffer-file-name))))


;;;;
;;;; web
;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/web-mode"
        "~/.emacs.d/elisp/rainbow-mode"))

(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  )

(setq web-mode-markup-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-sql-indent-offset 2
      web-mode-php-indent-offset 2
      web-mode-enable-auto-closing nil
      web-mode-enable-auto-pairing nil
      web-mode-enable-auto-quoting nil
      web-mode-script-padding 2
      web-mode-style-padding 2
      web-mode-block-padding 2)

(setq-default css-indent-offset 2)

(add-hook 'web-mode-hook
          (lambda ()
            (subword-mode 1)
            (c-set-offset 'case-label '+)
            (c-toggle-electric-state -1)))

(require 'rainbow-mode nil t)


;;;;
;;;; php
;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/php-mode"
        "~/.emacs.d/elisp/psysh.el"
        "~/.emacs.d/elisp/s.el"
        "~/.emacs.d/elisp/f.el"
        "~/.emacs.d/elisp/php-eldoc"))

(when (require 'php-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode)))

(when (featurep 'lsp-mode)
  (add-hook 'php-mode-hook 'lsp))

(add-hook 'php-mode-hook
          (lambda ()
            (setq comment-start "# "
                  comment-end ""
                  comment-column 40
                  tab-width 4
                  php-lineup-cascaded-calls t)
            (c-set-offset 'case-label '+)
;           (php-enable-default-coding-style)
            (dhl-define-keys php-mode-map
                             '(("C-c C-c" psysh-eval-region)
                               (("C-c d" "C-c C-d") psysh-doc)))
;           (flycheck-mode 1)  ; TODO: flycheck-global-modes should do this
            ))

;; (when (require 'php-eldoc nil t)
;;   (add-hook 'php-mode-hook
;;             (lambda ()
;;               (php-eldoc-enable))))

(when (require 'psysh nil t)
  (setq psysh-doc-buffer-color 'only-emacs)
  (setq-default psysh-comint-buffer-process '("psysh" "psysh" nil "--no-color")))

(defadvice php-send-region
    (after dhl-php-send-region-advice last () activate)
  "Show *PHP* buffer after `php-send-region' invocation."
  (pop-to-buffer "*PHP*")
  (view-mode 1))

(defadvice psysh-eval-region
    (after dhl-psysh-eval-region-advice last () activate)
  "Show *psysh* buffer after `eval-region' invocation."
  (let ((buf (psysh--make-process)))
    (comint-send-string buf "\n"))
  (pop-to-buffer "*psysh*"))


;;;;
;;;; restclient
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/restclient")

(when (require 'restclient nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(http\\|rest\\)\\'" . restclient-mode)))

(setq restclient-same-buffer-response nil)

;; TODO: make this work even when delete-window etc. is called
;;       i.e. quit view-mode automatically somehow
;;       works with restclient-same-buffer-response set to nil
(add-hook 'restclient-response-loaded-hook
          (lambda ()
            (local-set-key (kbd "z") 'dhl-kill-this-buffer)))


;;;;
;;;; request
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/emacs-request")

(require 'request nil t)


;;;;
;;;; sql
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/sql-indent")

(setq sql-sqlite-program "sqlite3"
      sql-sqlite-options '("-interactive")
;     sql-mysql-options '("-n")
      mysql-user (user-login-name))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines 1)
            (setq comint-process-echoes 1)
            (add-to-list 'comint-preoutput-filter-functions
                         (lambda (output)
                           (replace-regexp-in-string "" "" output)))
            (dhl-enable-comint-history "~/.emacs.d/sql-input-history")))

(when (require 'sql-completion nil t)
  (add-hook 'sql-interactive-mode-hook
            (lambda ()
              (define-key sql-interactive-mode-map (kbd "TAB")
                'comint-dynamic-complete)
              (sql-mysql-completion-init))))

(when (require 'sql-indent nil t)
  (add-hook 'sql-mode-hook
            (lambda ()
              (sqlind-minor-mode 1))))


;;;
;;; sql-workbench
;;;

;; (loop for package in '("sql-workbench"
;;                        "ov"
;;                        "shut-up"
;;                        "json-mode"
;;                        "json-snatcher"
;;                        "json-reformat")
;;       do (add-to-list 'load-path (concat "~/.emacs.d/elisp/" package)))

;; (and (require 'ov nil t)
;;      (require 'shut-up nil t)
;;      (require 'json-mode nil t)
;;      (require 'json-snatcher nil t)
;;      (require 'json-reformat nil t)
;;      (require 'sql-workbench nil t))


;;;
;;; edbi
;;;

;; dbi:mysql:db:host:port

(add-to-list 'load-path "~/.emacs.d/elisp/emacs-edbi")

(require 'edbi nil t)

(setq edbi:query-result-fix-header nil)


;;;
;;; ob-sql-mode (for org mode code blocks)
;;;

;; (add-to-list 'load-path "~/.emacs.d/elisp/ob-sql-mode")

;; (require 'ob-sql-mode)


;;;;
;;;; semantic web
;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/manchester-mode"
        "~/.emacs.d/elisp/ttl-mode"
        "~/.emacs.d/elisp/emacs-async"
        "~/.emacs.d/elisp/sparql-mode"))

(when (require 'manchester-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.omn\\'" . manchester-mode)))

(when (require 'ttl-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.\\(n3\\|ttl\\)\\'" . ttl-mode)))

(setq ttl-electric-punctuation nil)

(when (and (require 'async nil t)
           (require 'sparql-mode nil t))
  (add-to-list 'auto-mode-alist '("\\.\\(sparql\\|rq\\)\\'" . sparql-mode)))

(setq sparql-default-base-url "http://dbpedia.org/sparql"
      sparql-default-format "text/html")

;; (define-key sparql-result-mode-map (kbd "z")
;;   'View-kill-and-leave)

(when (require 'dhl-sparql-output-html-prefix nil t)
  (defadvice sparql-query-region
      (after dhl-sparql-output-frobbing activate)
    (other-window 1)
    (dhl-render-sparql-html-output-buffer)))


;;;;
;;;; elisp
;;;;

(setq max-specpdl-size 10000
      max-lisp-eval-depth 5000
      debug-on-error nil
      eval-expression-print-level nil)

(defun dhl-lisp-eval-print-defun ()
  "Move behind current toplevel form, evaluate, and insert result."
  (interactive)
  (end-of-defun)
  (eval-print-last-sexp))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (local-set-key (kbd "<C-return>") 'dhl-lisp-eval-print-defun)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (dhl-define-keys lisp-interaction-mode-map
                             '(("<C-return>" eval-print-last-sexp)
                               ("<C-return>" dhl-lisp-eval-print-defun)))))

(define-key read-expression-map (kbd "TAB")
  (if (version< emacs-version "24.4")
      'lisp-complete-symbol
    'completion-at-point))


;;;;
;;;; asm
;;;;

(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 8)
            (local-set-key (kbd "RET") 'newline)))


;;;;
;;;; shell-script-mode
;;;;

(setq-default sh-basic-offset 2)

(add-hook 'sh-mode-hook
          (lambda ()
            (sh-electric-here-document-mode -1)))


;;;;
;;;; fish
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/fish")
(when (require 'fish-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
  (setq fish-indent-offset 2))


;;;;
;;;; ess
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/ESS/lisp")

(when (require 'ess-site nil t)
  (add-hook 'ess-mode-hook
            (lambda ()
              (electric-indent-mode -1)
              (local-set-key (kbd "RET") 'newline)))
  (add-hook 'julia-mode-hook
            (lambda ()
              (setq julia-basic-offset 2))))

(when (eq system-type 'windows-nt)
  (setq inferior-R-program-name
        "C:/Program Files/R/R-2.13.0/bin/i386/Rterm.exe"))

(setq ess-tab-complete-in-script t)


;;;;
;;;; markdown-mode
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/markdown-mode")

(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode)))


;;;;
;;;; tf
;;;;

(when (require 'tinyfugue nil t)
  (add-to-list 'auto-mode-alist '("\\.tf\\'" . tinyfugue-mode))
  (add-to-list 'auto-mode-alist '("^\\.?tfrc\\'" . tinyfugue-mode)))


;;;;
;;;; docker
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/dockerfile-mode")

(when (require 'dockerfile-mode nil t)
    (add-to-list 'auto-mode-alist '("\\`Dockerfile\\'" . dockerfile-mode)))


;;;;
;;;; yaml
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/yaml-mode")

(when (require 'yaml-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode)))


;;;;
;;;; ielm
;;;;

(setq ielm-prompt "elisp> ")

(add-hook 'ielm-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (setq comint-dynamic-complete-functions
                  `(ielm-tab
                    comint-replace-by-expanded-history
                    ielm-complete-filename
                    ielm-complete-symbol
                    ,(if (< emacs-major-version 24)
                         'lisp-complete-symbol
                       'completion-at-point)))))


;;;;
;;;; eshell
;;;;

(setq eshell-prefer-lisp-functions nil
      eshell-bad-command-tolerance 5)

;(add-to-list 'eshell-visual-commands "zsh")

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") 'dhl-eshell-maybe-bol)
            (local-set-key (kbd "<tab>") 'dhl-eshell-tab)
            (local-set-key (kbd "C-j") 'dhl-eshell-newline-and-indent)
            (eldoc-mode 1)))

;; nice, but slow
;; (when (require 'em-smart nil t)
;;   (setq eshell-where-to-jump 'begin)
;;   (setq eshell-review-quick-commands nil)
;;   (setq eshell-smart-space-goes-to-end t)
;;   (add-hook 'eshell-mode-hook
;;             (lambda ()
;;               (eshell-smart-initialize))))

(defun dhl-eshell-maybe-bol ()
  "Move point behind the eshell prompt, or to the beginning of line."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (when (= p (point))
      (beginning-of-line))))

(defun dhl-eshell-tab ()
  "Indent or complete."
  (interactive)
  (if (or (eq (preceding-char) ?\n)
          (eq (char-syntax (preceding-char)) ?\s))
      (lisp-indent-line)
    (eshell-pcomplete)))

(defun dhl-eshell-newline-and-indent ()
  (interactive "*")
  (delete-horizontal-space t)
  (newline nil t)
  (lisp-indent-line))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; adapted from the emacswiki
(eval-after-load "em-ls"
  '(progn
     (defun dhl-eshell-ls-find-file-at-point (point)
       "RET on Eshell's `ls' output to open files."
       (interactive "d")
       (find-file (buffer-substring-no-properties
                   (previous-single-property-change point 'help-echo)
                   (next-single-property-change point 'help-echo))))

     (defun dhl-eshell-ls-find-file-at-mouse-click (event)
       "Click on Eshell's `ls' output to open files."
       (interactive "e")
       (dhl-eshell-ls-find-file-at-point (posn-point (event-end event))))

     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET") 'dhl-eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'dhl-eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-1>") 'dhl-eshell-ls-find-file-at-mouse-click)
       (defvar dhl-eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after dhl-electrify-ls activate)
       "Clickable and RETable `ls' file names in Eshell."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-1: visit this file"
                                  'mouse-face 'highlight
                                  'keymap dhl-eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))


;;;;
;;;; hippie-expansion
;;;;

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
        try-expand-line
        try-expand-line-all-buffers))


;;;;
;;;; ivy
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/swiper")

(defun dhl-counsel-git-grep-or-ag ()
  "Use `counsel-git-grep' if in a project, `counsel-ag' otherwise."
  (interactive)
  (condition-case nil
      (counsel-git-grep)
    (error (counsel-ag))))

 (setq projectile-completion-system 'ivy
        ivy-height 15
        ivy-wrap t
        ivy-use-virtual-buffers t
        counsel-ack-base-command "ack -S --nocolor --nogroup %s")

(when (and (require 'ivy nil t)
           (require 'swiper nil t)
           (require 'counsel nil t))
  (setq projectile-completion-system 'ivy)
  (loop for (key binding)
        on '("s-f" swiper
             "s-F" swiper-all
             "s-r" ivy-resume
             "s-g" dhl-counsel-git-grep-or-ag
             "s-G" counsel-ag
             "s-i" counsel-semantic-or-imenu
             "s-O" counsel-file-jump
             "s-j" counsel-bookmark
             "s-J" counsel-org-goto-all
             "s-U" counsel-unicode-char
             "M-X" counsel-M-x
             "C-<tab>" counsel-switch-buffer
             "C-x B" counsel-switch-buffer
             "C-h A" counsel-apropos
             "C-h V" counsel-describe-variable
             "C-h C-S-l" counsel-find-library
             "C-h M" counsel-descbinds
             "C-x r B" counsel-bookmark
             "C-x ESC ESC" counsel-command-history)
        by #'cddr
        do (global-set-key (kbd key) binding))
  (define-key ivy-minibuffer-map (kbd "C-c o") 'ivy-occur)
  (define-key ivy-minibuffer-map (kbd "C-c r") 'ivy-rotate-preferred-builders)
  (define-key ivy-minibuffer-map (kbd "s-.") 'ivy-avy)
  (define-key ivy-occur-mode-map (kbd "e") 'ivy-wgrep-change-to-wgrep-mode)
  (define-key ivy-occur-mode-map (kbd "z") 'dhl-kill-this-buffer)
  (define-key ivy-occur-grep-mode-map (kbd "z") 'dhl-kill-this-buffer)
  (define-key ivy-occur-grep-mode-map (kbd "e") 'ivy-wgrep-change-to-wgrep-mode)
  (define-key ivy-switch-buffer-map (kbd "C-<tab>") 'ivy-next-line)
  (define-key ivy-switch-buffer-map (kbd "C-M-<tab>") 'ivy-previous-line))

(setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))


;;;;
;;;; avy
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/avy")

(when (require 'avy nil t)
  (loop for (key binding)
        on '("s-." avy-goto-word-or-subword-1
             "s-l" avy-goto-line)
        by #'cddr
        do (global-set-key (kbd key) binding)))

(setq avy-keys '(?j ?d ?k ?s ?l ?a ?g ?h ?e ?i ?w ?o ?r ?u ?v ?n ?f))

(setq avy-orders-alist '((avy-goto-char . avy-order-closest)
                         (avy-goto-word-1 . avy-order-closest)
                         (avy-goto-subword-0 . avy-order-closest)))


;;;;
;;;; ido
;;;;

(when (require 'ido nil t)
  (ido-mode 1)
  (ido-everywhere 1))

(when (require 'idomenu nil t)
  (global-set-key (kbd "M-g i") 'idomenu))

(defadvice idomenu
    (after dhl-idomenu-recenter last () activate)
  "Recenter after invoking `idomenu'."
  (recenter-top-bottom))

(setq ido-use-filename-at-point 'guess ; 'ffap-guesser
      ido-use-url-at-point t
      ido-auto-merge-work-directories-length -1)


;; TODO: should I keep this? never use it.
(add-to-list 'load-path "~/.emacs.d/elisp/smex")


;;;
;;; smex
;;;

(when (require 'smex nil t)
  (smex-initialize)
  (setq smex-save-file "~/.emacs.d/smex")
  (smex-auto-update))

(defun dhl-invoke-smex (x)
  "Invoke smex. For commands with prefix argument X.

If called without a prefix argument, invoke vanilla `smex'.
Otherwise `smex-major-mode-commands'. Note that this prevents
using commands with prefix arguments."
  (interactive "p")
  (if (= x 1)
      (smex)
    (smex-major-mode-commands)))


;;;
;;; ido-grid
;;;

(add-to-list 'load-path "~/.emacs.d/elisp/ido-grid.el")

(defvar dhl-ido-grid-enabled nil)

(when (require 'ido-grid nil t)
  (setq ido-grid-bind-keys nil
        ido-grid-rows 10
        ido-grid-start-small nil)

  (defun dhl-toggle-ido-grid ()
    (interactive)
    (if dhl-ido-grid-enabled
        (ido-grid-disable)
      (ido-grid-enable)))

  (defadvice ido-grid-enable
      (after dhl-ido-grid-enable-keybindings last () activate)
    (setq dhl-ido-grid-enabled t
          ido-setup-hook
          (lambda ()
            (ido-grid--setup)
            (loop for (binding function)
                  on '("C-f" ido-grid-right
                       "C-b" ido-grid-left
                       "C-s" ido-grid-right
                       "C-r" ido-grid-left
                       "C-p" ido-grid-up-or-expand
                       "C-n" ido-grid-down-or-expand
                       "C-c" ido-toggle-case)
                  by #'cddr
                  do (define-key ido-completion-map (kbd binding) function))))
    (funcall ido-setup-hook))

  (defadvice ido-grid-disable
      (before dhl-ido-grid-disable-keybindings first () activate)
    (setq dhl-ido-grid-enabled nil
          ido-setup-hook
          (lambda ()
            (loop for (binding function)
                  on '("C-f" ido-magic-forward-char
                       "C-b" ido-magic-backward-char
                       "C-s" ido-next-match
                       "C-r" ido-prev-match)
                  by #'cddr
                  do (define-key ido-completion-map (kbd binding) function))))
    (funcall ido-setup-hook))
  (define-key ido-common-completion-map (kbd "C-t") 'dhl-toggle-ido-grid))


;;;
;;; flx
;;;

(add-to-list 'load-path "~/.emacs.d/elisp/flx")
(require 'flx nil t)

;; (when (require 'flx-ido nil t)
;;   (flx-ido-mode t)
;;   (setq ido-enable-flex-matching t
;;         ido-use-faces nil))


;;;;
;;;; flycheck
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/flycheck")

(setq flycheck-global-modes '(php-mode js-mode)
      flycheck-disabled-checkers '(php-phpcs)
      flycheck-check-syntax-automatically
      ;; TODO: including 'save hangs in tramp
      '(idle-change new-line mode-enabled))

(when (require 'flycheck nil t)
  (global-flycheck-mode 1))

(add-hook 'flycheck-mode-hook
          (lambda ()
            (dhl-define-keys flycheck-mode-map
                             '(("M-n" next-error)
                               ("M-p" previous-error)))))


;;;;
;;;; session management
;;;;

(loop for parameter in '(foreground-color
                         background-color
                         cursor-color
                         cursor-type
                         mouse-color
                         menu-color
                         font
                         background-mode
                         ns-appearance)
      do (push (cons parameter :never) frameset-filter-alist))

(desktop-save-mode 1)

(setq history-length 250
      history-delete-duplicates t
;     desktop-files-not-to-save "^$"    ; for tramp, slow
      )

(mapc (apply-partially 'add-to-list 'desktop-globals-to-save)
      '(global-mark-ring
        mark-ring
        kmacro-ring
        kill-ring
        file-name-history
        register-alist))

(add-hook 'auto-save-hook
          (lambda () (desktop-save-in-desktop-dir)))

(savehist-mode 1)


;;;;
;;;; backups
;;;;

(setq backup-directory-alist
      `(("." . "~/.emacs.d/bak/")
;       (,tramp-file-name-regexp . nil)               ; no remote file backups
;       (,tramp-file-name-regexp . "~/.emacs.d/bak/") ; locally TODO: CDPATH error
        )
      tramp-backup-directory-alist backup-directory-alist ; remotely
      tramp-auto-save-directory "~/.emacs.d/bak/"
      auto-save-file-name-transforms '((".*" "~/.emacs.d/bak/" t))
      backup-by-copying t
      version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t)

(add-hook 'before-save-hook (lambda ()
                              (setq buffer-backed-up nil)))


;;;;
;;;; whitespace-mode
;;;;

(setq whitespace-style
      '(spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])             ; space, ·, .
        (space-mark 160 [9085] [95])           ; non-breaking space, ⍽, _
        (space-mark 8201 [9251] [95])          ; thin space, ␣, _
        (space-mark 8202 [9251] [95])          ; hair space, ␣, _
        (space-mark 8203 [9251] [95])          ; zero width space​, ␣, _
        (newline-mark 10 [182 10] [92 110 10]) ; line feed, ¶, \n
        (newline-mark 13 [8629] [92 114])      ; carriage return, ↵, \r
        (tab-mark 9 [8677 9] [92 116 9])))     ; character tabulation, ⇥, \t


;;;;
;;;; magit
;;;;

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/magit"
        "~/.emacs.d/elisp/magit/lisp"
        "~/.emacs.d/elisp/git-modes"
        "~/.emacs.d/elisp/magit-popup"
        "~/.emacs.d/elisp/with-editor"
        "~/.emacs.d/elisp/ghub"
        "~/.emacs.d/elisp/graphql.el"
        "~/.emacs.d/elisp/treepy.el"
        "~/.emacs.d/elisp/marshal.el"))

(and (executable-find "git")
     (require 'magit nil t)
     (add-to-list 'Info-additional-directory-list
                  (expand-file-name "~/.emacs.d/elisp/magit/")))

(setq magit-auto-revert-mode nil
      magit-last-seen-setup-instructions "1.4.2")

(require 'rebase-mode nil t)

(when (and (> emacs-major-version 24)
           (require 'magit-popup nil t)
           (require 'with-editor nil t)
           (require 'ghub nil t)
           (require 'magit-section nil t)
           (require 'graphql nil t)
           (require 'treepy nil t)
           (require 'marshal nil t))
  (add-hook 'magit-section-movement-hook
            'magit-hunk-set-window-start)
  (when (featurep 'counsel)
    (define-key magit-mode-map (kbd "<C-tab>") 'counsel-switch-buffer))
  (load "~/.emacs.d/elisp/magit/magit-autoloads"))

(when (require 'vc-dir nil t)
  (define-key vc-dir-mode-map (kbd "d") 'vc-dir-delete-file))


;;;
;;; gists
;;;

(when (< emacs-major-version 24)
  (add-to-list 'load-path "~/.emacs.d/elisp/tabulated-list.el"))

(mapc (apply-partially 'add-to-list 'load-path)
      '("~/.emacs.d/elisp/logito"
        "~/.emacs.d/elisp/pcache"
        "~/.emacs.d/elisp/gh.el"
        "~/.emacs.d/elisp/gist.el"))

(require 'gist nil t)


;;;;
;;;; darcs
;;;;

(require 'vc-darcs nil t)


;;;;
;;;; browsing
;;;;

(setq browse-url-generic-program
      (case system-type
        (windows-nt "~/AppData/Local/Google/Chrome/Application/Chrome.exe")
        (gnu/linux "/usr/bin/firefox")
        (darwin "open")))

(setq browse-url-browser-function 'browse-url-generic)

;; TODO: use eww?

(when (member system-type '(cygwin gnu/linux))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m"))

;; (when (require 'w3m nil t)
;;   (setq browse-url-browser-function 'w3m-browse-url))

(setq w3m-session-load-last-sessions t)


;;;;
;;;; erc
;;;;

;; TODO: upgrade this
(add-to-list 'load-path "~/.emacs.d/elisp/erc-5.3-extras")

(when (require 'erc nil t)
; (require 'erc-match nil t)
  (require 'erc-list-old nil t)
  (erc-spelling-mode -1)
  (erc-list-mode 1)
  (erc-timestamp-mode -1)
  (erc-smiley-mode -1)
  (erc-scrolltobottom-mode 1)
  (erc-truncate-mode 1))

(setq erc-keywords '()
;     erc-pals '()
;     erc-fools '()
      erc-current-nick-highlight-type 'nick-or-keyword
      erc-notice-highlight-type 'prefix
      erc-auto-query 'window-noselect
;     erc-user-full-name "Daniel H. Leidisch"
      erc-nick "danlei"
      erc-track-exclude-server-buffer nil
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 20
      erc-fill-column 90
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-max-buffer-size 50000
      erc-interpret-controls-p t
      erc-rename-buffers nil
      erc-default-server "irc.freenode.net"
      erc-default-port 6697
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
      erc-autojoin-timing 'ident
      erc-server-auto-reconnect t
      erc-whowas-on-nosuchnick t
      erc-disable-ctcp-replies t)

(setq erc-button-url-regexp
      (concat "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+"
              "[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]"))

(setq erc-track-exclude-types
      '("NICK" "MODE" "324" "329" "332" "333" "353" "477"))

(setq erc-part-reason (lambda (x)
                        (or x "去る者は追わず。"))
      erc-quit-reason erc-part-reason)


;;;;
;;;; dired
;;;;

(require 'dired-x)

(setq dired-recursive-deletes 'top
      dired-recursive-copies 'top
      wdired-allow-to-change-permissions t
      wdired-allow-to-redirect-links t
      dired-listing-switches "-lah"
      find-ls-option '("-exec ls -lah {} \\;" . "")
      dired-isearch-filenames 'dwim
      dired-omit-files "^\\." ; exclude hidden; was: "^\\.?#\\|^\\.$\\|^\\.\\.$"
      dired-omit-extensions (append '(".bak") dired-omit-extensions))

(setq dired-garbage-files-regexp
      "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyc\\)\\)\\'")

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
          (lambda ()
            (dhl-define-keys dired-mode-map
                             '(("e" wdired-change-to-wdired-mode)))
            (when (eq system-type 'darwin)
              (define-key dired-mode-map (kbd "C-c o")
                'dhl-dired-open-mac))))

(when (eq system-type 'darwin)
  (defun dhl-dired-open-mac ()
    "Open files on the mac from dired."
    (interactive)
    (let ((file-name (dired-get-file-for-visit)))
      (if (file-exists-p file-name)
          (shell-command (concat "open '" file-name "'" nil))))))


;;;;
;;;; ibuffer
;;;;

(require 'ibuffer nil t)

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-expert t)

(setq ibuffer-saved-filter-groups
      ;; TODO: use ' instead of $
      '(("default"
         ("elisp" (or (name . "\\.el$")
                      (mode . emacs-lisp-mode)))
         ("cl" (or (name . "\\.lisp$")
                   (name . "\\.asdf$")
                   (mode . lisp-mode)
                   (mode . slime-mode)))
         ("scheme" (or (name . "\\.scm$")
                       (mode . scheme-mode)
                       (mode . geiser-mode)))
         ("clojure" (or (name . "\\.clj$")
                        (mode . clojure-mode)))
         ("qi" (or (name . "\\.qi$")
                   (mode . qi-mode)))
         ("prolog" (mode . prolog-mode))
         ("python" (or (name . "\\.py$")
                       (mode . python-mode)
                       (mode . python-2-mode)
                       (mode . python-3-mode)))
         ("ruby" (or (name . "\\.rb$")))
         ("perl" (or (mode . cperl-mode)
                     (mode . sepia-mode)
                     (mode . perl6-mode)))
         ("php" (or (mode . php-mode)
                    (name . "\\.php$")))
         ("shell" (or (name . "\\.\\(sh\\|bash\\)$")
                      (name . "^\\.zshrc$")
                      (name . "^\\.profile")
                      (mode . shell-script-mode)))
         ("tcl" (or (mode . tcl-mode)
                    (name . "\\.tcl$")))
         ("octave" (or (name . "\\.m$")
                       (mode . octave-mode)))
         ("R" (name . "\\.R$"))
         ("julia" (name . "\\.jl$"))
         ("apl" (or (name . "\\.apl$")
                    (mode . gnu-apl-mode)))
         ("maxima" (or (name . "\\.max$")
                       (mode . maxima-mode)))
         ("haskell" (or (name . "\\.hs$")
                        (mode . haskell-mode)))
         ("erlang" (or (name . "\\.erl$")
                       (name . "\\.hrl$")
                       (mode . erlang-mode)))
         ("f#" (or (name . "\\.fs$")
                   (mode . fsharp-mode)))
         ("C" (or (name . "\\.c$")
                  (name . "\\.h$")
                  (mode . c-mode)))
         ("C++" (or (name . "\\.cpp$")
                    (name . "\\.hpp$")
                    (mode . c++-mode)))
         ("java" (or (name . "\\.java$")
                     (mode . java-mode)))
         ("groovy" (or (name . "\\.\\(groovy\\|gvy\\|gy\\|gsh$\\)")
                       (mode . groovy-mode)))
         ("sql" (or (name . "\\.sql$")
                    (mode . sql-mode)))
         ("xml" (or (name . "\\.xml$")
                    (mode . nxml-mode)
                    (mode . xquery-mode)
                    (name . "\\.xqy$")))
         ("html" (or (name . "\\.html$")
                     (mode . html-mode)
                     (mode . web-mode)))
         ("sgml" (name . "\\.dtd$"))
         ("css" (or (name . "\\.css$")
                    (mode . css-mode)))
         ("semweb" (or (name . "\\.omn$")
                       (mode . manchester-mode)
                       (name . "\\.\\(ttl\\|rdf\\)$")
                       (mode . ttl-mode)))
         ("javascript" (or (name . "\\.js$")
                           (name . "\\.jsonl?$")
                           (mode . javascript-mode)
                           (mode . js2-mode)))
         ("coffeescript" (or (name . "\\.coffee$")
                             (mode . coffee-mode)))
         ("typescript" (or (name . "\\.ts$")
                           (mode . typescript-mode)))
         ("rst" (or (name . "\\.rst$")
                    (mode . rst-mode)))
         ("assembler" (or (name . "\\.asm$")
                          (name . "\\.S$")
                          (mode . asm-mode)))
         ("tex" (or (name . "\\.tex$")
                    (mode . tex-mode)))
         ("org" (or (name . "\\.org$")
                    (mode . org-mode)))
         ("tf" (or (name . "\\.tf$")
                   (mode . tinyfugue-mode)))
         ("text" (or (name . "\\.txt$")
                     (mode . text-mode)))
         ("dired" (mode . dired-mode))
         ("gnus" (or
                  (mode . message-mode)
                  (mode . bbdb-mode)
                  (mode . mail-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "^\\.bbdb$")
                  (name . "^\\.newsrc-dribble")))
         ("erc" (mode . erc-mode))
         ("magit" (name . "^magit"))
         ("special" (name . "^\\*.*\\*")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (ibuffer-auto-mode 1)
            (hl-line-mode 1)))

(defadvice ibuffer
    (around dhl-ibuffer-point-to-most-recent first () activate)
  "Open `ibuffer' with cursor pointed to most recent buffer name."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))


;; (defun dhl-ibuffer-hook ()
;;   (ibuffer-define-sorter pathname
;;     (:documentation
;;      "Sort the buffers by their pathname."
;;      :description "path")
;;     (string-lessp (with-current-buffer (car a)
;;                     (or buffer-file-name
;;                         (if (eq major-mode 'dired-mode)
;;                             (expand-file-name dired-directory))
;;                         "~"))
;;                   (with-current-buffer (car b)
;;                     (or buffer-file-name
;;                         (if (eq major-mode 'dired-mode)
;;                             (expand-file-name dired-directory))
;;                         "~"))))
;;   (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-pathname))

;; (add-hook 'ibuffer-mode-hooks 'dhl-ibuffer-hook)

(setq ibuffer-formats
      '((mark modified read-only locked " "
              (name 35 35 :left :elide) " " ; was 18 18
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;;;;
;;;; occur
;;;;

(add-hook 'occur-mode-hook
          (lambda ()
            (next-error-follow-minor-mode 1)
            (local-set-key (kbd "n") 'next-line)
            (local-set-key (kbd "p") 'previous-line)))

(defadvice occur
    (after dhl-switch-to-occur last () activate)
  "Switch to `occur' window automatically."
  (other-window 1))

(defadvice multi-occur
    (after dhl-switch-to-multi-occur last () activate)
  "Switch to `occur' window automatically."
  (other-window 1))

(defadvice multi-occur-in-matching-buffers
    (after dhl-switch-to-multi-occur-in-matching-buffers last () activate)
  "Switch to `occur' window automatically."
  (other-window 1))

(defadvice ibuffer-do-occur
    (after dhl-ibuffer-switch-to-occur last () activate)
  "Switch to `occur' window automatically."
  (other-window 1))


;;;;
;;;; org-mode
;;;;

;; TODO:
;;
;; something in ob-sql-mode, sql-workbench, and the clojure config
;; leads to problems when loading org-compat.el using the newest org
;; version (as of 2019-10-20.) as a result,
;; org-indent-initialize-agent doesn't work in the timer, because
;; org-time-add isn't defined (use list-timers and c to fix)

;; (mapc (apply-partially 'add-to-list 'load-path)
;;       '("~/.emacs.d/elisp/org-mode/lisp"
;;         "~/.emacs.d/elisp/org-mode/contrib/lisp"))

;; ;; just for testing with preceding configs commented out
;; (setq browse-url-generic-program "open")
;; (setq Info-additional-directory-list nil)

(require 'org nil t)
(require 'org-table nil t)
(require 'ox-bibtex nil t)

(add-to-list 'load-path "~/.emacs.d/elisp/htmlize")
(require 'htmlize nil t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-to-list 'Info-additional-directory-list
             (expand-file-name "~/.emacs.d/elisp/org-mode/doc/"))

(setq org-file-apps
      (append `(("\\.x?html?\\'" . ,(concat browse-url-generic-program " %s"))
                ("\\.pdf\\'" . ,(concat browse-url-generic-program " %s"))
                (directory . emacs))
              org-file-apps))

(add-to-list 'load-path "~/.emacs.d/elisp/ox-jira")
(require 'ox-jira nil t)

;; TODO: right control binding
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-up>") 'org-shiftmetaup)
            (local-set-key (kbd "<C-down>") 'org-shiftmetadown)
            (local-set-key (kbd "<C-right>") 'org-shiftmetaright)
            (local-set-key (kbd "<C-left>") 'org-shiftmetaleft)
            (local-set-key (kbd "M-n") 'outline-next-visible-heading)
            (local-set-key (kbd "M-p") 'outline-previous-visible-heading)
            (if (featurep 'counsel)
                (local-set-key (kbd "<C-tab>") 'counsel-switch-buffer))))

(setq org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-startup-indented t
      org-startup-folded nil
      org-return-follows-link t
      org-use-extra-keys nil
      org-use-speed-commands t
      org-footnote-auto-adjust t
      org-use-sub-superscripts '{}
      org-pretty-entities t
      org-table-formula-evaluate-inline nil
      org-M-RET-may-split-line nil
      org-default-notes-file "~/notes/notes.org"
      org-agenda-files (list org-default-notes-file)
      org-refile-use-outline-path t
      org-refile-allow-creating-parent-nodes t
      org-export-with-timestamps nil
      org-export-with-sub-superscripts '{}
      org-export-with-archived-trees nil
      org-export-with-creator 'comment
      org-export-with-section-numbers nil
      org-export-with-tags nil
      org-export-with-latex t
      org-export-time-stamp-file t
      org-export-with-toc nil
      org-export-skip-text-before-1st-heading t
      org-html-postamble nil
      org-html-with-latex t
      org-html-style-include-default nil
      org-html-htmlize-output-type 'inline-css
      org-html-table-caption-above nil
      org-latex-table-caption-above nil
      org-goto-interface 'outline-path-completion ; 'outline
      org-goto-max-level 10
      org-outline-path-complete-in-steps nil)

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil)

(add-to-list 'load-path "~/.emacs.d/elisp/ob-restclient.el")
(require 'ob-restclient nil t)

(org-babel-do-load-languages 'org-babel-load-languages
                             '((emacs-lisp . t)
                               (C . t)
                               (python . t)
                               (shell . t)
                               (js . t)
                               (groovy . t)
                               (sql . t)
                               (php . t)
                               (restclient . t))) ; TODO: check if available


;; TODO: fix note template
(setq org-capture-templates
      '(("n" "Note" entry (file+headline "" "Misc")
         "* %^{Title}% ^G\n%U\n\n%i%?"
         :empty-lines 1)
        ("c" "Note" entry (file+headline "" "Misc")
         "* %?\n%U\n\n%i"
         :empty-lines 1)
        ("y" "Yank" entry (file+headline "" "Misc")
         "* %^{Title}% ^G\n%U\n\n%c%?"
         :empty-lines 1)
        ("t" "Task" entry (file+headline "" "Tasks")
         "* TODO %^{Title}% ^G\n%^T\n\n%i%?"
         :empty-lines 1)))

(setq org-html-mathjax-options
      '((path "MathJax/MathJax.js")
        (scale "100")
        (align "center")
        (indent "2em")
        (mathml nil)))

(setq org-html-head "
<style type=\"text/css\">
    html {
      font-family: Verdana, Arial, sans-serif;
      font-size: 11pt;
    }

    body {
      margin: 10%;
    }

    h1, h2, h3, h4, h5, h6 {
      color: #505050;
    }

    h2, h3, h4 {
      margin-top: 1.8em;
    }

    .title  {
      text-align: left;
    }

    .todo   { color: red; }
    .done   { color: green; }
    .tag    { background-color: #add8e6; font-weight:normal; }
    .target { }

    .timestamp { color: #bebebe; }
    .timestamp-kwd { color: #5f9ea0; }

    .right  { margin-left: auto; margin-right: 0px;  text-align: right; }
    .left   { margin-left: 0px;  margin-right: auto; text-align: left; }
    .center { margin-left: auto; margin-right: auto; text-align: center; }

    p.verse { margin-left: 3%; }

    pre {
      border: 1pt solid #AEBDCC;
      background-color: #F3F5F7;
      padding: 1em;
      padding-bottom: 0px;
      font-family: courier, monospace;
      font-size: 90%;
      overflow:auto;
    }

    table {
      border-collapse: collapse;
      /*margin-left: auto;
      margin-right: auto;*/
      margin-left: 3%;
      margin-right: 3%;
      margin-top: 1.7em;
      margin-bottom: 1em;
    }

    th { background: #eee; padding: 0.3em; }
    td, th { vertical-align: top; }
    th.right { text-align:center; }
    th.left { text-align:center; }
    th.center { text-align:center; }
    td.right { text-align:right; }
    td.left { text-align:left; }
    td.center { text-align:center; }
    dt { font-weight: bold; }

    li table {
      margin-top: 1em;
      margin-bottom: 1em;
    }

    dt {
      line-height: 150%;
    }

    div.figure { padding: 0.5em; }
    div.figure p { text-align: center; }

    div.inlinetask {
      padding:10px;
      border:2px solid gray;
      margin:10px;
      background: #ffffcc;
    }

    textarea { overflow-x: auto; }

    a {
      color: #0099cc;
      text-decoration: none;
    }
    a:visited { color: #005c7a; }

    .linenr { font-size:smaller; }
    .code-highlighted { background-color:#ffff00; }
    .org-info-js_info-navigation { border-style:none; }
    #org-info-js_console-label {
      font-size:10px;
      font-weight:bold;
      white-space:nowrap;
    }
    .org-info-js_search-highlight {
      background-color:#ffff00;
      color:#000000;
      font-weight:bold;
    }

    .tog { cursor: pointer; }
</style>")

(setq org-html-scripts "
<script src=\"http://ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js\">
</script>
<script type=\"text/javascript\">
  function CodeHighlightOn(elem, id) {
    var target = document.getElementById(id);

    if(null != target) {
      elem.cacheClassElem   = elem.className;
      elem.cacheClassTarget = target.className;
      target.className      = \"code-highlighted\";
      elem.className        = \"code-highlighted\";
    }
  }

  function CodeHighlightOff(elem, id) {
    var target = document.getElementById(id);

    if(elem.cacheClassElem)
      elem.className = elem.cacheClassElem;
    if(elem.cacheClassTarget)
      target.className = elem.cacheClassTarget;
  }

  jQuery(document).ready(function() {
    var togSuffix = \" …\";

    jQuery('.tog').append(togSuffix);
    jQuery('.tog').next().hide();
    jQuery('.tog').toggle(
      function() {
        jQuery(this).html(function(idx, oldHtml) {  /* TODO */
          return oldHtml.replace(new RegExp(togSuffix+\"$\"), \"\");
        });
      },
      function() {
        jQuery(this).append(togSuffix);
      }
    ); /* END TODO */
    jQuery('.tog').click(function(e) {
      jQuery(this).next().toggle();
    });
  });
</script>")

(setq org-html-mathjax-template "
<script type=\"text/x-mathjax-config\">
<!--/*--><![CDATA[/*><!--*/
    MathJax.Hub.Config({
        // Only one of the two following lines, depending on user settings
        // First allows browser-native MathML display, second forces HTML/CSS
        :MMLYES: config: [\"MMLorHTML.js\"], jax: [\"input/TeX\"],
        :MMLNO: jax: [\"input/TeX\", \"output/HTML-CSS\"],
        extensions: [\"tex2jax.js\",\"TeX/AMSmath.js\",\"TeX/AMSsymbols.js\",
                     \"TeX/noUndefined.js\"],
        tex2jax: {
            inlineMath: [ [\"\\\\(\",\"\\\\)\"] ],
            displayMath: [ ['$$','$$'], [\"\\\\[\",\"\\\\]\"], [\"\\\\begin{displaymath}\",\"\\\\end{displaymath}\"] ],
            skipTags: [\"script\",\"noscript\",\"style\",\"textarea\",\"pre\",\"code\"],
            ignoreClass: \"tex2jax_ignore\",
            processEscapes: false,
            processEnvironments: true,
            preview: \"TeX\"
        },
        showProcessingMessages: true,
        displayAlign: \"%ALIGN\",
        displayIndent: \"%INDENT\",

        \"HTML-CSS\": {
             scale: %SCALE,
             availableFonts: [\"STIX\",\"TeX\"],
             preferredFont: \"TeX\",
             webFont: \"TeX\",
             imageFont: \"TeX\",
             showMathMenu: true,
        },
        MMLorHTML: {
             prefer: {
                 MSIE:    \"MML\",
                 Firefox: \"MML\",
                 Opera:   \"HTML\",
                 other:   \"HTML\"
             }
        }
    });
/*]]>*///-->
</script>
<script type=\"text/javascript\" src=\"%PATH\"></script>")

;; (setq org-html-head-extra
;;       "<link href=\"http://fonts.googleapis.com/css?family=Droid+Serif\"
;;              rel=\"stylesheet\" type=\"text/css\">
;;        <link href=\"http://fonts.googleapis.com/css?family=Droid+Sans\"
;;              rel=\"stylesheet\" type=\"text/css\">")

;; (org-add-link-type "togsp"
;;   (lambda (&rest rest)
;;     (message "This link just hides the following Html element."))
;;   (lambda (path desc format)
;;     (case format
;;       (html
;;        (format "<span class=\"tog\">%s</span>" desc))
;;       (latex
;;        (format "%s" desc)))))

;; (org-add-link-type "togsp"
;;   (lambda (&rest rest)
;;     (message "This link just hides the following Html element."))
;;   (lambda (path desc format)
;;     (case format
;;       (html
;;        (format "<span class=\"tog\">%s</span> <span>%s</span>" desc path))
;;       (latex
;;        (format "%s %s" desc path)))))

(setq org-list-allow-alphabetical nil)  ; TODO: hack to allow loading ox-latex

(when (require 'ox-latex nil t)
  (add-to-list 'org-latex-classes
               `("article-de"
                 ,(concat "\\documentclass[a4paper,\n"
                          "                headings=small,\n"
                          "                captions=tableheading]\n"
                          "               {scrartcl}\n"
                          "[NO-DEFAULT-PACKAGES]\n"
                          "[PACKAGES]\n"
                          "[EXTRA]\n"
                          "\\usepackage[ngerman]{babel}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               `("xe-article-de"
                 ,(concat "\\documentclass[11pt,\n"
                          "                a4paper]\n"
                          "               {article}\n"
;                         "                a4paper,\n"
;                         "                headings=small,\n"
;                         "                captions=tableheading]\n"
;                         "                {article}\n"
                          "\\usepackage[T1]{fontenc}\n"
                          "\\usepackage{graphicx}\n"
                          "\\usepackage{hyperref}\n"
                          "\\usepackage{fontspec}\n"
                          "\\defaultfontfeatures{Mapping=tex-text}\n"
                          "\\setmainfont{Linux Libertine O}\n"
;                         "\\setromanfont{Gentium}\n"
;                         "\\setromanfont [BoldFont={Gentium Basic Bold},\n"
;                         "ItalicFont={Gentium Basic Italic}]{Gentium Basic}\n"
                          "\\setsansfont{Charis SIL}\n"
                          "\\setmonofont[Scale=0.8]{DejaVu Sans Mono}\n"
                          "\\usepackage{geometry}\n"
                          "\\geometry{a4paper, textwidth=6.5in, textheight=10in,\n"
                          "           marginparsep=7pt, marginparwidth=.6in}\n"
                          "\\pagestyle{empty}\n"
                          "\\title{}\n"
                          "[NO-DEFAULT-PACKAGES]\n"
                          "[NO-PACKAGES]\n")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               `("beamer"
                 ,(concat "\\documentclass[presentation\]\{beamer\}\n"
                          "[NO-DEFAULT-PACKAGES]\n"
                          "[PACKAGES]\n"
                          "[EXTRA]\n"
                          "\\usepackage[ngerman]{babel}")
;                ("\\section\{%s\}" . "\\section*\{%s\}")
;                ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
;                ("\\subsection\{%s\}" . "\\frame\{%s\}")
;                ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")
                 )))

(plist-put org-format-latex-options :scale 1.2)

(setq org-latex-packages-alist
      '(("utf8" "inputenc" t)
        ("T1" "fontenc" t)
        ("" "parskip" t)
        ("" "fixltx2e" nil)
        ("" "graphicx" t)
        ("" "longtable" nil)
        ("" "float" nil)
        ("" "wrapfig" nil)
        ("" "soul" t)
        ("" "textcomp" t)
;       ("" "marvosym" t)
;       ("" "wasysym" t)
;       ("" "latexsym" t)
        ("" "amssymb" t)
        ("" "amsmath" t)
;       ("hidelinks" "hyperref" nil)
        "\\tolerance=1000"))


;; (setq org-export-latex-quotes
;;       '(("en" ("\\(\\s-\\|[[(]\\)\"" . "\\enquote{")
;;          ("\\(\\S-\\)\"" . "}")
;;          ("\\(\\s-\\|(\\)'" . "`"))))

;; (setq org-latex-pdf-process
;;       '("xelatex -interaction nonstopmode %f"
;;         "xelatex -interaction nonstopmode %f"))

;; (setq org-latex-to-pdf-process
;;       "latexmk -pdflatex='lualatex -shell-escape -interaction nonstopmode' -pdf -f %f")

(setq org-latex-pdf-process (list "latexmk -bibtex -pdf %f"))


(require 'reftex nil t)

(defadvice reftex-TeX-master-file
    (around dhl-reftex-Tex-master-file-fix last () activate)
  (cl-flet ((tex-main-file ()
              (buffer-file-name)))
    ad-do-it))


;;;;
;;;; abbrev
;;;;

(setq dabbrev-case-fold-search case-fold-search)


;;;;
;;;; todotxt
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/todotxt")

(when (require 'todotxt nil t)
  (add-to-list 'auto-mode-alist '("\\.todo\\'" . todotxt-mode)))

(add-hook 'todotxt-mode-hook
          (lambda ()
            (local-set-key (kbd "M-p") 'todotxt-transpose-lines-up)
            (local-set-key (kbd "M-n") 'todotxt-transpose-lines-down)))

(setq todotxt-use-creation-dates nil
      todotxt-file "~/.todo/todo.txt")


;;;;
;;;; flashcard
;;;;

;; TODO: still needed?

(require 'flashcard nil t)

(add-to-list 'auto-mode-alist '("\\.deck\\'" . flashcard-mode))

(add-hook 'flashcard-mode-hook
          'flashcard-add-scroll-to-bottom)

(add-hook 'flashcard-positive-feedback-functions
          'flashcard-method-leitner-positive-feedback)

(add-hook 'flashcard-mode-hook
          (lambda ()
            (variable-pitch-mode 1)
            (set-input-method 'greek-babel)
            (toggle-input-method)))

(setq flashcard-coding-system 'utf-8-unix
      flashcard-method-leitner-compartment-sizes [40 80 200 320 560])


;;;;
;;;; voctest
;;;;

;; TODO: still needed?

(require 'voctest nil t)

(setq voctest-test-direction '(1 . 0))

(add-hook 'voctest-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))

;;;;
;;;; japanese
;;;;

(require 'mozc nil t)

;(setq default-input-method "japanese-mozc")


;;;;
;;;; tramp
;;;;

(require 'tramp nil t)

;(setq explicit-shell-file-name "/bin/bash")

(when (require 'tramp nil t)
  ;; delete tramp-persistency-file-name after changes and restart
  (mapcar (apply-partially 'add-to-list 'tramp-remote-path)
          '("/home/dleidisch/bin"
            'tramp-own-remote-path)))   ; doesn't work w/o sh -l

(mapc (apply-partially 'add-to-list 'tramp-remote-process-environment)
      `("SHELL=/bin/sh"
        ,(format "PATH=/home/dleidisch/bin:%s" (getenv "PATH"))))

(setq tramp-default-method "scp"    ; still uses ssh if <tramp-copy-file-limit
      ange-ftp-try-passive-mode t)


;;;;
;;;; projectile
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/projectile")

(when (require 'projectile nil t)
  (projectile-global-mode t)
  (setq projectile-indexing-method 'hybrid
        projectile-sort-order 'recentf    ; ignored when using alien
        shell-file-name "/bin/sh"         ; TODO: use connection-local vars?
        projectile-enable-caching t)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map))

(setq explicit-shell-file-name "/bin/zsh")

(defadvice projectile-grep
    (after dhl-projectile-grep-advice last () activate)
  "Switch to the grep window after invocation."
  (other-window 1))


;;;;
;;;; wgrep
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/wgrep")

(require 'wgrep nil t)


;;;;
;;;; easy-kill
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/easy-kill")

(when (require 'easy-kill nil t)
  (global-set-key (kbd "M-w") 'easy-kill)
  (global-set-key (kbd "C-M-SPC") 'easy-mark))


;;;;
;;;; kill ring
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/emacs-noflet")
(add-to-list 'load-path "~/.emacs.d/elisp/browse-kill-ring")

(when (require 'browse-kill-ring nil t)
; (browse-kill-ring-default-keybindings)
  (global-set-key (kbd "C-c C-y") 'browse-kill-ring))

(when (and (require 'noflet nil t)
           (require 'kill-ring-ido nil t))
  (global-set-key (kbd "M-y") 'kill-ring-ido))

(setq kill-ring-ido-shortage-length 40)

(when (require 'kill-ring-search nil t)
  (global-set-key (kbd "C-M-y") 'kill-ring-search))

(setq browse-kill-ring-highlight-current-entry t)


;;;;
;;;; expand-region
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/expand-region.el")

(when (require 'expand-region nil t)
  (global-set-key (kbd "S-SPC") 'er/expand-region))


;;;;
;;;; cursor
;;;;

(setq-default cursor-type 'bar)
(setq blink-cursor-blinks 100)
(set-cursor-color "#ff7700")

(add-to-list 'load-path "~/.emacs.d/elisp/beacon")

(setq beacon-size 20
      beacon-blink-delay 0.2
      beacon-blink-duration 0.2
      beacon-color "#ff7700"
      beacon-blink-when-window-scrolls t)

(if (not (require 'beacon nil t))
    (global-hl-line-mode 1)
  (beacon-mode 1)
  (add-to-list 'beacon-dont-blink-commands 'mwheel-scroll)
  (global-set-key (kbd "C-c b") 'beacon-blink))


;;;
;;; multiple-cursors
;;;

(add-to-list 'load-path "~/.emacs.d/elisp/multiple-cursors.el")

(when (require 'multiple-cursors nil t)
  (global-set-key (kbd "C-,") 'mc/mark-next-like-this)
  (define-key org-mode-map (kbd "C-,") 'mc/mark-next-like-this))

(setq mc/always-run-for-all t)


;;;;
;;;; navigation
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/neotree")

(require 'neotree nil t)

(setq neo-smart-open t
      neo-window-fixed-size nil
      neo-window-width 30
      projectile-switch-project-action 'neotree-projectile-action)

;; (add-to-list 'load-path "~/.emacs.d/elisp/ggtags")

;; (when (and (executable-find "global")
;;            (require 'ggtags nil t))
;;   (add-hook 'c-mode-common-hook
;;             (lambda ()
;;               (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
;;                 ;; TODO: make this work without listing servers explicitly
;;                 (when (or (not (file-remote-p default-directory))
;;                           (string= (file-remote-p default-directory 'host) "noc-dev"))
;;                   (ggtags-mode 1)))))
;;   (define-key ggtags-navigation-map (kbd "M-<") 'beginning-of-buffer)
;;   (define-key ggtags-navigation-map (kbd "M->") 'end-of-buffer))

;; (add-hook 'ggtags-mode-hook
;;           (local-set-key (kbd "M-<") 'beginning-of-buffer)
;;           (local-set-key (kbd "M->") 'end-of-buffer))

;; (add-hook 'ggtags-global-mode-hook
;;           (local-set-key (kbd "M-<") 'beginning-of-buffer)
;;           (local-set-key (kbd "M->") 'end-of-buffer))

;; TODO: can this be done better?
(setq ggtags-completing-read-function
      (lambda (&rest args)
        (apply #'ido-completing-read
               (car args)
               (all-completions (let ((symbol-at-point (symbol-at-point)))
                                  (if symbol-at-point
                                      (symbol-name symbol-at-point)
                                    "")) ggtags-completion-table)
               (cddr args))))

;(setq ggtags-auto-jump-to-match 'first) ; TODO: no effect?


;;;;
;;;; elmacro
;;;;

(add-to-list 'load-path "~/.emacs.d/elisp/elmacro")
(require 'elmacro nil t)


;;;;
;;;; keyfreq
;;;;

;; keybinding use statistics

(add-to-list 'load-path "~/.emacs.d/elisp/keyfreq")

(setq keyfreq-excluded-commands
      '(self-insert-command
        abort-recursive-edit
        forward-char
        backward-char
        previous-line
        next-line
        mwheel-scroll
        isearch-printing-char
        org-self-insert-command)
      keyfreq-file "~/.emacs.d/keyfreq")

(when (require 'keyfreq nil t)
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;
;;;; misc
;;;;

(require 'misc nil t)
(require 'subr-x nil t)

(require 'iso-transl nil t)             ; fix dead-* is not defined (24.3)

(when (require 'uniquify nil t)
  (setq uniquify-buffer-name-style 'forward))

(if (< emacs-major-version 24)
    (partial-completion-mode 1)
  ;; TODO: messes up nrepl/cider completion
  (setq completion-styles '(partial-completion initials)
        completion-pcm-complete-word-inserts-delimiters t
        completion-cycle-threshold 8))

;; TODO: keep this?
;(add-to-list 'load-path "~/.emacs.d/elisp/gnugo")

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function (lambda ())
      scroll-conservatively 101
      scroll-margin 0
      isearch-allow-scroll nil
      require-final-newline nil
      ispell-personal-dictionary "~/.emacs.d/ispell"
      ispell-dictionary "german"
      standard-indent 2
      tab-always-indent 'complete
      woman-use-own-frame nil
      Man-notify-method 'aggressive
      sentence-end-double-space nil
      make-backup-files 1
      default-major-mode 'text-mode
      undo-limit 100000
      apropos-do-all 1
      extended-command-suggest-shorter t
      line-move-visual nil
      help-window-select t
      enable-recursive-minibuffers t
      resize-mini-windows t
      comment-empty-lines nil
      set-mark-command-repeat-pop t
      epa-file-cache-passphrase-for-symmetric-encryption nil
      epa-file-select-keys nil
      epa-pinentry-mode 'loopback
      gc-cons-threshold 20000000
      kill-ring-max 200
      split-width-threshold 200
      enable-remote-dir-locals t
      user-full-name "Daniel H. Leidisch"
      user-mail-address "public@leidisch.net")

(setq-default indent-tabs-mode nil
              tab-width 2)

;; (setq default-frame-alist
;;       (append default-frame-alist '((width . 90) (height . 31))))

(set-frame-size (selected-frame) 90 30)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)
(line-number-mode 1)
(size-indication-mode 1)
(setq display-time-24hr-format t
      display-time-default-load-average nil
      display-time-mail-file -1)
(display-time-mode 1)
(show-paren-mode 1)
(transient-mark-mode -1)
(electric-indent-mode -1)
(blink-cursor-mode 1)
(minibuffer-depth-indicate-mode 1)
(auto-compression-mode 1)
(auto-image-file-mode 1)
(winner-mode 1)

(add-hook 'write-file-hooks #'time-stamp)

(fset 'yes-or-no-p 'y-or-n-p)

;; TODO: use a loop
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'overwrite-mode 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'list-timers 'disabled nil)

;; (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

(set-language-environment   'utf-8)
(prefer-coding-system       'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;; (when (eq system-type 'darwin)
;;   (cua-mode 0)
;;   (delete-selection-mode -1)
;;   (set-face-font 'default
;;                  "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-utf-8")
;;   (setq special-display-regexps
;;         (remove "[ ]?\\*[hH]elp.*" special-display-regexps))
;;   (setq special-display-regexps nil))

(when (eq system-type 'darwin)
  (setq mac-right-option-modifier 'option
;       mac-right-command-modifier 'control
        ))

(setq diff-switches "-u"
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; (when (eq system-type 'gnu/linux)
;;   (setenv "PATH"
;;           (concat (getenv "PATH") ":"
;;                   "/home/danlei/.cabal/bin" ":"
;;                   "/home/danlei/bin")))

;; (re-)enable z binding for special modes
(require 'man nil t)
(dolist (mode-map (list special-mode-map
                        Man-mode-map
                        ibuffer-mode-map))
  (define-key mode-map (kbd "z") 'dhl-kill-this-buffer))

(require 'view nil t)
(define-key view-mode-map (kbd "z") 'View-kill-and-leave)

(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-v") 'find-variable)

(setq frame-title-format
      '(:eval (if buffer-file-name
                (abbreviate-file-name (buffer-file-name))
                "%b")))


;;;;
;;;; utility functions/macros
;;;;

(defun dhl-insert-date (prefix)
  "Insert the current date. PREFIX switches the output format.

Without an explicit PREFIX (i.e. 1), use german output format.
With a PREFIX of 4, use ISO format. With a PREFIX of 16, write
out the day and month name."
  (interactive "P")
  (let ((format (cond ((not prefix) "%d.%m.%Y")
                      ((not prefix) "%d.%m.%y")
                      ((equal prefix '(4)) "%Y-%m-%d")
                      ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

(defun dhl-define-keys (mode-map keybindings)
  "Define multiple KEYBINDINGS for MODE-MAP at once.

Take a MODE-MAP, and a list of (KEY FUNCTION-DESIGNATOR) lists.
Bind the functions to their respective KEY in the given MODE-MAP.
Keys are expected to be in kbd format. If KEY is a list of keys,
bind all of them to the respective function."
  (mapc (lambda (keybinding)
          (destructuring-bind (keys function) keybinding
            (funcall (if (consp keys) #'mapcar #'funcall)
                     (lambda (key)
                       (define-key mode-map (read-kbd-macro key) function))
                     keys)))
        keybindings))

(defun dhl-global-set-keys (keybindings)
  "Define multiple global KEYBINDINGS at once.

Take a list of (KEY FUNCTION-DESIGNATOR) lists. Bind the
functions to their respective KEY globally. Keys are expected to
be in kbd format. If KEY is a list of keys, bind all of them to
the respective function."
  (mapc (lambda (keybinding)
          (destructuring-bind (keys function) keybinding
            (funcall (if (consp keys) #'mapcar #'funcall)
                     (lambda (key)
                       (global-set-key (read-kbd-macro key) function))
                     keys)))
        keybindings))

(defun dhl-kill-all-rnc-input-buffers ()
  "Close all RNC Input buffers."
  (interactive)
  (dolist (b (buffer-list))
    (when (string-match "RNC Input" (buffer-name b))
      (kill-buffer b))))

(defun dhl-kill-this-buffer ()
  "Kill the current buffer.

`kill-this-buffer' is actually unsafe to use for this purpose, as
it has been changed to be used from the menu bar specifically."
  (interactive)
  (kill-buffer (current-buffer)))

(defmacro time (&rest body)
  (let ((start (gensym)))
    `(let ((,start (float-time)))
       (prog1
           ,@body
         (message "%f" (- (float-time) ,start))))))


;;;;
;;;; misc advice
;;;;

(defadvice split-window
    (after dhl-window-splitting-advice last () activate)
  "Open `other-buffer' in vertical split window."
  (set-window-buffer (next-window) (other-buffer)))


;;;;
;;;; global keybindings
;;;;

(dhl-global-set-keys '(("C-c i" indent-relative-first-indent-point)
                       ("C-c d" dhl-insert-date)
                       ("C-x C-b" ibuffer)
                       ("M-/" hippie-expand)
;                      ("C-c C-s" slime-selector)
                       ("C-x r v" view-register)
;                      ("M-X" dhl-invoke-smex)
                       (("C-^" "<C-dead-circumflex>") winner-undo)
                       (("M-C-^" "<M-C-dead-circumflex>") winner-redo)
                       ("M-s m o" multi-occur)
                       ("M-s m m" multi-occur-in-matching-buffers)
                       ("M-g d" xref-find-definitions)
                       ("M-g r" xref-find-references)
                       ("M-z" zap-up-to-char)
                       ("C-x r a" set-rectangular-region-anchor)
                       ("C-c l" org-store-link)
                       ("C-c c" org-capture)
                       ("C-c a" org-agenda)
                       ("C-c t" todotxt)
                       ("C-x RET i" set-input-method)
                       ("M-#" quick-calc)
                       ("C-c j" dired-jump)
                       ("C-c w" whitespace-mode)
                       ("C-c W" delete-trailing-whitespace)
                       ("C-c v" view-mode)
                       ("C-c n" neotree)
                       ("C-c r" revert-buffer)
                       ("C-c f" find-name-dired)
                       ("C-c F" find-dired)
                       ("C-c g" grep-find)
                       ("C-c G" find-grep-dired)
                       ("C-c DEL" kill-whole-line)
                       (("M-^" "<M-dead-circumflex>") delete-indentation)
                       ("<wheel-right>" (lambda ()
                                          (interactive)
                                          (scroll-left 10)))
                       ("<wheel-left>" (lambda ()
                                         (interactive)
                                         (scroll-right 10)))))

(when (fboundp 'cycle-spacing)
  (global-set-key (kbd "M-SPC") 'cycle-spacing))

(defun dhl-open-file-maybe-project (prefix)
  "Open project file, falling back to counsel, ido, or plain `find-file`."
  (interactive "p")
  (message (number-to-string prefix))
  (cond ((and (= prefix 1)
              (featurep 'projectile)
              (projectile-project-p))
         (projectile-find-file-dwim))
        ((featurep 'counsel)
         (counsel-find-file))
        ((featurep 'ido)
         (ido-find-file))
        (t (find-file))))

(global-set-key (kbd "s-o") 'dhl-open-file-maybe-project)


;;;;
;;;; gnus
;;;;

(setq gnus-select-method '(nntp "news.albasani.net"))

;; ~/.authinfo:
;; machine news.albasani.net login me@foo.net password pass

(setq gnus-user-date-format-alist
      '(((gnus-seconds-today) . "%H:%M ")
        (604800 . "%a, %H:%M ")
        ((gnus-seconds-month) . "%a, %d.   ")
        ((gnus-seconds-year) . "%a, %d.%m.")
        (t . "%d.%m.%y")))

(setq gnus-group-line-format "%2{%M%S%p%} %0{%5y%} %P%1{%G%}\n"
      gnus-topic-line-format "%i%3{[ %n -- %A ]%}%v\n"
      gnus-summary-line-format "%[%U%R%] %-55,55s %10&user-date; %B %a\n")

(setq gnus-sum-thread-tree-single-indent   "◎ "
      gnus-sum-thread-tree-false-root      "◯╮"
      gnus-sum-thread-tree-root            "● "
      gnus-sum-thread-tree-vertical        "│"
      gnus-sum-thread-tree-leaf-with-other "├─► "
      gnus-sum-thread-tree-single-leaf     "╰─► "
      gnus-sum-thread-tree-indent          " ")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-visible-headers
      '("^Subject:" "^From:" "^To:" "^Cc:" "^Resent-To:" "^\\(Message-\\)?Date:"
        "^Newsgroups:" "^Followup:" "^ID:" "^Organization-To:" "^Reply-To:"
        "^User-Agent:" "^X-Newsreader:" "^X-Mailer:"))

(setq gnus-sorted-header-list gnus-visible-headers)

;; (setq gnus-auto-select-first 'unseen-or-unread)
(add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads)
(add-hook 'gnus-summary-mode-hook (lambda ()
                                    (hl-line-mode 1)
                                    (setq cursor-type nil)))

;;;
;;; scoring/threading
;;;

(setq gnus-use-adaptive-scoring '(line)
      gnus-use-scoring t
      gnus-save-score t
      gnus-score-expiry-days nil
      gnus-home-score-file "~/.emacs.d/gnus/score"
      gnus-home-adapt-file "~/.emacs.d/gnus/adapt-score"
      gnus-summary-default-score 0
      gnus-score-thread-simplify t)

(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 5) (subject 2))
        (gnus-saved-mark (from 10) (subject 2))
        (gnus-dormant-mark (from 5) (subject 2))
        (gnus-del-mark (from -1) (subject -5))       ; marked as read with d
        (gnus-read-mark (from 3) (subject 5))
        (gnus-replied-mark (from 20) (subject 10))
        (gnus-expirable-mark (from -1) (subject -3))
        (gnus-killed-mark (from -1) (subject -5))
        (gnus-kill-file-mark)
        (gnus-ancient-mark)
        (gnus-low-score-mark)
        (gnus-catchup-mark (subject -1))))

(setq gnus-adaptive-pretty-print t)

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)))

(setq gnus-article-sort-functions
      '(gnus-article-sort-by-number))

(setq gnus-summary-expunge-below -500
      gnus-thread-expunge-below nil)

(defadvice gnus-score-find-trace
    (after dhl-switch-to-score-trace last () activate)
  "Switch to `gnus-score-find-trace' window automatically."
  (other-window 1))


;;;
;;; misc
;;;

(setq gnus-novice-user nil
      gnus-add-to-list t
      gnus-summary-goto-unread nil
      gnus-summary-make-false-root 'adopt
      gnus-directory "~/.emacs.d/gnus/"
      gnus-startup-file "~/.emacs.d/gnus/newsrc"
      gnus-article-save-directory "~/.emacs.d/gnus/articles/"
      gnus-cache-directory "~/.emacs.d/gnus/cache/"
      gnus-cache-active-file "~/.emacs.d/gnus/cache/active"
      gnus-kill-files-directory "~/.emacs.d/gnus/killfiles/"
      nndraft-directory "~/.emacs.d/gnus/drafts/"
      gnus-save-newsrc-file nil
      gnus-read-newsrc-file nil
      gnus-save-killed-list nil
      gnus-check-new-newsgroups 'ask-server
      gnus-default-article-saver 'gnus-summary-save-in-file
      gnus-show-all-headers nil
      gnus-fetch-old-headers t
      gnus-treat-capitalize-sentences nil
      gnus-treat-display-smileys t
      gnus-treat-display-x-face t
      gnus-treat-emphasize t
      gnus-treat-fill-long-lines nil
      gnus-treat-fill-article nil
      gnus-treat-hide-signature nil
      gnus-treat-overstrike nil
      gnus-treat-play-sounds nil
      gnus-treat-strip-banner nil
      gnus-treat-strip-cr t
      gnus-treat-strip-leading-blank-lines nil
      gnus-treat-strip-multiple-blank-lines nil
      gnus-treat-strip-pem nil
      gnus-treat-strip-pgp nil
      gnus-treat-strip-trailing-blank-lines nil
      gnus-treat-translate nil
      gnus-large-newsgroup 500
      gnus-agent t
      gnus-cache-enter-articles '(ticked dormant)
      gnus-article-wash-function (if (featurep 'w3m) 'w3m 'html2text)
      mm-text-html-renderer (if (featurep 'w3m) 'w3m 'html2text)
      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-permanently-visible-groups "Alle Nachrichten")

(setq gnus-message-archive-method
      '(nnfolder "archive"
                 (nnfolder-inhibit-expiry t)
                 ;; TODO: just set this directly?
                 (nnfolder-active-file "~/.emacs.d/gnus/sent/active")
                 (nnfolder-directory "~/.emacs.d/gnus/sent/")))

(setq gnus-message-archive-group
      '((if (message-news-p)
            "sent-news"
          "sent-mail")))


;;;
;;; posting styles
;;;

(setq gnus-posting-styles
      '((message-news-p (address "news@leidisch.net"))
        ("gmane" (address "lists@leidisch.net"))
        ("nnml:.*" (From (with-current-buffer gnus-article-buffer ; TODO
                           (message-fetch-field "to"))))))


;;;
;;; gmail, gmane
;;;

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(require 'nnir nil t)

(setq gnus-secondary-select-methods
      '((nnimap "imap.gmail.com"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
;               (nnimap-authenticator login)    ;; when used without ~/.authinfo
                (nnir-search-engine imap))
        (nntp "news.gmane.org"
              (nntp-address "news.gmane.org")
              (nntp-port-number 119))))

;; ~/.authinfo:
;; machine imap.gmail.com login me@foo.com password pass port 993

(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;     smtpmail-auth-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials (expand-file-name "~/.authinfo")
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "leidisch.net")

;; ~/.authinfo:
;; machine smtp.gmail.com login name@gmail.com password pass port 587

;; (add-hook 'message-setup-hook
;;            (lambda () (use-hard-newlines t t))) ;; format=flowed


;; (setq message-alternative-emails
;;       (regexp-opt '("public@leidisch.net"
;;                     "foo@bar.baz")))

(load (expand-file-name "~/.message-alternative-emails") t)

(setq mm-coding-system-priorities '(utf-8)
      mm-fill-flowed t)


;;;;
;;;; epilogue
;;;;


(server-start)

(setq-default custom-safe-themes
  '("e9f5c74f31fee49c7d592e2e8f1f02a15bdda3833c74acd5cba7b2ab2364b904"
    "540abc7ca9ee1078994c596a38c8f73cf1f88b698a6f5053334ed9496e3b2f7e"
    default))

(when (and (locate-file "zenburn-theme"
                        custom-theme-load-path
                        '(".el" ".elc"))
           (locate-file "tango-dhl-theme"
                        custom-theme-load-path
                        '(".el" ".elc")))
  (global-set-key (kbd "C-c T") 'dhl-toggle-theme)
  (dhl-toggle-theme))

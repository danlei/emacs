;;;;; -*- emacs-lisp -*-
;;;;;
;;;;; Emacs Configuration File (.emacs)
;;;;;
;;;;; Time-stamp: <2012-02-19 02:02:31 dhl>
;;;;;


(require 'cl)


;;;;
;;;; paths
;;;;

(add-to-list 'load-path "~/.emacs.d/")

(when (eq system-type 'windows-nt)
  (setenv "CYGWIN" (concat (getenv "CYGWIN") " nodosfilewarning"))
  (mapcar (apply-partially 'add-to-list 'exec-path)
          '("C:/Perl/bin"
            "C:/MinGW/bin"
            (expand-file-name "~/programme/latex/miktex/bin")
            "~/programme/darcs-2.5.2-win1"
            "C:/cygwin/bin"))
  (setenv "PATH" (concat "C:/Perl/bin/" ";"
                         "C:/MinGW/bin" ";"
                         (expand-file-name "~/programme/latex/miktex/bin") ";"
                         "C:/cygwin/bin" ";"
                         (expand-file-name "~/programme/darcs-2.5.2-win1") ";"
                         (getenv "PATH"))))


;;;;
;;;; color-theme
;;;;

(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0/")

(when (require 'color-theme "color-theme" t)
  (color-theme-initialize)
  (and (require 'color-theme-dhl-hober "color-theme-dhl-hober" t)
       (color-theme-dhl-hober)))

;;;;
;;;; smart-tab
;;;;

;; (add-to-list 'load-path "~/.emacs.d/smart-tab/")

;; (when (require 'smart-tab "smart-tab" t)
;;   (global-smart-tab-mode 1))

;; (setq smart-tab-using-hippie-expand t)

;; (setq smart-tab-completion-functions-alist
;;       '((emacs-lisp-mode . lisp-complete-symbol)
;;         (text-mode . dabbrev-completion)))


;;;;
;;;; slime
;;;;

(add-to-list 'load-path "~/.emacs.d/slime/")
(add-to-list 'load-path "~/.emacs.d/slime/contrib/")

(when (require 'slime "slime" t)
  (slime-setup '(slime-fancy slime-asdf slime-references
                 slime-indentation slime-xref-browser)))

(setq slime-enable-evaluate-in-emacs t
      slime-net-coding-system 'utf-8-unix
;;    lisp-indent-function 'cl-indent:function
      )

(setq slime-lisp-implementations
      `((ccl ,@(list (case system-type
                       ((windows-nt cygwin) '("~/build/ccl/wx86cl" "-K utf-8"))
                       (gnu/linux '("~/build/ccl/lx86cl" "-K utf-8")))))
        (clisp ,@(list (case system-type
                         ((cygwin gnu/linux) '("clisp" "-E utf-8" "-modern"))
                         (windows-nt '("~/build/clisp/clisp-2.49/clisp"
                                       "-modern"))))))
      slime-default-lisp 'ccl)

(add-hook 'slime-mode-hook
          (lambda ()
            (define-keys slime-mode-map
                '(("C-c s" slime-selector)
                  ("C-j" newline-and-indent)
                  ("TAB" slime-indent-and-complete-symbol)
                  ("C-c C-d c" cltl2-lookup)))))

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (define-keys slime-repl-mode-map
                '(("C-c s" slime-selector)
                  ("C-c C-d c" cltl2-lookup)))))

;; correct repl cursor position after compilation
(add-hook 'slime-compilation-finished-hook
          (lambda (compiler-notes)
            (with-current-buffer (slime-repl-buffer)
              (forward-word)))
          t)


;;; nyef's pathname fix for cygwin
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


;;;;
;;;; paredit
;;;;

(when (require 'paredit "paredit" t)
  (mapc (lambda (hook) (add-hook hook (lambda () (paredit-mode 1))))
        '(slime-mode-hook
          slime-repl-mode-hook
          emacs-lisp-mode-hook
          ielm-mode-hook
          scheme-mode-hook
          inferior-scheme-mode-hook
          inferior-qi-mode-hook
          qi-mode-hook))
  (setq clojure-enable-paredit t))

(add-hook 'paredit-mode-hook
          (lambda ()
            (define-keys paredit-mode-map
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
                  ("M-a" slime-beginning-of-defun)
                  ("M-e" slime-end-of-defun)
                  ("C-M-a" backward-sentence)
                  ("C-M-e" forward-sentence)
                  ("M-q" indent-pp-sexp)
                  ("C-M-q" fill-paragraph)))))


;;;;
;;;; clojure
;;;;

(add-to-list 'load-path "~/.emacs.d/clojure-mode/")
;; (add-to-list 'load-path "~/.emacs.d/swank-clojure/")
;; (add-to-list 'load-path "~/.emacs.d/swank-clojure/src/emacs")

;; (case system-type
;;   (gnu/linux
;;      (setq swank-clojure-jar-path
;;            "c:/cygwin/home/danlei/build/clojure/clojure.jar"))
;;   (cygwin
;;      (setq swank-clojure-jar-path
;;            "c:/cygwin/home/danlei/build/clojure/clojure.jar"))
;;   (windows-nt
;;      (setq swank-clojure-path
;;            "c:/cygwin/home/danlei/.emacs.d/swank-clojure/classes/"
;;            swank-clojure-jar-path
;;            "c:/cygwin/home/danlei/build/clojure/clojure.jar"
;;            swank-clojure-extra-classpaths
;;            '("c:/cygwin/home/danlei/Clojure/clojure-contrib/clojure-contrib.jar"
;;              "c:/cygwin/home/danlei/Clojure/swank-clojure/src/")
;;            swank-clojure-java-path
;;            "c:/Programme/Java/jdk1.6.0_16/bin/java.exe"
;;            swank-clojure-extra-vm-args (list "-server"))))

(require 'clojure-mode "clojure-mode" t)
;(require 'swank-clojure-autoload "swank-clojure-autoload" t)

;; (require 'swank-clojure "swank-clojure" t)

(add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))


;;;;
;;;; qi
;;;;

(require 'qi-mode "qi-mode" t)

(setq inferior-qi-program
      (case system-type
        (windows-nt "C:/Users/dhl/QiII1.06SBCL/sbcl.exe --noinform --core c:/Users/dhl/QiII1.06SBCL/Qi.core")
        (cygwin "~/build/QiII1.06SBCL/sbcl.exe --noinform --core c:/cygwin/home/danlei/build/QiII1.06SBCL/Qi.core")))


;;;;
;;;; scheme
;;;;

(setq scheme-program-name "guile")

(add-hook
 'inferior-scheme-mode-hook
 (lambda ()
   (define-keys inferior-scheme-mode-map
       '(("M-TAB" hippie-expand)))))

(when (require 'quack "quack" t)
  (setq quack-remap-find-file-bindings-p nil)
  (setq quack-pretty-lambda-p t))

(when (require 'scheme-complete "scheme-complete" t)
  (add-hook
   'inferior-scheme-mode-hook
   (lambda ()
     (define-keys inferior-scheme-mode-map
         '(("TAB" scheme-complete-or-indent)))
     (make-local-variable 'eldoc-documentation-function)
     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
     (eldoc-mode 1)))
  (add-hook
   'scheme-mode-hook
   (lambda ()
     (define-keys inferior-scheme-mode-map
         '(("TAB" scheme-complete-or-indent)))
     (make-local-variable 'eldoc-documentation-function)
     (setq eldoc-documentation-function 'scheme-get-current-symbol-info)
     (eldoc-mode 1))))


;;;;
;;;; newlisp
;;;;

(add-to-list 'load-path "~/.emacs.d/newlisp-mode/")

(add-to-list 'auto-mode-alist '("\\.lsp\\'" . newlisp-mode))
(autoload 'newlisp-mode "newlisp"
  "Newlisp editing mode." t)


;;;;
;;;; j-mode
;;;;

(autoload 'j-mode "j-mode.el"  "Major mode for J." t)
(autoload 'j-shell "j-mode.el" "Run J from emacs." t)

(add-to-list 'auto-mode-alist '("\\.ij[rstp]" . j-mode))

(setq j-path "/cygdrive/c/Dokumente und Einstellungen/danlei/j602/bin/")
;(setq j-dictionary-url "http://www.jsoftware.com/help/dictionary/")
(setq j-dictionary-url "file:///C:/Dokumente und Einstellungen/danlei/j602/help/dictionary/")

(when (require 'which-func "which-func" t)
  (which-func-mode 1))


;;;;
;;;; ruby
;;;;

(autoload 'run-ruby "inf-ruby.el" "Run irb from Emacs." t)
;(setq ruby-program-name "irb")

(when (eq system-type 'windows-nt)
  (setq ruby-program-name "C:/Ruby192/bin/ruby.exe"))

(add-hook
 'ruby-mode-hook
 (lambda ()
   (define-keys ruby-mode-map
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
  "Browse the ruby class documentation for a class queried in the
minibuffer, defaulting to word-at-point."
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

(add-to-list 'load-path "~/.emacs.d/python.el/")

(require 'python)

(setq python-process-kill-without-query t
      python-default-version 3)

(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-tab>") 'symbol-complete)
            (setq parens-require-spaces nil)
            (eldoc-mode 1)))

(add-hook 'inferior-python-mode-hook
          (lambda ()
            (setq parens-require-spaces nil)))

(when (eq system-type 'windows-nt)
  (setq-default python-command "C:/Python32/python.exe"
                python-python-command "C:/Python32/python.exe"
                python-shell-interpreter "C:/Python32/python.exe"
                python-shell-interpreter-args "-ui"))


;;;;
;;;; perl
;;;;

(add-to-list 'auto-mode-alist '("\.pl$" . sepia-mode))
(add-to-list 'interpreter-mode-alist '("perl" . sepia-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . sepia-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . sepia-mode))

(setq cperl-indent-level 4
      cperl-indent-parens-as-block nil)

;; (defun cperl-eldoc-documentation-function ()
;;   "Return meaningful doc string for `eldoc-mode'."
;;   (car
;;    (let ((cperl-message-on-help-error nil))
;;      (cperl-get-help))))

;; (add-hook 'cperl-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'eldoc-documentation-function)
;;                  'cperl-eldoc-documentation-function)))

(add-to-list 'load-path "~/.emacs.d/Sepia-0.992-S3oGPo/")
(setq sepia-perl5lib (list (expand-file-name "~/.emacs.d/Sepia-0.992-S3oGPo/lib")))
;(defalias 'perl-mode 'sepia-mode)
(require 'sepia)
(setq sepia-program-name "c:/Perl/bin/perl")


;;;;
;;;; tcl
;;;;

(require 'tcl "tcl" t)

(when (eq system-type 'cygwin)
  (setq tcl-application "/cygdrive/c/Tcl/bin/tclsh85.exe"))

(when (eq system-type 'cygwin)
  (add-hook 'inferior-tcl-mode-hook
            (lambda ()
              (tcl-send-string
               (inferior-tcl-proc) "set ::tcl_interactive 1\n")
              (tcl-send-string
               (inferior-tcl-proc)
               "namespace path {::tcl::mathop ::tcl::mathfunc}\n"))))


;;;;
;;;; maxima
;;;;

(setq maxima-command "C:/Program Files/Maxima-5.25.0/bin/maxima.bat")

(add-to-list 'load-path "C:/Program Files/Maxima-5.25.0/share/maxima/5.25.0/emacs/")

(require 'maxima "maxima" t)
(require 'maxima-font-lock "maxima-font-look" t)

;(load "c:/Program Files/Maxima-5.25.0/share/maxima/5.25.0/emacs/setup-imaxima-imath.el")
;(load "C:/Users/dhl/foo/setup-imaxima-imath.el")

;; (load "maxima.el")
;; (load "maxima-font-lock.el")
;; (setq imaxima-use-maxima-mode-flag t)
;; (load "imaxima.el")
;; (load "emaxima.el")
;; (setq imaxima-fnt-size "Large")

(add-to-list 'auto-mode-alist '("\\.max" . maxima-mode))


;;;;
;;;; haskell
;;;;

(add-to-list 'load-path "~/.emacs.d/haskell-mode/")

(and (require 'inf-haskell "inf-haskell" t)
     (require 'haskell-indent "haskell-indent" t))

(setq haskell-program-name (concat "ghci "
;                                   "-fglasgow-exts "
;                                   "-XNoMonomorphismRestriction "
;                                   "-XTupleSections "
                                   ))

;(setq inferior-haskell-find-project-root nil)

(setq haskell-font-lock-symbols 'unicode)

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs$" . literal-haskell-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(setq haskell-hoogle-command "hoogle")

(add-hook 'inferior-haskell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c h") 'haskell-hoogle)
            (turn-on-haskell-doc-mode 1)))

(add-hook
 'haskell-mode-hook
 (lambda ()
   (define-keys haskell-mode-map
       '(("RET" newline)
         ("TAB" haskell-indent-cycle)
         ("C-c =" haskell-indent-insert-equal)
         ("C-c |" haskell-indent-insert-guard)
         ("C-c o" haskell-indent-insert-otherwise)
         ("C-c w" haskell-indent-insert-where)
         ("C-c ." haskell-indent-align-guards-and-rhs)
;        ("C-c h" haskell-hoogle)
;        ("C-c t" inferior-haskell-type)
         ("C-c i" inferior-haskell-info)
         ("M-." inferior-haskell-find-definition)))))

(when (eq system-type 'cygwin)
  (defadvice inferior-haskell-load-file
      (around dhl-inferior-haskell-load-file-cygwin-fix)
    "Fixes inferior-haskell-load-file for Win Haskell/Cygwin Emacs."
    (save-buffer)
    (let ((buffer-file-name (concat "c:/cygwin" buffer-file-name)))
      ad-do-it))
  (ad-activate 'inferior-haskell-load-file))

;;;
;;; ghc-mod
;;;

(add-to-list 'load-path "~/.emacs.d/ghc-mod/")

(setq ghc-completion-key (kbd "<C-tab>")
      ghc-document-key (kbd "C-c d")
      ghc-import-key (kbd "C-c m")
      ghc-previous-key (kbd "M-p")
      ghc-next-key (kbd "M-n")
      ghc-help-key (kbd "C-c h")
      ghc-insert-key (kbd "C-c t")
      ghc-sort-key (kbd "C-c s")
      ghc-check-key (kbd "C-x C-s")
      ghc-toggle-key (kbd "C-c C-c"))

(autoload 'ghc-init "ghc" nil t)

(when (require 'ghc "ghc" t)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (ghc-init))))

(defadvice ghc-init
    (before dhl-ghc-mod-local-completion first () activate)
  "Makes ghc-mod completions buffer local."
  (make-local-variable 'ghc-loaded-module)
  (make-local-variable 'ghc-merged-keyword))

(defadvice ghc-import-module
    (before dhl-ghc-mod-reset-modules first () activate)
  "Makes ghc-import-module recognize dropped imports."
  (setq ghc-loaded-module nil)
  (ghc-comp-init))


;;;;
;;;; f#
;;;;

(add-to-list 'load-path "~/.emacs.d/fsharp/")

(require 'fsharp "fsharp" t)

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

(add-to-list 'auto-mode-alist '("\.fs$" . fsharp-mode))

(add-hook
 'fsharp-mode-hook
 (lambda ()
   (define-keys fsharp-mode-map
     `(("C-c b" fsharp-mark-block)
       ("C-c r" fsharp-run-executable-file)))))


;;;;
;;;; prolog
;;;;

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(require 'prolog "prolog" t)

(setq prolog-system 'swi)

(setq prolog-program-name
      (case system-type
        (windows-nt "C:/Program Files/pl/bin/swipl.exe")
        (cygwin "/usr/bin/pl")))


;(add-to-list 'auto-mode-alist '("\.pl$" . prolog-mode))


;;;;
;;;; java
;;;;

(add-to-list 'load-path "~/.emacs.d/javarun/")

(when (require 'javarun "javarun" t)
  (setq ; javarun-cygdir "c:/cygwin/"
        javarun-javac-command "javac.exe"
        javarun-java-command "java"
        javarun-java-path "C:/Program Files/Java/jdk1.6.0/bin/")
  (add-hook 'java-mode-hook
            (lambda ()
              (javarun-mode 1)
              (subword-mode 1))))


;;;;
;;;; c
;;;;

;(require 'cc-mode)

(setq compilation-window-height 10)
(setq compilation-read-command nil)

(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c c") 'compile)
            (local-set-key (kbd "M-p") 'previous-error)
            (local-set-key (kbd "M-n") 'next-error)
            (subword-mode 1)))

(defun dhl-gcc-compile-command (&optional extra-arguments)
  "Returns a gcc command line for use with compile-command.
Intended for use with little test files. Extra options may
be given as an optional argument."
  (concat "gcc -std=c99 -pedantic -Wall -o "
          (file-name-sans-extension (file-name-nondirectory buffer-file-name)) " "
          (file-name-nondirectory buffer-file-name) " "
          extra-arguments))

;;;
;;; lpc
;;;

;; (modify-syntax-entry ?' "'" c-mode-syntax-table)


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

(when (require 'rnc-mode "rnc-mode" t)
  (add-to-list 'auto-mode-alist '("\\.rnc\\'" . rnc-mode))
  (setq rnc-indent-level 2))

(add-to-list 'auto-mode-alist '("\\.\\(xml\\|xsd\\|xsl\\)\\'" . nxml-mode))

(setq nxml-slash-auto-complete-flag t)

(add-hook 'nxml-mode-hook
          (lambda ()
            (define-keys nxml-mode-map
                `(("TAB" ,(lambda (n)
                            (interactive "p")
                            (indent-for-tab-command)
                            (nxml-complete)))
                  ("<C-return>" ,(lambda (n)
                                   (interactive "p")
                                   (nxml-complete)
                                   (nxml-balanced-close-start-tag-block)))
                  ("M-ö" nxml-down-element)
                  ("M-u" nxml-backward-up-element)
                  ("M-n" nxml-forward-element)
                  ("M-p" nxml-backward-element)
                  ("C-M-ö" nxml-backward-down-element)
                  ("C-M-n" nxml-up-element)))))


;;;;
;;;; elisp
;;;;

(defun dhl-lisp-indent-and-complete (n)
  (interactive "p")
  (indent-for-tab-command)
  (lisp-complete-symbol))

(defun dhl-lisp-eval-print-defun ()
  (interactive)
  (end-of-defun)
  (eval-print-last-sexp))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (local-set-key (kbd "TAB") 'dhl-lisp-indent-and-complete)
            (local-set-key (kbd "<C-return>") 'dhl-lisp-eval-print-defun)))

(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (define-keys lisp-interaction-mode-map
              '(("TAB" dhl-lisp-indent-and-complete)
                ("<C-return>" eval-print-last-sexp)
                ("<C-return>" dhl-lisp-eval-print-defun)))))

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)


;;;;
;;;; asm
;;;;

(add-hook 'asm-mode-hook
          (lambda ()
            (setq tab-width 8)
            (local-set-key (kbd "RET") 'newline)))


;;;;
;;;; ess
;;;;

(add-to-list 'load-path "~/.emacs.d/ess-5.13/lisp/")

(require 'ess-site)

(setq inferior-R-program-name "C:/Program Files/R/R-2.13.0/bin/i386/Rterm.exe")


;;;;
;;;; ielm
;;;;

(setq ielm-prompt "elisp> ")

(add-hook 'ielm-mode-hook
          (lambda ()
            (eldoc-mode 1)
            (setq comint-dynamic-complete-functions
                  '(ielm-tab
                    comint-replace-by-expanded-history
                    ielm-complete-filename
                    ielm-complete-symbol
                    lisp-complete-symbol))))


;;;;
;;;; eshell
;;;;

(setq eshell-prefer-lisp-functions t
      eshell-bad-command-tolerance 5)

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-a") 'eshell-maybe-bol)
            (local-set-key (kbd "<C-tab>") 'PC-lisp-complete-symbol)
            (eldoc-mode 1)))

;; (require 'em-smart)

;; (setq eshell-where-to-jump 'begin)
;; (setq eshell-review-quick-commands t)
;; (setq eshell-smart-space-goes-to-end t)

(defun eshell-maybe-bol ()
  "Moves point behind the eshell prompt, or
at the beginning of line, if already there."
  (interactive)
  (let ((p (point)))
    (eshell-bol)
    (when (= p (point))
      (beginning-of-line))))

(defun eshell/clear ()
  "Clears the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))


;;;;
;;;; hippie-expansion
;;;;

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill))


;;;;
;;;; ido
;;;;

(add-to-list 'load-path "~/.emacs.d/smex/")

(and (require 'ido "ido" t)
     (ido-mode t)
     (require 'smex "smex" t)
     (smex-initialize)
     (setq smex-save-file "~/.smex")
     (smex-auto-update))

(defun dhl-invoke-smex (x)
  "Invokes smex, if called without a prefix argument,
smex-major-mode-commands otherwise. Note that this
prevents using commands with prefix arguments."
  (interactive "p")
  (if (= x 1)
      (smex)
      (smex-major-mode-commands)))


;;;;
;;;; session management
;;;;

(desktop-save-mode 1)

(setq desktop-dirname "~/"
      history-length 250)

(mapc (lambda (global)
        (add-to-list 'desktop-globals-to-save global))
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
;;;; whitespace-mode
;;;;

(setq whitespace-style
      '(spaces tabs newline space-mark tab-mark newline-mark))

(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
        (newline-mark 10 [182 10])
        (tab-mark 9 [8677 9] [92 9])))


;;;;
;;;; git
;;;;

;; (require 'vc-git)
;; (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;; (require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;    "Minor mode for incremental blame for Git." t)

(add-to-list 'load-path "~/.emacs.d/egg/")

(require 'egg "egg" t)
(require 'gist "gist" t)

(when (eq system-type 'windows-nt)
  (setq egg-git-command "C:/Program Files/Git/bin/git.exe"
        egg-patch-command "C:/Program Files/Git/bin/patch.exe"))


;;;;
;;;; darcs
;;;;

(require 'vc-darcs "vc-darcs" t)


;;;;
;;;; browsing
;;;;


(setq browse-url-generic-program
      (case system-type
        (windows-nt "~/AppData/Local/Google/Chrome/Application/Chrome.exe")
        (gnu/linux "google-chrome"))
      common-lisp-hyperspec-root
      (expand-file-name "~/doc/HyperSpec/")
      cltl2-root-url
      (expand-file-name "~/doc/cltl2/"))

(setq browse-url-browser-function 'browse-url-generic)

(require 'cltl2 "cltl2" t)

(defun dhl-random-hyperspec ()
  (interactive)
  (let* ((random-hyperspec-symbol
          (let ((syms '()))
            (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
            (nth (random (length syms)) syms)))
         (random-page (let ((pages (symbol-value random-hyperspec-symbol)))
                        (nth (random (length pages)) pages))))
    (browse-url (concat common-lisp-hyperspec-root "Body/" random-page))))


(when (member system-type '(cygwin gnu/linux))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/"))

;; (when (require 'w3m "w3m" t)
;;   (setq browse-url-browser-function 'w3m-browse-url))

(setq w3m-session-load-last-sessions t)


;;;;
;;;; erc
;;;;

(add-to-list 'load-path "~/.emacs.d/erc-5.3-extras/")

(when (require 'erc "erc" t)
; (require 'erc-match "erc-match" t)
  (require 'erc-list-old "erc-list-old" t)
  (erc-spelling-mode -1)
  (erc-list-mode 1)
  (erc-timestamp-mode -1)
  (erc-smiley-mode 1)
  (erc-scrolltobottom-mode 1)
  (erc-truncate-mode 1))

(setq erc-keywords '()
;     erc-pals '()
;     erc-fools '()
      erc-current-nick-highlight-type 'nick-or-keyword
      erc-notice-highlight-type 'prefix
      erc-auto-query 'window-noselect
      erc-user-full-name "Daniel H. Leidisch"
      erc-track-exclude-server-buffer nil
      erc-fill-function 'erc-fill-static
      erc-fill-static-center 16
      erc-fill-column 90
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-max-buffer-size 50000
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

;(make-variable-buffer-local 'erc-hide-list)

(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

(setq erc-track-exclude-types '("NICK" "MODE" "324" "329" "332" "333" "353" "477"))

(setq erc-spelling-dictionaries '(("#bsdforen.de" "/dev/null"))) ; FIXME


(setq erc-part-reason (lambda (x)
                        (or x "Ein guter Abgang ziert die Übung."))
      erc-quit-reason erc-part-reason)

(defvar erc-auth
  '(;freenode (:name "<irc-nick>" :password "<password>")
    ))

(load "~/.emacs-auth" t)

(defun erc-nick (server)
  (getf (getf erc-auth server) :name))

(defun erc-password (server)
  (getf (getf erc-auth server) :password))

;; (add-hook 'erc-after-connect
;;           (lambda (SERVER NICK)
;;             (cond ((string-match "freenode\\.net" SERVER)
;;                    (erc-message "PRIVMSG" (concat "NickServ identify "
;;                                                   (erc-password 'freenode)))
;;                    (erc-message "PRIVMSG" (concat "NickServ ghost "
;;                                                   (erc-nick 'freenode)))
;;                    (erc-message "NICK" (erc-nick 'freenode))))))


;;;;
;;;; dired
;;;;

(setq dired-recursive-deletes 'top
      dired-recursive-copies 'top
      wdired-allow-to-change-permissions t
      wdired-allow-to-redirect-links t
      dired-listing-switches "-lah"
      dired-isearch-filenames 'dwim)

(setq dired-garbage-files-regexp
      "\\(?:\\.\\(?:aux\\|bak\\|dvi\\|log\\|orig\\|rej\\|toc\\|pyc\\)\\)\\'")

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
          (lambda ()
            (define-keys dired-mode-map
                '(("e" wdired-change-to-wdired-mode)))
            (when (eq system-type 'darwin)
              (define-key dired-mode-map (kbd "C-c o")
                'dired-open-mac))))

(when (eq system-type 'darwin)
  (defun dired-open-mac ()
    (interactive)
    (let ((file-name (dired-get-file-for-visit)))
      (if (file-exists-p file-name)
          (shell-command (concat "open '" file-name "'" nil))))))


;;;;
;;;; ibuffer
;;;;

(setq ibuffer-show-empty-filter-groups nil
      ibuffer-expert t)

(setq ibuffer-saved-filter-groups
      '(("default"
         ("elisp" (or (name . "\\.el$")
                      (mode . emacs-lisp-mode)))
         ("cl" (or (name . "\\.lisp$")
                   (name . "\\.asdf$")
                   (mode . lisp-mode)
                   (mode . slime-mode)))
         ("clojure" (or (name . "\\.clj$")
                        (mode . clojure-mode)))
         ("python" (or (name . "\\.py$")
                       (mode . python-mode)
                       (mode . python-2-mode)
                       (mode . python-3-mode)))
         ("haskell" (or (name . "\\.hs$")
                        (mode . haskell-mode)))
         ("f#" (or (name . "\\.fs$")
                   (mode . fsharp-mode)))
         ("qi" (or (name . "\\.qi$")
                   (mode . qi-mode)))
         ("java" (or (name . "\\.java$")
                     (mode . java-mode)))
         ("sql" (or (name . "\\.sql$")
                    (mode . sql-mode)))
         ("xml" (or (name . "\\.xml$")
                    (mode . nxml-mode)))
         ("html" (or (name . "\\.html$")
                     (mode . html-mode)))
         ("css" (or (name . "\\.css$")
                    (mode . css-mode)))
         ("rst" (or (name . "\\.rst$")
                     (mode . rst-mode)))
         ("ruby" (or (name . "\\.rb$")))
         ("C" (or (name . "\\.c$")
                  (name . "\\.h$")
                  (mode . c-mode)))
         ("C++" (or (name . "\\.cpp$")
                    (name . "\\.hpp$")
                    (mode . c++-mode)))
         ("assembler" (or (name . "\\.asm$")
                          (name . "\\.S$")
                          (mode . asm-mode)))
         ("perl" (mode . cperl-mode))
         ("prolog" (mode . prolog-mode))
         ("tex" (or (name . "\\.tex$")
                    (mode . tex-mode)))
         ("R" (name . "\\.R$"))
         ("org" (or (name . "\\.org$")
                    (mode . org-mode)))
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
         ("special" (name . "^\\*.*\\*")))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")
            (ibuffer-auto-mode 1)))

(defadvice ibuffer
    (around ibuffer-point-to-most-recent first () activate)
  "Open ibuffer with cursor pointed to most recent buffer name."
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))


;; (defun my-ibuffer-hook ()
;;   (ibuffer-define-sorter pathname
;;                          (:documentation
;;                           "Sort the buffers by their pathname."
;;                           :description "path")
;;                          (string-lessp (with-current-buffer (car a)
;;                                          (or buffer-file-name
;;                                              (if (eq major-mode 'dired-mode)
;;                                                  (expand-file-name dired-directory))
;;                                              "~"))
;;                                        (with-current-buffer (car b)
;;                                          (or buffer-file-name
;;                                              (if (eq major-mode 'dired-mode)
;;                                                  (expand-file-name dired-directory))
;;                                              "~"))))
;;   (define-key ibuffer-mode-map (kbd "s p") 'ibuffer-do-sort-by-pathname))

;; (add-hook 'ibuffer-mode-hooks 'my-ibuffer-hook)


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
  "Switch to occur window automatically."
  (other-window 1))

(defadvice multi-occur
    (after dhl-switch-to-multi-occur last () activate)
  "Switch to occur window automatically."
  (other-window 1))

(defadvice multi-occur-in-matching-buffers
    (after dhl-switch-to-multi-occur-in-matching-buffers last () activate)
  "Switch to occur window automatically."
  (other-window 1))

(defadvice ibuffer-do-occur
    (after dhl-ibuffer-switch-to-occur last () activate)
  "Switch to occur window automatically."
  (other-window 1))


;;;;
;;;; org-mode
;;;;

(setq load-path (append (list "~/.emacs.d/org-mode/lisp/"
                              "~/.emacs.d/org-mode/contrib/lisp/")
                        load-path))

(require 'org-install)
(require 'org-latex)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "<C-up>") 'org-shiftmetaup)
            (local-set-key (kbd "<C-down>") 'org-shiftmetadown)
            (local-set-key (kbd "<C-right>") 'org-shiftmetaright)
            (local-set-key (kbd "<C-left>") 'org-shiftmetaleft)
            (local-set-key (kbd "M-n") 'outline-next-visible-heading)
            (local-set-key (kbd "M-p") 'outline-previous-visible-heading)))

(setq org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-startup-indented nil
      org-startup-folded nil
      org-return-follows-link t
      org-use-extra-keys nil
      org-use-speed-commands t
      org-footnote-auto-adjust t
      org-export-with-LaTeX-fragments t
      org-export-html-validation-link nil
      org-export-creator-info nil
      org-export-html-style-include-default nil
      org-export-htmlize-output-type 'inline-css
      org-default-notes-file "~/notes/notes.org"
      remember-data-file "~/notes/notes.org")

(setq org-export-html-style "
<style type=\"text/css\">
    html {
      font-family: Verdana, Arial, sans-serif;
      font-size: 12pt;
      /*max-width: 360pt;*/
    }

    body {
      margin: 10%;
    }

    .title  { text-align: center; }

    .todo   { color: red; }
    .done   { color: green; }
    .tag    { background-color: #add8e6; font-weight:normal; }
    .target { }

    .timestamp { color: #bebebe; }
    .timestamp-kwd { color: #5f9ea0; }

    .right  { margin-left:auto; margin-right:0px;  text-align:right; }
    .left   { margin-left:0px;  margin-right:auto; text-align:left; }
    .center { margin-left:auto; margin-right:auto; text-align:center; }

    p.verse { margin-left: 3%; }

    pre {
      border: 1pt solid #AEBDCC;
      background-color: #F3F5F7;
      padding: 5pt;
      font-family: courier, monospace;
      font-size: 90%;
      overflow:auto;
    }

    table {
      border-collapse: collapse;
      margin-left: auto;
      margin-right: auto;
    }
    td, th { vertical-align: top; }
    th.right { text-align:center; }
    th.left { text-align:center; }
    th.center { text-align:center; }
    td.right { text-align:right; }
    td.left { text-align:left; }
    td.center { text-align:center; }
    dt { font-weight: bold; }

    div.figure { padding: 0.5em; }
    div.figure p { text-align: center; }

    div.inlinetask {
      padding:10px;
      border:2px solid gray;
      margin:10px;
      background: #ffffcc;
    }

    textarea { overflow-x: auto; }

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

(setq org-export-html-scripts "
<script src=\"jquery-1.6.4.js\"></script>
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
        jQuery(this).html(function(idx, oldHtml) {
          return oldHtml.replace(new RegExp(togSuffix+\"$\"), \"\");
        });
      },
      function() {
        jQuery(this).append(togSuffix);
      }
    );
    jQuery('.tog').click(function(e) {
      jQuery(this).next().toggle();
    });
  });
</script>")

(add-to-list 'org-export-latex-classes
  `("article-de"
    ,(concat "\\documentclass[a4paper,\n"
             "               headings=small,\n"
             "               captions=tableheading]\n"
             "              {scrartcl}\n"
             "[NO-DEFAULT-PACKAGES]\n"
             "[PACKAGES]\n"
             "[EXTRA]\n"
             "\\usepackage[ngerman]{babel}")
    ("\\section{%s}" . "\\section*{%s}")
    ("\\subsection{%s}" . "\\subsection*{%s}")
    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
    ("\\paragraph{%s}" . "\\paragraph*{%s}")
    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-export-latex-packages-alist
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
        ("" "hyperref" nil)
        "\\tolerance=1000"))

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


;;;;
;;;; abbrev
;;;;

(setq dabbrev-case-fold-search case-fold-search)


;;;;
;;;; flashcard
;;;;

(require 'flashcard)

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


;;;;
;;;; voctest
;;;;

(require 'voctest)

(setq voctest-test-direction '(1 . 0))

(add-hook 'voctest-mode-hook
          (lambda ()
            (variable-pitch-mode 1)))


;;;;
;;;; misc
;;;;

(require 'misc)

(add-to-list 'load-path "~/.emacs.d/gnugo/")

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function (lambda ())
      scroll-conservatively 1
      require-final-newline nil
      ispell-personal-dictionary "~/.ispell-emacs"
      ispell-dictionary "german"
      woman-use-own-frame nil
      sentence-end-double-space nil
      make-backup-files 1
      default-major-mode 'text-mode
      undo-limit 100000
      apropos-do-all 1
      line-move-visual nil
      help-window-select t
      enable-recursive-minibuffers t
      max-specpdl-size 10000
      max-lisp-eval-depth 5000
      comment-empty-lines nil
      user-full-name "Daniel H. Leidisch"
      user-mail-address "public@leidisch.net")

(setq-default cursor-type 'bar
              indent-tabs-mode nil
              tab-width 2)

;(setq standard-indent 2)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(toggle-scroll-bar -1)
(column-number-mode 1)
(line-number-mode 1)
(display-time-mode -1)
(show-paren-mode 1)
(transient-mark-mode -1)
(partial-completion-mode 1)
(blink-cursor-mode -1)
(auto-compression-mode 1)
(auto-image-file-mode 1)
(winner-mode 1)

(add-hook 'write-file-hooks #'time-stamp)

(fset 'yes-or-no-p 'y-or-n-p)

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

;; (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

(setq-default c-basic-offset 2
              css-indent-offset 2
              sh-basic-offset 2
              js-indent-level 2)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(setq tramp-syntax 'ftp)
(setq tramp-default-method "ftp")

(setq ange-ftp-try-passive-mode t)
(setq ange-ftp-ftp-program-name "ftp")

(when (eq system-type 'darwin)
  (cua-mode 0)
  (delete-selection-mode -1)
  (set-face-font 'default
                 "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-utf-8")
  (setq special-display-regexps
        (remove "[ ]?\\*[hH]elp.*" special-display-regexps))
  (setq special-display-regexps nil))

(setq sql-sqlite-program "sqlite3"
      sql-sqlite-options '("-interactive"))

;; (when (eq system-type 'gnu/linux)
;;   (setenv "PATH"
;;           (concat (getenv "PATH") ":"
;;                   "/home/danlei/.cabal/bin" ":"
;;                   "/home/danlei/bin")))


;;;;
;;;; misc functions
;;;;

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond ((not prefix) "%d.%m.%Y")
                      ((not prefix) "%d.%m.%y")
                      ((equal prefix '(4)) "%Y-%m-%d")
                      ((equal prefix '(16)) "%A, %d. %B %Y")))
        (system-time-locale "de_DE"))
    (insert (format-time-string format))))

(defun define-keys (mode-map keybindings)
  "Takes a mode map, and a list of (key function-designator)
lists. The functions are bound to the keys in the given mode-map.
Keys are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (define-key mode-map (read-kbd-macro key) function)))
        keybindings))

(defun global-set-keys (keybindings)
  "Takes a list of (key function-designator) lists.
The functions are globally bound to the keys. Keys
are in kbd format."
  (mapc (lambda (keybinding)
          (destructuring-bind (key function) keybinding
            (global-set-key (read-kbd-macro key) function)))
        keybindings))

(defun kill-all-rnc-input-buffers ()
  "Closes all Rnc Input buffers."
  (interactive)
  (dolist (b (buffer-list))
    (when (string-match "RNC Input" (buffer-name b))
      (kill-buffer b))))


;;;;
;;;; misc advice
;;;;

(defadvice split-window-vertically
    (after dhl-window-splitting-advice last () activate)
  "Open other-buffer in vertical split window."
  (set-window-buffer (next-window) (other-buffer)))

(defadvice python-describe-symbol
    (after dhl-python-describe-symbol-advice last () activate)
  "Switch to help buffer after invocation."
  (other-window 1))


;;;;
;;;; global keybindings
;;;;

(global-set-keys '(("C-c i d" insert-date)
                   ("C-x C-b" ibuffer)
                   ("M-/" hippie-expand)
                   ("C-c C-s" slime-selector)
                   ("C-x r v" view-register)
                   ("M-X" dhl-invoke-smex)
                   ("C-^" winner-undo)
                   ("C-c ^" winner-redo)
                   ("M-s m o" multi-occur)
                   ("M-s m m" multi-occur-in-matching-buffers)
                   ("M-z" zap-up-to-char)
                   ("C-c L" org-store-link)
                   ("C-c R" org-remember)
                   ("C-c C" org-capture)
                   ("C-x RET i" set-input-method)))


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
      gnus-summary-line-format "%[%U%R%] %-40,40s %10&user-date; %B %a\n")

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
                                    (setq cursor-type nil)
                                    (set-face-background 'hl-line "#ee3b3b")
                                    (set-face-foreground 'hl-line "#191970")))

;;;
;;; scoring/threading
;;;

(setq gnus-use-adaptive-scoring '(line)
      gnus-use-scoring t
      gnus-save-score t
      gnus-score-expiry-days nil
      gnus-home-score-file "~/.emacs.d/gnus.score"
      gnus-home-adapt-file "~/.emacs.d/gnus-adapt.score"
      gnus-summary-default-score 0
      gnus-score-thread-simplify t)

(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 10))
        (gnus-dormant-mark (from 5))
        (gnus-del-mark (from -2) (subject -4))
        (gnus-read-mark (from 2) (subject 5))
        (gnus-expirable-mark (from -3) (subject -5))
        (gnus-killed-mark (from -1) (subject -4))
        (gnus-kill-file-mark)
        (gnus-ancient-mark)
        (gnus-low-score-mark)
        (gnus-catchup-mark (subject -1))))

(setq gnus-thread-sort-functions
      '((not gnus-thread-sort-by-number)))

(setq gnus-article-sort-functions
      '(gnus-article-sort-by-number))

(setq gnus-summary-expunge-below -500)


;;;
;;; misc
;;;

(setq gnus-add-to-list t
      gnus-summary-goto-unread nil
      gnus-summary-make-false-root 'adopt
      gnus-article-save-directory "~/.news"
      gnus-cache-directory "~/.news/cache"
      gnus-cache-active-file "~/.news/cache/active"
      gnus-kill-files-directory "~/.news"
      nndraft-directory "~/.gnus/drafts/"
      gnus-default-article-saver 'gnus-summary-save-in-file
      gnus-show-all-headers nil
      gnus-fetch-old-headers nil
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
      gnus-cache-enter-articles '(ticked)
      gnus-article-wash-function (if (featurep 'w3m) 'w3m 'html2text)
      mm-text-html-renderer (if (featurep 'w3m) 'w3m 'html2text)
      mm-discouraged-alternatives '("text/html" "text/richtext")
      gnus-permanently-visible-groups "Alle Nachrichten")


;;;
;;; posting styles
;;;

(setq gnus-posting-styles
      '((message-news-p (address "news@leidisch.net"))))

;;;
;;; gmail, gmane
;;;

(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

(require 'nnir)

(setq gnus-secondary-select-methods
      '((nnimap "imap.gmail.com"
         (nnimap-address "imap.gmail.com")
         (nnimap-server-port 993)
         (nnimap-stream ssl)
;        (nnimap-authenticator login)      ;; when used without ~/.authinfo
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

;; (add-hook 'message-setup-hook '(lambda() (use-hard-newlines t t))) ;; format=flowed


;; (setq message-alternative-emails
;;       (regexp-opt '("public@leidisch.net"
;;                     "foo@bar.baz")))

(load (expand-file-name "~/.message-alternative-emails") t)

(setq mm-coding-system-priorities '(utf-8)
      mm-fill-flowed t)



(gnus-compile)



;;;;
;;;; epilogue
;;;;


(server-start)


;;;;
;;;; customize
;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-default-program "mzscheme")
 '(quack-fontify-style nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "gray4" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "outline" :family "DejaVu Sans Mono"))))
 '(buffer-menu-buffer ((t (:foreground "indian red"))))
 '(erc-underline-face ((t nil)))
 '(gnus-header-subject ((t (:foreground "lemon chiffon"))))
 '(ido-only-match ((((class color)) (:foreground "turquoise"))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "cornflower blue"))))
 '(message-header-subject ((t (:foreground "lemon chiffon"))))
 '(rst-level-1-face ((default nil) (nil (:background "black" :foreground "gray"))) t)
 '(underline ((t nil))))

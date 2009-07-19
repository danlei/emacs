;;;;; -*- emacs-lisp -*-
;;;;;
;;;;; Emacs Configuration File (.emacs)
;;;;;
;;;;; Time-stamp: <2009-07-19 10:40:32 danlei>
;;;;;


(require 'cl)

(mapc (lambda (path) (add-to-list 'load-path path))
      '("~/.emacs.d/"
	"~/.emacs.d/erc-5.3-extras/"
	"~/.emacs.d/slime/"
	"~/.emacs.d/slime/contrib/"
	"~/.emacs.d/clojure-mode/"
	"~/.emacs.d/swank-clojure/"
	"~/.emacs.d/color-theme-6.6.0/"
	"~/.emacs.d/smex/"
	))


;;;;
;;;; color-theme
;;;;

(require 'color-theme)
(require 'color-theme-dhl-hober)
(color-theme-initialize)
(color-theme-dhl-hober)


;;;;
;;;; slime
;;;;

(require 'slime)

(slime-setup '(slime-fancy slime-asdf slime-references slime-indentation))

(setq slime-enable-evaluate-in-emacs t
      slime-net-coding-system 'utf-8-unix
      lisp-indent-function 'cl-indent:function
      )

(setq slime-lisp-implementations
      '(
;;	(sbcl ("/usr/bin/sbcl"))
;;	(ccl ("/home/danlei/build/clozure/ccl/lx86cl64"))
;;	(cmucl ("/home/danlei/build/bin/lisp"))
	(clisp ("/usr/bin/clisp"))
;;	(allegro ("/home/danlei/build/acl81_express/alisp"))
;;	(lispworks ("/home/danlei/build/lispworks-personal-5-1-1-x86-linux"))
;; 	(abcl ("/home/danlei/build/abcl/j/abcl"))
	))


(add-hook 'slime-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (define-keys slime-mode-map
		'(("C-c s" slime-selector)
		  ("C-j" newline-and-indent)
		  ("TAB" slime-indent-and-complete-symbol)
		  ("C-c C-d c" cltl2-lookup)
		  ))))

(add-hook 'slime-repl-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (define-keys slime-repl-mode-map
		'(("C-c s" slime-selector)
		  ("C-c C-d c" cltl2-lookup)
		  ))))

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

(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(add-hook 'paredit-mode-hook
	  (lambda ()
	    (define-keys paredit-mode-map
		'((")" paredit-close-parenthesis-and-newline)
		  ("M-)" paredit-close-brace)
		  ("}" paredit-close-parenthesis)
		  ("M-{" paredit-brace-wrap-sexp)
		  ("{" paredit-open-brace)
		  ("M-f" paredit-forward)
		  ("C-M-f" forward-word)
		  ("M-b" paredit-backward)
		  ("C-M-b" forward-word)
		  ("M-u" backward-up-list)
		  ("C-M-u" upcase-word)
		  ("C-ö" paredit-backward-slurp-sexp)
		  ("M-ä" paredit-forward-barf-sexp)
		  ("M-ö" paredit-backward-barf-sexp)
		  ("M-ü" down-list)
		  ("M-t" transpose-sexps)
		  ("C-M-t" transpose-words)
		  ("<M-backspace>" paredit-backward-kill-word)
		  ))))


;;;;
;;;; clojure
;;;;

(when (eq system-type 'cygwin)
  (setq swank-clojure-jar-path
	"e:/cygwin/home/danlei/build/clojure/trunk/clojure.jar"
	swank-clojure-extra-classpaths
	'("e:/cygwin/home/danlei/coding/lisp/clojure/")))

(require 'swank-clojure-autoload)
(require 'swank-clojure)
(require 'clojure-mode)

(setq clojure-enable-paredit t)

;;; bill clementson
(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

;;; bill clementson
(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

(add-hook 'slime-connected-hook
	  (lambda ()
	    (interactive)
	    (require 'clojure-mode)
	    (define-keys slime-mode-map
		'(("C-c d" slime-java-describe)
		  ("C-c d" slime-java-describe)
		  ("C-c D" slime-javadoc)
		  ("C-c D" slime-javadoc)
		  ))))


;;;;
;;;; qi
;;;;

;(require 'qi-mode)


;;;;
;;;; scheme
;;;;

(setq scheme-program-name "guile")

(add-hook
 'inferior-scheme-mode-hook
 (lambda ()
   (define-keys inferior-scheme-mode-map
       '(("M-TAB" hippie-expand)
	 ("M-TAB" 'hippie-expand)
	 ))))

;(require 'quack)


;;;;
;;;; newlisp
;;;;

;; (add-to-list 'load-path
;; 	     (expand-file-name "/usr/share/emacs/site-lisp/newlisp-mode"))
;; (add-to-list 'auto-mode-alist '("\\.lsp\\'" . newlisp-mode))
;; (autoload 'newlisp-mode "newlisp" t)


;;;;
;;;; tcl
;;;;

(require 'tcl)

(when (eq system-type 'cygwin)
  (setq tcl-application "/cygdrive/e/Tcl/bin/tclsh85.exe"))

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

;; (add-to-list 'load-path "/usr/share/maxima/5.17.1/emacs/")

;; (setq imaxima-use-maxima-mode-flag t)
;; (load "/usr/share/maxima/5.17.1/emacs/maxima-font-lock.el")
;; (load "/usr/share/maxima/5.17.1/emacs/maxima.el")
;; (load "/usr/share/maxima/5.17.1/emacs/imaxima.el")
;; (load "/usr/share/maxima/5.17.1/emacs/emaxima.el")
;; (setq imaxima-fnt-size "Large")

;; (add-to-list 'auto-mode-alist '("\\.max" . maxima-mode))


;;;;
;;;; haskell-mode
;;;;

(add-to-list 'load-path "~/.emacs.d/haskell-mode/")

(require 'inf-haskell)
(require 'haskell-indent)

(when (eq system-type 'cygwin)
  (setq haskell-program-name "/cygdrive/e/ghc/ghc-6.4.2/bin/ghci.exe"))

(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook
 'haskell-mode-hook
 (lambda ()
   (define-key haskell-mode-map "\r" 'newline)
   (define-key haskell-mode-map "\t" 'haskell-indent-cycle)
   (define-key haskell-mode-map "\C-c=" 'haskell-indent-insert-equal)
   (define-key haskell-mode-map "\C-c|" 'haskell-indent-insert-guard)
   (define-key haskell-mode-map "\C-co" 'haskell-indent-insert-otherwise)
   (define-key haskell-mode-map "\C-cw" 'haskell-indent-insert-where)
   (define-key haskell-mode-map "\C-c." 'haskell-indent-align-guards-and-rhs)))


;;;;
;;;; prolog
;;;;

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(require 'prolog)

(setq prolog-system 'swi)

(if (eq system-type 'cygwin)
    (setq prolog-program-name "/usr/bin/pl"))

(add-to-list 'auto-mode-alist '("\.pl$" . prolog-mode))


;;;;
;;;; auctex
;;;;

;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default Tex-master nil)

;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)


;;;;
;;;; browsing
;;;;

;; (require 'w3m)
;; (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m/")
;; (setq browse-url-browser-function 'w3m-browse-url)

;; (add-to-list 'exec-path "/cygdrive/e/Programme/Mozilla Firefox/")
;; (setq browse-url-firefox-program "/cygdrive/e/Programme/Mozilla Firefox/firefox.exe")
;; (setq browse-url-browser-function 'browse-url-firefox)

(when (eq system-type 'cygwin)
  (setq browse-url-generic-program
	"/cygdrive/e/Programme/Mozilla Firefox/firefox.exe"
	common-lisp-hyperspec-root
	"file:///e:/cygwin/home/danlei/doc/HyperSpec/"
	cltl2-root-url
	"file:///e:/cygwin/home/danlei/doc/cltl2/"))

(setq browse-url-browser-function 'browse-url-generic)

(require 'cltl2)

(defun random-hyperspec ()
  (interactive)
  (let* ((random-hyperspec-symbol
	  (let ((syms '()))
	    (do-symbols (sym common-lisp-hyperspec-symbols) (push sym syms))
	    (nth (random (length syms)) syms)))
	 (random-page (let ((pages (symbol-value random-hyperspec-symbol)))
			(nth (random (length pages)) pages))))
    (browse-url (concat common-lisp-hyperspec-root "Body/" random-page))))


;;;;
;;;; erc
;;;;

(require 'erc)
(require 'erc-match)
(require 'erc-list-old)

(erc-scrolltobottom-mode 1)

(setq erc-keywords '()
      erc-pals '()
      erc-fools '()
      erc-current-nick-highlight-type 'nick-or-keyword
      erc-notice-highlight-type 'prefix
      erc-auto-query 'nil
      erc-fill-function 'erc-fill-static
      erc-user-full-name "Daniel H. Leidisch"
      erc-track-exclude-server-buffer t
      erc-fill-static-center 16
      erc-fill-column 100
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      )

(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

(erc-spelling-mode 1)
(setq erc-spelling-dictionaries '(("#bsdforen.de" "/dev/null"))) ; FIXME

(add-to-list 'load-path "~/.emacs.d/erc-5.3-extras/")
(erc-list-mode 1)
(erc-timestamp-mode -1)
(erc-smiley-mode 1)

(setq erc-part-reason (lambda (x)
                        (or x "Ein guter Abgang ziert die Übung."))
      erc-quit-reason erc-part-reason)

;;; ~/.emacs-auth now takes care of this
;; (add-hook 'erc-after-connect
;; 	  (lambda (SERVER NICK)
;; 	    (cond ((string-match "freenode\\.net" SERVER)
;; 		   (erc-message "PRIVMSG" "NickServ identify <password>")))))


;;;;
;;;; dired
;;;;

(setq dired-recursive-deletes 'top)
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook
          (lambda ()
	    (define-keys dired-mode-map
		'(("e" wdired-change-to-wdired-mode)
		  ))
	    (when (eq system-type 'darwin)
	      (define-key dired-mode-map (kbd "C-c o")
		'dired-open-mac))))

(when (eq system-type 'darwin)
  (defun dired-open-mac ()
    (interactive)
    (let ((file-name (dired-get-file-for-visit)))
      (if (file-exists-p file-name)
	  (shell-command (concat "open '" file-name "'" nil ))))))


;;;;
;;;; elisp
;;;;

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (eldoc-mode 1)
	    (paredit-mode 1)
	    ))


;;;;
;;;; ielm
;;;;

(setq ielm-prompt "elisp> ")

(add-hook 'ielm-mode-hook
	  (lambda ()
	    (paredit-mode 1)
	    (eldoc-mode 1)
	    (setq comint-dynamic-complete-functions
		  '(ielm-tab
		    comint-replace-by-expanded-history
		    ielm-complete-filename
		    ielm-complete-symbol
		    PC-lisp-complete-symbol
		    ))))

;;;;
;;;; eshell
;;;;

(add-hook 'eshell-mode-hook
          (lambda ()
            (define-keys eshell-mode-map
                '(("C-a" eshell-maybe-bol)
                  ))))

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
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-whole-kill
;	ispell-complete-word
	))


;;;;
;;;; ido
;;;;

(require 'ido)
(require 'smex)

(ido-mode t)

(smex-initialize)
(setq smex-save-file "~/.smex")
(smex-auto-update)

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

(setq desktop-modes-not-to-save
      '(
	))

(mapc (lambda (variable)
	(add-to-list 'desktop-globals-to-save variable))
      '(
	))

(add-hook 'auto-save-hook
	  (lambda () (desktop-save-in-desktop-dir)))

(savehist-mode 1)
(setq savehist-additional-variables
      '(search-ring
	regexp-search-ring
	global-mark-ring
	mark-ring
	kmacro-ring
	kill-ring
	))

(require 'saveplace)
(setq-default save-place t)


;;;;
;;;; misc
;;;;

(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function (lambda ())
      scroll-conservatively 1
      require-final-newline t
      ispell-personal-dictionary "~/.ispell-emacs"
      ispell-dictionary "american"
      woman-use-own-frame nil
      sentence-end-double-space nil
      make-backup-files 1
      default-major-mode 'text-mode
      undo-limit 100000
      apropos-do-all 1
      line-move-visual nil
      )

(setq-default cursor-type 'bar
	      indent-tabs-mode nil)

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

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'overwrite-mode 'disabled nil)

;; (setq time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S")

;;; lpc:
;; (modify-syntax-entry ?' "'" c-mode-syntax-table)

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

(setq tramp-syntax 'url)
;(setq tramp-default-method "ftp")

;;; git
(require 'vc-git)
(when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;; (require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;    "Minor mode for incremental blame for Git." t)

(when (eq system-type 'darwin)
  (cua-mode 0)
  (delete-selection-mode -1)
  (set-face-font 'default
		 "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-utf-8")
  (setq special-display-regexps
	(remove "[ ]?\\*[hH]elp.*" special-display-regexps))
  (setq special-display-regexps nil))


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

(defun mark-line (&optional arg)
  "Marks a line from start of indentation to end"
  (interactive "p")
  (back-to-indentation)
  (end-of-line-mark arg))

(defun define-keys (mode-map keybindings)
  "Takes a mode map, and a list of (key function-designator)
lists. The functions are bound to the keys in the given mode-map.
Keys are in kbd format."
  (mapc (lambda (keybinding)
	  (let ((key (car keybinding))
		(function (cadr keybinding)))
	    (define-key mode-map (read-kbd-macro key) function)))
	keybindings))

(defun global-set-keys (keybindings)
  "Takes a list of (key function-designator) lists.
The functions are globally bound to the keys. Keys
are in kbd format."
  (mapc (lambda (keybinding)
	  (let ((key (car keybinding))
		(function (cadr keybinding)))
	    (global-set-key (read-kbd-macro key) function)))
	keybindings))


;;;;
;;;; advice
;;;;

(defadvice split-window-vertically
    (after my-window-splitting-advice first () activate)
  (set-window-buffer (next-window) (other-buffer)))


;;;;
;;;; global keybindings
;;;;

(global-set-keys '(("C-c i d" insert-date)
		   ("C-c l" mark-line)
		   ("C-x C-b" buffer-menu)
		   ("M-/" hippie-expand)
		   ("C-c s" slime-selector)
		   ("C-x r v" view-register)
		   ("M-X" dhl-invoke-smex)
                   ("C-^" winner-undo)
                   ("C-°" winner-redo)
		   ))


;;;;
;;;; gnus
;;;;

(setq gnus-select-method '(nntp "news.t-online.de"))

(setq gnus-group-line-format "%2{%M%S%p%} %0{%5y%} %P%1{%G%}\n"
      gnus-topic-line-format "%i%3{[ %n -- %A ]%}%v\n"
      gnus-summary-line-format "%U%R%z %3{%u%}: %1{%B%-23,23n%} %s\n"
      gnus-summary-line-format "%[%U%R%] %30a %[%6d%] %B %s\n")

(setq
 gnus-sum-thread-tree-single-indent   "◎ "
 gnus-sum-thread-tree-false-root      " ◯ "
 gnus-sum-thread-tree-root            "● "
 gnus-sum-thread-tree-vertical        "│"
 gnus-sum-thread-tree-leaf-with-other "├─► "
 gnus-sum-thread-tree-single-leaf     "╰─► "
 gnus-sum-thread-tree-indent          " ")

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-visible-headers
      '("^From:" "^Subject:" "^To:" "^Cc:" "^Resent-To:" "^Message-ID:"
        "^Date:" "^Newsgroups:" "^Organization:" "Followup-To:"
        "Reply-To:" "^X-Newsreader:" "^X-Mailer:"
        "^X-Spam-Level:"))

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
(setq gnus-use-adaptive-scoring '(line))
(setq gnus-default-adaptive-score-alist
      '((gnus-unread-mark)
        (gnus-ticked-mark (from 5))
        (gnus-dormant-mark (from 7))
        (gnus-del-mark (from -2) (subject -5))
        (gnus-read-mark (from 2) (subject 5))
        (gnus-expirable-mark (from -3) (subject -5))
        (gnus-killed-mark (from -2) (subject -5))
        (gnus-kill-file-mark)
        (gnus-ancient-mark (subject -10))
        (gnus-low-score-mark)
        (gnus-catchup-mark (subject -10))))
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-subject
        (not gnus-thread-sort-by-date)
        gnus-thread-sort-by-total-score))

;;;
;;; misc
;;;
(setq gnus-add-to-list t
      gnus-use-scoring t
      gnus-summary-default-score 0
      gnus-summary-make-false-roots 'dummy
      gnus-score-expiry-days nil
      gnus-home-score-file "~/.emacs.d/gnus-score"
      gnus-article-save-directory "~/.news"
      gnus-cache-directory "~/.news/cache"
      gnus-cache-active-file "~/.news/cache/active"
      gnus-kill-files-direcotry "~/.news"
      nndraft-directory "~/.gnus/drafts/"
      gnus-default-article-saver 'gnus-summary-save-in-file
      gnus-show-all-headers nil
      gnus-treat-capitalize-sentences nil
      gnus-treat-display-smileys t
      gnus-treat-display-x-face t
      gnus-treat-emphasize t
      gnus-treat-fill-long-lines nil
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
      gnus-cache-enter-articles '(ticked)
      )

;;;
;;; gmail, gmane
;;;
(setq gnus-secondary-select-methods
      '((nnimap "imap.gmail.com"
	 (nnimap-stream ssl)
	 (nnimap-authenticator login))
	(nntp "news.gmane.org"
	 (nntp-address "news.gmane.org")
	 (nntp-port-number 119))))

;; (setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       send-mail-function 'smtpmail-send-it
;;       message-send-mail-function 'smtpmail-send-it
;;       smtpmail-smtp-service 587
;;       smtpmail-auth-credentials "~/.authinfo")


;;;;
;;;; epilogue
;;;;

(load "~/.emacs-auth")

;; (server-start)


;;;;
;;;; customize
;;;;

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(quack-browse-url-browser-function (quote w3m-browse-url))
 '(quack-pretty-lambda-p t)
 '(safe-local-variable-values (quote ((Package . ccl) (Package . CL-FAD) (Syntax . COMMON-LISP) (Package . CCL) (Base . 10) (Package . LISP-UNIT) (syntax . ANSI-COMMON-LISP) (Package SERIES :use "COMMON-LISP" :colon-mode :external)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(buffer-menu-buffer ((t (:foreground "indian red"))))
 '(erc-underline-face ((t nil)))
 '(ido-only-match ((((class color)) (:foreground "turquoise"))))
 '(ido-subdir ((((min-colors 88) (class color)) (:foreground "cornflower blue"))))
 '(underline ((t nil))))

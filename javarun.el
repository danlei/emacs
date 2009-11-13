;;;;;
;;;;; javarun.el
;;;;;
;;;;; Time-stamp: <2009-11-13 21:03:05 danlei>
;;;;;
;;;;; License: LLGPL
;;;;;


(defvar *javarun-java-path* ""
  "Holds the path where java and javac can be found.")

(defvar *javarun-cygdir* ""
  "Holds the cygwin root directory path.")

(defvar *javarun-old-window-configuration* nil
  "Holds the window configuration as it was before a javarun popup.")


(defun javarun-popup-buffer (buffer)
  "Splits window vertically and popups buffer in a new window.
The window can be closed (and the buffer buried} by typing \"q\".
\"Q\" will close the popup window and kill the buffer."
  (setq *javarun-old-window-configuration*
        (current-window-configuration))
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer buffer)
  (local-set-key (kbd "q")
                 (lambda ()
                   (interactive)
                   (bury-buffer)
                   (set-window-configuration
                    *javarun-old-window-configuration*)))
  (local-set-key (kbd "Q")
                 (lambda ()
                   (interactive)
                   (kill-buffer)
                   (set-window-configuration
                    *javarun-old-window-configuration*))))

(defun javarun-read-args ()
  "Tries to read command line args shell-like."
  (mapcar (lambda (x)
            (if (stringp x) x (prin1-to-string x)))
          (car (read-from-string
                (concat "(" (read-input "Command line arguments: ") ")")))))

(defun javarun (argsp)
  "Compiles and (if successful) runs a Java file.
If a positive prefix arg is given, reads a string
of command-line args interactively. Compile errors
or the program's output are shown in a popup window."
  (interactive "p")
  (if (/= 0 (call-process (concat (file-name-as-directory *javarun-java-path*)
                                  "javac")
                          nil "*javac-output*" t
                          (if (eq system-type 'cygwin)
                              (concat (file-name-as-directory *javarun-cygdir*)
                                      (substring (buffer-file-name) 1))
                              (buffer-file-name))))
      (javarun-popup-buffer "*javac-output*")
      (progn
        (apply 'call-process
               "java" nil "*java-output*" t
               (file-name-nondirectory
                (file-name-sans-extension (buffer-file-name)))
               (when (/= argsp 1) (javarun-read-args))))
      (javarun-popup-buffer "*java-output*")))

(provide 'javarun)

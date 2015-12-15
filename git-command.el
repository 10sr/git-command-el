;;; git-command.el --- Dead simple git command interface

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-command-el
;; Version: 0.1
;; Package-Requires: ((term-run "20150601.6") (with-editor "20151126.323") (ansi-color "0") (shell-split-string "20150202.2036"))
;; Keywords: utility git

;; This file is not part of GNU Emacs.

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org/>

;;; Commentary:

;; Dead simple git interface.  No major-mode, only provides command-line like
;; interface using minibuffer.  You need not remember additional keybinds for
;; using git from Emacs.

;;; Code:

(eval-and-compile
  (require 'term)
  (require 'term-run)
  (require 'ansi-color)
  (require 'with-editor)
  (require 'shell-split-string)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables

;; TODO: remove?
(defvar git-command-default-options
  nil
  "List of options always passed to git.")

;; TODO: use defcustom properly
(defvar git-command-default-command
  "git -c color.ui=always "
  "Default value for `git-command' interactive execution.")


;; variables for __git_ps1
;; TODO: use getenv
(defvar git-command-ps1-showdirtystate "t"
  "Value of  GIT_PS1_SHOWDIRTYSTATE when running __git_ps1.")

(defvar git-command-ps1-showstashstate ""
  "Value of GIT_PS1_SHOWSTASHSTATE when running __git_ps1.")

(defvar git-command-ps1-showuntrackedfiles ""
  "Value of GIT_PS1_SHOWUNTRACKEDFILES when running __git_ps1.")

(defvar git-command-ps1-showupstream "auto"
  "Value of GIT_PS1_SHOWUPSTREAM when running __git_ps1.")


(defvar git-command-history nil
  "History list for `git-command'.")

(defvar git-comand--pager-output-timer nil
  "Timer object to open buffer to show pager output.")


(defvar git-command-view-command-list
  '("log" "show" "help")
  "List of commands that will only output something for read.")

;; TODO: maybe I do not need this by GIT_PAGER=cat >a.txt
(defvar git-command-output-handler-alist
  '(
    ("diff" . (lambda (output new-buffer-p)
                (let ((buf (if new-buffer-p
                               (generate-new-buffer "*git diff*")
                             (get-buffer-create "*git diff*")))
                      (dir default-directory))
                  (with-current-buffer buf
                    (cd dir)
                    (erase-buffer)
                    (insert (substring-no-properties output))
                    (diff-mode))
                  (display-buffer buf)
                  buf)))
    )
  "Alist of git subcommand its output handler.

Each element should be like (COMMAND . FUNCTION).
COMMAND should be a string of git subcommand like \"diff\", and FUNCTION should
be a function that will be called with two argument: the first one is a output
string of the git command with text properties, and second is NEW-BUFFER-P
flag.  This function should return the buffer which is displaying the output.")

;; TODO: remove
(defvar git-command-aliases-alist
  '(("diff" . (lambda (options cmd args new-buffer-p)
                (let ((buf (if new-buffer-p
                               (generate-new-buffer "*git diff*")
                             (get-buffer-create "*git diff*")))
                      (dir default-directory))
                  (with-current-buffer buf
                    (cd dir)
                    (erase-buffer)
                    (shell-command (concat "git "
                                           (git-command-construct-commandline
                                            `(,@options "-c" "color.diff=never")
                                            "diff"
                                            args))
                                   t)
                    (diff-mode))
                  (display-buffer buf))))
    ("grep" . (lambda (options cmd args new-buffer-p)
                (compilation-start (concat "git "
                                           (git-command-construct-commandline
                                            `(,@options "--no-pager"
                                                        "-c" "color.grep=false")
                                            cmd
                                            `("-nHe" ,@args)))
                                   'grep-mode
                                   (and new-buffer-p
                                        (lambda (s)
                                          (generate-new-buffer-name "*git grep*"))))
                )))
  "Alist of cons of command and function to run.
The function should get three argument: see `git-command-exec'.")

(defun git-command-find-git-ps1 (f)
  "Return F if F exists and it contain function \"__git_ps1\"."
  (and (file-readable-p f)
       (with-temp-buffer
         (insert ". " f "; "
                 "__git_ps1 %s;")
         (eq 0 (shell-command-on-region (point-min)
                                        (point-max)
                                        "bash -s"
                                        nil
                                        t)))
       f))

(defvar git-command-prompt-file
  (or (git-command-find-git-ps1 "/usr/share/git/completion/git-prompt.sh")
      (git-command-find-git-ps1
       "/opt/local/share/doc/git-core/contrib/completion/git-prompt.sh")
      (git-command-find-git-ps1 "/etc/bash_completion.d/git")
      (git-command-find-git-ps1 "/etc/bash_completion.d/git-prompt")
      (git-command-find-git-ps1
       "/opt/local/share/git-core/git-prompt.sh")
      (git-command-find-git-ps1 "/opt/local/etc/bash_completion.d/git")
      ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utility

(defun git-command--git-dir ()
  "Execute \"git rev-parse --git-dir\" and return result string or nil."
  (with-temp-buffer
    (and (eq 0
             (call-process "git"
                           nil
                           t
                           nil
                           "rev-parse" "--git-dir"))
         (progn (goto-char (point-min))
                (buffer-substring-no-properties (point-at-bol)
                                                (point-at-eol))))))

(defun git-command-ps1 (fmt)
  "Generate git ps1 string from FMT and return that string."
  (let ((gcmpl (or git-command-prompt-file))
        (process-environment `(,(concat "GIT_PS1_SHOWDIRTYSTATE="
                                        git-command-ps1-showdirtystate)
                               ,(concat "GIT_PS1_SHOWSTASHSTATE="
                                        git-command-ps1-showstashstate)
                               ,(concat "GIT_PS1_SHOWUNTRACKEDFILES="
                                        git-command-ps1-showuntrackedfiles)
                               ,(concat "GIT_PS1_SHOWUPSTREAM="
                                        git-command-ps1-showupstream)
                               ,@process-environment)))
    (if (and (executable-find "bash")
             gcmpl
             (file-readable-p gcmpl))
        (with-temp-buffer
          (insert ". " gcmpl
                  "; __git_ps1 "
                  (shell-quote-argument fmt)
                  ";")
          (shell-command-on-region (point-min)
                                   (point-max)
                                   "bash -s"
                                   nil
                                   t)
          (buffer-substring-no-properties (point-min)
                                          (point-max)))
      "")))

(defun git-command-get-output-handler (cmd)
  "Return alias function for CMD if available in `git-command-alias-alist'.
CMD should be a string of git subcommand."
  (cdr (assoc cmd
              git-command-output-handler-alist)))


(defvar git-command--with-git-pager-executable
  (expand-file-name (concat user-emacs-directory "git-command/pager.sh"))
  "File path to executable for `git-command-with-git-pager'.")

;; Remove this file on reloading this library in case of updating.
(when (file-readable-p git-command--with-git-pager-executable)
  (delete-file git-command--with-git-pager-executable))


(defconst git-command--with-git-pager-executable-content
  "#!/bin/sh

tmp=`mktemp --tmpdir tmp.XXXXXX`
cat >\"$tmp\"
sh -s <<__EOF__
$GIT_EDITOR \
  --eval \"(display-buffer (with-current-buffer (generate-new-buffer \\\"\*git pager\*\\\") (insert-file-contents \\\"$tmp\\\") (require 'ansi-color) (ansi-color-apply-on-region (point-min) (point-max)) (current-buffer)))\"
__EOF__
rm -f \"$tmp\"
"
  "Script content for `git-command--with-git-pager-executable'.")


(defmacro git-command-with-git-editor-git-pager (&rest body)
  "Evaluate BODY with $GIT_EDITOR and $GIT_PAGER are set."
  (declare (indent defun) (debug (body)))
  `(with-editor "GIT_PAGER"
     (let ((process-environment (cons (format "GIT_PAGER=%s"
                                              git-command--with-git-pager-executable)
                                      process-environment)))
       (unless (file-readable-p git-command--with-git-pager-executable)
         ;; Create executable for GIT_PAGER
         (make-directory (file-name-directory git-command--with-git-pager-executable)
                         t)
         (with-temp-buffer
           (insert git-command--with-git-pager-executable-content)
           (write-region (point-min)
                         (point-max)
                         git-command--with-git-pager-executable))
         (set-file-modes git-command--with-git-pager-executable
                         #o755))

       ,@body)))


(defun git-command--find-subcommand (cmd)
  "Find git subcommand from CMD.")

(defun git-command--find-subcommand-index (l &optional index)
  "Find subcommand index from list L and return the index number.
Options that lead subcommand are distinguished by if they start with hyphens.
\"-c\" option is a special case which take one parameter.
If no subcommand was found, returns the length of L.

INDEX is always the original index of car of L.
The value nil is equivalent to 0."
  (let ((options-w-param '("-c"))
        (first (car l))
        (rest (cdr l))
        (i (or index
               0)))
    (if l

        ;; l is not empty
        (if (member first
                    options-w-param)
            ;; if first is the command that requires a parameter, skip the parameter
            (if rest
                (git-command-find-subcommand (cdr rest)
                                             (+ 2 i))
              ;; Only -c is given: this is undesirable situation, but anyway returns the length of original L
              (1+ i)
              )

          (if (eq ?-
                  (aref first
                        0))
              ;; the first element is not in optinos-w-params but
              ;; the first letter of first element of L is '-'
              (git-command-find-subcommand rest
                                           (1+ i))
            i))

      ;; if L is empty returns the original length of L
      i)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user commands

(defun git-command (cmd &optional new-buffer-p)
  "Shell like git command interface.

CMD is the commandline string to run.
If NEW-BUFFER-P is non-nil, generate new buffer for running command."
  (interactive (list (read-shell-command (format "[%s]%s $ git : "
                                                 (abbreviate-file-name
                                                  default-directory)
                                                 (git-command-ps1 "[GIT:%s]"))
                                         git-command-default-command
                                         'git-command-history)
                     current-prefix-arg))
  (let* ((pager-output-file (make-temp-file "git-command-pager-output"))
         (process-environment `(,(format "GIT_PAGER=cat >\"%s\""
                                         pager-output-file)
                                ,@process-environment))
         (process-buffer (if new-buffer-p
                             (generate-new-buffer "*git command*")
                           "*git command*")))
    (git-command-with-git-editor-git-pager
      (term-run shell-file-name
                process-buffer
                shell-command-switch
                cmd))
    process-buffer))

(defun -git-command (cmd &optional new-buffer-p)
  "Shell like git command interface.  CMD is the commandline strings to run.

If NEW-BUFFER-P is non-nil, generate new buffer for running command."
  (interactive (list (read-shell-command (format "[%s]%s $ git : "
                                                 (abbreviate-file-name
                                                  default-directory)
                                                 (git-command-ps1 "[GIT:%s]"))
                                         nil
                                         'git-command-history)
                     current-prefix-arg))
  (apply 'git-command-exec (append (git-command-parse-commandline cmd)
                                   (list new-buffer-p))))

(defun -git-command-exec (options command args &optional new-buffer-p)
  "Execute git.

This function accept three arguments.  OPTIONS is a list of options that will be
passed to git itself.  Tipically they are appeared before the git subcommand.
COMMAND is a string of git subcommand.  ARGS is a list of arguments for git
subcommand.

These arguments are tipically constructed with `git-command-parse-commandline'.

Set optional argument NEW-BUFFER-P to non-nil to generate new buffer for the
process."
  (let ((alias (git-command-get-alias-function command)))
    (if alias
        ;; if alias is defined in git-command-get-alias-function
        (funcall alias
                 options command args new-buffer-p)
      (if (member command
                  git-command-view-command-list)
          ;; if this command is a view command
          (let* ((bname (concat "*"
                                "git "
                                command
                                "*"))
                 (bf (if new-buffer-p (generate-new-buffer bname)
                       (and (get-buffer bname)
                            (kill-buffer bname))
                       (get-buffer-create bname))))
            (display-buffer bf)
            (with-current-buffer bf
              (let ((inhibit-read-only t))
                (erase-buffer)
                (if (fboundp 'ansi-color-apply-on-region)
                    (progn
                      (shell-command (concat "git "
                                             (git-command-construct-commandline
                                              `(,@git-command-default-options
                                                ,@options
                                                "-c"
                                                "color.ui=always")
                                              command
                                              args))
                                     t)
                      (ansi-color-apply-on-region (point-min)
                                                  (point-max)))
                  (shell-command (concat "git "
                                         (git-command-construct-commandline
                                          `(,@git-command-default-options
                                            ,@options "-c" "color.ui=never")
                                          command
                                          args))
                                 t))
                (fundamental-mode)
                (view-mode))))
        ;; if this command is not a view command
        (with-editor "GIT_EDITOR"
          (term-run
           shell-file-name
           (if new-buffer-p
               (generate-new-buffer "*git command*")
             "*git command*")
           shell-command-switch
           (concat "git "
                   (git-command-construct-commandline
                    ;; TODO: fix colorize: currently output is not colorized
                    `(,@git-command-default-options
                      ,@options "-c" "color.ui=always")
                    command
                    args))
           ))))))

(provide 'git-command)

;;; git-command.el ends here

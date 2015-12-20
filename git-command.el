;;; git-command.el --- Dead simple git command interface

;; Author: 10sr <8slashes+el [at] gmail [dot] com>
;; URL: https://github.com/10sr/git-command-el
;; Version: 0.1
;; Package-Requires: ((term-run "20150601.6") (with-editor "20151126.323") (git-ps1-mode "20151220.415") (ansi-color "0"))
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

(require 'term)
(require 'term-run)
(require 'ansi-color)
(require 'with-editor)
(require 'git-ps1-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables


(defgroup git-command nil
  "Dead simple git command interface."
  :group 'tools)

(defcustom git-command-default-command
  "git "
  "Default value for `git-command' interactive execution."
  :group 'git-command
  :type 'string)


(defvar git-command-history nil
  "History list for `git-command'.")


(defconst git-command--with-git-pager-executable
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
  --eval \"(git-command--with-pager-display-contents \\\"$tmp\\\")\"
__EOF__
rm -f \"$tmp\"
"
  "Script content for `git-command--with-git-pager-executable'.")


(defvar git-command--pager-buffer-create-new nil
  "Non-nil to create new buffer for each GIT_PAGER invocation.

This variable is used internally only.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pager


(defun git-command--with-pager-display-contents (filename)
  "Insert contents of FILENAME in a buffer and popup with `display-buffer'."
  (let ((buf (if git-command--pager-buffer-create-new
                  (generate-new-buffer "*git pager*")
                (when (get-buffer "*git pager*")
                  (kill-buffer "*git pager*"))
                (get-buffer-create "*git pager*"))))
    (with-current-buffer buf
      (insert-file-contents filename)
      (ansi-color-apply-on-region (point-min)
                                  (point-max)))
    (display-buffer buf)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; user commands

;;;###autoload
(defun git-command (cmd &optional new-buffer-p)
  "Shell like git command interface.

CMD is the commandline string to run.
If NEW-BUFFER-P is non-nil, generate new buffer for running command."
  (interactive (list (read-shell-command (format "[%s]%s $ "
                                                 (abbreviate-file-name
                                                  default-directory)
                                                 (git-ps1-mode-get-current "[GIT:%s]"))
                                         git-command-default-command
                                         'git-command-history)
                     current-prefix-arg))
  (setq git-command--pager-buffer-create-new
        new-buffer-p)
  (git-command-with-git-editor-git-pager
    (term-run shell-file-name
              (if new-buffer-p
                  (generate-new-buffer "*git command*")
                "*git command*")
              shell-command-switch
              cmd)))

(provide 'git-command)

;;; git-command.el ends here

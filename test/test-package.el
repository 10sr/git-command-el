;;; test-package.el --- Packaging test       -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

(ert-deftest test-package ()
  (dolist (el '("git-command.el"))
    (message "Loading info: %s"
             el)
    (with-temp-buffer
      (insert-file-contents el)
      (message "%S"
               (package-buffer-info)))))

(provide 'test-package)
;;; test-package.el ends here

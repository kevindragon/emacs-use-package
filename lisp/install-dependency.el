;;; install-dependency.el --- Install dependency

;; Filename: install-dependency.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(defcustom kj/install-directory "c:/Software"
  "指定需要安装的应用的根目录."
  :type 'string
  :group 'kj/install)

(defun kj/install-yq ()
  (interactive))

(provide 'install-dependency)

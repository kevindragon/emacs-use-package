;;; defines.el --- Define global variables

;; Filename: defines.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2022, Kevin Jiang, all rights reserved.

(defvar HOME (file-name-as-directory (expand-file-name "~")))

(defvar PYTHON_PATH
  (cond
   ((is-windows?)
    (concat HOME "Miniconda3/python.exe"))
   ((is-osx?)
    (concat HOME  "Miniconda3/python.exe"))
   (t
    nil))
  "python命令路径")


(provide 'defines)

;;; defines.el ends here

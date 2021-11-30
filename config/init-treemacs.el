;;; init-treemacs.el --- Init for treemacs

;; Filename: init-treemacs.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.


;;; Code:
(use-package treemacs
  :init
  (setq-default treemacs-python-executable "c:/Users/jiangkx/Miniconda3/python.exe")
  :config
  (setq treemacs-python-executable "c:/Users/jiangkx/Miniconda3/python.exe")
  (when (featurep 'lsp-mode)
    (use-package lsp-treemacs :commands lsp-treemacs-errors-list))
  (treemacs-resize-icons 16))

;; (setq treemacs-silent-refresh t)
;; ;; 会影响lsp的启动
;; ;; (lsp-treemacs-sync-mode 1)


(provide 'init-treemacs)

;;; init-treemacs.el ends here

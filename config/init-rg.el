;;; init-rg.el --- Init for rg

;; Filename: init-rg.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package ripgrep)
(use-package rg
  :bind ("C-c s" . rg-menu)
  :config (rg-enable-default-bindings))



(provide 'init-rg)

;;; init-rg.el ends here

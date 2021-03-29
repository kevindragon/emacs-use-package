;;; init-flycheck.el --- Init for flycheck

;; Filename: init-flycheck.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package flycheck)
(use-package flycheck-posframe
  :hook (flycheck-mode-hook . flycheck-posframe-mode))



(provide 'init-flycheck)

;;; init-flycheck.el ends here

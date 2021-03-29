;;; init-symbol-overlay.el --- Init for symbol-overlay

;; Filename: init-symbol-overlay.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package symbol-overlay
  :bind (("C-c M-i" . symbol-overlay-put)
         ("C-c M-n" . symbol-overlay-switch-forward)
         ("C-c M-p" . symbol-overlay-switch-backward))
  :config
  (symbol-overlay-mode +1))



(provide 'init-symbol-overlay)

;;; init-symbol-overlay.el ends here

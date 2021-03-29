;;; init-company.el --- Init for company

;; Filename: init-company.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package company
  :config
  (global-company-mode +1)
  (setq company-minimum-prefix-length 2))

(use-package posframe
  :config (setq posframe-mouse-banish nil))

(use-package company-posframe
  :config (company-posframe-mode 1))

(use-package company-quickhelp
  :hook (company-mode-hook . company-quickhelp-mode))



(provide 'init-company)

;;; init-company.el ends here

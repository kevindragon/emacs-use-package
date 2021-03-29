;;; init-basic-packages.el --- Init for basic packages

;; Filename: init-basic-packages.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;; window-numbering
(use-package window-numbering
  :init
  (add-hook 'after-init-hook 'window-numbering-mode t))

;; emacs-which-key
(use-package which-key
    :config (which-key-mode))

;; 添加exec-path-from-shell
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package page-break-lines
  :config (global-page-break-lines-mode +1))

(use-package hydra)


(provide 'init-basic-packages)

;;; init-basic-packages.el ends here

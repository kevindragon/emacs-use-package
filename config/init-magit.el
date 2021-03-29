;;; init-magit.el --- Init for magit

;; Filename: init-magit.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package magit
  :bind ("M-n g" . magit-status)
  :config
  (modify-coding-system-alist 'file "\\.git/COMMIT_EDITMSG\\'" 'utf-8)
  (modify-coding-system-alist 'file "COMMIT_EDITMSG\\'" 'utf-8))



(provide 'init-magit)

;;; init-magit.el ends here

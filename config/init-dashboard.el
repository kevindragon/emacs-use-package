;;; init-dashboard.el --- Init for dashboard

;; Filename: init-dashboard.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((agenda . 10)
                          ;; (bookmarks . 5)
                          (projects . 20)
                          (recents  . 20)
                          ;; (registers . 5)
                          ))
  (when (featurep 'all-the-icons)
    (setq dashboard-footer-icon
          (all-the-icons-octicon "dashboard"
                                 :height 1.1
                                 :v-adjust -0.05
                                 :face 'font-lock-keyword-face))))



(provide 'init-dashboard)

;;; init-dashboard.el ends here

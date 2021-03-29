;;; init-projectile.el --- Init for projectile

;; Filename: init-projectile.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  ;; 忽略更多的目录
  (setq projectile-globally-ignored-directories
	(append projectile-globally-ignored-directories
		'("node_modules" ".vscode" ".git" ".pytest_cache" ".log"
                  "logs" ".eggs"))))


(provide 'init-projectile)

;;; init-projectile.el ends here

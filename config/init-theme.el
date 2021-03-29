;;; init-theme.el --- Init for theme

;; Filename: init-theme.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(defun custom-default-theme ()
  (custom-set-faces
   '(font-lock-comment-face ((t (:foreground "light slate gray"))))
   '(fringe ((t (:background "ghost white"))))
   '(mmm-default-submode-face ((t (:background "white"))))
   '(window-divider ((t (:foreground "gray30"))))))

(defun reset-theme ()
  "恢复默认的主题"
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes)
  (custom-default-theme))

;; (use-package dracula-theme
;;   :config (load-theme 'dracula t))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-challenger-deep t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)

  ;; or for treemacs users
  ;; use the colorful treemacs theme
  ;; (setq doom-themes-treemacs-theme "doom-colors")
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


(provide 'init-theme)

;;; init-theme.el ends here

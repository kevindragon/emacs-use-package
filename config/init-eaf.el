;;; init-eaf.el --- Init for Emacs Application Framework

;; Filename: init-eaf.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package eaf
  ; Set to "/usr/share/emacs/site-lisp/eaf" if installed from AUR
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :init
  (use-package epc :defer t :ensure t)
  (use-package ctable :defer t :ensure t)
  (use-package deferred :defer t :ensure t)
  (use-package s :defer t :ensure t)
  :custom
  (eaf-browser-continue-where-left-off t)
  :config
  (require 'eaf-browser)
  (require 'eaf-pdf-viewer)
  (require 'eaf-org-previewer)
  ;; (eaf-setq eaf-browser-enable-adblocker "true")
  ;; (eaf-setq eaf-python-command "C:/Users/jiangkx/Miniconda3/python.exe")
  ;; (setq eaf-python-command "C:/Users/jiangkx/Miniconda3/python.exe")
  ;; (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;; (eaf-bind-key take_photo "p" eaf-camera-keybinding)
  ;; (eaf-bind-key nil "M-q" eaf-browser-keybinding)
  ) ;; unbind, see more in the Wiki


(provide 'init-eaf)

;;; init-eaf.el ends here

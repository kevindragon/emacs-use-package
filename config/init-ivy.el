;;; init-ivy.el --- Init for ivy, counsel, swiper

;; Filename: init-ivy.el
;; Description: Init for smex
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.
;; Created: 2020-03-16 28:48:57
;; Version: 0.1
;; Last-Updated: 2020-03-16 28:48:57
;;           By: Kevin Jiang
;; URL: http://www.emacswiki.org/emacs/download/init-smex.el
;; Keywords:
;; Compatibility: GNU Emacs GNU Emacs 27.0.60
;;
;; Features that might be required by this library:
;;

;;; Code:
(use-package smex
  :config (smex-initialize))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)))

(use-package swiper
  :bind ("C-s" . swiper))



(provide 'init-ivy)

;;; init-ivy.el ends here

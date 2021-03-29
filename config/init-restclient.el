;;; init-restclient.el --- Init for restclient

;; Filename: init-restclient.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.


;;; Code:
(use-package restclient)
(use-package ob-restclient
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((restclient . t))))


(provide 'init-restclient)

;;; init-restclient.el ends here

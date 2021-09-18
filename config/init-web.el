;;; init-web.el --- Init for spinner

;; Filename: init-web.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package typescript-mode
  :hook (typescript-mode . lsp)
  )

(use-package tide
  :hook (typescript-mode . tide-setup))

(use-package web-mode
  :mode ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.html?\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 0
        web-mode-script-padding 0))


(use-package ng2-mode)

(with-eval-after-load "lsp-mode"
  (setq lsp-clients-angular-language-server-command
        '("node"
          ;; "C:/software/vscode-ng-language-service-master"
          "C:/Users/jiangkx/AppData/Roaming/npm/node_modules/@angular/language-server"
          "--ngProbeLocations"
          "C:/Users/jiangkx/AppData/Roaming/npm/node_modules/@angular/language-service"
          "--tsProbeLocations"
          "C:/Users/jiangkx/AppData/Roaming/npm/node_modules/typescript"
          "--stdio")))


;;; js-mode
(setq js-indent-level 2)


(provide 'init-web)

;;; init-web.el ends here

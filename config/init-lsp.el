;;; init-lsp.el --- Init for lsp-mode

;; Filename: init-lsp.el
;; Description: Init for lsp-mode
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Installation:
;;
;; Put init-lsp.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-lsp)
;;

;;; Code:
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands lsp)

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-cursor nil)
  :commands lsp-ui-mode)

(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;; optionally if you want to use debugger
(use-package dap-mode
  :config
  (dap-mode 1)
  ;; The modes below are optional
  (dap-ui-mode 1)
  ;; enables mouse hover support
  (dap-tooltip-mode 1)
  ;; use tooltips for mouse hover
  ;; if it is not enabled `dap-mode' will use the minibuffer.
  (tooltip-mode 1)
  ;; displays floating panel with debug buttons
  ;; requies emacs 26+
  (dap-ui-controls-mode 1))

;; (with-eval-after-load 'lsp-ui-doc
;;   (lsp-ui-doc-frame-mode +1))

;; (custom-set-variables
;;  '(lsp-signature-render-documentation nil)
;;  ;; '(lsp-ui-doc-alignment 'frame)
;;  '(lsp-ui-doc-position 'at-point)
;;  ;; '(lsp-ui-doc-include-signature nil)
;;  ;; '(lsp-ui-sideline-show-diagnostics nil)
;;  ;; '(lsp-keep-workspace-alive nil)
;;  '(lsp-headerline-breadcrumb-enable nil)
;;  )

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))



(provide 'init-lsp)

;;; init-lsp.el ends here

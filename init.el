;;; init Emacs
;; author: Kevin Jiang
;; E-mail: wenlin1988@126.com

;; setup my info
(setq user-full-name "Kevin Jiang")
(setq user-mail-address "wenlin1988@126.com")

;; 加速配置。
(setq
 ;; 不要缩放frame.
 frame-inhibit-implied-resize t
 ;; 默认用最简单的模式
 ;; initial-major-mode 'fundamental-mode
 ;; 不要自动启用package
 package-enable-at-startup nil
 package--init-file-ensured t)

(defvar my-workspace-dir
  (if (string-equal system-type "windows-nt")
      "C:/workspace"
    (expand-file-name "~/workspace")))

(defvar my-emacs-config-dir
  (file-name-as-directory (expand-file-name "config" user-emacs-directory)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path my-emacs-config-dir)

(defun is-windows? ()
  (string-equal system-type "windows-nt"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
;; (package-refresh-contents)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(require 'init-dashboard)
(require 'init-benchmark)
(require 'init-startup)
(require 'init-theme)
(require 'init-basic-packages)
(require 'init-icons)
(require 'init-projectile)
(require 'init-symbol-overlay)
(require 'init-yasnippet)
(require 'init-rg)
(require 'init-org)
(require 'init-prog)
(require 'init-company)
(require 'init-lsp)
(require 'init-ivy)
(require 'init-python)
(require 'init-java)
(require 'init-web)
(require 'init-restclient)

(require 'init-yaml)

(require 'init-sql)

;; (require 'init-eaf)

(require 'kevinj)
(require 'kj-docker)
(require 'randomize-region)

(require 'antlr-mode)
(autoload 'antlr-v4-mode "antlr-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-v4-mode))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(tide web-mode ng2-mode ein-notebook ob-restclient typescript-mode yaml-mode ein elpy plantuml-mode treemacs-all-the-icons kubernetes doom-themes restclient lsp-java python-pytest counsel ivy dap-java dap-python company-posframe magit hl-todo htmlize org-pomodoro powershell flycheck-posframe flycheck rg ripgrep yasnippet-snippets yasnippet symbol-overlay benchmark-init dashboard all-the-icons exec-path-from-shell page-break-lines window-numbering dracula-theme company-quickhelp company-help lsp-pyright company projectile use-package))
 '(safe-local-variable-values
   '((project-enable-remote . t)
     (remote-path . "/data/sdb/kevin/workspace/lnip_backend_semantic_analysis")
     (remote-user . "tpo")
     (remote-host . "10.123.4.230")
     (remote-path . "")
     (remote-user . "")
     (remote-host . ""))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

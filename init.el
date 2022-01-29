;;; init Emacs
;; author: Kevin Jiang
;; E-mail: wenlin1988@126.com

;; setup my info
(setq user-full-name "Kevin Jiang")
(setq user-mail-address "wenlin1988@126.com")

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; 加速配置。
(setq
 ;; 不要缩放frame.
 frame-inhibit-implied-resize t
 ;; 默认用最简单的模式
 ;; initial-major-mode 'fundamental-mode
 ;; 不要自动启用package
 package-enable-at-startup nil
 package--init-file-ensured t)

(defun is-windows? ()
  (string-equal system-type "windows-nt"))

(defun is-osx? ()
  (string-equal system-type "darwin"))

(defvar my-workspace-dir
  (if (is-windows?)
      "C:/workspace"
    (expand-file-name "~/workspace")))

(defvar my-emacs-config-dir
  (file-name-as-directory (expand-file-name "config" user-emacs-directory)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path my-emacs-config-dir)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(when (not (featurep 'use-package))
  (package-install 'use-package))

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

(require 'init-sql)
(require 'init-org)
(require 'init-treemacs)

(require 'init-prog)
(require 'init-company)
(require 'init-magit)
(require 'init-lsp)
(require 'init-ivy)
(require 'init-python)
(require 'init-java)
(require 'init-web)
(require 'init-restclient)
(require 'init-plantuml)
(require 'init-yaml)

(when (is-windows?)
  (require 'init-eaf))

(require 'kevinj)
(require 'kj-docker)
(require 'randomize-region)

(require 'antlr-mode)
(autoload 'antlr-v4-mode "antlr-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-v4-mode))


;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(dracula-theme org-present epc org-roam emacsql-sqlite3 magit yasnippet-snippets yaml-mode window-numbering which-key web-mode use-package tide symbol-overlay smex ripgrep rg python-pytest powershell plantuml-mode page-break-lines org-pomodoro ob-restclient lsp-ui lsp-pyright lsp-java lsp-ivy htmlize hl-todo exec-path-from-shell ein doom-themes dashboard counsel company-quickhelp company-posframe benchmark-init all-the-icons)))

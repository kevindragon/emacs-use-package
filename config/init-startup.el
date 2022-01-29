;;; init-startup.el --- Config load when startup

;; Filename: init-startup.el
;; Description: Config load when startup
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2014, Kevin Jiang, all rights reserved.

;;; replace yes no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; coding system
(when (is-windows?)
  (prefer-coding-system 'cp950)
  (prefer-coding-system 'gb2312)
  (prefer-coding-system 'cp936)
  (prefer-coding-system 'gb18030)
  (prefer-coding-system 'utf-16)
  (prefer-coding-system 'utf-8)
  (prefer-coding-system 'utf-8-dos)
  (prefer-coding-system 'utf-8-unix))
(when (is-osx?)
  (prefer-coding-system 'utf-16)
  (prefer-coding-system 'utf-8)
  (prefer-coding-system 'utf-8-dos)
  (prefer-coding-system 'utf-8-unix)
  ;; macOS下把command键设置为meta键
  (progn
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)
    (message "Mac OS X")))

;; (when (eq system-type 'windows-nt)
;;   ;; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
;;   (setq utf-translate-cjk-mode nil)
;;   (set-language-environment 'utf-8)
;;   (setq locale-coding-system 'utf-8)
;;   (set-default-coding-systems 'utf-8)
;;   (set-terminal-coding-system 'utf-8)
;;   (set-selection-coding-system 'utf-16-le)
;;   (prefer-coding-system 'utf-8)
;;   )

;; (setenv "LANG" "zh_CN.UTF-8")
(setenv "LANG" "en_US.UTF-8")

;;; set fonts
(require 'font-utils)
(if (font-utils-exists-p "Source Code Pro")
    (set-face-attribute
     'default nil
     :font (font-spec
            :family "Source Code Pro"  ; "Courier New"
            :weight 'normal
            :slant 'normal
            :size (if (is-windows?) 9.5 13.0)))
  (error "Font family `Source Code Pro' is not existing, please install."))
(dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font
     (frame-parameter nil 'font)
     charset
     (font-spec
      :family (if (is-windows?) "Microsoft Yahei" "Monaco")
      :weight 'normal
      :slant 'normal
      :size (if (is-windows?) 12.0 16.0))))
;; (when (eq system-type 'windows-nt)
;;   (set-face-attribute
;;    'default nil
;;    :font (font-spec
;;           ;; :name "-outline-Courier New-bold-italic-normal-mono-*-*-*-*-c-*-iso10646-1"
;;           :family "Source Code Pro"  ; "Courier New"
;;           :weight 'normal
;;           :slant 'normal
;;           :size 9.5))
;;   (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;     (set-fontset-font
;;      (frame-parameter nil 'font)
;;      charset
;;      (font-spec
;;       ;; :name "-outline-微软雅黑-normal-normal-normal-sans-*-*-*-*-p-*-iso10646-1"
;;       :family "Microsoft Yahei"
;;       :weight 'normal
;;       :slant 'normal
;;       :size 12.0)))
;;   ;; (setq face-font-rescale-alist '(("Microsoft Yahei" . 1.2)))
;;   )

;;; set location on frame title
(defun frame-title-string ()
  "Return the file name of current buffer, using ~ if under home directory"
  (let ((fname (or (buffer-file-name (current-buffer)) (buffer-name)))
        (max-len 100))
    (when (string-match (getenv "HOME") fname)
      (setq fname (replace-match "~" t t fname)))
    (if (> (length fname) max-len)
        (setq fname (concat "..." (substring fname (- (length fname) max-len)))))
    fname))
(setq frame-title-format '("Kevin@"(:eval (frame-title-string))))

;; 不生成~结尾的文件
(setq make-backup-files nil)

;; 不需要toolbar和menu bar
(tool-bar-mode 0)
(menu-bar-mode 0)
;; 要不滚动条
(scroll-bar-mode 0)
;; 保留上次打开的位置
(save-place-mode 1)
;; tab size
(setq default-tab-width 4)
(setq tab-width 4)
;; 括号高亮匹配显示
(show-paren-mode 1)
;; 自动插入配对的括号
(electric-pair-mode 1)
;; 在mode line显示行和列
(line-number-mode 1)
(column-number-mode 1)
(global-hl-line-mode +1)
;; 超长行开启so-long-mode
(global-so-long-mode 1)

(recentf-mode 1)

(setq-default indent-tabs-mode nil)

(defalias 'list-buffers 'ibuffer)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default kill-ring-max 200000)



(provide 'init-startup)

;;; init-startup.el ends here

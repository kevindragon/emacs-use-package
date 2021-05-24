;;; init-org.el --- Init for org

;; Filename: init-org.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package powershell)
(use-package org-pomodoro)
(use-package htmlize)

(require 'org)
(require 'ob-eshell)
(require 'ob-python)
(require 'ob-shell)

(setq org-directory (concat my-workspace-dir "/orgs"))
(setq org-agenda-files (list (concat my-workspace-dir "/orgs/todo.org")
                             (concat my-workspace-dir "/orgs/learn.org")
                             (concat my-workspace-dir "/orgs/task.org")))
(setq org-default-notes-file (concat org-directory "/task.org"))
(setq org-hide-leading-stars t)         ;使星号不可见
(setq org-enable-table-editor 1)        ;启用内建的电子表格
(setq org-log-done t)                   ;日志记录
;; (setq org-log-done '(done))             ;日志记录类型
;; (setq org-log-done 'time)
(setq org-agenda-include-diary t)       ;集成日历
(setq org-startup-truncated t)          ;默认换行
(setq org-startup-indented t)           ;打开缩进

(setq org-agenda-span 10
      org-agenda-start-on-weekday nil
      org-agenda-start-day "-3d")

;; 编辑代码按 C-c ' 返回org的时候前面的空格
(setq org-edit-src-content-indentation 0)

(defun kj/org-mode-hook ()
  (when (bound-and-true-p org-html-table-default-attributes)
    (setq org-html-table-default-attributes
	  (plist-put org-html-table-default-attributes :rules "all"))
    (setq org-html-table-default-attributes
	  (plist-put org-html-table-default-attributes :frame "border"))))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; configure org-capture templates
(setq org-capture-templates
      '(("t" ; hotkey
         "Todo list item" ; name
         entry ; type
         (file+headline org-default-notes-file "Tasks") ; heading type and title
         "* TODO %?\n  %i\n  %a") ; template
        ))

(add-hook 'org-mode-hook #'kj/org-mode-hook)

(setq org-archive-location "%s_archive::date-tree")
(advice-add 'org-archive-default-command :after #'org-save-all-org-buffers)

;; agenda 里面时间块彩色显示
;; From: https://emacs-china.org/t/org-agenda/8679/3
;; (defun kj/org-agenda-time-grid-spacing ()
;;   "Set different line spacing w.r.t. time duration."
;;   (save-excursion
;;     (let* ((background (alist-get 'background-mode (frame-parameters)))
;;            (background-dark-p (string= background "dark"))
;;            (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
;;            pos
;;            duration)
;;       (nconc colors colors)
;;       (goto-char (point-min))
;;       (while (setq pos (next-single-property-change (point) 'duration))
;;         (goto-char pos)
;;         (when (and (not (equal pos (point-at-eol)))
;;                    (setq duration (org-get-at-bol 'duration)))
;;           (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
;;                 (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
;;             (overlay-put ov 'face `(:background ,(car colors)
;;                                                 :foreground
;;                                                 ,(if background-dark-p "black" "white")))
;;             (setq colors (cdr colors))
;;             (overlay-put ov 'line-height line-height)
;;             (overlay-put ov 'line-spacing (1- line-height))))))))

;; (add-hook 'org-agenda-finalize-hook #'kj/org-agenda-time-grid-spacing)


;;; 我的博客配置
(require 'ox-publish)
(setq
 org-publish-project-alist
 '(("blog-notes"
    :base-directory "c:/workspace/orgs/knowledge-base"
    :base-extension "org"
    :publishing-directory "c:/workspace/orgs/knowledge-base-html/"
    :recursive t
    :publishing-function org-html-publish-to-html
    :headline-levels 4
    :auto-preamble t
    :section-numbers nil
    :author "KevinJiang"
    :email "wenlin1988@126.com"
    :auto-sitemap t
    :sitemap-filename "sitemap.org"
    :sitemap-title "Sitemap"
    :sitemap-sort-files anti-chronologically
    :sitemap-file-entry-format "%d %t"
    )

   ("blog-static"
    :base-directory "c:/workspace/orgs/knowledge-base"
    :base-extension "css|js|png|jpg|gif|pdf|mp3|ogg|swf"
    :publishing-directory "c:/workspace/orgs/knowledge-base-html"
    :recursive t
    :publishing-function org-publish-attachment
    )

   ("blog" :components ("blog-notes" "blog-static"))
   ))


(provide 'init-org)

;;; init-org.el ends here

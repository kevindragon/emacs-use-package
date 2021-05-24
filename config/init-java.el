;;; init-java.el --- Init for java

;; Filename: init-java.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(defvar kj/java-lombok-jar "c:/software/lombok.jar")

(defun kj/java-mode-hook ()
  (symbol-overlay-mode 1)
  (setq-default tab-width 4)
  (lsp)
  ;; (setq lsp-java-configuration-maven-user-settings
  ;;       "c:/workspace/lexisnexis/TotalPatentOne/settings_lnip.xml")
  )

(use-package lsp-java
  :config
  (with-eval-after-load "dap-mode"
    (require 'dap-java))
  (require 'lsp-java-boot)
  (setq lsp-java-vmargs
        (list "-noverify"
              "-Xmx2G"
              "-XX:+UseG1GC"
              "-XX:+UseStringDeduplication"
              (concat "-javaagent:" kj/java-lombok-jar)
              (concat "-Xbootclasspath/a:" kj/java-lombok-jar))
        lsp-file-watch-ignored
        '(".idea" ".ensime_cache" ".eunit" "node_modules"
          ".git" ".hg" ".fslckout" "_FOSSIL_"
          ".bzr" "_darcs" ".tox" ".svn" ".stack-work"
          "build"))
  (add-hook 'java-mode-hook #'kj/java-mode-hook)
  (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)
  )


(provide 'init-java)

;;; init-java.el ends here

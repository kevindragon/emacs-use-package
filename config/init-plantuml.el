;;; init-plantuml.el --- Init for plantuml

;; Filename: init-plantuml.el
;; Author: Kevin Jiang <wenlin1988@126.com>
;; Maintainer: Kevin Jiang <wenlin1988@126.com>
;; Copyright (C) 2020, Kevin Jiang, all rights reserved.

;;; Code:
(use-package plantuml-mode
  :config

  ;; Sample jar configuration
  (setq plantuml-jar-path
        (expand-file-name "bin/plantuml/plantuml-1.2022.6.jar" user-emacs-directory))
  (setq plantuml-default-exec-mode 'jar)

  (setq org-plantuml-jar-path plantuml-jar-path)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

  ;; Sample executable configuration
  ;; (setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
  ;; (setq plantuml-default-exec-mode 'executable)

  ;; Enable plantuml-mode for PlantUML files
  (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
  )


(provide 'init-plantuml)

;;; init-plantuml.el ends here

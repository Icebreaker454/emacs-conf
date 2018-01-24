(require 'package) ;; You might already have this line

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize) ;; You might already have this line

;; Basic stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-linum-mode)
(setq inhibit-startup-screen t)


;; Theme
(load-theme 'intellij t t)
(enable-theme 'intellij)
		  

;; Package configuration

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (add-to-list 'auto-mode-alist
	       '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist
	       '("\\.jsx\\'" . web-mode))
)
(setq-default indent-tabs-mode nil)

(use-package
  company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode))
    (setq company-dabbrev-downcase 0)
    (setq company-idle-delay 0))

(use-package
  company-anaconda
  :config
  (progn
    (add-to-list 'company-backends '(company-anaconda :with company-capf))))

(use-package
  rjsx-mode
  :config
  (progn
    (add-to-list 'auto-mode-alist
		 '("components\\/.*\\.js\\'" . rjsx-mode))))
(use-package
  web-mode
  :config
  (progn
    (require 'web-mode)
    (add-hook 'web-mode-hook 'my-web-mode-hook)
    ))
(use-package
  flycheck
  :config
  (progn
    (require 'flycheck)
    (add-hook 'after-init-hook 'global-flycheck-mode)
    (setq-default flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(javascript-jshint)))

    ;; use eslint with web-mode for jsx files
    (flycheck-add-mode 'javascript-eslint 'web-mode)

    ;; customize flycheck temp file prefix
    (setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
    (setq-default flycheck-disabled-checkers
      (append flycheck-disabled-checkers
        '(json-jsonlist)))

    )
)
(use-package
  pyenv-mode
  :config
  (progn
    (pyenv-mode)))
(use-package
  anaconda-mode
  :config
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)))
(use-package
  projectile
  :init
  (progn
    (projectile-global-mode)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("b48150eac948d6de3f8103e6e92f105979277b91c96e9687c13f2d80977d381d" "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" default)))
 '(package-selected-packages
   (quote
    (company-anaconda company ac-anaconda auto-complete intellij-theme ample-zen-theme projectile flycheck indent-guide web-mode anaconda-mode pyenv-mode rjsx-mode use-package)))
 '(pyenv-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


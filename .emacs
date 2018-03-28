(require 'package) ;; You might already have this line

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize) ;; You might already have this line

;; Basic stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(global-linum-mode)
(global-auto-revert-mode t)
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)

;; Fullscreen by default
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "b48150eac948d6de3f8103e6e92f105979277b91c96e9687c13f2d80977d381d" "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" default)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (helm-projectile helm exotica-theme melancholy-theme emmet-mode json-mode py-autopep8 importmagic company-anaconda company ac-anaconda auto-complete intellij-theme ample-zen-theme projectile flycheck indent-guide web-mode anaconda-mode pyenv-mode use-package)))
 '(pyenv-mode t))

;; Remove extra whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)


;; Theme
(load-theme 'exotica t t)
(enable-theme 'exotica)


;; Package configuration

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))

  )

(defun eslint-fix-and-revert ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and (equal web-mode-content-type "jsx") (and eslint (file-executable-p eslint)))
      (interactive)
      (shell-command (concat eslint (concat " --fix " (buffer-file-name))))
      (revert-buffer t t))))

(use-package py-autopep8
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode))
    (setq company-dabbrev-downcase nil)
    (setq company-idle-delay 0))

(use-package company-anaconda
  :config
  (progn
    (add-to-list 'company-backends '(company-anaconda :with company-capf))))

(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))

(use-package web-mode
  :config
  (progn
    (require 'web-mode)
    (add-to-list 'auto-mode-alist
                 '("\\.html\\'" . web-mode))
    (add-to-list 'auto-mode-alist
                 '("\\.jsx\\'" . web-mode))
    (add-to-list 'auto-mode-alist
                 '("\\.js\\'" . web-mode))
    (add-to-list 'auto-mode-alist
                 '("\\.css\\'" . web-mode))
    (add-hook 'after-save-hook 'eslint-fix-and-revert)
    (add-hook 'web-mode-hook 'my-web-mode-hook)))

(use-package flycheck
  :config
  (progn
    (require 'flycheck)

    (defun my/use-eslint-from-node-modules ()
      (let* ((root (locate-dominating-file
                    (or (buffer-file-name) default-directory)
                    "node_modules"))
             (eslint (and root
                          (expand-file-name "node_modules/eslint/bin/eslint.js"
                                            root))))
        (when (and eslint (file-executable-p eslint))
          (setq-local flycheck-javascript-eslint-executable eslint))))
    (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

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



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

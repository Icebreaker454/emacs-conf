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
(scroll-bar-mode 0)
(menu-bar-mode 0)
(global-linum-mode)
(global-auto-revert-mode t)
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Fullscreen by default
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#ffffff" "#032f62" "#6a737d" "#d73a49" "#6a737d" "#6a737d" "#6f42c1" "#6a737d"])
 '(custom-safe-themes
   (quote
    ("748d0e2ffdaf95015a539dcc95ab888283284ad7b076963760422cbe5e21903a" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "b48150eac948d6de3f8103e6e92f105979277b91c96e9687c13f2d80977d381d" "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" default)))
 '(fci-rule-color "#6a737d")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(nrepl-message-colors
   (quote
    ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(package-selected-packages
   (quote
    (dracula-theme auto-minor-mode tabbar neotree github-modern-theme helm-projectile helm exotica-theme melancholy-theme emmet-mode json-mode py-autopep8 importmagic company-anaconda company ac-anaconda auto-complete intellij-theme ample-zen-theme projectile flycheck indent-guide web-mode anaconda-mode pyenv-mode use-package)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(pyenv-mode t)
 '(tabbar-mode t nil (tabbar))
 '(tabbar-separator (quote (1.5)))
 '(vc-annotate-background "#3390ff")
 '(vc-annotate-color-map
   (quote
    ((20 . "#6a737d")
     (40 . "#032f62")
     (60 . "#6a737d")
     (80 . "#6a737d")
     (100 . "#6a737d")
     (120 . "#d73a49")
     (140 . "#6a737d")
     (160 . "#6a737d")
     (180 . "#6a737d")
     (200 . "#6a737d")
     (220 . "#22863a")
     (240 . "#005cc5")
     (260 . "#6f42c1")
     (280 . "#6a737d")
     (300 . "#005cc5")
     (320 . "#6a737d")
     (340 . "#d73a49")
     (360 . "#6a737d"))))
 '(vc-annotate-very-old-color "#6a737d"))

;; Remove extra whitespaces on save
(add-hook 'before-save-hook 'whitespace-cleanup)


;; Theme
(load-theme 'dracula t t)
(enable-theme 'dracula)


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
                 '("\\.js[x]?\\'" . web-mode))
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

;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]  " (tabbar-tab-tabset tab))
                  (format "%s  " (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
(tabbar-current-tabset)))))))))

(use-package
  tabbar
  :init
  (progn
    (custom-set-variables '(tabbar-separator (quote (1.5))))
    (tabbar-mode 1)
  )
  :config
  (progn
    (global-set-key (kbd "M-p") 'tabbar-backward-tab)
    (global-set-key (kbd "M-n") 'tabbar-forward-tab)
  )
)

(use-package
  neotree
  :init
  (require 'neotree)
  :config
  (global-set-key [f8] 'neotree-toggle)
)

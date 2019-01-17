(require 'package) ;; You might already have this line

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'load-path
             "/home/icebreaker/Stuff/elisp/awesome-tab/")
(package-initialize) ;; You might already have this line

;; Basic stuff
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(global-linum-mode)
(global-auto-revert-mode t)
(setq inhibit-startup-screen t)
(setq-default indent-tabs-mode nil)

(set-face-attribute 'default nil :family (quote (Monaco Regular)))

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
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#ffffff" "#032f62" "#6a737d" "#d73a49" "#6a737d" "#6a737d" "#6f42c1" "#6a737d"])
 '(custom-safe-themes
   (quote
    ("a566448baba25f48e1833d86807b77876a899fc0c3d33394094cf267c970749f" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "9d9fda57c476672acd8c6efeb9dc801abea906634575ad2c7688d055878e69d6" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "b59d7adea7873d58160d368d42828e7ac670340f11f36f67fa8071dbf957236a" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "748d0e2ffdaf95015a539dcc95ab888283284ad7b076963760422cbe5e21903a" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "b48150eac948d6de3f8103e6e92f105979277b91c96e9687c13f2d80977d381d" "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" default)))
 '(fci-rule-color "#6a737d")
 '(hl-sexp-background-color "#efebe9")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(package-selected-packages
   (quote
    (airline-themes sublimity fzf tern-auto-complete doom-themes zenburn-theme eslint-fix company-tern powerline leuven-theme subatomic256-theme gotham-theme atom-dark-theme dracula-theme auto-minor-mode tabbar neotree github-modern-theme exotica-theme melancholy-theme emmet-mode json-mode py-autopep8 importmagic company-anaconda company ac-anaconda auto-complete intellij-theme ample-zen-theme projectile flycheck indent-guide web-mode anaconda-mode pyenv-mode use-package)))
 '(pdf-view-midnight-colors (quote ("#6a737d" . "#fffbdd")))
 '(pyenv-mode t)
 '(tool-bar-mode nil)
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
(setq gotham-tty-256-colors t)
(setq gotham-tty-extended-palette t)


;; Package configuration

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-engines-alist
        '(("django" . "\\.html\\'")))
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))

  (defun eslint-fix ()
    (interactive)
    (when (equal web-mode-content-type "jsx")
      (shell-command (concat flycheck-javascript-eslint-executable (concat " --fix " (concat " --cache " buffer-file-name))))
    (revert-buffer t t)
  ))
  (add-hook 'after-save-hook #'eslint-fix)

  )

(use-package py-autopep8
  :config
  (setq py-autopep8-options '("--max-line-length=100"))
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

(use-package company
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode))
    (setq company-dabbrev-downcase nil)
    (setq company-idle-delay 0))

(use-package tern-auto-complete
  :config
  (progn
    (add-to-list 'company-backends 'company-tern)))

(use-package importmagic
  :ensure t
  :config
  (add-hook 'python-mode-hook 'importmagic-mode))

(use-package powerline
  :config
  (progn
    (require 'powerline)
    (powerline-default-theme)))

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
    (add-hook 'web-mode-hook #'my-web-mode-hook)))

;; AWESOME EMACS BUNDLE - https://github.com/emacs-tw/awesome-emacs#interface-enhancement
(use-package airline-themes
  :config
  (progn
    (require 'airline-themes)
    (load-theme 'airline-light t t)))

(use-package flycheck
  :config
  (progn
    (require 'flycheck)

    (defun my/use-js-executables-from-node-modules ()
      "Set executables of JS checkers from local node modules."
      (-when-let* ((file-name (buffer-file-name))
                   (root (locate-dominating-file file-name "node_modules"))
                   (module-directory (expand-file-name "node_modules" root)))
        (pcase-dolist (`(,checker . ,module) '((javascript-jshint . "jshint")
                                               (javascript-eslint . "eslint")
                                               (javascript-jscs   . "jscs")))
          (let ((package-directory (expand-file-name module module-directory))
                (executable-var (flycheck-checker-executable-variable checker)))
            (when (file-directory-p package-directory)
              (set (make-local-variable executable-var)
                   (expand-file-name (concat "bin/" module ".js")
                                     package-directory)))))))
    (add-hook 'flycheck-mode-hook #'my/use-js-executables-from-node-modules)

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
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode))
  :config
  (use-package company-anaconda
    :requires company
    :config (add-to-list 'company-backends 'company-anaconda)))

(use-package
  projectile
  :init
  (progn
    (projectile-mode)
    (setq projectile-keymap-prefix (kbd "C-c p"))
  ))

(use-package
  doom-themes
  :init
  (progn
    (require 'doom-themes)

    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'ample-zen t)
    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    (doom-themes-neotree-config)
    ;; or for treemacs users
    (doom-themes-treemacs-config)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
 ))

(use-package
  neotree
  :init
  (require 'neotree)
  :config
  (global-set-key [f8] 'neotree-toggle)
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :foundry "ADBO" :slant normal :weight normal :height 100 :width normal)))))

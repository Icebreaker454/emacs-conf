;;; emacs-conf --- Emacs configuration by Pavlo Pukach a.k.a. Icebreaker (pavlopukach@gmail.com)
;;; Commentary:
;;; This initialization file covers the basics for full-fledged Python development.
;;; Features:
;;; * Flycheck for code linting
;;; * Importmagic, epc, anaconda-moce for autocompletion.
;;; * Web-mode for JS development. Eslint checks with per-project eslint executable locating.


;;; Code:
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; Wrapping init file inside let file-name-handler-alist nil speeds up the startup
(let ((file-name-handler-alist nil))

  ;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
  ;; Set garbage collection threshold to 100 MB
  (setq gc-cons-threshold (* 100 1000 1000))

  (require 'package) ;; You might already have this line

  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives
               '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize) ;; You might already have this line

  ;;
  ;; BASICS
  ;;
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)

  (global-auto-revert-mode t)

  (setq-default bidi-display-reordering nil)
  ;; Prevent Emacs from asking about git-controlled symlink editing.
  (setq vc-follow-symlinks t)
  (setq tramp-default-method "ssh")

  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

  (set-face-attribute 'default nil :family (quote (JetBrains Mono)))

  (byte-compile-file "~/.emacs")
  (byte-recompile-directory "~/.emacs.d")
  (setq load-prefer-newer t)

  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))
  ;; Remove extra whitespaces on save
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; UTF-8 all the things!
  (set-charset-priority 'unicode)
  (setq locale-coding-system   'utf-8)
  (set-terminal-coding-system  'utf-8)
  (set-keyboard-coding-system  'utf-8)
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system        'utf-8)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

  (setq inhibit-splash-screen t)
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-buffer-menu t)
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)
  (setq ring-bell-function 'ignore)
  (setq create-lockfiles nil)
  (setq message-log-max 10000)
  (fset 'yes-or-no-p 'y-or-n-p)

  (global-set-key (kbd "C-x C-c") 'delete-frame)
  (global-set-key (kbd "s-[") 'other-window)
  (global-set-key (kbd "s-]") 'other-window)

  ;; Shortcuts for commonly used files
  (global-set-key (kbd "C-c e") (lambda() (interactive)(find-file "~/.emacs")))
  (global-set-key (kbd "C-c i") (lambda() (interactive)(find-file "~/OneDrive - Intellias/Notebooks/LeanSA/Info/")))

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
      ("6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "748d0e2ffdaf95015a539dcc95ab888283284ad7b076963760422cbe5e21903a" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "b48150eac948d6de3f8103e6e92f105979277b91c96e9687c13f2d80977d381d" "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" default)))
   '(fci-rule-color "#6a737d")
   '(hl-sexp-background-color "#efebe9")
   '(initial-frame-alist (quote ((fullscreen . maximized))))
   '(menu-bar-mode nil)
   '(nrepl-message-colors
     (quote
      ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
   '(org-agenda-files nil)
   '(package-selected-packages
     (quote
      (magit persp-projectile perspective nlinum ag afternoon-theme pyenv-mode-auto markdown-mode smart-hungry-delete docker-compose-mode dockerfile-mode tern-auto-complete doom-themes zenburn-theme eslint-fix company-tern powerline leuven-theme subatomic256-theme gotham-theme atom-dark-theme dracula-theme auto-minor-mode tabbar neotree github-modern-theme exotica-theme melancholy-theme emmet-mode json-mode py-autopep8 importmagic company-anaconda company ac-anaconda auto-complete intellij-theme ample-zen-theme projectile flycheck indent-guide web-mode anaconda-mode pyenv-mode use-package)))
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

  ;; Packages configuration


  ;;
  ;; CORE EMACS STUFF
  ;;
  (use-package smart-hungry-delete
    :ensure t
    :bind (("<backspace>" . smart-hungry-delete-backward-char)
                   ("C-d" . smart-hungry-delete-forward-char))
    :defer nil ;; dont defer so we can add our functions to hooks
    :config (smart-hungry-delete-add-default-hooks)
    )
  ;;
  ;; Multi-cursors
  ;;
  (use-package multiple-cursors
    :config
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

  ;; TODO entries Highlighting
  (use-package hl-todo
    :config
    (global-hl-todo-mode))

  ;; YASnippet
  (use-package yasnippet
    :config
    (yas-global-mode 1))

  (use-package telephone-line
    :config
    (progn
      (telephone-line-mode 1)))
  (use-package nlinum
    :init
    (global-nlinum-mode))

  (use-package helm
    :delight
    :bind (("M-x"     . #'helm-M-x))
    :bind (("C-x C-f" . #'helm-find-files))
    :bind (("C-x C-b" . #'helm-buffers-list))
    :config
    (use-package helm-rg)
    (require 'helm-config)
    (use-package helm-projectile
      :init
      (setq helm-projectile-fuzzy-match nil)
      :config
      (helm-projectile-on))
    (helm-mode 1))

  (use-package projectile
    :init
    (progn
      (projectile-mode)
      (define-key projectile-mode-map  (kbd "C-c p") 'projectile-command-map)
      (setq projectile-mode-line-prefix " Proj")
      (setq projectile-completion-system 'helm)
      (setq projectile-project-search-path '("~/Projects" "~/OneDrive - Intellias/Notebooks/"))
      ))

  (use-package magit
    :config
    (progn
      (global-set-key (kbd "C-c m g s") 'magit-status)
      (global-set-key (kbd "C-c m g p") 'magit-pull)
      ))
  (use-package org
    :config
    (progn
      (setq org-agenda-files (list "~/OneDrive - Intellias/Notebooks/LeanSA/Interviews.org"
                                   "~/OneDrive - Intellias/Notebooks/LeanSA/TaskLog.org"
                                   "~/OneDrive - Intellias/Notebooks/LeanSA/Info/Main.org"))
      (setq org-todo-keywords
            '((sequence "TODO(t)" "IN PROGRESS(p)" "BLOCKED(b@)" "|" "DONE(d!)" "CANCELED(c@)")))
      ))


  ;;
  ;; PYTHON
  ;;

  (defun my/python-configuration-hook ()
    (interactive)
    (setq tab-width     4
          python-indent-offset 4
          python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")
  (setq-default indent-tabs-mode nil))

  (add-hook 'python-mode-hook 'my/python-configuration-hook)

  (use-package
    company-jedi
    :bind (("M-." . 'jedi:goto-definition))
    :bind (("M-," . 'jedi:goto-definition-pop-marker))
    :bind (("M-/" . 'jedi:show-doc))
    :config
    (progn
      (defun my/company-jedi-hook ()
        (add-to-list 'company-backends 'company-jedi))
      (add-hook 'python-mode-hook 'my/company-jedi-hook)))

  (use-package
    pyenv-mode
    :after python company-jedi
    :commands pyenv-mode-versions
    :init
    (defun my/projectile-switch ()
      "Set pyenv version and JEDI venv matching project name."
      (let ((pyenv-file (projectile-expand-root ".python-version")))
        (if (file-exists-p pyenv-file)
          (progn
            (let ((venv-name (replace-regexp-in-string
                              "\n"
                              ""
                              (with-temp-buffer (insert-file-contents pyenv-file) (buffer-string)))))
              (pyenv-mode-set venv-name)
              (setq jedi:server-args (list "--virtual-env" (pyenv-mode-full-path venv-name))))
          (pyenv-mode-unset)))))
    (add-hook 'python-mode-hook 'pyenv-mode)
    (add-hook 'projectile-after-switch-project-hook 'my/projectile-switch))

  (use-package pyenv-mode-auto)

  ;; Py.Test integration
  (use-package python-pytest)

  (use-package py-autopep8
    :config
    (setq py-autopep8-options '("--max-line-length=100"))
    (add-hook 'python-mode-hook 'py-autopep8-enable-on-save))

  (use-package company
    :ensure t
    :defer t
    :config
    (progn
      (add-hook 'after-init-hook 'global-company-mode)
      (setq company-dabbrev-downcase nil
            company-minimum-prefix-length 0
            company-tooltip-limit 20)))

  (use-package importmagic
    :ensure t
    :config
    (add-hook 'python-mode-hook 'importmagic-mode))

  ;;
  ;; GO
  ;;
  (use-package go-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))


  ;;
  ;; JAVASCRIPT / Front-end
  ;;
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
      (add-hook 'web-mode-hook #'my/my-web-mode-hook)))

  ;; LUA!!!!
  (use-package lua-mode
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
      (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))))

  (use-package flycheck
    :config
    (progn
      (require 'flycheck)

      (add-hook 'after-init-hook 'global-flycheck-mode)
      (setq-default flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(javascript-jshint)))
      (setq-default flycheck-disabled-checkers
        (append flycheck-disabled-checkers
          '(python-flake8)))

      ;; use eslint with web-mode for jsx files
      (flycheck-add-mode 'javascript-eslint 'web-mode)

      ;; customize flycheck temp file prefix
      (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
      (setq-default flycheck-disabled-checkers
        (append flycheck-disabled-checkers
                '(json-jsonlist)))
      ))

  (defun my/my-web-mode-hook ()
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
  (use-package tern-auto-complete
    :config
    (progn
      (add-to-list 'company-backends 'company-tern)))


  ;;
  ;; ENHANCED VISUALS  - Themes
  ;;

  (use-package doom-themes
    :config
      (progn
        ;; Global settings (defaults)
        (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
              doom-themes-enable-italic t) ; if nil, italics is universally disabled

        ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
        ;; may have their own settings.
        (load-theme 'doom-one t)
        (setq doom-one-light-brighter-comments t)
        (setq doom-one-light-padded-modeline t)
        ;; Enable flashing mode-line on errors
        (doom-themes-visual-bell-config)

        ;; Enable custom neotree theme (all-the-icons must be installed!)
        (doom-themes-neotree-config)
        ;; or for treemacs users
        (doom-themes-treemacs-config)
        ))
  (use-package doom-modeline
    :ensure t
    :hook (after-init . doom-modeline-mode))
  ;;
  ;; UTILITY MODULES (markdown, Dockerfile and so on)
  ;;
  (use-package dockerfile-mode
    :config
    (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

  (use-package docker-compose-mode)

  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))


  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:family "Monaco" :height 115)))))

)
;;; .emacs ends here
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
    ("d7383f47263f7969baf3856ab8b3df649eb77eafdff0c5731bee2ad18e0faed2" "0f1733ad53138ddd381267b4033bcb07f5e75cd7f22089c7e650f1bb28fc67f4" "fa477d10f10aa808a2d8165a4f7e6cee1ab7f902b6853fbee911a9e27cf346bc" "51043b04c31d7a62ae10466da95a37725638310a38c471cc2e9772891146ee52" "7d4340a89c1f576d1b5dec57635ab93cdc006524bda486b66d01a6f70cffb08e" "a9d67f7c030b3fa6e58e4580438759942185951e9438dd45f2c668c8d7ab2caf" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "170bb47b35baa3d2439f0fd26b49f4278e9a8decf611aa33a0dad1397620ddc3" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "2c88b703cbe7ce802bf6f0bffe3edbb8d9ec68fc7557089d4eaa1e29f7529fe1" "b35a14c7d94c1f411890d45edfb9dc1bd61c5becd5c326790b51df6ebf60f402" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "b4c13d25b1f9f66eb769e05889ee000f89d64b089f96851b6da643cee4fdab08" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "190a9882bef28d7e944aa610aa68fe1ee34ecea6127239178c7ac848754992df" "9a155066ec746201156bb39f7518c1828a73d67742e11271e4f24b7b178c4710" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "e9460a84d876da407d9e6accf9ceba453e2f86f8b86076f37c08ad155de8223c" "748d0e2ffdaf95015a539dcc95ab888283284ad7b076963760422cbe5e21903a" "2cfc1cab46c0f5bae8017d3603ea1197be4f4fff8b9750d026d19f0b9e606fae" "3edbdd0ad45cb8f7c2575c0ad8f6625540283c6e928713c328b0bacf4cfbb60f" "b48150eac948d6de3f8103e6e92f105979277b91c96e9687c13f2d80977d381d" "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2" default)))
 '(fci-rule-color "#6a737d")
 '(hl-sexp-background-color "#efebe9")
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   (quote
    ("#032f62" "#6a737d" "#d73a49" "#6a737d" "#005cc5" "#6f42c1" "#d73a49" "#6a737d")))
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (go-mode realgud-ipdb realgud helm-ag doom-modeline helm-projectile helm-rg helm-flyspell helm zenburn python-pytest hl-todo lua-mode kaolin-themes company-jedi yasnippet-snippets telephone-line magit persp-projectile perspective nlinum ag afternoon-theme pyenv-mode-auto markdown-mode smart-hungry-delete docker-compose-mode dockerfile-mode tern-auto-complete doom-themes zenburn-theme eslint-fix company-tern powerline leuven-theme subatomic256-theme gotham-theme atom-dark-theme dracula-theme auto-minor-mode tabbar neotree github-modern-theme exotica-theme melancholy-theme emmet-mode json-mode py-autopep8 importmagic company-anaconda company ac-anaconda auto-complete intellij-theme ample-zen-theme projectile flycheck indent-guide web-mode anaconda-mode pyenv-mode use-package)))
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
 '(vc-annotate-very-old-color "#6a737d")
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Monaco" :height 115))))
 '(mode-line ((t (:background "dark magenta" :box nil))))
 '(powerline-active0 ((t (:inherit mode-line :background "goldenrod" :foreground "black"))))
 '(powerline-active2 ((t (:inherit mode-line :background "navajo white" :foreground "gray0"))))
 '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "SlateGray1")))))

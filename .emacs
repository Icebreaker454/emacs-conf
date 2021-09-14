(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(menu-bar-mode -1)
(toggle-scroll-bar -1) 
(tool-bar-mode -1)

(setq column-number-mode t)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(put 'lsp-pylsp-plugins-jedi-environment 'safe-local-variable #'stringp)
(put 'flycheck-python-pylint-executable 'safe-local-variable #'stringp)

;; BREW's /usr/local is not included in Emacs by default
(add-to-list 'exec-path "/usr/local/bin")

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))
  
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks 
  :config (smart-hungry-delete-add-default-hooks)
  )

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (progn
    (setq lsp-keymap-prefix "C-c l")
    (defvar lsp-language-id-configuration
      '((python-mode . "python")))
    (setq lsp-pylsp-plugins-pylint-enabled t))

  :config
  (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "pylsp")
                  :activation-fn (lsp-activate-on "python")
                  :server-id 'pylsp))
  
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp))
  :bind
  ("M-." . 'lsp-find-definition)
  :commands lsp)

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; 
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))


(use-package pyvenv
  :config
  (pyvenv-mode))

(use-package ivy
  :ensure t
  :demand t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  :bind
  (("\C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   :map projectile-command-map
   ("s" . counsel-projectile-rg)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package company
  :ensure t
  :defer t
  :init
  (global-company-mode)

  :config
    ;; Use Company for completion
  (setq company-tooltip-align-annotations t
          ;; Easy navigation to candidates with M-<n>
        company-show-quick-access t)
  (setq company-dabbrev-downcase nil)
  :diminish company-mode)

(set-face-attribute 'default nil :font "Monaco")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(package-selected-packages
   '(smart-hungry-delete hl-todo python-black pyvenv ag counsel-projectile swiper-helm counsel ivy company flycheck doom-themes lsp-mode doom-modeline dot-mode helm helm-ag helm-projectile projectile use-package))
 '(safe-local-variable-values
   '((eval pyvenv-activate "~/Projects/Ferret/conversario.notifications")
     (lsp-pylsp-plugins-jedi-environment . "/Users/icebreaker/Projects/Ferret/conversario.notifications")
     (lsp-pylsp-plugins-jedi-environment . "~/Projects/Ferret/conversario.notifications"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

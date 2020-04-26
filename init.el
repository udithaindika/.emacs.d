;; Emacs configuration of Uditha Karunaratn

(when (version< emacs-version "26.3")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))
;; Garbage collector - decrease threshold to 5 MB after startup
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;; Update Default Settings
(setq default-directory "~/")
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq visible-bell t)
(setq make-backup-file nil)
(setq auto-save-default nil)

(when window-system (global-hl-line-mode t))
(when window-system (global-prettify-symbols-mode t))


(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)


;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?"
)
(defconst sys/mapc
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defvar my-term-shell "/bin/bash")
(when sys/linuxp (setq my-term-shell "/bin/bash"))
(when sys/win32p (setq my-term-shell "C:\\Program Files\\Git\\bin\\bash.exe"))
(when sys/macp (setq my-term-shell "/bin/bash"))

(defun run-bash ()
      (interactive)
      (let ((shell-file-name my-term-shell))
	(shell "*bash*")))

(defun run-cmdexe ()
      (interactive)
      (let ((shell-file-name "cmd.exe"))
            (shell "*cmd.exe*")))

(defun my-windows-shell-setup () (interactive)
     ;; The variable `git-shell-path' contains the path to the `Git\bin'
     ;; file on my system. I install this in      
     (setq explicit-shell-file-name my-term-shell)
     (setq explicit-bash.exe-args '("--login" "-i"))
     (message "Windows preferences set."))

(if sys/win32p (my-windows-shell-setup))




(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'spacemacs-theme)
  (package-refresh-contents)
  (package-install 'spacemacs-theme))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))



;; Util Functions
(defun my-open-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))


;; Key Binding
(global-set-key (kbd "C-c I") #'my-find-user-init-file)

;;; Org Mode Stuff
;; Enable line wrap in Org mode
(add-hook 'org-mode-hook '(lambda () (visual-line-mode 1)))
(setq org-agenda-files (append
			(file-expand-wildcards  "~/org-notes/*.org")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-dark)))
 '(custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (beacon spacemacs-theme which-key use-package paradox gotest go-projectile gnu-elpa-keyring-update flycheck exec-path-from-shell diminish company-go benchmark-init auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Source Code Pro")))))

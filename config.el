(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(line-number-mode 1)
(column-number-mode 1)

(global-subword-mode 1)

(setq electric-pair-pairs '(
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\{ . ?\})
			    ))
(electric-pair-mode 1)

(setq display-time-24hr-format t)
(display-time-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)

(setq default-directory "~/")

;;Fuck that bell
(setq ring-bell-function 'ignore)

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

(global-display-line-numbers-mode)
(fset 'yes-or-no-p 'y-or-n-p)

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

(setq org-src-window-setup 'current-window)
(add-to-list 'org-structure-template-alist 
'("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Enable line wrap in Org mode
(add-hook 'org-mode-hook '(lambda () (visual-line-mode 1)))
(setq org-agenda-files (append
			(file-expand-wildcards  "~/org-notes/*.org")))

;; Key Binding
(global-set-key (kbd "C-c I") #'my-find-user-init-file)

(setq ido-enable-flex-matching nil)
(setq ido-create-new-buffer 'always)
(setq ido-everywhere t)
(ido-mode 1)

(use-package ido-vertical-mode
	      :ensure t
	      :init
	      (ido-vertical-mode 1))
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(use-package smex
  :ensure t
  :init (smex-initialize)
  :bind ("M-x" . smex ))

;; (global-set-key (kbd "C-x b") 'ido-switch-buffer) - This is the default

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package hungry-delete
  :ensure t
  :config (global-hungry-delete-mode t))

(use-package beacon
  :ensure t
  :init (beacon-mode 1))

(defun kill-whole-word ()
  (interactive)
  (backward-word)
  (kill-word 1))

(global-set-key (kbd "C-c w w") 'kill-whole-word)

(defun my-open-init-file ()
  "Edit the `user-init-file', in another window."
  (interactive)
  (find-file-other-window user-init-file))

(defun copy-whole-line()
  (interactive)
  (save-excursion (kill-new
		   (buffer-substring (point-at-bol)
				     (point-at-eol)))))

(global-set-key (kbd "C-c w l") 'copy-whole-line)

(defun config-visit ()
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(global-set-key (kbd "C-c e") 'config-visit)

(defun config-reload ()
  (interactive)
  (org-babel-load-file (expand-file-name "~/.emacs.d/config.org")))
(global-set-key (kbd "C-c r") 'config-reload)

(substitute-key-definition 'kill-buffer
			   'kill-buffer-and-window
			   global-map)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(defun kill-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))
(global-set-key (kbd "C-c k k") 'kill-all-buffers)

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode 1))

(use-package switch-window
  :ensure t
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-style 'qwerty)
  (setq switch-window-qwerty-shortcuts '("a" "s" "d" "f" "h" "j" "k" "l"))
  :bind ([remap other-window] . switch-window))

(defun split-and-follow-horizontally()
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically()
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents . 10)))
  (setq dashboard-banner-logo-title "Hello World!"))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package spaceline
    :ensure t
    :config
    (require 'spaceline-config)
    (setq powerline-default-separator (quote arrow))
    (spaceline-spacemacs-theme))

(use-package diminish
  :ensure t
  :init
  (diminish 'hungry-delete-mode)
  (diminish 'beacon-mode)
  (diminish 'which-key-mode)
  (diminish 'subword-mode))

(use-package dmenu
  :ensure t
  :bind
  ("C-c d" . dmenu))

(use-package symon
  :ensure t
  :bind ("C-c p" . 'symon-mode))

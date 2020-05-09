(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(use-package use-package-ensure-system-package
  :ensure t)

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

;; This is gonna speed up the keyboard scrolling - https://lists.gnu.org/archive/html/emacs-devel/2006-09/msg00814.html
(setq auto-window-vscroll nil)  

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
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files
(setq create-lockfiles nil)  ; stop creating .# files


(when window-system (global-hl-line-mode t))
(when window-system (global-prettify-symbols-mode t))

(global-display-line-numbers-mode)
(fset 'yes-or-no-p 'y-or-n-p)

(setq visible-cursor nil)
(blink-cursor-mode  `blink-cursor-blinks)

(global-auto-revert-mode nil)

;; check OS type
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)
    (message "Mac OS X")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux"))))

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

(add-hook 'org-mode-hook 'org-indent-mode)

(setq ido-enable-flex-matching t)
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

;; If nothing is marked yanks whole line
(use-package whole-line-or-region
  :ensure t
  :config (whole-line-or-region-global-mode 1))

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
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

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
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 5)
                          (projects . 5)
                          (agenda . 5)
                          (registers . 5)))
  (setq dashboard-banner-logo-title "Hello World!"))

(setq dashboard-center-content t)
(setq dashboard-set-footer nil)

(use-package company
  :ensure t
  :init
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (add-hook 'after-init-hook 'global-company-mode))

(with-eval-after-load 'company
  (define-key company-active-map(kbd "M-n") nil)
  (define-key company-active-map(kbd "M-p") nil)
  (define-key company-active-map(kbd "C-n") #'company-select-next)
  (define-key company-active-map(kbd "C-p") #'company-select-previous))

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

(use-package popup-kill-ring
  :ensure t
  :bind ("M-y" . popup-kill-ring))

(use-package swiper 
    :ensure t
    :bind ("C-s" . swiper))
;; Some hack to make swiper startup faster https://www.reddit.com/r/emacs/comments/cfdv1y/swiper_is_extreamly_slow/
;; By default if you have visual line mode on swiper scans every visual line, which can be really slow in large files. This forces swiper to revert back to searching only every actual line even if the user is using visual line mode
(setq swiper-use-visual-line nil)
(setq swiper-use-visual-line-p (lambda (a) nil))

(use-package mark-multiple
  :ensure t
  :bind ("C-c q" . 'mark-next-like-this))

(use-package expand-region
  :ensure t
  :bind ("C-q" . 'er/expand-region))

(setq test 1)
  ;; JavaScript mode
  ;; Better highlighting for JS files (potential support for JSX too)
  ;; (use-package js2-mode
  ;;   :ensure t
  ;;   :interpreter ("node" . js2-mode)
  ;;   :mode ("\\.m?jsx?\\'" . js2-mode)
  ;;   :config (setq js2-basic-offset 2
  ;;                 js2-indent-switch-body t
  ;;                 js2-strict-missing-semi-warning t
  ;;                 js2-mode-show-strict-warnings nil))
  ;;
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  ;; ;; Better imenu
  ;; (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  ;;
  ;; (use-package prettier-js
  ;;   :after js2-mode
  ;;   :init
  ;;   (add-hook 'js2-mode-hook 'prettier-js-mode)
  ;;   (add-hook 'web-mode-hook 'prettier-js-mode)
  ;;   :config
  ;;   (setq prettier-js-args '("--trailing-comma" "all"
  ;;                            "--bracket-spacing" "false"
  ;;                            "--print-width" "200")))
  ;;
  ;; (add-hook 'prog-mode-hook #'hs-minor-mode)
  ;; (global-set-key (kbd "C-c <right>") 'hs-show-block)
  ;; (global-set-key (kbd "C-c <left>") 'hs-hide-block)
  ;;
  ;;
  ;; ;; (add-hook 'js2-mode-hook
  ;;      (lambda ()
  ;;      (add-hook 'before-save-hook 'prettier-js nil 'make-it-local)))

  ;; Setup the JS as followis
  ;; https://www.youtube.com/watch?v=0zuYCEzrchk

  ;;(setq lsp-keymap-prefix "s-l")

  ;;(use-package lsp-mode
  ;;  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
  ;;         (js2-mode . lsp)
  ;;         ;; if you want which-key integration
  ;;         (lsp-mode . lsp-enable-which-key-integration))
  ;;  :commands lsp)

  ;; optionally
  ;;(use-package lsp-ui :commands lsp-ui-mode)
  ;; if you are helm user
  ;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
  ;; if you are ivy user
  ;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
  ;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

  ;; optionally if you want to use debugger
  ;;(use-package dap-mode)
  ;; (use-package dap-LANGUAGE) to load the dap adapter for your language



;; (use-package js2-mode
;;   :init (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
;;
;; (use-package flycheck
;;   :init
;;   (add-hook 'prog-mode-hook 'flycheck-mode)    ;;  global-flycheck-mode
;;   (setq flycheck-display-errors-delay .4))
;;
;; (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
;;       flycheck-idle-change-delay 0.8)
;; (add-hook 'after-init-hook #'global-flycheck-mode)
;;
;;
;; (use-package company
;;   :init (add-hook 'prog-mode-hook 'company-mode)
;;   :config (setq company-tooltip-align-annotations t) ;; aligns annotation to the right hand side
;;   (setq company-minimum-prefix-length 1))
;;
;; (use-package lsp-mode
;;   :commands lsp
;;   :hook
;;   (js2-mode . #'lsp))
;;
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :hook
;;   (lsp-mode . lsp-ui-mode))
;;
;; (use-package company-lsp
;;   :commands company-lsp)
;;
;; (push 'company-lsp company-backends)

;;  (use-package projectile
  ;;    :ensure t
  ;;    :config
  ;;    (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  ;;    (projectile-mode +1 ))

  (use-package projectile
    :init (progn
            (setq projectile-enable-caching t)
            (setq projectile-indexing-method 'hybrid)
            (setq projectile-globally-ignored-directories '("node_modules" "node_modules/" "dist" "dist/" "coverage" ))
            (setq projectile-ignored-directories '("_output" "node_modules" "node_modules/" "pkg" "dist" "dist/" "dist/js" "coverage" ""))
            (setq projectile-ignored-files '(".DS_Store" ".gitmodules" "package-lock.json" "yarn.lock" ".svg" "#" "~" "yarn-error.log" ".log" "*log" "yarn*"))
            )
    :bind (
           ("C-x p" . projectile-command-map))
    :config (projectile-mode 1))


  ;; Hybrid is slower than alein but use both native and git indexing
;;  (setq projectile-indexing-method 'hybrid)
  ;; Alien is the Fastest, Where it checkes the Git for File Indexing
  ;;(setq projectile-indexing-method 'alien)
  (setq projectile-git-submodule-command nil);; This is to support the Git Indexing, Without this it will fail

(use-package magit
  :ensure t
  :config
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  :bind
  ("M-g" . magit-status))

(setq vc-handled-backends nil)
(setq magit-refresh-status-buffer nil)

(use-package all-the-icons
  :ensure t)

;; https://stackoverflow.com/questions/13625080/looking-forward-a-way-to-make-cursor-blinks-like-a-heartbeat-in-emacs
 (require 'cl)
  (require 'color)

  (defvar heartbeat-fps 16)
  (defvar heartbeat-period 5)

  (defun heartbeat-range (from to cnt)
    (let ((step (/ (- to from) (float cnt))))
      (loop for i below cnt collect (+ from (* step i)))))

  (defun heartbeat-cursor-colors ()
    (let ((cnt (* heartbeat-period heartbeat-fps)))
      (mapcar (lambda (r)
                (color-rgb-to-hex r 0 0))
              (nconc (heartbeat-range .2 1 (/ cnt 2))
                     (heartbeat-range 1 .2 (/ cnt 2))))))

  (defvar heartbeat-cursor-timer nil)
  (defvar heartbeat-cursor-old-color)

  (define-minor-mode heartbeat-cursor-mode
    "Change cursor color with the heartbeat effect."
    nil "" nil
    :global t
    (when heartbeat-cursor-timer
      (cancel-timer heartbeat-cursor-timer)
      (setq heartbeat-cursor-timer nil)
      (set-face-background 'cursor heartbeat-cursor-old-color))
    (when heartbeat-cursor-mode
      (setq heartbeat-cursor-old-color (face-background 'cursor)
            heartbeat-cursor-timer
            (run-with-timer
             0 (/ 1 (float heartbeat-fps))
             (lexical-let ((colors (heartbeat-cursor-colors)) tail)
               (lambda ()
                 (setq tail (or (cdr tail) colors))
                 (set-face-background 'cursor (car tail))))))))

(use-package async
  :ensure t
  :init (dired-async-mode 1))

(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))

(global-set-key (kbd "C-c 1")
                (lambda()
                  (interactive)
                  (rotate-windows 1)))


(global-set-key (kbd "C-c 2")
                (lambda()
                  (interactive)
                  (rotate-windows -1)))

(use-package js2-mode
  :ensure t
  :mode ("\\.js" . js2-mode)
  :init
  (setq js2-global-externs '("it" "afterEach" "beforeEach" "before" "after" "describe" "require" "module"))
  :config
  (setq-default js2-basic-offset 2)
  (setq js-indent-level 2))


(use-package json-mode
  :ensure t
  :mode ("\\.json" . json-mode))

(use-package js2-refactor
  :ensure t
  :requires js2-mode)

;;======================== LSP Mode ====================

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-;")
  :hook ((js2-mode . lsp)
         (yaml-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp lsp-deferred
  :ensure-system-package
  ((typescript-language-server . "npm install -g typescript-language-server")
   (javascript-typescript-langserver . "npm install -g javascript-typescript-langserver")
   (yaml-language-server . "npm install -g yaml-language-server")
   (tsc . "npm install -g typescript")))

(use-package lsp-ui
  :ensure t
  ;; flycheck integration & higher level UI modules
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  ;; company-mode completion
  :commands company-lsp
  :config (push 'company-lsp company-backends))

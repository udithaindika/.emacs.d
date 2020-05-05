;; Emacs configuration of Uditha Karunaratn

(when (version< emacs-version "26.0")
  (warn "This configuration needs Emacs trunk, but this is %s!" emacs-version))

;; Increase the garbage collection threshold to 500 MB to ease startup
(setq gc-cons-threshold (* 500 1024 1024))
;; Garbage collector - decrease threshold to 5 MB after startup
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

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

(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

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
    (prettier-js js2-mode expand-region mark-multiple swiper popup-kill-ring symon dmenu spaceline dashboard rainbow-delimiters hungry-delete hugry-delete switch-window avy smex ido-vertical-mode org-bullets beacon spacemacs-theme which-key use-package paradox gotest go-projectile gnu-elpa-keyring-update flycheck exec-path-from-shell diminish company-go benchmark-init auto-package-update))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "outline" :family "Source Code Pro")))))



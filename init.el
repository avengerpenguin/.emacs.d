;;; init -- my init
;;; Commentary:
;;; Code:
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t
              use-package-always-ensure t
              indent-tabs-mode nil
              tab-width 2
              css-indent-offset 2)

(fset 'yes-or-no-p 'y-or-n-p)

(setq make-backup-files nil
      auto-save-default nil
      inhibit-splash-screen t
      confirm-kill-emacs 'yes-or-no-p
      epg-gpg-program "/usr/local/bin/gpg"
      visible-bell nil)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

(setq exec-path (append exec-path '("/usr/local/bin")))

(use-package flycheck
  :defer 1
  :config (progn
            (setq flycheck-global-modes '(rjsx-mode emacs-lisp-mode))
            ;;https://github.com/flycheck/flycheck/issues/1129#issuecomment-319600923
            (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))))

(use-package projectile
  :config (progn
            (projectile-global-mode)
            (setq projectile-enable-caching nil)
            (setq projectile-completion-system 'ivy)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config (progn
            (add-hook 'markdown-mode-hook 'visual-line-mode)
            (add-hook 'markdown-mode-hook (lambda () (flyspell-mode 1)))))

(use-package which-key
  :defer 1
  :config (which-key-mode))

(use-package reveal-in-osx-finder)

(use-package emmet-mode
  :defer 1
  :config (progn
            (setq emmet-move-cursor-between-quotes t)
            (add-hook 'sgml-mode-hook 'emmet-mode)
            (add-hook 'css-mode-hook  'emmet-mode)))

(setq dired-dwim-target t
      dired-recursive-deletes t
      dired-use-ls-dired nil
      delete-by-moving-to-trash t)

(use-package editorconfig
  :defer 1
  :config (editorconfig-mode 1))

(use-package flyspell
  :defer 1
  :config (progn
            (setq flyspell-issue-message-flag nil)))

(use-package ivy
  :defer 1
  :config (progn
            (ivy-mode)
            (setq ivy-use-virtual-buffers t)
            (setq ivy-count-format "")
            (setq ivy-use-selectable-prompt t)))

(use-package counsel
  :defer 1
  :config (progn
            (global-set-key (kbd "M-x") 'counsel-M-x)))

(use-package swiper :defer 1)

(use-package vlf)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eslint-fix jira-markup-mode graphviz-dot-mode dot-mode ivy auto-complete markdown-mode org vlf editorconfig emmet-mode reveal-in-osx-finder which-key rjsx-mode zenburn-theme yaml-mode whitespace-cleanup-mode web-mode use-package unkillable-scratch scala-mode rich-minority projectile php-mode php+-mode org-plus-contrib markdown-mode+ magit indent-guide htmlize groovy-mode flymake-yaml flycheck feature-mode expand-region exec-path-from-shell ein dockerfile-mode counsel comment-dwim-2 bbdb auto-org-md))))

;; Allow hash to be entered
(defun insert-pound ()
  "Insert a pound into the buffer."
  (insert "#"))
(global-set-key (kbd "M-3") '(lambda()(interactive)(insert-pound)))

(package-initialize)

(if (eq system-type 'darwin)            ;installed with homebrew
   (require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
 (require 'cask "~/.cask/cask.el"))
(cask-initialize)

(when (not (cl-remove-if-not
	    (lambda (p) (equal 'org (car p)))
	    package-alist))
 (message "No org-mode package found; installing now...")
 (package-install 'org))


(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/")))

(defun recker/package-init ()
  "Initialize the package manager and install use-package."
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

(recker/package-init)
(org-babel-load-file "~/.emacs.d/README.org")
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(provide 'init)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Fira Code")))))

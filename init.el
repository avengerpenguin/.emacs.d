(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

(require 'package)                   ; Bring in to the environment all package management functions

;; A list of package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))

(package-initialize)                 ; Initializes the package system and prepares it to be used

(unless package-archive-contents     ; Unless a package archive already exists,
  (package-refresh-contents))        ; Refresh package contents so that Emacs knows which packages to load


;; Initialize use-package on non-linux platforms
(unless (package-installed-p 'use-package)        ; Unless "use-package" is installed, install "use-package"
  (package-install 'use-package))

(require 'use-package)                            ; Once it's installed, we load it using require

;; Make sure packages are downloaded and installed before they are run
;; also frees you from having to put :ensure t after installing EVERY PACKAGE.
(setq use-package-always-ensure t)


(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Double-spaces after periods is morally wrong.
 sentence-end-double-space nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Let C-k delete the whole line.
 kill-whole-line t
 ;; search should be case-sensitive by default
 case-fold-search nil
 default-directory "~/"
)

;; Allow hash to be entered
(defun insert-pound ()
  "Insert a pound into the buffer."
  (insert "#"))
(global-set-key (kbd "M-3") '(lambda()(interactive)(insert-pound)))

;; Never mix tabs and spaces. Never use tabs, period.
;; We need the setq-default here because this becomes
;; a buffer-local variable when set.
(setq-default indent-tabs-mode nil)


(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))


(delete-selection-mode t)
(global-display-line-numbers-mode t)
(column-number-mode)

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

(setq custom-safe-themes t)
(use-package doom-themes
  :config
  (let ((chosen-theme 'doom-oceanic-next))
    (doom-themes-visual-bell-config)
    (doom-themes-org-config)
    (setq doom-challenger-deep-brighter-comments t
          doom-challenger-deep-brighter-modeline t)
    (load-theme chosen-theme)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" default))
 '(package-selected-packages
   '(jedi graphviz-dot-mode feature-mode ivy lua-mode plantuml-mode ttl-mode web-mode all-the-icons-dired all-the-icons use-package doom-themes))
 '(send-mail-function 'sendmail-send-it))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(ignore-errors (set-frame-font "Menlo-14"))

(use-package all-the-icons)

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(add-to-list 'default-frame-alist '(fullscreen . maximized))


(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))


(show-paren-mode)

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package magit
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :bind (("C-c g" . #'magit-status))
  :custom
  (magit-repository-directories '(("~/workspace" . 1)))
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package forge
  :after magit)

(use-package projectile
  :diminish
  :bind (("C-c k" . #'projectile-kill-buffers)
  ("C-c M" . #'projectile-compile-project))
  :custom (projectile-completion-system 'ivy)
  :config (projectile-mode))
(define-key projectile-mode-map (kbd "s-,") 'projectile-command-map)
(define-key projectile-mode-map [?\s-d] 'projectile-find-dir)
(define-key projectile-mode-map [?\s-p] 'projectile-switch-project)
(define-key projectile-mode-map [?\s-f] 'projectile-find-file)
(define-key projectile-mode-map [?\s-g] 'projectile-grep)

(use-package blacken
  :hook ((python-mode . blacken-mode)))
(use-package jedi)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t) 

(setq jedi:environment-virtualenv
      (list "/usr/local/opt/python@3.10/bin/python3" "-m" "venv"))


(use-package typescript-mode)
(setq js-indent-level 2)

(use-package web-mode)
(use-package dockerfile-mode)
(use-package markdown-mode
  :bind (("C-c C-s a" . markdown-table-align))
  :mode ("\\.md$" . gfm-mode))

(use-package yaml-mode)
(use-package dockerfile-mode)
(use-package toml-mode)

(use-package neotree)
(global-set-key [f8] 'neotree-toggle)


(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

(if (eq system-type 'darwin)
    (setq mu4e-mu-binary "/usr/local/bin/mu")
  (setq mu4e-mu-binary "/usr/bin/mu"))
(setq mu4e-get-mail-command "/usr/local/bin/offlineimap")
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      smtpmail-debug-verb t)

(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Personal"
           :enter-func (lambda () (mu4e-message "Entering Personal context"))
           :leave-func (lambda () (mu4e-message "Leaving Personal context"))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/Personal" (mu4e-message-field msg :maildir))))
           :vars '(
                   (user-mail-address	    . "post@rossfenning.co.uk"  )
                   (user-full-name	    . "Ross Fenning" )
                   (mu4e-trash-folder . "/Personal/Trash")
                   (mu4e-refile-folder . "/Personal/Archives")
                   (mu4e-sent-folder . "/Personal/sent-mail")
                   (mu4e-spam-folder . "/Personal/spam")
                   (smtpmail-smtp-user . "post@rossfenning.co.uk")
                   (smtpmail-smtp-server . "mail.rossfenning.co.uk")
                   (smtpmail-smtp-service . 25)
                   ))
         ,(make-mu4e-context
           :name "Work"
           :enter-func (lambda () (mu4e-message "Switch to the BBC context"))
           :match-func (lambda (msg)
                         (when msg
                           (string-match-p "^/BBC" (mu4e-message-field msg :maildir))))
           :vars '(
                   (user-mail-address	     . "ross.fenning@bbc.co.uk" )
                   (user-full-name	     . "Ross Fenning" )
                   (mu4e-trash-folder . "/BBC/Trash")
                   (mu4e-refile-folder . "/BBC/Archive")
                   (mu4e-sent-folder . "/Personal/Sent")
                   (mu4e-spam-folder . "/BBC/Junk")
                   (smtpmail-smtp-user . "fennir01")
                   (smtpmail-smtp-server . "localhost")
                   (smtpmail-smtp-service . 1025)
                   ))
         ))
(global-set-key (kbd "<f9>") 'mu4e)

(add-to-list 'mu4e-marks
             '(spam
               :char       "J"
               :prompt     "Spam"
               :show-target (lambda (target) mu4e-spam-folder)
               :action      (lambda (docid msg target)
                              (mu4e~proc-move docid mu4e-spam-folder "+S-u-N"))))

(mu4e~headers-defun-mark-for spam)
(define-key mu4e-headers-mode-map (kbd "J") 'mu4e-headers-mark-for-spam)
(mu4e~view-defun-mark-for spam)
(define-key mu4e-view-mode-map (kbd "J") 'mu4e-view-mark-for-spam)

(setq mu4e-headers-date-format "%Y-%m-%d %H:%M")
(setq mu4e-headers-fields
      '( (:date          .  24)
         (:flags         .   6)
         (:from          .  22)
         (:subject       .  nil)))

(setq mu4e-context-policy 'pick-first)
(setq mu4e-compose-context-policy nil)
(setq message-kill-buffer-on-exit t)
(setq mu4e-view-show-addresses 't)
(setq mu4e-update-interval 900)

(setq mu4e-bookmarks
      `(
        ("maildir:/Personal/INBOX"     "Personal"             ?p)
        ("maildir:/BBC/INBOX"          "Work"                 ?w)
        ))


(setq mu4e-html2text-command 'mu4e-shr2text)
(setq shr-color-visible-luminance-min 60)
(setq shr-color-visible-distance-min 5)
(setq shr-use-colors nil)


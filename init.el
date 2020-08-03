(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'tramp)

;; This sets $MANPATH, $PATH and exec-path from your shell, but only when executed in a GUI frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck-rust rust-mode eslint-fix evil-smartparens smartparens doom-modeline smart-hungry-delete yaml-mode vterm which-key wgrep dashboard counsel-projectile counsel lsp-ivy doom-themes tide yasnippet flycheck lsp-treemacs company lsp-ui lsp-mode gruvbox-theme evil-leader projectile evil-magit magit evil exec-path-from-shell js2-mode web-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; example of setting env var named “path”, by appending a new path to existing path

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;; Settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(set-frame-font "Source Code Pro 12" nil t)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq-default indent-tabs-mode nil)
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-script-padding 0)
(setq web-mode-style-padding 0)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Evil
(setq evil-want-C-u-scroll t)
(global-evil-leader-mode) ;; Enable evil-leader in every buffer where evil is enabled
(evil-mode 1)
(require 'evil-magit)

(use-package vterm
    :ensure t)

(defun my-open-init-file ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
    "ff" 'counsel-find-file
    "fed" 'my-open-init-file
    "<SPC>" 'counsel-M-x
    "pp" 'counsel-projectile-switch-project
    "pf" 'counsel-projectile-find-file
    "pd" 'counsel-projectile-find-dir
    "?"  'counsel-rg
    "/" 'counsel-projectile-rg
    "pb" 'counsel-projectile-switch-to-buffer
    "p'" 'projectile-run-vterm
    "bb" 'ivy-switch-buffer
    "[" 'previous-buffer
    "]" 'next-buffer

    "ef" 'eslint-fix

    ; G
    ";" 'comment-line
    
    ; Registers
    "rv" 'view-register
    "rs" 'copy-to-register
    "ri" 'insert-register
    "ry" 'counsel-yank-pop
    "rr" 'rust-run

    ; Window / Web
    "ww" 'select-window
    "ws" 'split-window-right
    "wS" 'split-window-below
    "wd" 'delete-window
    "wD" 'delete-other-windows
    "wb" 'eww ; Browse

    ; Manual / Macros
    "mm" 'info
    "ms" 'kmacro-start-macro-or-insert-counter ; Start
    "me" 'kmacro-end-or-call-macro ; End/Execute

    ; Help
    "hs" 'sp-cheat-sheet

    ; Navigate
    "nh" 'windmove-left
    "nk" 'windmove-up
    "nj" 'windmove-down
    "nl" 'windmove-right

    ; Git
    "gs" 'magit-status

    ; Shell
    "'" 'open-terminal-in-workdir
    )

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

; Counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;; (load-theme 'doom-city-lights t)
(load-theme 'doom-tomorrow-day t)
(doom-themes-visual-bell-config)

; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


;; Lsp Mode
(require 'lsp-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks)
  )

(use-package web-mode
  :mode (("\\.vue" . web-mode))
  :hook ((web-mode . (lambda()
		       (flycheck-mode)
		       (lsp))))
  :config
  (flycheck-add-next-checker 'lsp 'javascript-eslint 'append))

(use-package js2-mode
  :mode (("\\.js$" . js2-mode))
  :hook ((js2-mode . (lambda ()
		       (flycheck-mode)
		       (lsp))))
  :config
  (flycheck-add-next-checker 'lsp 'javascript-eslint 'append)
  )

(use-package typescript-mode
  :mode (("\\.ts" . typescript-mode))
  :hook ((typescript-mode . (lambda ()
			      (flycheck-mode)
			      (lsp))))

  :config
  (flycheck-add-next-checker 'lsp 'javascript-eslint 'append)
  )

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))

;; use eslint with web-mode for vue files
(flycheck-add-mode 'javascript-eslint 'web-mode)


(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
	  '(json-jsonlist)))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
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

(which-key-mode)

(require 'smartparens-config)
(smartparens-global-mode t)

(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)

(use-package rust-mode
  :hook (rust-mode . lsp))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; init.el ends here

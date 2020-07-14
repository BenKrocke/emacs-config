;; example of setting env var named “path”, by appending a new path to existing path

;; Web Mode
(add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))

(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)
;; Settings
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; Evil
(setq evil-want-C-u-scroll t)
(global-evil-leader-mode) ;; Enable evil-leader in every buffer where evil is enabled
(evil-mode 1)
(require 'evil-magit)

(evil-leader/set-leader "<SPC>")
(evil-leader/set-key
    "ff" 'find-file
    "fed" 'my-open-init-file
    "<SPC>" 'execute-extended-command
    "pp" 'helm-projectile-switch-project
    "pf" 'helm-projectile-find-file
    "pF" 'helm-projectile-find-file-in-known-projects
    "pr" 'helm-projectile-recentf
    "pd" 'helm-projectile-find-dir
    "/" 'helm-projectile-ag
    "pb" 'helm-projectile-switch-to-buffer
    "bb" 'helm-buffers-list
    "[" 'previous-buffer
    "]" 'next-buffer
    
    ; Registers
    "rv" 'view-register
    "rs" 'copy-to-register
    "ri" 'insert-register

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

    ; Navigate
    "nh" 'windmove-left
    "nk" 'windmove-up
    "nj" 'windmove-down
    "nl" 'windmove-right

    ; Git
    "gs" 'magit-status
    "gcc" 'comment-dwim

    ; Shell
    "'" 'open-terminal-in-workdir
)

;; Helm
;; Here goes config


;; Load helm
(with-eval-after-load "helm.el"
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "C-z") 'helm-select-action)
)
(helm-mode 1)

(load-theme 'gruvbox-light-hard t)

;; set transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(helm-projectile-on)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

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

;; This sets $MANPATH, $PATH and exec-path from your shell, but only when executed in a GUI frame on OS X and Linux.
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;;; init.el ends here

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(setq package-list
      '(cc-mode
        ido
        compile
        lsp-mode
        go-mode
        helm))
(package-initialize)

(setenv "PATH" (concat (getenv "PATH")))

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


;; sample `helm' configuration use https://github.com/emacs-helm/helm/ for details
(helm-mode)
(require 'helm)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

;; (which-key-mode)
;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)


(load-theme 'zenburn t)

(setq tab-width 4 indent-tabs-mode nil)

(setq line-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode)

; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(scroll-bar-mode -1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)

(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)

(require 'lsp-mode)

(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

; Turn off the bell sound on "errors"
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"    . c++-mode)
         ("\\.hin$"    . c++-mode)
         ("\\.cin$"    . c++-mode)
         ("\\.inl$"    . c++-mode)
         ("\\.rdc$"    . c++-mode)
         ("\\.h$"    . c++-mode)
         ("\\.c$"   . c++-mode)
         ("\\.cc$"   . c++-mode)
         ("\\.c8$"   . c++-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$" . gen-mode)
         ("\\.ms$" . fundamental-mode)
         ("\\.m$" . objc-mode)
         ("\\.mm$" . objc-mode)
         ("\\.go$" . go-mode)
         ) auto-mode-alist))


(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)

(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)

(define-key global-map "\eg" 'goto-line)
(define-key global-map (kbd "C-x g") 'goto-line)
(define-key global-map (kbd "C-x C-g") 'goto-line)

(define-key global-map (kbd "C-x C-c") 'kill-ring-save)
(define-key global-map (kbd "C-x C-x") 'kill-region)
(define-key global-map (kbd "C-x C-v") 'yank)

(define-key global-map "\ew" 'other-window)

(define-key global-map [C-tab] 'indent-region)


(defun enter-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<C-return>") 'enter-line-below)

(defun enter-line-above ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "<C-S-return>") 'enter-line-above)

(global-set-key (kbd "C-S-c") 'comment-region)
(global-set-key (kbd "C-S-u") 'uncomment-region)

; Autoclose bracket, braces and parenthesis.
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)


(load-file "~/.emacs.d/lsp-go.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(grails-mode ggo-mode helm-xref lsp-mode zenburn-theme yaml-mode tuareg toml-mode tide smex scala-mode rust-mode racket-mode qml-mode purescript-mode proof-general powershell php-mode paredit org-cliplink nix-mode nim-mode nginx-mode nasm-mode multiple-cursors move-text markdown-mode magit love-minor-mode kotlin-mode js2-mode jinja2-mode ido-completing-read+ hindent helm-ls-git helm-git-grep helm-cmd-t haskell-mode gruber-darker-theme graphviz-dot-mode go-mode glsl-mode elpy editorconfig dockerfile-mode dash-functional d-mode csharp-mode cmake-mode clojure-mode ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

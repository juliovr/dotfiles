(setq tab-width 4
      indent-tabs-mode nil)

(setq line-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode)

; Stop Emacs from losing undo information by
; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

(scroll-bar-mode -1)
(tool-bar-mode 0)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(load-theme 'zenburn t)

(require 'cc-mode)
(require 'ido)
(require 'compile)
(ido-mode t)


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

(define-key global-map (kbd "C-x C-v") 'yank)

(define-key global-map "\ew" 'other-window)

(define-key global-map [C-tab] 'indent-region)

(defun enter-previous-line ()
  (interactive)
  (move-beginning-of-line)
  (open-line)
  )

(define-key global-map [C-o] 'enter-previous-line)

(require 'cc-mode)
(require 'compile)
(require 'ido)

;; Default modes
(ido-mode t)

(scroll-bar-mode -1)
(tool-bar-mode 0)
(menu-bar-mode 0)
(column-number-mode 1)
(show-paren-mode 1)
(delete-selection-mode 1)
(global-auto-revert-mode t)

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(global-visual-line-mode 1)

;; (global-whitespace-mode 1)

;; see the apropos entry for whitespace-style
(setq
   whitespace-style
   '(face ; viz via faces
     trailing ; trailing blanks visualized
      space-before-tab
     space-after-tab
     newline ; lines with only blanks
     indentation ; spaces used for indent
                 ; when config wants tabs
     empty ; empty lines at beginning or end
     )
   whitespace-line-column 100 ; column at which
        ; whitespace-mode says the line is too long
)

;; Highlight current line
(global-hl-line-mode 1)

(abbrev-mode 1)

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

(setq c-default-style "linux"
          c-basic-offset 4)

(defun make-without-asking ()
  "Make the current build."
  (interactive)
  (compile "build.bat")
  (other-window 1))
(define-key global-map "\em" 'make-without-asking)
(define-key global-map "\eb" 'make-without-asking)

;; Make the backspace properly erase the tab instead of removing 1 space at a time.
(setq backward-delete-char-untabify-method 'hungry)

(setq line-number-mode t)
(add-hook 'prog-mode-hook 'linum-mode)

;; Autoclose bracket, braces and parenthesis.
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;; Making electric-indent behave sanely
(setq-default electric-indent-inhibit t)

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Startup windowing
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq next-line-add-newlines nil)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(split-window-horizontally)

(setq backup-directory-alist `(("." . "~/.emacs.d/auto-saves/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-saves/" t)))

;; Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'julius t)
;;(load-theme 'flatland t)

;;(set-face-attribute 'default nil :font "Consolas-12")
(set-face-attribute 'default nil :font "Liberation Mono-12")


;; Turn off the bell sound on "errors"
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)

;; Customizations for C++ mode
(setq-default c-basic-offset 4)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq tab-width 4)
            (setq indent-tabs-mode nil)))

;; Accepted file extensions and their appropriate modes
(setq auto-mode-alist
      (append
       '(("\\.cpp$"   . c++-mode)
         ("\\.hin$"   . c++-mode)
         ("\\.cin$"   . c++-mode)
         ("\\.inl$"   . c++-mode)
         ("\\.rdc$"   . c++-mode)
         ("\\.h$"     . c++-mode)
         ("\\.c$"     . c++-mode)
         ("\\.cc$"    . c++-mode)
         ("\\.c8$"    . c++-mode)
         ("\\.txt$"   . indented-text-mode)
         ("\\.emacs$" . emacs-lisp-mode)
         ("\\.gen$"   . gen-mode)
         ("\\.ms$"    . fundamental-mode)
         ("\\.m$"     . objc-mode)
         ("\\.mm$"    . objc-mode)
         ("\\.go$"    . go-mode)
         ("\\.python$". python-mode)
         ) auto-mode-alist))


;; ==================================
;; Keys
;; ==================================
;; IMPORTANT: \e could be ESC or ALT
(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)
(define-key global-map "\en" 'next-error)
(define-key global-map "\eN" 'previous-error)
(define-key global-map "\eg" 'goto-line)
(define-key global-map "\ej" 'imenu)
(define-key global-map "\ew" 'other-window)
(define-key global-map "\eo" 'query-replace)
(define-key global-map "\e;" 'exchange-point-and-mark)
;;(define-key global-map "\eO" 'casey-replace-string)

(define-key global-map [f9] 'first-error)
(define-key global-map [f10] 'previous-error)
(define-key global-map [f11] 'next-error)

(define-key global-map (kbd "C-o") 'find-file)
(define-key global-map (kbd "C-x g") 'goto-line)
(define-key global-map (kbd "C-x C-g") 'goto-line)
(define-key global-map (kbd "C-x C-c") 'kill-ring-save)
(define-key global-map (kbd "C-c C-c") 'kill-ring-save)
(define-key global-map (kbd "C-x C-x") 'kill-region)
(define-key global-map (kbd "C-x C-v") 'yank)
(define-key global-map (kbd "C-v") 'yank)
(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)


(defun casey-find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq CorrespondingFileName nil)
  (setq BaseFileName (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat BaseFileName ".c")) (setq CorrespondingFileName (concat BaseFileName ".c"))
	    (setq CorrespondingFileName (concat BaseFileName ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq CorrespondingFileName (concat BaseFileName ".h")))
  (if CorrespondingFileName (find-file CorrespondingFileName)
    (error "Unable to find a corresponding file")))
(defun casey-find-corresponding-file-other-window ()
  "Find the file that corresponds to this one."
  (interactive)
  (find-file-other-window buffer-file-name)
  (casey-find-corresponding-file)
  (other-window -1))
(define-key c++-mode-map [f12] 'casey-find-corresponding-file)
(define-key c++-mode-map [M-f12] 'casey-find-corresponding-file-other-window)

; Alternate bindings for F-keyless setups (ie MacOS X terminal)
(define-key c++-mode-map "\ec" 'casey-find-corresponding-file)
(define-key c++-mode-map "\eC" 'casey-find-corresponding-file-other-window)


(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-S-c") 'comment-region)
(global-set-key (kbd "C-S-u") 'uncomment-region)

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

(defun my-kill-back ()
  (interactive)
  (if (bolp)  ; beginnning of line, just delete 1
      (backward-delete-char 1)
    (if (string-match "[^[:space:]]" (buffer-substring (point-at-bol) (point)))
        ; There's a word on the line, delete it
        (backward-kill-word 1)
      (delete-region (point-at-bol) (point))))) ; all whitespace, delete it
(global-set-key [C-backspace] 'my-kill-back)



;; Navigation
(defun previous-blank-line ()
  "Moves to the previous line containing nothing but whitespace."
  (interactive)
  (search-backward-regexp "^[ \t]*\n")
)

(defun next-blank-line ()
  "Moves to the next line containing nothing but whitespace."
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1)
)

(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)


(defun move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
            (exchange-point-and-mark))
     (let ((column (current-column))
              (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
         (transpose-lines arg))
       (forward-line -1)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [\M-up] 'move-text-up)
(global-set-key [\M-down] 'move-text-down)

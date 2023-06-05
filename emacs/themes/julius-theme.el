;; My color scheme based on Nick Aversano's https://github.com/nickav/naysayer-theme.el

(unless (>= emacs-major-version 24)
  (error "The julius theme requires Emacs 24 or later!"))

(deftheme julius "The julius color theme")

;; Monokai colors
(defcustom julius-theme-yellow "#E6DB74" "Primary colors - yellow" :type 'string :group 'monokai)
(defcustom julius-theme-orange "#FD971F" "Primary colors - orange" :type 'string :group 'monokai)
(defcustom julius-theme-red "#F92672" "Primary colors - red" :type 'string :group 'monokai)
(defcustom julius-theme-magenta "#FD5FF0" "Primary colors - magenta" :type 'string :group 'monokai)
(defcustom julius-theme-blue "#66D9EF" "Primary colors - blue" :type 'string :group 'monokai)
(defcustom julius-theme-green "#A6E22E" "Primary colors - green" :type 'string :group 'monokai)
(defcustom julius-theme-cyan "#A1EFE4" "Primary colors - cyan" :type 'string :group 'monokai)
(defcustom julius-theme-violet "#AE81FF" "Primary colors - violet" :type 'string :group 'monokai)

(let ((background "#161616") ;;(background "#010d0f") #232424
      (gutters    "#062329")
      (gutter-fg  "#ebcfab")
      (gutters-active "#ebcfab")
      (builtin      "#ffffff")
      (selection  "#868691")
      (text       "#ebcfab")
;;      (comments   "#44b340")
      (comments "#808080")
;;      (punctuation "#8cde94")
      (punctuation "#c99b10")
      (keywords "#ffffff")
      (variables "#c1d1e3")
      (functions "#ffffff")
      (methods    "#90b1d6")
      (strings    "#2ec09c")
      (constants "#7ad0c6")
      (macros "#8cde94")
      (numbers "#7ad0c6")
      (white     "#ffffff")
      (error "#ff0000")
      (warning "#ffaa00")
      (highlight-line "#0b3335")
      (line-fg "#126367"))

  (custom-theme-set-faces
   'julius

   ;; Default colors
   ;; *****************************************************************************

   `(default                          ((t (:foreground ,text :background ,background, :weight normal))))
   `(region                           ((t (:foreground nil :background ,selection))))
   `(cursor                           ((t (:background ,"#40FF40"))))
   `(fringe                           ((t (:background ,background   :foreground ,white))))
   `(linum                            ((t (:background ,background :foreground ,gutter-fg))))
   `(highlight                        ((t (:foreground nil :background ,selection))))

   ;; Font lock faces
   ;; *****************************************************************************

   `(font-lock-keyword-face           ((t (:foreground ,keywords))))
   `(font-lock-type-face              ((t (:foreground ,punctuation))))
   `(font-lock-constant-face          ((t (:foreground ,constants))))
   `(font-lock-variable-name-face     ((t (:foreground ,variables))))
   `(font-lock-builtin-face           ((t (:foreground ,builtin))))
   `(font-lock-string-face            ((t (:foreground ,strings))))
   `(font-lock-comment-face           ((t (:foreground ,comments))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,comments))))
   `(font-lock-doc-face               ((t (:foreground ,comments))))
   `(font-lock-function-name-face     ((t (:foreground ,functions))))
   `(font-lock-doc-string-face        ((t (:foreground ,strings))))
   `(font-lock-preprocessor-face      ((t (:foreground ,macros))))
   `(font-lock-warning-face           ((t (:foreground ,warning))))

   ;; Plugins
   ;; *****************************************************************************
   `(trailing-whitespace ((t (:foreground nil :background ,warning))))
   ;;`(whitespace-trailing ((t (:background nil :foreground ,warning :inverse-video t))))

   `(linum                       ((t (:foreground ,line-fg :background ,background))))
   `(linum-relative-current-face ((t (:foreground ,white :background ,background))))
   `(line-number                 ((t (:foreground ,line-fg :background ,background))))
   `(line-number-current-line    ((t (:foreground ,white :background ,background))))

   ;; hl-line-mode
   `(hl-line      ((t (:background ,highlight-line))))
   `(hl-line-face ((t (:background ,highlight-line))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face  ((t (:foreground ,julius-theme-violet))))
   `(rainbow-delimiters-depth-2-face  ((t (:foreground ,julius-theme-blue))))
   `(rainbow-delimiters-depth-3-face  ((t (:foreground ,julius-theme-green))))
   `(rainbow-delimiters-depth-4-face  ((t (:foreground ,julius-theme-yellow))))
   `(rainbow-delimiters-depth-5-face  ((t (:foreground ,julius-theme-orange))))
   `(rainbow-delimiters-depth-6-face  ((t (:foreground ,julius-theme-red))))
   `(rainbow-delimiters-depth-7-face  ((t (:foreground ,julius-theme-violet))))
   `(rainbow-delimiters-depth-8-face  ((t (:foreground ,julius-theme-blue))))
   `(rainbow-delimiters-depth-9-face  ((t (:foreground ,julius-theme-green))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,julius-theme-yellow))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,julius-theme-orange))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,julius-theme-red))))

   ;; mode-line and powerline
   `(mode-line-buffer-id ((t (:foreground ,background :distant-foreground ,text :text ,text :weight bold))))
   `(mode-line ((t (:inverse-video unspecified
                                   :underline unspecified
                                   :foreground ,background
                                   :background ,text
                                   :box nil))))
   `(powerline-active1 ((t (:background ,text :foreground ,background))))
   `(powerline-active2 ((t (:background ,text :foreground ,background))))

   `(mode-line-inactive ((t (:inverse-video unspecified
                                            :underline unspecified
                                            :foreground ,text
                                            :background ,background
                                            :box nil))))
   `(powerline-inactive1 ((t (:background ,background :foreground ,text))))
   `(powerline-inactive2 ((t (:background ,background :foreground ,text))))

   ;; js2-mode
   `(js2-function-call ((t (:inherit (font-lock-function-name-face)))))
   `(js2-function-param ((t (:foreground ,text))))
   `(js2-jsdoc-tag ((t (:foreground ,keywords))))
   `(js2-jsdoc-type ((t (:foreground ,constants))))
   `(js2-jsdoc-value((t (:foreground ,text))))
   `(js2-object-property ((t (:foreground ,text))))
   `(js2-external-variable ((t (:foreground ,constants))))
   `(js2-error ((t (:foreground ,error))))
   `(js2-warning ((t (:foreground ,warning))))

   ;; highlight numbers
   `(highlight-numbers-number ((t (:foreground ,numbers))))
  )

  (custom-theme-set-variables
    'julius
    '(linum-format " %5i ")
  )
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; *****************************************************************************

(provide-theme 'julius)

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'julius-theme)

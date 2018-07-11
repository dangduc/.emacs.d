(deftheme default-black
  "default-black-theme is the default dark emacs theme. This is what you get if
   you passed --reverse-video on emacs start
   ```
   emacs --reverse-video
   ```
   ")

;; references
;; http://ergoemacs.org/emacs/emacs_playing_with_color_theme.html
;; https://emacsair.me/2017/09/01/magit-walk-through/

;; credits
;; github.com/magnars/.emacs.d/themes/default-black-theme.el

(custom-theme-set-faces
 'default-black
 '(default ((t (:inherit nil
                :stipple nil
                :background "Black"
                :foreground "White"
                :strike-t*hrough nil))))

 '(highlight ((((class color)
                (min-colors 88)
                (background dark))
                (:background "#111111"))))
 '(region ((nil (:background "#464740"))))
 '(hl-line ((nil (:background "#222222"))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
 '(show-paren-match ((nil (:background "#333399"))))
 '(show-paren-mismatch ((((class color)) (:background "red")))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'default-black)

;;; moom-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "moom" "moom.el" (0 0 0 0))
;;; Generated autoloads from moom.el

(autoload 'moom-fill-screen "moom" "\
Expand frame width and height to fill the screen.
The font size in buffers will be increased so that the frame width could be
maintained at 80. Add appropriate functions to `moom-before-fill-screen-hook'
in order to move the frame to specific position.

\(fn)" t nil)

(autoload 'moom-toggle-frame-maximized "moom" "\
Toggle frame maximized.

\(fn)" t nil)

(autoload 'moom-fill-top "moom" "\
Fill upper half of screen.

\(fn)" t nil)

(autoload 'moom-fill-bottom "moom" "\
Fill lower half of screen.

\(fn)" t nil)

(autoload 'moom-fill-left "moom" "\
Fill left half of screen.

\(fn)" t nil)

(autoload 'moom-fill-right "moom" "\
Fill right half of screen.

\(fn)" t nil)

(autoload 'moom-fill-top-left "moom" "\
Fill top left quarter of screen.

\(fn)" t nil)

(autoload 'moom-fill-top-right "moom" "\
Fill top right quarter of screen.

\(fn)" t nil)

(autoload 'moom-fill-bottom-left "moom" "\
Fill bottom left quarter of screen.

\(fn)" t nil)

(autoload 'moom-fill-bottom-right "moom" "\
Fill bottom right quarter of screen.

\(fn)" t nil)

(autoload 'moom-fill-band "moom" "\
Fill screen by band region.
If PLIST is nil, `moom-fill-band-options' is used.

\(fn &optional PLIST)" t nil)

(autoload 'moom-cycle-line-spacing "moom" "\
Change `line-spacing’ value between a range.

\(fn)" t nil)

(autoload 'moom-reset-line-spacing "moom" "\
Reset to the defaut value for line spacing.

\(fn)" t nil)

(autoload 'moom-move-frame-right "moom" "\
PIXEL move the current frame to right.

\(fn &optional PIXEL)" t nil)

(autoload 'moom-move-frame-left "moom" "\
PIXEL move the current frame to left.

\(fn &optional PIXEL)" t nil)

(autoload 'moom-move-frame-to-horizontal-center "moom" "\
Move the current frame to the horizontal center of the screen.

\(fn)" t nil)

(autoload 'moom-move-frame-to-vertical-center "moom" "\
Move the current frame to the vertical center of the screen.

\(fn)" t nil)

(autoload 'moom-move-frame-to-edge-top "moom" "\
Move the current frame to the top of the screen.
If you find the frame is NOT moved to the top exactly,
please configure the margins by `moom-screen-margin'.

\(fn)" t nil)

(autoload 'moom-move-frame-to-edge-bottom "moom" "\
Move the current frame to the top of the screen.
If you find the frame is NOT moved to the bottom exactly,
please configure the margins by `moom-screen-margin'.

\(fn)" t nil)

(autoload 'moom-move-frame-to-edge-right "moom" "\
Move the current frame to the right edge of the screen.

\(fn)" t nil)

(autoload 'moom-move-frame-to-edge-left "moom" "\
Move the current frame to the left edge of the screen.

\(fn)" t nil)

(autoload 'moom-move-frame-to-center "moom" "\
Move the current frame to the center of the screen.

\(fn)" t nil)

(autoload 'moom-move-frame "moom" "\
Move the frame to somewhere (default: '(0 0)).
When ARG is a list like '(10 10), move the frame to the position.
When ARG is a single number like 10, shift the frame horizontally +10 pixel.
When ARG is nil, then move to the default position '(0 0).

\(fn &optional ARG)" t nil)

(autoload 'moom-cycle-frame-height "moom" "\
Change frame height and update the internal ring.
If you find the frame is NOT changed as expected,
please configure the margins by `moom-screen-margin'.

\(fn)" t nil)

(autoload 'moom-expand-height "moom" "\
Expand frame height to fill screen vertically without changing frame width.

\(fn)" nil nil)

(autoload 'moom-change-frame-height "moom" "\
Change the hight of the current frame.
Argument FRAME-HEIGHT specifies new frame height.
If PIXELWISE is non-nil, the frame height will be changed by pixel value.

\(fn &optional FRAME-HEIGHT PIXELWISE)" t nil)

(autoload 'moom-change-frame-width "moom" "\
Change the frame width by the FRAME-WIDTH argument.
This function does not effect font size.
If FRAME-WIDTH is nil, `moom-frame-width-single' will be used.

\(fn &optional FRAME-WIDTH)" t nil)

(autoload 'moom-change-frame-width-single "moom" "\
Change the frame width to single.
This function does not effect font size.

\(fn)" t nil)

(autoload 'moom-change-frame-width-double "moom" "\
Change the frame width to double.
This function does not effect font size.

\(fn)" t nil)

(autoload 'moom-change-frame-width-half-again "moom" "\
Change the frame width to half as large again as single width.
This function does not effect font size.

\(fn)" t nil)

(autoload 'moom-reset "moom" "\
Reset associated parameters.

\(fn)" t nil)

(autoload 'moom-update-height-steps "moom" "\
Change number of steps of the height ring by ARG.
The default step is 4.

\(fn ARG)" nil nil)

(autoload 'moom-screen-margin "moom" "\
Change top, bottom, left, and right margin by provided MARGINS.
MARGINS shall be a list consists of 4 integer variables like '(23 0 0 0).
If FILL is non-nil, the frame will cover the screen with given margins.

\(fn MARGINS &optional FILL)" nil nil)

(autoload 'moom-restore-last-status "moom" "\
Restore the last frame position, size, and font-size.
STATUS is a list consists of font size, frame position, frame region, and pixel-region.

\(fn &optional STATUS)" t nil)

(autoload 'moom-toggle-font-module "moom" "\
Toggle `moom--font-module-p'.
When `moom--font-module-p' is nil, font size is fixed except for `moom-reset' even if \"moom-font.el\" is loaded.

\(fn)" t nil)

(autoload 'moom-generate-font-table "moom" "\
Generate a font table.
The last frame position and size will be restored.

\(fn)" t nil)

(autoload 'moom-recommended-keybindings "moom" "\
Apply pre defined keybindings.
OPTIONS is a list of moom API types.  If you want to set all recommemded
keybindings, put the following code in your init.el.
 (with-eval-after-load \"moom\"
   (moom-recommended-keybindings 'all))
'all is identical to '(move fit expand fill font reset).
If you give only '(reset) as the argument, then \\[moom-reset] is activated.
The keybindings will be assigned when Emacs runs in GUI.

\(fn OPTIONS)" nil nil)

(autoload 'moom-print-status "moom" "\
Print font size, frame size and origin in mini buffer.

\(fn)" t nil)

(autoload 'moom-version "moom" "\
The release version of Moom.

\(fn)" t nil)

(defvar moom-mode nil "\
Non-nil if Moom mode is enabled.
See the `moom-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `moom-mode'.")

(custom-autoload 'moom-mode "moom" nil)

(autoload 'moom-mode "moom" "\
Toggle the minor mode `moom-mode'.
This mode provides a set of commands to control frame position and size.
The font size in buffers will be changed with synchronization of the updated
frame geometry so that the frame width could be maintained at 80.

No keybindings are configured as default but recommended as follows:

\(with-eval-after-load \"moom\"
  (define-key moom-mode-map (kbd \"M-0\") 'moom-move-frame) ;; move to top-left
  (define-key moom-mode-map (kbd \"M-1\") 'moom-move-frame-left)
  (define-key moom-mode-map (kbd \"M-2\") 'moom-move-frame-to-center)
  (define-key moom-mode-map (kbd \"M-3\") 'moom-move-frame-right)
  (define-key moom-mode-map (kbd \"<f2>\") 'moom-cycle-frame-height)
  (define-key moom-mode-map (kbd \"M-<f2>\") 'moom-toggle-frame-maximized)
  (define-key moom-mode-map (kbd \"C-c f s\") 'moom-change-frame-width-single)
  (define-key moom-mode-map (kbd \"C-c f d\") 'moom-change-frame-width-double))

You can use `moom-recommended-keybindings' to apply recommended keybindings in your init.el.

To see more details and examples, please visit https://github.com/takaxp/moom.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "moom" '("moom-")))

;;;***

;;;### (autoloads nil "moom-font" "moom-font.el" (0 0 0 0))
;;; Generated autoloads from moom-font.el

(autoload 'moom-font-ascii "moom-font" "\
Set ASCII font family by given FONT.
If PLIST is non-nil and it has immediate property,
given FONT is immediately applied.

\(fn FONT &optional PLIST)" nil nil)

(autoload 'moom-font-ja "moom-font" "\
Set Japanese font family by given FONT.
If PLIST is non-nil and it has immediate property,
given FONT is immediately applied.

\(fn FONT &optional PLIST)" nil nil)

(autoload 'moom-font-resize "moom-font" "\
Resize font.
`frame-width' will be updated accordingly.
Optional argument N specifies the target font size.
If WIDTH is non-nil, ensure an appropriate font size so that
the actual pixel width will not exceed the WIDTH.

\(fn &optional N WIDTH)" t nil)

(autoload 'moom-font-size-reset "moom-font" "\
Reset font to the initial size.

\(fn)" t nil)

(autoload 'moom-font-increase "moom-font" "\
Increase font size.
Optional argument INC specifies an increasing step.

\(fn &optional INC)" t nil)

(autoload 'moom-font-decrease "moom-font" "\
Decrease font size.
Optional argument DEC specifies a decreasing step.

\(fn &optional DEC)" t nil)

(autoload 'moom-font-print-name-at-point "moom-font" "\
Print font family name at point.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "moom-font" '("moom-font-")))

;;;***

;;;### (autoloads nil nil ("moom-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; moom-autoloads.el ends here

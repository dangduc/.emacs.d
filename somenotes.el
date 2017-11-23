(use-package typescript-mode
  ;; npm install -g typescript
  :ensure t
  :mode
  ("\\.ts\\'" . typescript-mode)
  ("\\.ts$\\'" . typescript-mode)
  :init
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local typescript-indent-level (+indent-offset))
              (with-eval-after-load 'evil
                (setq-local evil-shift-width typescript-indent-level))))
  :config
  (setq typescript-enabled-frameworks '(typescript)))

(use-package tide
  :ensure t
  :commands
  (tide-setup)
  :init
  (defun +setup-tide-mode ()
    (interactive)
    (when (locate-dominating-file default-directory "tsfmt.json")
      (add-hook 'before-save-hook #'tide-format-before-save nil t))
    ;; Disable linting for Typescript Definition files.
    (when (and (buffer-file-name)
               (string-match-p ".d.ts$" (buffer-file-name)))
      (flycheck-mode -1))
    (tide-setup)
    (tide-hl-identifier-mode +1)
    (+company-merge-backends))
  (add-hook 'typescript-mode-hook #'+setup-tide-mode)

  (add-hook 'js2-mode-hook
            (lambda ()
              (when (or
                     (locate-dominating-file default-directory "tsconfig.json")
                     (locate-dominating-file default-directory "jsconfig.json"))
                (+setup-tide-mode))))

  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up Tide mode if Typescript.
              (when (string-equal (file-name-extension buffer-file-name) "tsx")
                (setq-local web-mode-enable-auto-quoting nil)
                (when (fboundp 'yas-activate-extra-mode)
                  (yas-activate-extra-mode 'typescript-mode))
                (+setup-tide-mode))))
  :config
  ;; Set up Typescript linting with `web-mode'.
  ;; https://github.com/ananthakumaran/tide/pull/161
  (eval-after-load 'flycheck
    (lambda ()
      (flycheck-add-mode 'typescript-tslint 'web-mode)))

  (smart-jump-register :modes 'tide-mode
                       :jump-fn 'tide-jump-to-definition
                       :pop-fn 'tide-jump-back
                       :refs-fn 'tide-references
                       :should-jump t
                       :heuristic 'point
                       :async t))
(use-package web-mode
  :ensure t
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.blade\\.php\\'" . web-mode)
  ("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.jsp\\'" . web-mode)
  ("\\.eex\\'" . web-mode)
  ("\\.tsx\\'" . web-mode)
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              ;; Set up indentation.
              (let ((n (+indent-offset)))
                (setq-local web-mode-markup-indent-offset n)
                (setq-local web-mode-css-indent-offset n)
                (setq-local web-mode-code-indent-offset n)
                (with-eval-after-load 'evil
                  (setq-local evil-shift-width n)))))
  :config
  ;; Use `company-dabbrev-code' with `web-mode'.
  (when (boundp 'company-dabbrev-code-modes)
    (push 'web-mode company-dabbrev-code-modes))

  (with-eval-after-load 'evil
    (evil-define-key 'normal web-mode-map
      (kbd "C-d") 'evil-scroll-down))

  ;; https://github.com/bbatsov/prelude/blob/master/modules/prelude-web.el#L50
  (eval-after-load 'smartparens
    (lambda ()
      (setq web-mode-enable-auto-pairing nil)
      (sp-with-modes '(web-mode)
        (sp-local-pair "%" "%"
                       :unless '(sp-in-string-p)
                       :post-handlers '(((lambda (&rest _ignored)
                                           (just-one-space)
                                           (save-excursion (insert " ")))
                                         "SPC" "=" "#")))
        (sp-local-tag "%" "<% "  " %>")
        (sp-local-tag "=" "<%= " " %>")
        (sp-local-tag "#" "<%# " " %>")))))

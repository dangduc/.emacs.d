;;; duc-treesit.el --- Tree-sitter across Emacs versions -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'treesit nil t)
(require 'duc-bootstrap)

(defconst duc-treesit-p
  (and (fboundp 'treesit-available-p) (treesit-available-p))
  "Non-nil when this Emacs supports tree-sitter.")

(defconst duc-treesit-language-sources
  '(;; Built-in Emacs ts-modes
    (bash       "https://github.com/tree-sitter/tree-sitter-bash")
    (c          "https://github.com/tree-sitter/tree-sitter-c")
    (c-sharp    "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (cmake      "https://github.com/uyha/tree-sitter-cmake")
    ;; Newer cpp grammar breaks `c++-ts-mode' highlighting.
    (cpp        "https://github.com/tree-sitter/tree-sitter-cpp"
                "v0.22.0")
    (css        "https://github.com/tree-sitter/tree-sitter-css")
    (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
    (elixir     "https://github.com/elixir-lang/tree-sitter-elixir")
    (go         "https://github.com/tree-sitter/tree-sitter-go")
    (gomod      "https://github.com/camdencheek/tree-sitter-go-mod")
    (heex       "https://github.com/phoenixframework/tree-sitter-heex")
    (html       "https://github.com/tree-sitter/tree-sitter-html")
    (java       "https://github.com/tree-sitter/tree-sitter-java")
    (javascript "https://github.com/tree-sitter/tree-sitter-javascript"
                "master")
    (json       "https://github.com/tree-sitter/tree-sitter-json")
    (lua        "https://github.com/tree-sitter-grammars/tree-sitter-lua")
    (php        "https://github.com/tree-sitter/tree-sitter-php"
                nil "php/src")
    (python     "https://github.com/tree-sitter/tree-sitter-python")
    (ruby       "https://github.com/tree-sitter/tree-sitter-ruby")
    (rust       "https://github.com/tree-sitter/tree-sitter-rust")
    (toml       "https://github.com/tree-sitter-grammars/tree-sitter-toml")
    (tsx        "https://github.com/tree-sitter/tree-sitter-typescript"
                "master" "tsx/src")
    (typescript "https://github.com/tree-sitter/tree-sitter-typescript"
                "master" "typescript/src")
    (yaml       "https://github.com/tree-sitter-grammars/tree-sitter-yaml")
    ;; Sub-grammars used internally by built-in ts-modes.
    (jsdoc      "https://github.com/tree-sitter/tree-sitter-jsdoc")
    (markdown   "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                nil "tree-sitter-markdown/src")
    (markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                     nil "tree-sitter-markdown-inline/src")
    ;; MELPA-provided ts-modes.
    (awk        "https://github.com/Beaglefoot/tree-sitter-awk")
    (bibtex     "https://github.com/latex-lsp/tree-sitter-bibtex")
    (blueprint  "https://github.com/huanie/tree-sitter-blueprint")
    (clojure    "https://github.com/sogaiu/tree-sitter-clojure")
    ;; (cobol      "https://github.com/yutaro-sakamoto/tree-sitter-cobol")
    (commonlisp "https://github.com/tree-sitter-grammars/tree-sitter-commonlisp")
    (dart       "https://github.com/UserNobody14/tree-sitter-dart")
    (gitcommit  "https://github.com/gbprod/tree-sitter-gitcommit")
    (glsl       "https://github.com/tree-sitter-grammars/tree-sitter-glsl")
    (gowork     "https://github.com/omertuc/tree-sitter-go-work")
    (haskell    "https://github.com/tree-sitter/tree-sitter-haskell")
    (hyprlang   "https://github.com/tree-sitter-grammars/tree-sitter-hyprlang")
    (janet-simple "https://github.com/sogaiu/tree-sitter-janet-simple")
    (julia      "https://github.com/tree-sitter/tree-sitter-julia")
    (kotlin     "https://github.com/fwcd/tree-sitter-kotlin")
    (magik      "https://github.com/krn-robin/tree-sitter-magik")
    (make       "https://github.com/tree-sitter-grammars/tree-sitter-make")
    (nix        "https://github.com/nix-community/tree-sitter-nix")
    (nu         "https://github.com/nushell/tree-sitter-nu")
    (org        "https://github.com/milisims/tree-sitter-org")
    (perl       "https://github.com/ganezdragon/tree-sitter-perl")
    (proto      "https://github.com/mitchellh/tree-sitter-proto")
    (r          "https://github.com/r-lib/tree-sitter-r")
    (scala      "https://github.com/tree-sitter/tree-sitter-scala")
    (solidity   "https://github.com/JoranHonig/tree-sitter-solidity")
    (sql        "https://github.com/DerekStride/tree-sitter-sql"
                "gh-pages")
    (surface    "https://github.com/connorlay/tree-sitter-surface")
    (typespec   "https://github.com/happenslol/tree-sitter-typespec/"
                "main")
    (typst      "https://github.com/uben0/tree-sitter-typst"
                "master")
    ;; (verilog    "https://github.com/gmlarumbe/tree-sitter-systemverilog")
    (vhdl       "https://github.com/alemuller/tree-sitter-vhdl")
    (vue        "https://github.com/tree-sitter-grammars/tree-sitter-vue")
    (wast       "https://github.com/wasm-lsp/tree-sitter-wasm"
                nil "wast/src")
    (wat        "https://github.com/wasm-lsp/tree-sitter-wasm"
                nil "wat/src")
    (wgsl       "https://github.com/mehmetoguzderin/tree-sitter-wgsl")
    (zig        "https://github.com/maxxnino/tree-sitter-zig")
    ;; User override: swift's default branch lacks generated parser.c.
    (swift      "https://github.com/alex-pinkus/tree-sitter-swift"
                "with-generated-files"))
  "Adapted grammar recipes.")

(defvar duc-treesit-directory
  (expand-file-name (format "tree-sitter/%s/" emacs-major-version)
                    user-emacs-directory))
(defvar duc-treesit-install-on-startup t
  "Install missing grammars for configured modes after startup.")
(defvar duc-treesit-install-run nil)
(defconst duc-treesit-mode-recipes
  '((sh-mode bash-ts-mode bash)
    (c-mode c-ts-mode c)
    (c++-mode c++-ts-mode cpp c)
    (csharp-mode csharp-ts-mode c-sharp)
    (cmake-mode cmake-ts-mode cmake)
    (css-mode css-ts-mode css)
    (dockerfile-mode dockerfile-ts-mode dockerfile)
    (elixir-mode elixir-ts-mode elixir)
    (go-mode go-ts-mode go gomod)
    (html-mode html-ts-mode html)
    (java-mode java-ts-mode java)
    (js-mode js-ts-mode javascript jsdoc)
    (js-json-mode json-ts-mode json)
    (json-mode json-ts-mode json)
    (lua-mode lua-ts-mode lua)
    (markdown-mode markdown-ts-mode markdown markdown-inline)
    (gfm-mode markdown-ts-mode markdown markdown-inline)
    (php-mode php-ts-mode php)
    (python-mode python-ts-mode python)
    (ruby-mode ruby-ts-mode ruby)
    (rust-mode rust-ts-mode rust)
    (conf-toml-mode toml-ts-mode toml)
    (typescript-mode typescript-ts-mode typescript)
    (yaml-mode yaml-ts-mode yaml)
    (kotlin-mode kotlin-ts-mode kotlin)
    (swift-mode swift-ts-mode swift))
  "Legacy modes, replacement modes, and their required grammars.")

(defun duc-treesit--mode-present-p (mode)
  "Return non-nil when MODE has an available implementation."
  (or (fboundp mode) (locate-library (symbol-name mode))))

(defun duc-treesit-configured-languages ()
  "Return grammar names for available configured modes."
  (delete-dups
   (append
    (cl-loop for (_old mode . languages) in duc-treesit-mode-recipes
             when (duc-treesit--mode-present-p mode)
             append (copy-sequence languages))
    (when (duc-treesit--mode-present-p 'tsx-ts-mode)
      '(tsx javascript jsdoc)))))

(defun duc-treesit-enable-modes ()
  "Enable supported tree-sitter modes without losing legacy fallbacks."
  (when duc-treesit-p
    (when (boundp 'treesit-enabled-modes)
      (setopt treesit-enabled-modes t))
    ;; Include legacy third-party modes absent from Emacs 31's remap table.
    ;; On Emacs 30, these entries also supply the missing automatic remapping.
    (dolist (entry duc-treesit-mode-recipes)
      (when (and (duc-treesit--mode-present-p (cadr entry))
                 (cl-every #'treesit-language-available-p (cddr entry)))
        (unless (fboundp (cadr entry))
          (autoload (cadr entry) (symbol-name (cadr entry)) nil t))
        (setf (alist-get (car entry) major-mode-remap-alist) (cadr entry))))
    ;; TSX must win over the existing web-mode association.  Do not claim
    ;; web-mode itself: it also handles templates that are not JSX or TSX.
    (when (and (duc-treesit--mode-present-p 'tsx-ts-mode)
               (treesit-language-available-p 'tsx))
      (let ((entry '("\\.\\(?:jsx\\|tsx\\)\\'" . tsx-ts-mode)))
        ;; add-to-list leaves an existing entry behind later web-mode rules.
        (setq auto-mode-alist (cons entry (delete entry auto-mode-alist)))))))

(defun duc-treesit--install-arguments (args)
  "Use this major version's grammar directory when ARGS omit OUT-DIR."
  (if (cadr args) args (list (car args) duc-treesit-directory)))

(defun duc-treesit-install-all-async (&optional languages)
  "Install missing grammars in background workers from this Emacs binary.
With LANGUAGES, restrict installation to those names.  An interactive call
uses every registered recipe.  A second call reuses an active run."
  (interactive)
  (unless duc-treesit-p (user-error "This Emacs has no tree-sitter support"))
  (if (and duc-treesit-install-run
           (or (duc-bootstrap-run-pending duc-treesit-install-run)
               (duc-bootstrap-run-processes duc-treesit-install-run)))
      duc-treesit-install-run
    (let* ((entries (cl-remove-if
                     (lambda (entry)
                       (or (and languages (not (memq (car entry) languages)))
                           (treesit-language-available-p (car entry))))
                     treesit-language-source-alist))
           (jobs
            (mapcar
             (lambda (entry)
               (cons
                (car entry)
                `(progn
                   (require 'treesit)
                   (setq user-emacs-directory ,user-emacs-directory
                         native-comp-eln-load-path ',(when (boundp 'native-comp-eln-load-path)
                                                       native-comp-eln-load-path)
                         treesit-extra-load-path ',treesit-extra-load-path
                         treesit-language-source-alist ',(list entry))
                   (treesit-install-language-grammar ',(car entry) ,duc-treesit-directory)
                   ;; This installer can warn and return nil on failure.
                   (unless (treesit-language-available-p ',(car entry))
                     (error "Grammar is not loadable: %s" ',(car entry))))))
             entries)))
      (when jobs
        (make-directory duc-treesit-directory t)
        (setq duc-treesit-install-run
              (duc-bootstrap-run-jobs
               jobs nil
               (lambda (run)
                 (duc-treesit-enable-modes)
                 (if (duc-bootstrap-run-failures run)
                     (display-warning
                      'duc-treesit
                      (format "Grammar workers failed: %s. See *duc worker: NAME* buffers"
                              (duc-bootstrap-run-failures run)))
                   (message "Tree-sitter: installed %s grammars"
                            (duc-bootstrap-run-total run))))))))))

(defun duc-treesit-install-configured-async ()
  "Install only grammars used by available configured modes at startup."
  (when (and duc-treesit-p duc-treesit-install-on-startup (not noninteractive))
    (duc-treesit-install-all-async (duc-treesit-configured-languages))))

(when duc-treesit-p
  (add-to-list 'treesit-extra-load-path duc-treesit-directory)
  (dolist (entry duc-treesit-language-sources)
    (setf (alist-get (car entry) treesit-language-source-alist) (cdr entry)))
  (when (>= emacs-major-version 31)
    (dolist (entry '((common-lisp "https://codeberg.org/zshaftel/tree-sitter-cl-syntax"
                                  nil "common_lisp/src")
                     (cl-format "https://codeberg.org/zshaftel/tree-sitter-cl-syntax"
                                nil "cl_format/src")))
      (setf (alist-get (car entry) treesit-language-source-alist) (cdr entry))))
  (setq treesit-font-lock-level 4)
  (when (boundp 'treesit-auto-install-grammar)
    (setq treesit-auto-install-grammar 'ask))
  (advice-add 'treesit-install-language-grammar :filter-args #'duc-treesit--install-arguments)
  (duc-treesit-enable-modes)
  ;; Run after package declarations so TSX wins over web-mode's extension.
  (add-hook 'after-init-hook #'duc-treesit-enable-modes t)
  (add-hook 'emacs-startup-hook #'duc-treesit-install-configured-async t))

(provide 'duc-treesit)
;;; duc-treesit.el ends here

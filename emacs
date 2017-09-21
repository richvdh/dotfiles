; something, somewhere, resets the default of auto-fill-function to c-do-auto-fill.
; (setq-default auto-fill-function nil)

; idiotic 3-space perl indents
; (setq-default cperl-indent-level 3)

(set-face-background 'default "black")
(set-face-foreground 'default "white")
(set-face-attribute 'default nil :height 80)

(nconc load-path (mapcar 'expand-file-name
                         '("~/dotfiles/emacs.d/packages"
                           "~/dotfiles/emacs.d/packages/puppet-syntax-emacs"
                           "~/.emacs.d/matrix.el")))


(defun load-file-if-exists (x)
  (if (file-exists-p x)
      (load-file x)))

;; loaddefs.el is maintained automatically by (update-autoloads-in-package-area)
(mapc (lambda(x) (load-file-if-exists (expand-file-name x)))
      '("~/dotfiles/emacs.d/packages/loaddefs.el"
        "~/dotfiles/emacs.d/packages/Emacs-Groovy-Mode/groovy-mode.el"
        "~/dotfiles/emacs.d/packages/puppet-syntax-emacs/puppet-mode-init.el"
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; package initialisation
;

; add melpa to the list of package repositories
(require 'package)
(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/")
 t)

; By default, packages aren't initialised until after the init.el runs. We
; force it to happen here, and then do the stuff which depends on them.
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; file recognition
(setq auto-mode-alist
      (append
       '(
	 ( "[]>:/]\\.gnus\\'" . emacs-lisp-mode )
	 ( "emacs\\'" . emacs-lisp-mode )
	 ( "\\.[vV]\\'" . verilog-mode )
	 ( "\\.[tT]e[xX]\\'" . latex-mode )
	 ( "\\.asm\\'" . asm-mode )
         ( "\\.ipp\\'" . c++-mode )
         ( "SConstruct\\'" . python-mode )
         ( "SConscript\\'" . python-mode )
         ( "README\\'" . text-mode )
         ( "COMMIT_EDITMSG\\'" . text-mode )
         ( "\\.md\\'" . markdown-mode )
	 ) auto-mode-alist))

;; prefer js2 for javascript, if it's installed (via package-install js2)
(if (fboundp 'js2-mode)
    (add-to-list 'auto-mode-alist
                 '("\\.jsx?\\'" . js2-jsx-mode)
                 ))


;; prefer cperl-mode to perl-mode
(defalias 'perl-mode 'cperl-mode)

(setq make-backup-files nil)
;(redspace-mode t)

; wtf does this do?
; (setq minibuffer-max-depth nil)


(if (fboundp 'set-scroll-bar-mode)  ; not on xemacs, apparently
    (set-scroll-bar-mode 'right))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Customizations for all of c-mode, c++-mode, and objc-mode


(defconst my-c-style
  '((c-tab-always-indent           . t)
    (c-comment-only-line-offset    . 0)
    (c-hanging-braces-alist        . ((substatement-open after)
				      (brace-list-open)))
    (c-hanging-colons-alist        . ((member-init-intro before)
				      (inher-intro)
				      (case-label after)
				      (label after)
				      (access-label after)))
    (c-cleanup-list                . (scope-operator
				      empty-defun-braces
				      defun-close-semi))
    (c-offsets-alist               . ((arglist-close     . 0)
                                      ;(arglist-close     . c-lineup-arglist)
				      (substatement-open . 0)
				      (member-init-intro . ++)
				      (knr-argdecl-intro . 0)
				      (comment-intro     . 0)
				      (inline-open       . 0)
				      (access-label      . -2)
				      (case-label        . +)
                                      (innamespace       . 0)
                                      (inextern-lang     . 0)
                                      ))
    (c-echo-syntactic-information-p . t)
    (c-basic-offset                 . 4)
    (truncate-lines		    . t)
;    (fill-column		    . 79 )
    ) "rav's common cc-mode Customisations")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; mode-specific hooks

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; add my personal style and set it for the current buffer
            (set-tab-width 8)
	    (c-add-style "PERSONAL" my-c-style t)
	    (auto-fill-mode t)
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
	    (define-key c-mode-map "\C-m" 'newline-and-indent)
	    (define-key c-mode-map [(control meta prior)] 'c-beginning-of-defun)
	    (define-key c-mode-map [(control meta next)] 'c-end-of-defun)
            ))

;(add-hook 'cperl-mode-hook
;	  (lambda ()
;	    (define-key cperl-mode-map [backspace] 'cperl-electric-backspace)
;	    (define-key cperl-mode-map "\^C\^X"	   'cperl-find-pods-heres)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq truncate-lines t )))

(add-hook 'go-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
            (add-hook 'before-save-hook 'gofmt-before-save)
            ))

(add-hook 'html-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (define-key java-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'js-mode-hook
	  (lambda ()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
            ))

(add-hook 'latex-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (ispell-minor-mode))
	  )

(add-hook 'python-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
            ))

(add-hook 'rust-mode-hook
          (lambda()
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
            ))

(add-hook 'text-mode-hook
	  (lambda ()
            ;; ispell-minor-mode fights with electric-buffer-mode, which apparently runs
            ;; text-mode-hook.
            ;; (ispell-minor-mode)
            (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)
	    (auto-fill-mode 1)
            )
	  )

(add-hook 'dns-mode-hook
          (lambda()
            (auto-fill-mode 0)
            (setq indent-tabs-mode t)
            (setq indent-line-function 'insert-tab)))

(add-hook 'verilog-mode-hook
	  (lambda ()
	    (set-tab-width 4)
	    (setq verilog-indent-level 4
		  verilog-indent-level-module 4
		  verilog-indent-level-declaration 4 )))

(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(defun conditionally-fontify-buffer ()
  (interactive)
  (and font-lock-mode
       (font-lock-fontify-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; stub out html-helper-mode on systems without it
(cond ((not (fboundp 'html-helper-mode))
       (defun html-helper-mode (&rest args))
       (provide 'html-helper-mode)
       ))

(defun reverse-yank-pop (arg)
   "Like yank-pop, only backwards."
   (interactive "*p")
   (yank-pop (- arg)))

; this doesn't work on emacs 23.3 (no device-type or mouse-consolidated-yank)
; and anyway, yank seems to do the right thing (ie, paste the primary
; selection).
;
; Meta-w (kill-ring-save) also does the right thing (claims the X 'clipboard').

;(defun clipboard-yank ()
;   "yank the current clipboard"
;   (interactive)
;   (case (device-type)
;;     (x (x-yank-function))
;     (x (mouse-consolidated-yank))
;     (tty (yank))
;     (mswindows (mswindows-paste-clipboard))
;     (otherwise (yank))))



;; key bindings - names depend on emacs/xemacs
(cond
 ((string-match "XEmacs" emacs-version)
  (global-set-key [end]			    'end-of-line)
  (global-set-key [(meta end)]		    'end-of-line-other-buffer)
  (global-set-key [(control end)]	    'end-of-buffer)
  (global-set-key [(meta control end)]	    'end-of-buffer-other-buffer)

  (global-set-key [home]		    'beginning-of-line)
  (global-set-key [(meta home)]		    'beginning-of-line-other-buffer)
  (global-set-key [(control home)]	    'beginning-of-buffer)
  (global-set-key [(meta control home)]	    'beginning-of-buffer-other-buffer)

  (global-set-key [(control menu)]	    'apropos)
  (global-set-key [f2]			    'find-file-other-frame)
  (global-set-key "\M-\C-y"		    'reverse-yank-pop)
  (global-set-key "\M-`"	            'compile)
  (global-set-key "\C-x\C-b"		'electric-buffer-list)
  (global-set-key "\C-x\M-f"		'auto-fill-mode)

  ; shift-insert doesn't get the X selection by default
;  (global-set-key [(shift insert)]	    'clipboard-yank)
  )

 ( t
  (global-set-key [delete]		'delete-char)

  (global-set-key [end]			'end-of-line)
  (global-set-key [M-end]		'end-of-line-other-buffer)
  (global-set-key [C-end]		'end-of-buffer)
  (global-set-key [M-C-end]		'end-of-buffer-other-buffer)
  (global-set-key [home]		'beginning-of-line)
  (global-set-key [M-home]		'beginning-of-line-other-buffer)
  (global-set-key [C-home]		'beginning-of-buffer)
  (global-set-key [M-C-home]		'beginning-of-buffer-other-buffer)

  (global-set-key [C-menu]		'apropos)
  (global-set-key "\M-\C-y"		'reverse-yank-pop)
  (global-set-key "\M-`"	        'compile)
  (global-set-key "\C-x\C-b"		'electric-buffer-list)
  (global-set-key "\C-x\M-f"		'auto-fill-mode)
;  (global-set-key [S-insert]		'clipboard-yank)

  (global-set-key "\M-g"                'goto-line)

  (global-set-key "\C-z"                'undo)
  (global-set-key (kbd "C-S-w")         'delete-region)

  ; windows-up/down/left/right - change window
  (global-set-key [s-up]                'windmove-up)
  (global-set-key [s-down]              'windmove-down)
  (global-set-key [s-left]              'windmove-left)
  (global-set-key [s-right]             'windmove-right)
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; bodge customisations to be ignored under emacs 19
;

(if (not (fboundp 'custom-set-variables))
  (defun custom-set-variables (&rest args)))
(if (not (fboundp 'custom-set-faces))
  (defun custom-set-faces (&rest args)))

;;;;;;;;;;;;;;
;
; tab stuff

; default tab width

; set up tab stops
(defun set-tab-width (width)
  "Set tab-width to width and set tab-stop-list to stops every width chars"
  (interactive "Nwidth: ")
  (setq tab-width width )
  (setq tab-stop-list (number-sequence tab-width 132 tab-width)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; line endings

(defun toggle-line-endings ()
  "Toggle between unix and DOS line endings"
   (interactive)
   (let ((eol ( coding-system-eol-type buffer-file-coding-system ))
	 (base (coding-system-base buffer-file-coding-system )))
     (if (or (eq eol 1) (eq eol 'crlf))
	 (set-buffer-file-coding-system (coding-system-eol-lf base))
       (set-buffer-file-coding-system (coding-system-eol-crlf base)))
     )
   )

; this is in xemacs, but not emacs
(cond
 ((not (fboundp 'coding-system-eol-lf))
  (defun coding-system-eol-lf (system)
    (coding-system-change-eol-conversion system 'unix))
  (defun coding-system-eol-crlf (system)
    (coding-system-change-eol-conversion system 'dos))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; miscellany


;(setq compile-command "EXTRA_CPPFLAGS='-DDEBUG' make -j2")
(setq compile-command "make -j2")


;; set up the dvi view command
;( cond
;  (( eq window-system 'x )
;   ( setq tex-dvi-view-command "xdvi" ))
;  ( t
;    ( setq tex-dvi-view-command "dvi2tty" )
;    ))

; sort out ispell-minor-mode
(cond
 ((not (functionp 'ispell-minor-mode))
  (defun ispell-minor-mode (&optional ARG)
    t
    )
  )
 ((eq system-type 'windows-nt)
  (defadvice ispell-minor-mode (around disable-ispell activate)
    t
    )
  )
)

(defadvice switch-to-buffer (around confirm-non-existing-buffers activate)
  "Switch to non-existing buffers only upon confirmation."
  (interactive "BSwitch to buffer: ")
  (if (or (get-buffer (ad-get-arg 0))
          (y-or-n-p (format "`%s' does not exist, create? " (ad-get-arg 0))))
      ad-do-it))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; customisation stuff

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-generic))
 '(browse-url-generic-program "gnome-open")
 '(column-number-mode t)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(dns-mode-soa-auto-increment-serial nil)
 '(fill-column 79)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-switch-indent-offset 4)
 '(js2-strict-trailing-comma-warning nil)
 '(load-home-init-file t t)
 '(mouse-autoselect-window 0.5)
 '(perl-indent-parens-as-block t)
 '(puppet-include-indent 4)
 '(puppet-indent-level 4))

;; load custom per-host files
(let ((host-file (format "~/dotfiles/emacs.d/hosts/%s.el" system-name)))
  (if (file-exists-p host-file)
      (load host-file)))


(require 'matrix-client nil t)


(put 'narrow-to-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; keep this early

;(and
;  (string-match "XEmacs" emacs-version)
;  (gnuserv-start))

(set-face-background 'default "black")


(setq load-path (nconc '("~/lisp" "H:\\lisp") load-path))

(load "utils")
(load "kill-ring")
(load "vc")
(autoload 'ast-mode "ast-mode")

;; file recognition
(setq auto-mode-alist
      (append
       '(
	 ( "[]>:/]\\.gnus\\'" . emacs-lisp-mode )
	 ( "\\.[vV]\\'" . verilog-mode )
	 ( "\\.[tT]e[xX]\\'" . latex-mode )
	 ( "\\.\\(ast\\|ah\\)\\'" . ast-mode )
	 ( "\\.rno\\'" . rno-mode )
	 ( "\\.asm\\'" . asm-mode )
	 ( "\\.ph\\'" . cperl-mode )
	 ) auto-mode-alist))

(setq make-backup-files nil)


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
    (c-offsets-alist               . ((arglist-close     . c-lineup-arglist)
				      (substatement-open . 0)
				      (member-init-intro . ++)
				      (knr-argdecl-intro . 0)
				      (comment-intro     . 0)
				      (inline-open       . 0)
				      (access-label      . 0)
				      (case-label		. +)))
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
	    (define-key c-mode-map "\^M" 'newline-and-indent)
	    (define-key c-mode-map [(control meta prior)] 'c-beginning-of-defun)
	    (define-key c-mode-map [(control meta next)] 'c-end-of-defun)
))

(add-hook 'cperl-mode-hook
	  (lambda ()
	    (define-key cperl-mode-map [backspace] 'cperl-electric-backspace)
	    (define-key cperl-mode-map [delete]	   'backward-or-forward-delete-char)
	    (define-key cperl-mode-map "\^C\^X"	   'cperl-find-pods-heres)))

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (setq truncate-lines t )))

(add-hook 'html-mode-hook 
	  (lambda ()
	    (auto-fill-mode 1)))

(add-hook 'java-mode-hook
	  (lambda ()
	    (define-key java-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'latex-mode-hook
	  (lambda () 
	    (auto-fill-mode 1)
	    (ispell-minor-mode))
	  )

(add-hook 'text-mode-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (ispell-minor-mode))
	  )

(add-hook 'verilog-mode-hook 
	  (lambda ()
	    (set-tab-width 4)
	    (setq verilog-indent-level 4
		  verilog-indent-level-module 4
		  verilog-indent-level-declaration 4 )))

(defun conditionally-fontify-buffer ()
  (interactive)
  (and font-lock-mode
       (font-lock-fontify-buffer)))

; a pseudo-mode for editing rno files in
; just uses fundamental mode with a few tweaks.
(defun rno-mode ()
  (interactive)
  (fundamental-mode)
  (setq fill-column 128)
  (auto-fill-mode)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; stub out html-helper-mode on systems without it
(cond ((not (fboundp 'html-helper-mode))
       (defun html-helper-mode (&rest args))
       (provide 'html-helper-mode)
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; terminal hooks

; make meta work on pct
(defun init-term ()
  (set-input-mode nil nil t)
  (define-key function-key-map "\e[1~" [home])
  (define-key function-key-map "\e[4~" [end])
)

(add-hook 'create-console-hook
	  (lambda (console)
	    (select-console console)
	    (init-term)
	    ))
(add-hook 'term-setup-hook 'init-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

( cond
  (( string-match "XEmacs" emacs-version)
   ; only for xemacs
   ( line-number-mode t )
   ( column-number-mode t )

   ( cond
     (( not (eq system-type 'windows-nt ))
      ; replace x colours with nearby vt100 ones
      ( register-tty-color "dodgerblue" "\e[1;34m" "\e[0;44m" )  ; dodgerblue -> bold blue
      ( register-tty-color "lightcoral" "\e[1;31m" "\e[0;41m" )  ; light coral -> bold red
      ( register-tty-color "lightpink" "\e[1;31m" "\e[0;41m" )   ; lightpink -> bold red
     ))

   ; site-dependent stuff
   (setq domainname (exec-to-string "hostname -d"))
   (cond 
    ((or
      (string-match "^tarragon-et" domainname)
      (string-match "^h901" domainname)  ; pct 
      )
     (set-variable 'user-mail-address "r.vanderhoff@tarragon-et.co.uk")
     (set-face-font 'default      "-*-fixed-medium-r-*-*-*-120-*-*-*-*-iso8859-1" )
     (set-face-font 'bold         "-*-fixed-bold-r-*-*-*-120-*-*-*-*-iso8859-1" )
     (set-face-font 'bold-italic  "-*-courier-bold-o-*-*-*-100-*-*-*-*-iso8859-1" )
     (set-face-font 'italic       "-*-courier-medium-o-*-*-*-100-*-*-*-*-iso8859-1" )

     ; make meta work properly on pct
     ( set-input-mode nil nil t )
     )
    ((string-match "^techiehouse" domainname)
     (set-variable 'user-mail-address "richard@rvanderhoff.org.uk")
     (set-face-font 'default      "-*-fixed-medium-r-*-*-*-120-*-*-*-*-iso8859-15" )
     (set-face-font 'bold         "-*-fixed-bold-r-*-*-*-120-*-*-*-*-iso8859-15" )
     (set-face-font 'bold-italic  "-*-courier-bold-o-*-*-*-100-*-*-*-*-iso8859-15" )
     (set-face-font 'italic       "-*-courier-medium-o-*-*-*-100-*-*-*-*-iso8859-15" )
     ))

   )
  (( >= emacs-major-version 20 )
   ; only for recent emacs - done with options file for xemacs
   ( global-font-lock-mode t )
   ( show-paren-mode t ))


  (t
   ; hack other emacsen for font lock
   (defun turn-on-fontlock-mode ()
     (font-lock-mode t))

   (mapcar (lambda (HOOK)
	     (add-hook HOOK 'turn-on-fontlock-mode))
	   '(c-mode-common-hook asm-mode-hook emacs-lisp-mode-hook )))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; scrolling with mouse

( cond
  ;; this is in xemacs, but not emacs
  ((not (fboundp 'event-window))
   (defun event-window ( event )
     "Given an event, return the window where it started"
     ( posn-window ( event-start event ))
     )
   )
)


(defun scroll-up-with-mouse ( dist )
  "Scroll the window where the last input event occured up by dist lines"
  ( let (( curwin ( selected-window )))
    ( select-window ( event-window last-input-event ))
    ( condition-case err
					; ignore end of buffer errors
      (scroll-up dist)
      (end-of-buffer
       (select-window curwin)
       ( error "%s" ( car err ))
       ))
    (select-window curwin) 
    )
)

(defun scroll-down-with-mouse ( dist )
  "Scroll the window where the last input event occured down by dist lines"
  ( let (( curwin ( selected-window )))
    ( select-window ( event-window last-input-event ))
    ( condition-case err
					; ignore beginning of buffer errors
      ( scroll-down dist )
      ( beginning-of-buffer
       (select-window curwin)
       ( error "%s" ( car err ))
       ))
    ( select-window curwin )
    )
)


(defun scroll-up-one        () "Scroll up one line."      (interactive) ( scroll-up-with-mouse   1 ))
(defun scroll-down-one      () "Scroll down one line."    (interactive) ( scroll-down-with-mouse 1 ))
(defun scroll-up-slightly   () "Scroll up 5 lines."       (interactive) ( scroll-up-with-mouse   5 ))
(defun scroll-down-slightly () "Scroll down 5 lines."     (interactive) ( scroll-down-with-mouse 5 ))
(defun scroll-up-half       () "Scroll up half a page."   (interactive) ( scroll-up-with-mouse   (/ (window-height) 2 )))
(defun scroll-down-half     () "Scroll down half a page." (interactive) ( scroll-down-with-mouse (/ (window-height) 2 )))
(defun scroll-up-a-lot      () "Scroll up a page."        (interactive) ( scroll-up-with-mouse   (- (window-height) 2 )))
(defun scroll-down-a-lot    () "Scroll down a page."      (interactive) ( scroll-down-with-mouse (- (window-height) 2 )))



(defun reverse-yank-pop (arg)
   "Like yank-pop, only backwards."
   (interactive "*p")
   (yank-pop (- arg)))

(defun clipboard-yank ()
   "yank the current clipboard"
   (interactive)
   (case (device-type)
;     (x (x-yank-function))
     (x (mouse-consolidated-yank))
     (tty (yank))
     (mswindows (mswindows-paste-clipboard))
     (otherwise (yank))))



;; key bindings - names depend on emacs/xemacs
(cond
 ((string-match "XEmacs" emacs-version)
  (global-set-key [button4]		    'scroll-down-slightly )
  (global-set-key [button5]		    'scroll-up-slightly )
  (global-set-key [(shift button4)]	    'scroll-down-one)
  (global-set-key [(shift button5)]	    'scroll-up-one)
  (global-set-key [(control button4)]	    'scroll-down-a-lot)
  (global-set-key [(control button5)]	    'scroll-up-a-lot)
  (global-set-key [(control shift button4)] 'scroll-down-half )
  (global-set-key [(control shift button5)] 'scroll-up-half )

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
  (global-set-key [(shift insert)]	    'clipboard-yank)
  )

 ( t
  (global-set-key [mouse-4]		'scroll-down-slightly )
  (global-set-key [mouse-5]		'scroll-up-slightly )
  (global-set-key [S-mouse-4]		'scroll-down-one)
  (global-set-key [S-mouse-5]		'scroll-up-one)
  (global-set-key [C-mouse-4]		'scroll-down-a-lot)
  (global-set-key [C-mouse-5]		'scroll-up-a-lot)
  (global-set-key [C-S-mouse-4]		'scroll-down-half )
  (global-set-key [C-S-mouse-5]		'scroll-up-half )

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
  (global-set-key [S-insert]		'clipboard-yank)

  (global-set-key "\M-g"                'goto-line)
  ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; bodge customisations to be ignored under emacs 19
;

(if (not (fboundp 'custom-set-variables))
  (defun custom-set-variables (&rest args)))
(if (not (fboundp 'custom-set-faces))
  (defun custom-set-faces (&rest args)))

( set-face-foreground 'default "white" )


;;;;;;;;;;;;;;
;
; tab stuff

; default tab width

; set up tab stops
(defun set-tab-width (width)
  "Set tab-width to width and set tab-stop-list to stops every width chars"
  (interactive "Nwidth: ")
  (setq tab-width width )
  (setq tab-stop-list (range tab-width 132 tab-width )))


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
; handy kbd macros
;
(defalias 'insert-preprocess-lines (read-kbd-macro
"PREPROCESS_ C-SPC C-e M-u 4*<backspace> =cat C-a C-SPC <down> M-w C-y <up> 3*<right> 2*<backspace> OST <down> C-a"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; automatic stuff follows - don't fiddle!


;; Options Menu Settings
;; =====================
(cond
 ((and (string-match "XEmacs" emacs-version)
       (boundp 'emacs-major-version)
       (or (and
            (= emacs-major-version 19)
            (>= emacs-minor-version 14))
           (= emacs-major-version 20))
       (fboundp 'load-options-file))
  (load-options-file "~/.xemacs-options")))
;; ============================
;; End of Options Menu Settings




(setq minibuffer-max-depth nil)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(load-home-init-file t t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

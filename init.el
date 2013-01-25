;;-----------------------------------------------------------------------;;
;;----------------------[ init.el by Lars Tveito ]-----------------------;;
;;---------------------------[ 15. Jul, 2012 ]---------------------------;;
;;-----------------------------------------------------------------------;;


;------------------------------[ libraries ]------------------------------;

(let ((default-directory "~/Dropbox/ifi/.emacs.d/site-lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;-------------------------------------------------------------------------;



;---------------------------[ initializations ]---------------------------;

(package-initialize)
(color-theme-initialize)

;-------------------------------------------------------------------------;



;--------------------------[ loads color-theme ]--------------------------;

(load-library "subdued")
(require 'color-theme-subdued)
(color-theme-subdued)

;-------------------------------------------------------------------------;



;--------------------[ remove bars, blink and splash ]--------------------;

(menu-bar-mode                  -1)
(tool-bar-mode                  -1)
(scroll-bar-mode                -1)
(blink-cursor-mode               0)
(setq inhibit-startup-message    t)
(setq-default indent-tabs-mode nil)

;-------------------------------------------------------------------------;



;-----------------------------[ Für Package ]-----------------------------;

(setq package-archives 
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")))

;-------------------------------------------------------------------------;



;----------------------[ UTF-8 and tex-input mode ]-----------------------;

(set-language-environment "UTF-8")
(setq default-input-method "TeX"
      initial-scratch-message '())

;-------------------------------------------------------------------------;



;-----------------[ show parentheses, ido-mode and y/n ]------------------;

(ido-mode t)
(show-paren-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(setq ido-file-extensions-order 
      '("src" ".Org" ".java" ".c" ".h" ".emacs" ".el" ".tex"))

;-------------------------------------------------------------------------;



;-------------[ Delete selection, veiw column, load header ]--------------;

(delete-selection-mode t)
(setq column-number-mode t)
(autoload 'header "header" nil t)

;-------------------------------------------------------------------------;



;--------------------------------[ Mail ]---------------------------------;

(setq
 user-mail-address             "larstvei@student.matnat.uio.no"
 user-full-name                "Lars Tveito"
 send-mail-function            'smtpmail-send-it
 smtpmail-smtp-server          "smtp.uio.no"
 smtpmail-smtp-service         587)

;-------------------------------------------------------------------------;



;------------------------------[ Autosave ]-------------------------------;

(defvar emacs-autosave-directory "~/Dropbox/ifi/.emacs.d/autosaves/")
(setq backup-directory-alist
      `((".*" . ,emacs-autosave-directory))
      auto-save-file-name-transforms
      `((".*" ,emacs-autosave-directory t)))
      
;-------------------------------------------------------------------------;



;---------------------------[ Resize buffers ]----------------------------;

(global-set-key (kbd "<M-left>") 
		(lambda () (interactive)
                  (enlarge-window -1 t)))
(global-set-key (kbd "<M-right>") 
		(lambda () (interactive)
                  (enlarge-window 1 t)))
(global-set-key (kbd "<M-up>") 
		(lambda () (interactive)
                  (enlarge-window -1)))
(global-set-key (kbd "<M-down>") 
		(lambda () (interactive)
                  (enlarge-window 1)))

;-------------------------------------------------------------------------;



;--------------------[ start eshell and bind to key ]---------------------;

(global-set-key (kbd "C-x t") 
		(lambda () (interactive) 
		  (if (string= (buffer-name) "*eshell*")
		      (switch-to-buffer (second (buffer-list)))
		      (eshell))))

;-------------------------------------------------------------------------;



;----------------------------[ Auto-complete ]----------------------------;

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories 
	     "/home/lars/Dropbox/ifi/.emacs.d/lisp//ac-dict")
(ac-config-default)

;-------------------------------------------------------------------------;


;-------------------------------[ DocView ]-------------------------------;

(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(setq doc-view-continuous t)

;-------------------------------------------------------------------------;


;----------------------------[ Key bindings ]-----------------------------;

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(key-chord-mode 1)

(key-chord-define-global ";'" 'ace-jump-mode)
(key-chord-define-global "qw" 'mc/mark-all-like-this)
(key-chord-define-global "qs" 'mc/mark-next-like-this)

(global-set-key (kbd "C-'") 'er/expand-region)
(global-set-key (kbd "C-;") 'er/contract-region)

;-------------------------------------------------------------------------;



;-------------------[ Replace expression with output ]--------------------

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

;-------------------------------------------------------------------------;


;-------------------------------------------------------------------------;
;--------------[ * Programming-language-modes specifics * ]---------------;
;-------------------------------------------------------------------------;

;--------------------------------[ Java ]---------------------------------;

(autoload 'java-extras "java-extras" "Fold brackets")
(add-hook 'java-mode-hook 'java-extras)
;;TODO Fix this ugliness

;-------------------------------------------------------------------------;


;----------------------------------[ C ]----------------------------------;

(load-library "hideshow-on")
(add-hook 'c-mode-common-hook 'hideshow-on)

;-------------------------------------------------------------------------;


;--------------------------------[ Lisp ]---------------------------------;

;; Show λ in Lisp code.
(defun sm-lambda-mode-hook ()
  (font-lock-add-keywords
   nil `(("\\<lambda\\>"
	  (0 (progn (compose-region (match-beginning 0) (match-end 0)
				    ,(make-char 'greek-iso8859-7 107))
		    nil))))))
(dolist (h '(lisp-mode-hook
             scheme-mode-hook
             emacs-lisp-mode-hook
             slime-repl-mode-hook
             inferior-lisp-mode-hook
             inferior-scheme-mode-hook
             lisp-interaction-mode-hook))
  (progn
    (add-hook h (lambda () (paredit-mode 1)))
    (add-hook h 'sm-lambda-mode-hook)))

(when (file-exists-p "~/quicklisp/slime-helper.el")
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

(setq inferior-lisp-program "sbcl")

;-------------------------------------------------------------------------;


;-------------------------------[ Scheme ]--------------------------------;

(require 'quack)

(autoload 'scheme-smart-complete "scheme-complete" nil t)
(eval-after-load 'scheme
  '(define-key scheme-mode-map "\t" 'scheme-complete-or-indent))
(autoload 'scheme-get-current-symbol-info "scheme-complete" nil t)
(add-hook 'scheme-mode-hook
          (lambda ()
            (make-local-variable 'eldoc-documentation-function)
            (setq eldoc-documentation-function
                  'scheme-get-current-symbol-info)
            (eldoc-mode)))

;-------------------------------------------------------------------------;

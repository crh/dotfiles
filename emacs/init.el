; TODO: on start, full screen
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("elpa" . "http://tromey.com/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ))

(package-initialize)

;; VIM like
; long live vim binding
(require 'evil)
(evil-mode 1)

; simulate vim tabs
(load "elscreen" "ElScreen" t)

(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;creat tab
(define-key evil-normal-state-map (kbd "C-w x") 'elscreen-kill) ;kill tab

(define-key evil-normal-state-map "gT" 'elscreen-previous) ;previous tab
(define-key evil-normal-state-map "gt" 'elscreen-next) ;next tab

; show line number
(global-linum-mode 1)

; ido mode
(ido-mode 1)
(setq ido-enable-flex-matching t)

; spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

; auto indent
(define-key global-map (kbd "RET") 'newline-and-indent)

; whitespaces
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing whitespace-line-column))
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

(setq whitespace-style (quote (tabs newline tab-mark newline-mark)))
(global-whitespace-mode t)
(setq whitespace-display-mappings
      ;; all numbers are Unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (newline-mark 10 [182 10]) ; 10 LINE FEED
        (tab-mark 9 [9655 9] [92 9]) ; 9 TAB, 9655 WHITE RIGHT-POINTING TRIANGLE 「▷」
        ))

; on save hook
; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; org mode
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

(require 'org)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

; org babel
(org-babel-do-load-languages
  'org-babel-load-languages
  '((perl . t)
    (js . t)
    (java . t)
    (ruby . t)
    (sh . t)
    (clojure . t)
    (python . t)
    (emacs-lisp . t)
  ))

; syntax highlight for code
(setq org-src-fontify-natively t)

; please be silent
(setq org-confirm-babel-evaluate nil)

; spell correction
(setq ispell-program-name "/opt/local/bin/aspell")
(setq ispell-list-command "list")

;; SLIME
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime)
(slime-setup)

;; SWANK JS
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))
(load-file "~/.emacs.d/setup-slime-js.el")

;; JavaScript

; TODO: js, ruby, python, nodejs REPL, SLIME
; TODO: make editorconfig works.
;(add-to-list 'load-path "~/.emacs.d/")
; (load "editorconfig")
(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq-default js2-basic-offset 2) ; 2 spaces for indentation (if you prefer 2 spaces instead of default 4 spaces for tab)

     ;; add from jslint global variable declarations to js2-mode globals list
     ;; modified from one in http://www.emacswiki.org/emacs/Js2Mode
     (defun my-add-jslint-declarations ()
       (when (> (buffer-size) 0)
         (let ((btext (replace-regexp-in-string
                       (rx ":" (* " ") "true") " "
                       (replace-regexp-in-string
                        (rx (+ (char "\n\t\r "))) " "
                        ;; only scans first 1000 characters
                        (save-restriction (widen) (buffer-substring-no-properties (point-min) (min (1+ 1000) (point-max)))) t t))))
           (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                 (split-string
                  (if (string-match (rx "/*" (* " ") "global" (* " ") (group (*? nonl)) (* " ") "*/") btext)
                      (match-string-no-properties 1 btext) "")
                  (rx (* " ") "," (* " ")) t))
           )))
     (add-hook 'js2-post-parse-callbacks 'my-add-jslint-declarations)))

; set color theme molokai
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-molokai)

; backup files in ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

; blink it
(blink-cursor-mode 1)

(set-default-font "Monaco-12")

(require 'grails-mode)
(setq grails-mode t)
(setq project-mode t)
(add-to-list 'auto-mode-alist '("\.gsp$" . html-mode)) ; Use whatever mode you want for views.


; language tool
; https://github.com/mhayashi1120/Emacs-langtool

(require 'langtool)
(setq langtool-language-tool-jar "~/.emacs.d/crh/LanguageTool-2.1/languagetool-commandline.jar")
(setq langtool-mother-tongue "en")

;; key binding
(global-set-key "\C-x4w" 'langtool-check)
(global-set-key "\C-x4W" 'langtool-check-done)
(global-set-key "\C-x4l" 'langtool-switch-default-language)
(global-set-key "\C-x44" 'langtool-show-message-at-point)
(global-set-key "\C-x4c" 'langtool-correct-buffer)

; mark-multiple
;; github:  https://github.com/magnars/mark-multiple.el
;; demo://emacsrocks.com/e08.html
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(add-hook 'sgml-mode-hook
          (lambda ()
            (require 'rename-sgml-tag)
            (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))

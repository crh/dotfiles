;;TODO install org-confluence
; (require 'org-confluence)
(setq url-proxy-services '(("no_proxy" . "sap.corp")
                           ("http" . "proxy.wdf.sap.corp:8080")))

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

(define-key evil-normal-state-map (kbd "C-w t") 'elscreen-create) ;create tab
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
(setq ispell-program-name "/usr/bin/aspell")
(setq ispell-list-command "list")

;; SLIME
;(add-to-list 'load-path "~/.emacs.d/slime")
;(require 'slime)
;(slime-setup)

;; SWANK JS
;(autoload 'js2-mode "js2-mode" nil t)
;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;(global-set-key [f5] 'slime-js-reload)
;(add-hook 'js2-mode-hook
;          (lambda ()
;            (slime-js-minor-mode 1)))
;(add-hook 'css-mode-hook
;          (lambda ()
;            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)
;            (define-key css-mode-map "\C-c\C-r" 'slime-js-embed-css)))
;(load-file "~/.emacs.d/setup-slime-js.el")

;; JavaScript

; TODO: js, ruby, python, nodejs REPL, SLIME
; TODO: make editorconfig works.
;(add-to-list 'load-path "~/.emacs.d/")
; set color theme molokai
(require 'color-theme)
(setq color-theme-is-global t)
;(color-theme-molokai)
(load-theme 'solarized-dark t)

; backup files in ~/.saves
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

; blink it
(blink-cursor-mode 1)

;(require 'grails-mode)
;(setq grails-mode t)
;(setq project-mode t)
;(add-to-list 'auto-mode-alist '("\.gsp$" . html-mode)) ; Use whatever mode you want for views.


; language tool
; https://github.com/mhayashi1120/Emacs-langtool

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("4a60f0178f5cfd5eafe73e0fc2699a03da90ddb79ac6dbc73042a591ae216f03" default)))
 '(org-agenda-files nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Source Code Pro" :foundry "adobe" :slant normal :weight normal :height 98 :width normal)))))

;; mobileorg settings
; staging area for sync
;(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
;(setq org-directory "~/Dropbox/org")
;(setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
;(setq org-mobile-files '("~/Dropbox/org"))

;(defvar org-mobile-push-timer nil
;  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

;(defun org-mobile-push-with-delay (secs)
;  (when org-mobile-push-timer
;    (cancel-timer org-mobile-push-timer))
;  (setq org-mobile-push-timer
;        (run-with-idle-timer
;         (* 1 secs) nil 'org-mobile-push)))

;(add-hook 'after-save-hook
; (lambda ()
;   (when (eq major-mode 'org-mode)
;     (dolist (file (org-mobile-files-alist))
;      (if (string= (file-truename (expand-file-name (car file)))
;         (file-truename (buffer-file-name)))
;           (org-mobile-push-with-delay 30)))
;   )))

;(run-at-time "00:05" 86400 '(lambda () (org-mobile-push-with-delay 1))) ;; refreshes agenda file each day

;(org-mobile-pull) ;; run org-mobile-pull at startup

;(defun install-monitor (file secs)
;  (run-with-timer
;   0 secs
;   (lambda (f p)
;     (unless (< p (second (time-since (elt (file-attributes f) 5))))
;       (org-mobile-pull)))
;   file secs))

;(install-monitor (file-truename
;                  (concat
;                   (file-name-as-directory org-mobile-directory)
;                          org-mobile-capture-file))
;                 5)

;; Do a pull every 5 minutes to circumvent problems with timestamping
;; (ie. dropbox bugs)
;(run-with-timer 0 (* 5 60) 'org-mobile-pull)

(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

; Invoke M-x without the ALT key
; Source: https://sites.google.com/site/steveyegge2/effective-emacs
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; Item 3: Prefer backward-kill-word over Backspace
; bind C-w to `delete previous word` or `db` in vim
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;;; langtool-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (langtool-switch-default-language langtool-check-buffer)
;;;;;;  "langtool" "langtool.el" (20544 26226))
;;; Generated autoloads from langtool.el

(autoload 'langtool-check-buffer "langtool" "\
Check context current buffer and light up errors.
Optional \\[universal-argument] read LANG name.

You can change the `langtool-default-language' to apply all session.

\(fn &optional LANG)" t nil)

(autoload 'langtool-switch-default-language "langtool" "\
Switch `langtool-read-lang-name' to LANG

\(fn LANG)" t nil)

;;;***

;;;### (autoloads nil nil ("langtool-pkg.el") (20544 26226 951775))

;;;***

(provide 'langtool-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; langtool-autoloads.el ends here

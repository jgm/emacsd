;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(add-to-list 'load-path "~/.emacs.d")

(menu-bar-mode -1)

(quietly-read-abbrev-file "~/.emacs.d/abbrev_defs")

(recentf-mode)

(setq visible-bell t)   ; use visual instead of audio bell

;; VIMPULSE - vim bindings in emacs!

;(add-to-list 'load-path "~/.emacs.d/vimpulse")
;(require 'vimpulse)
;(setq viper-mode t)                ; enable Viper at load time
;(setq viper-ex-style-editing nil)  ; can backspace past start of insert / line
;(setq woman-use-own-frame nil)     ; don't create new frame for manpages
;(setq woman-use-topic-at-point t)  ; don't prompt upon K key (manpage display)

;; Org

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-agenda-files (list "~/org/todo.org"))
(setq org-tag-alist '(("read" . ?r)
		      ("142" . ?1)
		      ("dgs" . ?d)
		      ("seminar" . ?s)
		      ("code" . ?c)))

(require 'redo)       ; enables C-r (redo key)
(require 'rect-mark)  ; enables nice-looking block visual mode

;;; IDE

(setq show-trailing-whitespace t)
;;; Delete selected text on insert
(delete-selection-mode 1)
;;; Display current column
(column-number-mode 1)
;;; Show parentheses matching
(show-paren-mode 1)

;;; Text files
(require 'markdown-mode)
(add-hook 'text-mode-hook (lambda ()
          (turn-on-filladapt-mode)
          (turn-on-auto-fill)
          (setq indent-tabs-mode nil)
	  (markdown-mode)))

;;; Lisp
(setq inferior-lisp-program "clojure")
;(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook
    (lambda ()
      (slime-mode t)
      ;;(paredit-mode +1)
      (setq lisp-indent-function 'common-lisp-indent-function)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;; Haskell
(load "~/.emacs.d/haskellmode-emacs/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook (lambda () (require 'inf-haskell)))
(add-hook 'haskell-mode-hook (lambda ()
             (require 'hs-lint)
             (setq hs-lint-command "~/.cabal/bin/hlint")
             (setq hs-lint-replace-with-suggestions t)))
(setq haskell-program-name "ghci")


;; Make AUCTeX aware of style files and multi-file documents
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook
    (lambda ()
      (add-to-list 'TeX-command-list '("make" "make" TeX-run-compile nil
               (latex-mode doctex-mode) :help
               "Run make"))))

;; Start up reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;;; Colors
;(require 'color-theme)
;(color-theme-charcoal-black)
;(color-theme-billw)
;(color-theme-comidia)

;;; Snippets
 (add-to-list 'load-path
                  "~/.emacs.d/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")
(yas/load-directory "~/.emacs.d/mysnippets")

;; (server-start)

;;; Interactively do things (switch buffers, open files)
(require 'ido)
(ido-mode t)

(if (fboundp 'toggle-save-place-globally) (toggle-save-place-globally 1))

(global-set-key [f2] 'recentf-open-files)
(global-set-key [S-f2] 'dired)

(global-set-key [f3] 'query-replace-regexp)

(global-set-key [f4] 'delete-other-windows)

(global-set-key [f5] 'switch-prev-buffer)
(global-set-key [S-f5] 'kill-this-buffer)

(global-set-key [f6] 'other-window)
(global-set-key [S-f6] 'delete-window)
(global-set-key [f7] 'split-window-vertically)
(global-set-key [S-f7] 'split-window-horizontally)

(global-set-key "\M-s" 'save-buffer)
(global-set-key "\M-g" 'goto-line)

(defun switch-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(setq-default ispell-program-name "aspell")

;;; Backup .emacs files
;;; see chapter M.3.1 (Backup Files) in the Emacs manual
;;;   use numbered backups
(setq version-control t)
;;;   always keep the oldest backup
(setq kept-old-versions 1)
;;;   keep the most recent two backups
(setq kept-new-versions 2)
;;;   emacs will delete the excess middle versions silently
(setq delete-old-versions t)

;;; magit
(require 'magit)
(global-set-key "\C-cg" 'magit-status)

;; or:
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
;; Declaration scanning: just use M-x imenu or bind `imenu' to a key.  E.g.
;; (global-set-key [(control meta down-mouse-3)] 'imenu) or you can also add
;; it to the menubar with (add-hook 'haskell-mode-hook 'imenu-add-menubar-index)
;; Interaction with inferior Haskell interpreter: just hit C-c C-z  or  C-c C-l.

(put 'upcase-region 'disabled nil)

;; keep customizations in another file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file 'noerror)

;; local config (not in the repository)
(if (file-exists-p "~/.emacs.d/local/init.el")
  (load "~/.emacs.d/local/init.el"))

;; Internationalization
(add-hook 'server-visit-hook
	  (lambda ()
	    (prefer-coding-system 'utf-8)
	    (setq locale-coding-system 'utf-8)
	    (set-terminal-coding-system 'utf-8)
	    (set-keyboard-coding-system 'utf-8)
	    (set-selection-coding-system 'utf-8)))

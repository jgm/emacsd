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
(if window-system
    (fringe-mode))
(if (fboundp 'toggle-save-place-globally)
    (toggle-save-place-globally 1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))

;; local config (not in the repository)
(if (file-exists-p "~/.emacs.d/local/init.el")
  (load "~/.emacs.d/local/init.el"))

(quietly-read-abbrev-file "~/.emacs.d/abbrev_defs")

(recentf-mode)

(setq visible-bell t)   ; use visual instead of audio bell

;;; Configuration for editing emails in mutt
(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist
             '("\\.*mutt-*\\|.article\\|\\.followup"
                . post-mode))
(add-hook 'post-mode-hook
  (lambda()
    (set-default 'post-attachment-regexp "^[^>]*attach")
    (auto-fill-mode t)
    (setq fill-column 72)  ; rfc 1855 for usenet messages
    (post-goto-body)))

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

(setq org-default-notes-file "~/org/notes.org")
(org-remember-insinuate)
(global-set-key (kbd "C-c r") 'remember)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/org/todo.org" top)
	("Note" ?n "* %^{Brief Description} %^g\n%?\nAdded: %U" "~/org/notes.org" top)))

(defun todo ()
  (interactive)
  (find-file-existing "~/org/todo.org"))

(require 'redo)       ; enables C-r (redo key)
(require 'rect-mark)  ; enables nice-looking block visual mode

;;; IDE

(require 'whitespace)
(set-default 'whitespace-style
	     '(tabs trailing space-before-tab space-after-tab))
(global-whitespace-mode)
;;; delete selected text on insert
(delete-selection-mode 1)
;;; Display current column
(column-number-mode 1)
;;; Show parentheses matching
(show-paren-mode 1)

(require 'generalized-shell-command)
(global-set-key (kbd "M-!") 'generalized-shell-command)

;;; Text files
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
	     '("\\.txt$" . markdown-mode))
(add-hook 'text-mode-hook (lambda ()
			    (turn-on-auto-fill)
			    (setq-default line-spacing 5)
			    (setq indent-tabs-mode nil)))

;;; Lisp
(setq inferior-lisp-program "sbcl")

(add-hook 'lisp-mode-hook
    (lambda ()
      (slime-mode t)
      ;;(paredit-mode +1)
      (setq lisp-indent-function 'common-lisp-indent-function)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;; Haskell
(add-to-list 'load-path "~/.emacs.d/haskellmode-emacs")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook (lambda () (require 'inf-haskell)))
(add-hook 'haskell-mode-hook (lambda ()
             (require 'hs-lint)
             (setq hs-lint-command "~/.cabal/bin/hlint")
             (setq hs-lint-replace-with-suggestions t)))
(setq haskell-program-name "ghci")

;;; OCaml
(add-to-list 'load-path "~/.emacs.d/tuareg-2.0.4")
(add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;; Make AUCTeX aware of style files and multi-file documents
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook
    (lambda ()
      (setq-default line-spacing 5)
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

;;; Interactively do things (switch buffers, open files)
(require 'ido)
(ido-mode t)

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

(cua-mode 'emacs)
(global-set-key (kbd "M-SPC") 'cua-set-rectangle-mark)

;;; Winner mode makes C-c left and C-c right cycle through
;;; changes in window configuration.  We also bind ESC (arrow keys)
;;; to window movement.  And, because these bindings are overridden
;;; in org-mode, we also bind to C-x (arrow keys).
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "ESC <left>") 'windmove-left)
  (global-set-key (kbd "ESC <right>") 'windmove-right)
  (global-set-key (kbd "ESC <up>") 'windmove-up)
  (global-set-key (kbd "ESC <down>") 'windmove-down)
  (global-set-key (kbd "C-x <left>") 'windmove-left)
  (global-set-key (kbd "C-x <right>") 'windmove-right)
  (global-set-key (kbd "C-x <up>") 'windmove-up)
  (global-set-key (kbd "C-x <down>") 'windmove-down))

;;; Full screen for OSX
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
                         (if (equal 'fullboth current-value)
                             (if (boundp 'old-fullscreen) old-fullscreen nil)
                           (progn (setq old-fullscreen current-value)
                                  'fullboth)))))
(if (boundp 'osx-key-mode-map)
    (define-key osx-key-mode-map (kbd "A-F") 'toggle-fullscreen))

(defun switch-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(setq-default ispell-program-name "aspell")

;;; Backup .emacs files
;;; see chapter M.3.1 (Backup Files) in the Emacs manual
;;;   use numbered backups
;(setq version-control t)
;;;   always keep the oldest backup
;(setq kept-old-versions 1)
;;;   keep the most recent two backups
;(setq kept-new-versions 2)
;;;   emacs will delete the excess middle versions silently
;(setq delete-old-versions t)

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

;; Internationalization
(add-hook 'server-visit-hook
	  (lambda ()
	    (prefer-coding-system 'utf-8)
	    (setq locale-coding-system 'utf-8)
	    (set-terminal-coding-system 'utf-8)
	    (set-keyboard-coding-system 'utf-8)
	    (set-selection-coding-system 'utf-8)))

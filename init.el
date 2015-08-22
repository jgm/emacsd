(setq my-packages
   '(use-package evil evil-leader evil-jumper deft markdown-mode
     magit full-ack yasnippet js2-mode phi-rectangle haskell-mode
     ghc solarized-theme helm cmake-mode lua-mode))

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)
  ;; Fetch packages from melpa/elpa if not present:
  (defvar refreshed nil)
  (mapc
    (lambda (package)
      (unless (package-installed-p package)
        (unless refreshed
          (package-refresh-contents)
          (setq refreshed t))
        (package-install package))) my-packages))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;;; Local config (not in the repository)
(if (file-exists-p "~/.emacs.d/local/init.el")
  (load "~/.emacs.d/local/init.el"))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (save-buffers-kill-terminal)
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(setq inhibit-splash-screen t)         ; hide welcome screen

(when window-system
  (load-theme 'solarized-dark t))

(menu-bar-mode -1)
(if window-system
    (fringe-mode))
(if (fboundp 'toggle-save-place-globally)
    (toggle-save-place-globally 1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(set-default 'line-spacing 5)
(setq visible-bell t)   ; use visual instead of audio bell

;;; Show whitespace
(add-hook 'find-file-hook
	  (lambda ()
	    (setq show-trailing-whitespace t)))

;;; Display current column
(column-number-mode 1)

;;; Show parentheses matching
(show-paren-mode 1)

;;; Use spaces for automatic indentation
(setq-default indent-tabs-mode nil)
(add-hook 'c-mode-hook (lambda ()
                         (setq c-default-style "linux")
                         (setq c-basic-offset 2)
                         (setq indent-tabs-mode f)
                         (setq 'tab-width 2)))

;;; Ask y-or-n instead of yes-or-no
(fset 'yes-or-no-p 'y-or-n-p)

(quietly-read-abbrev-file "~/.emacs.d/abbrev_defs")

(require 'helm-config)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "M-x") 'helm-M-x)
(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t) ; fuzzy matching buffer names when non--nil
;;; (helm-mode 1)

;;; Interactively do things (switch buffers, open files)
;; (require 'ido)
;; (setq completion-ignored-extensions '(".pdf" ".aux" ".toc" ".tex~"))
;; (setq ido-ignore-extensions t)
;; (setq ido-file-extensions-order '(".org" ".txt" ".tex" ".bib" ".el" ".xml" ".html" ".text" ".rb" ".py" ".ml" ".hs" ".cabal" ".css" ".js"))
;; (add-hook 'ido-setup-hook
;;           (lambda ()
;;             (define-key ido-completion-map [tab] 'ido-next-match)))
;; (ido-mode t)

(cua-mode 'emacs)
(global-set-key (kbd "M-SPC") 'cua-set-rectangle-mark)

(require 'phi-rectangle)  ; enables nice-looking block visual mode
;;; Delete selected text on insert
(delete-selection-mode 1)

;;; Winner mode makes C-c left and C-c right cycle through
;;; changes in window configuration.  We also bind C-x (arrow keys)
;;; to window movement.
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (global-set-key (kbd "C-x <left>") 'windmove-left)
  (global-set-key (kbd "C-x <right>") 'windmove-right)
  (global-set-key (kbd "C-x <up>") 'windmove-up)
  (global-set-key (kbd "C-x <down>") 'windmove-down))

;;; Line cursor not block
(setq-default cursor-type 'bar)

(defun switch-prev-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(setq-default ispell-program-name "aspell")

(put 'upcase-region 'disabled nil)

;; Internationalization
(add-hook 'server-visit-hook
	  (lambda ()
 	    (prefer-coding-system 'utf-8)
 	    (setq locale-coding-system 'utf-8)
 	    (set-terminal-coding-system 'utf-8)
 	    (set-keyboard-coding-system 'utf-8)
 	    (set-selection-coding-system 'utf-8)))

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

;;; Configuration for editing emails in mutt
;; autoload 'post-mode "post" "mode for e-mail" t)
;; (add-to-list 'auto-mode-alist
;;              '("\\.*mutt-*\\|.article\\|\\.followup"
;;                 . post-mode))
;; (add-hook 'post-mode-hook
;;   (lambda()
;;     (set-default 'post-attachment-regexp "^[^>]*attach")
;;     (auto-fill-mode t)
;;     (setq fill-column 72)  ; rfc 1855 for usenet messages
;;     (post-goto-body)))

;;; EVIL mode - vim bindings
(use-package evil)
(use-package evil-leader)
(use-package evil-jumper)
(evil-leader/set-leader ",")
(evil-leader/set-key "b" 'helm-mini)
(setf sentence-end-double-space nil)
(global-evil-leader-mode)
(evil-mode 1)
(load "~/.emacs.d/evil-customizations")

;;; Org
;; (use-package org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook (auto-fill-mode 1))
(setq org-return-follows-link t)
(setq org-log-done t)
(setq org-server "~/Dropbox/")
(setq org-agenda-files (mapcar (lambda (x) (concat org-server x))
			       '("org/todo.org")))
(setq org-tag-alist '(("read" . ?r)
		      ("142" . ?1)
		      ("dgs" . ?d)
		      ("seminar" . ?s)
		      ("work" . ?w)
		      ("code" . ?c)))
(setq wiki-entry-point (concat org-server "wiki/wiki.org"))
(setq org-default-notes-file wiki-entry-point)
;; (org-remember-insinuate)
(global-set-key (kbd "C-c r") 'remember)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" (car org-agenda-files) "UNFILED")
	("Note" ?n "* %^{Brief Description} %^g\n%?\nAdded: %U" org-default-notes-file "UNFILED")))

(defun todo ()
  (interactive)
  (find-file-existing (car org-agenda-files)))

(defun wiki ()
  (interactive)
  (find-file-existing wiki-entry-point))

;;; Deft - note taking
(use-package deft)
(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/notes/")
(setq deft-text-mode 'markdown-mode)
(setq deft-use-filename-as-title t)
; (setq deft-use-filter-string-for-filename t)
(setq deft-file-naming-rules '((noslash . "-")
                               (nospace . "-")))
(global-set-key [f9] 'deft)

(use-package generalized-shell-command)
(global-set-key (kbd "M-!") 'generalized-shell-command)

;;; Text files
(use-package markdown-mode)
(add-hook 'markdown-mode-hook
          ;; fix Markdown mode so it allows link labels to wrap:
          (lambda ()
            (remove-hook 'fill-nobreak-predicate
                         'markdown-inside-link-text-p t)))

(add-to-list 'auto-mode-alist
	     '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.md$" . markdown-mode))
(add-hook 'text-mode-hook (lambda ()
			    (turn-on-auto-fill)
			    (setq indent-tabs-mode nil)))

(use-package cmake-mode)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt$" . cmake-mode))

(use-package lua-mode)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))

;;; Rust
;; (add-to-list 'load-path "~/.emacs.d/rust")
;; (use-package rust-mode)
;; (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;;; Javascript
(use-package js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

;;; Lisp
;; (setq inferior-lisp-program "sbcl")

;; (add-hook 'lisp-mode-hook
;;     (lambda ()
;;       (slime-mode t)
;;       ;;(paredit-mode +1)
;;       (setq lisp-indent-function 'common-lisp-indent-function)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;; Haskell
(use-package haskell-mode)
(defun start-ghc-mod ()
  (interactive)
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (ghc-init))
(setq haskell-interactive-mode-eval-mode 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(global-set-key [(control meta down-mouse-3)] 'imenu)
(setenv "PATH" (concat "~/.cabal/bin:~/Library/Haskell/bin:" (getenv "PATH")))
(add-to-list 'exec-path "~/.cabal/bin")
(add-to-list 'exec-path "~/Library/Haskell/bin")

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-tags-on-save t)
  '(haskell-process-log t))

(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;;; OCaml
;; (add-to-list 'load-path "~/.emacs.d/tuareg-2.0.4")
;; (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; Coq
;; (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;;; LaTeX (AUCTeX)
;; (use-package auctex)
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)
;; (setq-default reftex-toc-split-windows-horizontally t)
;; (setq-default TeX-PDF-mode t)
;; (add-hook 'LaTeX-mode-hook
;;     (lambda ()
;;       (add-to-list 'TeX-command-list '("make" "make" TeX-run-compile nil
;;                (latex-mode doctex-mode) :help
;;                "Run make"))))
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
;; (add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;;; LaTeX lightweight
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;;; Snippets
(use-package yasnippet)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)
(yas/global-mode 1)

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
(use-package magit)
(global-set-key "\C-cg" 'magit-status)

;;; tidy xml buffer
(defun xml-tidy-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max)
			   "tidy -q -xml -utf8 -i" t t)
  (keyboard-quit))

;;; full ack
(autoload 'ack-same  "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; Make C-x k kill buffer w/o prompt if emacsclient
(add-hook 'server-switch-hook
	  (lambda ()
	    (when (current-local-map)
	      (use-local-map (copy-keymap (current-local-map))))
	    (when server-buffer-clients
	      (local-set-key (kbd "C-x k") 'server-edit))))

;; Rebind C-x C-b so it doesn't list buffers in other window.
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; keep customizations in another file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file 'noerror)

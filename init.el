;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

(add-to-list 'load-path "~/.emacs.d")

(setq inhibit-splash-screen t)         ; hide welcome screen

;;; Local config (not in the repository)
(if (file-exists-p "~/.emacs.d/local/init.el")
  (load "~/.emacs.d/local/init.el"))

(menu-bar-mode -1)
(if window-system
    (fringe-mode))
(if (fboundp 'toggle-save-place-globally)
    (toggle-save-place-globally 1))
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(set-default 'line-spacing 3)
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

(quietly-read-abbrev-file "~/.emacs.d/abbrev_defs")

;;; Interactively do things (switch buffers, open files)
(require 'ido)
(setq completion-ignored-extensions '(".pdf" ".aux" ".toc" ".tex~"))
(setq ido-ignore-extensions t)
(setq ido-file-extensions-order '(".org" ".txt" ".tex" ".bib" ".el" ".xml" ".html" ".text" ".rb" ".py" ".ml" ".hs" ".cabal" ".css" ".js"))
(add-hook 'ido-setup-hook 
          (lambda () 
            (define-key ido-completion-map [tab] 'ido-next-match)))
(ido-mode t)

(cua-mode 'emacs)
(global-set-key (kbd "M-SPC") 'cua-set-rectangle-mark)
(require 'rect-mark)  ; enables nice-looking block visual mode
;;; Delete selected text on insert
(delete-selection-mode 1)

;;; Winner mode makes C-c left and C-c right cycle through
;;; changes in window configuration.  We also bind ESC (arrow keys)
;;; to window movement.  And, because these bindings are overridden
;;; in org-mode, we also bind to C-x (arrow keys).
;; (when (fboundp 'winner-mode)
;;   (winner-mode 1)
;;   (global-set-key (kbd "ESC <left>") 'windmove-left)
;;   (global-set-key (kbd "ESC <right>") 'windmove-right)
;;   (global-set-key (kbd "ESC <up>") 'windmove-up)
;;   (global-set-key (kbd "ESC <down>") 'windmove-down)
;;   (global-set-key (kbd "C-x <left>") 'windmove-left)
;;   (global-set-key (kbd "C-x <right>") 'windmove-right)
;;   (global-set-key (kbd "C-x <up>") 'windmove-up)
;;   (global-set-key (kbd "C-x <down>") 'windmove-down))

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

;;; EVIL mode - vim bindings
;(add-to-list 'load-path "~/.emacs.d/evil") ; (now we use MELPA version)
(require 'evil)
(require 'evil-leader)
(evil-leader/set-leader ",")
(require 'evil-jumper)
(evil-mode 1)
(load "evil-customizations")

;;; Org
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
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
(add-to-list 'load-path "~/.emacs.d/deft")
(require 'deft)
(setq deft-extension "txt")
(setq deft-directory "~/Dropbox/notes/")
(setq deft-text-mode 'markdown-mode)
(global-set-key [f8] 'deft)

(require 'generalized-shell-command)
(global-set-key (kbd "M-!") 'generalized-shell-command)

;;; Text files
(require 'markdown-mode)
(add-to-list 'auto-mode-alist
	     '("\\.txt$" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist
             '("\\.md$" . markdown-mode))
(add-hook 'text-mode-hook (lambda ()
			    (turn-on-auto-fill)
			    (setq indent-tabs-mode nil)))

;;; Rust
;; (add-to-list 'load-path "~/.emacs.d/rust")
;; (require 'rust-mode)
;; (add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;;; Javascript
(add-to-list 'load-path "~/.emacs.d/js2-mode")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-basic-offset 2)

;;; Go
;; (require 'go-mode-load)

;;; Lisp
;; (setq inferior-lisp-program "sbcl")

;; (add-hook 'lisp-mode-hook
;;     (lambda ()
;;       (slime-mode t)
;;       ;;(paredit-mode +1)
;;       (setq lisp-indent-function 'common-lisp-indent-function)))
;; (add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

;;; Haskell
(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(global-set-key [(control meta down-mouse-3)] 'imenu)
;; Interaction with inferior Haskell interpreter: just hit C-c C-z or C-c C-l.

;;; OCaml
;; (add-to-list 'load-path "~/.emacs.d/tuareg-2.0.4")
;; (add-to-list 'auto-mode-alist '("\\.ml[iylp]?" . tuareg-mode))
;; (autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
;; (autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; Coq
;; (setq auto-mode-alist (cons '("\\.v$" . coq-mode) auto-mode-alist))
;; (autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)

;;; LaTeX (AUCTeX)
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

;; from http://superuser.com/questions/125027/word-count-for-latex-within-emacs
;; (defun my-latex-setup ()
;;   (defun latex-word-count ()
;;     (interactive)
;;     (let* ((this-file (buffer-file-name))
;;            (word-count
;;             (with-output-to-string
;;               (with-current-buffer standard-output
;;                 (call-process "texcount" nil t nil "-brief" "-inc" this-file)))))
;;       (string-match "\n$" word-count)
;;       (message (replace-match "" nil nil word-count))))
;;     (define-key LaTeX-mode-map "\C-cw" 'latex-word-count))
;; (add-hook 'LaTeX-mode-hook 'my-latex-setup t)

;;; LaTeX lightweight
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;;; Colors
(defun colors ()
  (interactive)
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-select))

;;; Snippets
(require 'yasnippet)
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
(require 'magit)
(global-set-key "\C-cg" 'magit-status)

;;; tidy xml buffer
(defun xml-tidy-buffer ()
  (interactive)
  (shell-command-on-region (point-min) (point-max)
			   "tidy -q -xml -utf8 -i" t t)
  (keyboard-quit))

;;; full ack
(add-to-list 'load-path "~/.emacs.d/full-ack")
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

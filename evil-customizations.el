(setq evil-default-state 'normal)
(defun evil-space-del (statemap)
  (define-key statemap (kbd "SPC") (lambda ()
				     (interactive)
				     (evil-scroll-down nil)))
  (define-key statemap [backspace] (lambda ()
				     (interactive)
				     (evil-scroll-up nil)))
  (define-key statemap (kbd "DEL") (lambda ()
				     (interactive)
				     (evil-scroll-up nil))))
(evil-space-del evil-normal-state-map)
(evil-space-del evil-visual-state-map)

(define-key evil-normal-state-map (kbd "C-n") (lambda ()
						(interactive)
						(evil-next-buffer)))
(define-key evil-normal-state-map (kbd "C-p") (lambda ()
						(interactive)
						(evil-prev-buffer)))
(define-key evil-normal-state-map (kbd "C-\\") (lambda ()
						(interactive)
						(fill-paragraph)))

; make evil work for org-mode!
(evil-define-key 'normal org-mode-map "O" (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-heading)
                     (evil-append nil)
                     ))

(defun always-insert-item ()
     (interactive)
     (if (not (org-in-item-p))
       (insert "\n- ")
       (org-insert-item)))

(evil-define-key 'normal org-mode-map "O" (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-heading)
                     (evil-append nil)
                     ))
(evil-define-key 'normal org-mode-map "o" (lambda ()
                     (interactive)
                     (end-of-line)
                     (always-insert-item)
                     (evil-append nil)
                     ))
(evil-define-key 'normal org-mode-map "t" (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-todo-heading nil)
                     (evil-append nil)
                     ))
(evil-define-key 'normal org-mode-map (kbd "M-o") (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-heading)
                     (org-metaright)
                     (evil-append nil)
                     ))
(evil-define-key 'normal org-mode-map (kbd "M-t") (lambda ()
                     (interactive)
                     (end-of-line)
                     (org-insert-todo-heading nil)
                     (org-metaright)
                     (evil-append nil)
                     ))
(evil-define-key 'normal org-mode-map "T" 'org-todo) ; mark a TODO item as DONE
(evil-define-key 'normal org-mode-map ";a" 'org-agenda) ; access agenda buffer
(evil-define-key 'normal org-mode-map "-" 'org-cycle-list-bullet) ; change bullet style
(evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle) ; cycle

; allow us to access org-mode keys directly from Evil's Normal mode
(evil-define-key 'normal org-mode-map (kbd "M-l") 'org-metaright)
(evil-define-key 'normal org-mode-map (kbd "M-h") 'org-metaleft)
(evil-define-key 'normal org-mode-map (kbd "M-k") 'org-metaup)
(evil-define-key 'normal org-mode-map (kbd "M-j") 'org-metadown)
(evil-define-key 'normal org-mode-map (kbd "M-L") 'org-shiftmetaright)
(evil-define-key 'normal org-mode-map (kbd "M-H") 'org-shiftmetaleft)
(evil-define-key 'normal org-mode-map (kbd "M-K") 'org-shiftmetaup)
(evil-define-key 'normal org-mode-map (kbd "M-J") 'org-shiftmetadown)

;;; Next and previous error

(evil-ex-define-cmd "[cn]ext" 'next-error)
(evil-ex-define-cmd "[c]prev" 'previous-error)

;;; Make

(evil-ex-define-cmd "make" 'compile)

;;; ZZ

(define-key evil-normal-state-map "ZZ" 'evil-save-and-close)

;; The following will create a minor mode foo-mode with Normal
;; state bindings for the keys w and e:
;;      (define-minor-mode foo-mode
;;        "Foo mode."
;;        :keymap (make-sparse-keymap))
;;      (evil-define-key ’normal foo-mode-map "w" ’bar)
;;      (evil-define-key ’normal foo-mode-map "e" ’baz)
;; This minor mode can then be enabled in any buffers where the
;; custom bindings are desired:
;;     (add-hook ’text-mode-hook ’foo-mode) ; enable alongside text-mode
;;
;; Define a command with command properties keyword-args.
;;   (evil-define-command command (args. . .) doc keyword-args. . . body. . .)

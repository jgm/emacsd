;;; Configuration for editing emails in mutt
(add-to-list 'load-path "~/.emacs.d")
(require 'post)
(add-hook 'mail-mode-hook 'turn-on-auto-fill)
(add-hook 'post-mode-hook 
  (lambda()
    (auto-fill-mode t)
    (setq fill-column 72)))    ; rfc 1855 for usenet messages


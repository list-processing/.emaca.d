(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.
;; See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want
;; to do this.
;;(add-to-list 'package-archives
;;           '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)

;; http://joaotavora.github.io/yasnippet/
(require 'yasnippet)
(yas-global-mode 1)

;; Auto add HEADER in new file
(add-hook 'find-file-hook
          '(lambda ()
             (when (and (buffer-file-name)
                        (not (file-exists-p (buffer-file-name)))
                        (= (point-max) 1))
               (let ((header-snippet "HEADER")
                     (yas/fallback-behavior 'return-nil))
                 (insert header-snippet)
                 ;; if can't expand snippet, delete insert string
                 (if (not (yas/expand))
                     (delete-region (point-min) (point-max)))))))

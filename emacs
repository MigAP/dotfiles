; comment test 
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(monokai))
 '(custom-safe-themes
   '("f3ab34b145c3b2a0f3a570ddff8fabb92dafc7679ac19444c31058ac305275e1" default))
 '(org-agenda-files
   '("~/Nextcloud/orgRoam/20201008150915-mechanical_design_optimization_for_aerial_manipulators.org"))
 '(package-selected-packages
   '(matlab-mode visual-fill-column monokai-pro-theme cdlatex elpy toc-org ivy-bibtex org-ref magit yasnippet-snippets org-roam linum-relative ivy auctex monokai-theme yasnippet key-chord evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;====================================
;; EMACS BEHAVIOUR
;;====================================
;; Turn off bell 
(setq ring-bell-function 'ignore)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; Scroll one line at the time 
(setq scroll-step 1)

;; hide tool-bar 
(tool-bar-mode -1)

;; To use all the symbols in the keyboard 
(require 'iso-transl) 

;; Font size 
(set-face-attribute 'default nil :height 120)


; make ibuffer default
(defalias 'list-buffers 'ibuffer) 

; start the emacs server 
(server-start) 


; load theme 
;(load-theme 'monokai-pro t)
;;====================================
;;ORG MODE 
;;====================================

;; Default setting: Use "python3" under Linux and macOS, but "Python" under Windows.
;; The default for R is "R" for all platforms (predefined by Emacs).
(setq org-babel-python-command
      (if (memq system-type '(windows-nt ms-dos))
          "Python"
        "python3 -q"))

; Org mode languages execution 
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   ))

;; Do not ask for confirmation all the time!!
(setq org-confirm-babel-evaluate nil) 

;;This often fails, yielding an ugly warning, and isn't of any use in Org-mode anyway
(setq python-shell-completion-native-enable nil)

;; Display images after code evaluation
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images) 

(add-to-list 'org-latex-packages-alist
             '("AUTO" "babel" t ("pdflatex")))

; scale the latex preview in org mode 
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

;;======MOOC Reproducible Research Templates=======

;;====================================
;;ORG CAPTURE 
;;====================================
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-directory "~/Nextcloud/these/org/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Nextcloud/these/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/Nextcloud/these/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;;====================================
;; ORG REF
;;====================================
;;; org-ref
(setq org-ref-default-bibliography '("/home/migap/Nextcloud/bibliography/allBiblio.bib"))
(setq org-ref-completion-library 'org-ref-ivy-cite)
(require 'org-ref)
(setq org-latex-prefer-user-labels t) ; to use my own references 

; For LaTeX export settings 
(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -interaction nonstopmode -output-directory %o %f"
	"pdflatex -interaction nonstopmode -output-directory %o %f"))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

;;====================================
;; MAGIT
;;====================================
(use-package magit
  :ensure t 
  :bind ("C-x g" . magit-status))


;;====================================
;;EVIL 
;;====================================
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (evil-set-initial-state 'dired-mode 'emacs)); disbable evil mode when dired 

;;====================================
;; KEY-CHORD 
;;====================================
;;Exit insert mode by pressing k and then j quickly
(use-package key-chord
  :ensure t 
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state) 
  (key-chord-mode 1))

;;====================================
;; LINUM RELATIVE
;;====================================
(setq linum-relative-current-symbol "") ; instead of 0, displays actual line number in current line 

;;====================================
;; YAsnippet
;;====================================
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'octave-mode-hook 'yas-minor-mode)
  (add-hook 'LaTeX-mode-hook 'yas-minor-mode)
  (add-hook 'org-mode-hook 'yas-minor-mode))

;;====================================
;; AUCTeX
;;====================================

(use-package tex
  ; It tries to install tex package and not auctex :ensure t 
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'latex-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
  (add-hook 'LaTeX-mode-hook 'linum-relative-mode); with AUCTeX LaTeX mode
  (setq reftex-plug-into-AUCTeX t) ; makes reftex colaborate with AUCTex

  ;; ##### Enable synctex correlation. From Okular just press
  ;; ##### Shift + Left click to go to the good line.
  (setq TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)
  )

;; ### Set Okular as the default PDF viewer.
(eval-after-load "tex"
  '(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Okular"))

;;====================================
;; IVY
;;====================================
;; Ivy mode for completition 
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

;;====================================
;; IVY BIBTEX
;;====================================
;; Ivy-bibtex : managing bibliography 
(setq bibtex-completion-bibliography
      '("/home/migap/Nextcloud/documents/miq5/pfe/dextair/bibtex/dextair.bib"
        ))
(setq bibtex-completion-pdf-field "file")

; citation configuration 
(setq bibtex-completion-format-citation-functions
  '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF) ; in order to insert a link to the files 
    (latex-mode    . bibtex-completion-format-citation-cite)
    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
    (default       . bibtex-completion-format-citation-default)))

(global-set-key (kbd "C-c b") 'ivy-bibtex)


;;====================================
;; ORG ROAM 
;;====================================
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Nextcloud/orgRoam")
      :config
      (setq org-roam-index-file "~/Nextcloud/orgRoam/20200511193348-index.org")
      (setq org-roam-link-title-format "R:%s")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph-show)
               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
 
;;====================================
;; ELPY
;;====================================

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "-i --simple-prompt"))

(defun company-yasnippet-or-completion ()
  "Solve company yasnippet conflicts."
  (interactive)
  (let ((yas-fallback-behavior
         (apply 'company-complete-common nil)))
    (yas-expand)))

(add-hook 'company-mode-hook
          (lambda ()
            (substitute-key-definition
             'company-complete-common
             'company-yasnippet-or-completion
             company-active-map)))


;;====================================
;; visual-fill-column  
;;====================================
; wrap lines when using visual-line-mode (does not work)
(add-hook 'visual-fill-column-mode-hook 'visual-line-mode)
; column at which the text is wrapepd 
(setq visual-fill-column-width 100) 
; center the text 
(setq visual-fill-column-center-text nil)

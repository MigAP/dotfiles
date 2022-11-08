(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; This is only needed once, near the top of the file
(eval-when-compile
  (require 'use-package))

;;====================================
;; EMACS BEHAVIOUR
;;====================================
;; Separate custom file from configuration file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Turn off bell 
(setq ring-bell-function 'ignore)

;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; Scroll one line at the time 
(setq scroll-step 1)
(setq scroll-conservatively 10000)

; Pixel scroll
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.

;; hide tool-bar 
(tool-bar-mode -1)

;; To use all the symbols in the keyboard 
(require 'iso-transl) 

;; Font setup 
;;(set-face-attribute 'default nil :height 120)
(set-frame-font "Monospace 14" nil t)

; make ibuffer default
(defalias 'list-buffers 'ibuffer) 

; start the emacs server 
(server-start) 

; load theme 
;(load-theme 'monokai-pro t)
;;====================================
;;ORG MODE 
;;====================================
(require 'org)
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

(require 'ox-latex)
; use minted package and also babel configuration
(add-to-list 'org-latex-packages-alist
             '("AUTO" "babel" t ("pdflatex") '("" "minted")))
(setq org-latex-listings 'minted)

; scale the latex preview in org mode 
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

; enable visual line mode by default in org files
(add-hook 'org-mode-hook 'turn-on-visual-line-mode) 
;;====================================
;; MOOC Reproducible Research Templates
;;====================================
(require 'cl)
(setq rrmooc/new-org-templates (version<= "9.2" (org-version)))
(when  rrmooc/new-org-templates
  (require 'org-tempo))
(require 'subr-x)
(defun rrmooc/add-org-template (old-style-template)
  (add-to-list 'org-structure-template-alist
	       (if rrmooc/new-org-templates
		   (cons
		    (first old-style-template)
		    (string-trim-right (substring (second old-style-template) 8 -9)))
		 old-style-template)))

(unless rrmooc/new-org-templates
  ;; this template is predefined in the new templating system
  (rrmooc/add-org-template
   '("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")))

(rrmooc/add-org-template
 '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

(rrmooc/add-org-template
 '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(rrmooc/add-org-template
 '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(rrmooc/add-org-template
 '("P" "#+begin_src python :results output :session :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(rrmooc/add-org-template
 '("PP" "#+begin_src python :results file :session :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports both\nimport matplotlib.pyplot as plt\n\nimport numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(if (memq system-type '(windows-nt ms-dos))
    ;; Non-session shell execution does not seem to work under Windows, so we use
    ;; a named session just like for B.
    (rrmooc/add-org-template
     '("b" "#+begin_src shell :session session :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))
  (rrmooc/add-org-template
   '("b" "#+begin_src shell :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>")))

(rrmooc/add-org-template
 '("B" "#+begin_src shell :session *shell* :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(rrmooc/add-org-template
 '("g" "#+begin_src dot :results output graphics :file \"/tmp/graph.pdf\" :exports both
digraph G {
node [color=black,fillcolor=white,shape=rectangle,style=filled,fontname=\"Helvetica\"];
A[label=\"A\"]
B[label=\"B\"]
A->B
}\n#+end_src" "<src lang=\"dot\">\n\n</src>"))

;;====================================
;;ORG CAPTURE 
;;====================================
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-default-notes-file (concat org-directory "~/org/capture/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/capture/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/capture/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;;====================================
;; ORG REF
;;====================================
;;; org-ref
(setq org-ref-default-bibliography '("/home/migap/org/bibliography.bib"))
(setq org-ref-completion-library 'org-ref-ivy-cite)
(require 'org-ref)
(setq org-latex-prefer-user-labels t) ; to use my own references 
(define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link) ; key-binding for inserting reference
; For LaTeX export settings 
; add shell escape for minted latex package
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

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
;;ORG OX-HUGO
;;====================================

(use-package ox-hugo
  :after ox)

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
  (evil-set-initial-state 'dired-mode 'emacs); disbable evil mode when dired 
  (evil-set-initial-state 'deft-mode 'emacs); disbable evil mode when in deft  
  (evil-set-initial-state 'elfeed-search-mode 'emacs); disbable evil mode when in elfeed  
  (evil-set-initial-state 'elfeed-show-mode 'emacs); disbable evil mode when in elfeed  
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1)
  )

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
(setq ivy-use-selectable-prompt t) ; allows to select "bar" when "barricade" already exists 

;;====================================
;; IVY BIBTEX
;;====================================
;; Ivy-bibtex : managing bibliography 
(setq bibtex-completion-bibliography
      '("/home/migap/org/bibliography.bib"
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
  :after org
  :init
  (setq org-roam-v2-ack t) ;; acknowledge upgrade and remove warning at startup
  :config
  (setq org-roam-directory
        (file-truename "/home/migap/org/roam/"))
  (setq org-roam-db-location
        (concat org-roam-directory "org-roam.db"))
  (org-roam-setup)
  :bind
  ("C-c n i" . org-roam-node-insert)
  ("C-c n f" . org-roam-node-find)
  ("C-c n l" . org-roam-buffer-toggle)
  )

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

;;====================================
;; Ispell and hunspell config
;;====================================

(with-eval-after-load "ispell"
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US")
  (setq ispell-program-name "hunspell")
  ;; Configure English, French and Spanish.
  (setq ispell-dictionary "en_US,fr_FR,es_ES")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "en_US,fr_FR,es_ES")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.
(unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0))

;;====================================
;; Deft
;;====================================
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/org/"))

;;====================================
;; Markdown mode
;;====================================

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;====================================
;; Elfeed
;;====================================
(setq elfeed-feeds
      '("https://lukesmith.xyz/rss.xml"
	"https://lukesmith.xyz/videos"
        "https://notrelated.xyz/rss"
        "https://robohub.org/feed?cat=-473"
        "https://xkcd.com/atom.xml"
        "http://www.aaronsw.com/2002/feeds/pgessays.rss"
        "https://ambrevar.xyz/atom.xml"
        "https://nullprogram.com/feed/"
	"https://bzg.fr/en/index.xml"
        ))

;;====================================
;; Projectile 
;;====================================
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;====================================
;; ERC 
;;====================================
;; Set our nickname & real-name as constant variables
(setq
 erc-nick "migap"     ; Our IRC nick
 erc-user-full-name "migap") ; Our /whois name

;; Define a function to connect to a server
(defun libera-server ()
  (interactive)
  (erc-tls :server "irc.libera.chat"
       :port   "6697"))

;; Or assign it to a keybinding
;; This example is also using erc's TLS capabilities:
;(global-set-key "\C-cen"
;  (lambda ()
;  (interactive)
;  (erc-tls :server "server2.example.com"
;           :port   "6697")))

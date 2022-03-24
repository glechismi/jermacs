;;; init.el --- My custom emacs config               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jerm

;; Author: Jerm <glechisma.k@aleeas.com>
;; Keywords: calendar, games, multimedia, internal, vc, comm, abbrev, extensions,
;; terminals, processes, languages, frames, faces, files, tools, help, hypermedia,
;; matching, outlines, wp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; * Commentary:

;;

;;; * Code:
;;


;; ** [[https://github.com/raxod502/straight.el][straight.el]]

;;
;; Bootstrap =straight.el=

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;
;; Setup [[https://github.com/jwiegley/use-package][=use-package=]] for use with =straight.el=

(straight-use-package 'use-package)


;; ** PATH

;;
;; Add my Elisp folder to the load path

(add-to-list 'load-path (expand-file-name "elisp/" user-emacs-directory))


;; ** Misc

;;
;; Make large file warning start at 100 MB

(customize-set-variable 'large-file-warning-threshold 100000000)

;;
;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; [[https://github.com/seanohalpin/org-link-minor-mode][org-link-minor-mode]]
(straight-use-package 'org-link-minor-mode)


;; ** TODO STUFF

;;
;; *** TODO setup config file maybe
;; ;; Find the user configuration file
;; (defvar rational-config-file (expand-file-name "config.el" rational-config-path)
;;   "The user's configuration file.")
;; ;; Load the user configuration file if it exists
;; (when (file-exists-p rational-config-file)
;;   (load rational-config-file nil 'nomessage))

(require 'rational-defaults)
(require 'rational-ui)
(require 'rational-completion)
(require 'rational-windows)
(require 'rational-editing)
(require 'rational-updates)
(require 'rational-project)


;; ** My elisp

;;
;; Download and setup my custom elisp

(straight-use-package '(jermacs
                        :type git :host github :repo "glechismi/jermacs"))
;; [[https://github.com/iqbalansari/restart-emacs][restart-emacs]]
(straight-use-package 'restart-emacs)
(global-set-key (kbd "C-c M-r") 'restart-emacs)

(defun open-rational-config ()
  "Open the user config file"
  (interactive)
  (find-file rational-config-file))
(require 'jermacs)
(require 'mode-line-bell)
(mode-line-visual-bell)
(require 'page-break)
(require 'vertico-config)
(require 'electric-case-config)
(require 'case-control)
(require 'popper-config)
(require 'rpg-stuff)
(require 'agenda-config)
(require 'keymaps)
;; [[https://github.com/abo-abo/swiper][counsel]]
(straight-use-package 'counsel)
;; [[https://gitlab.com/sawyerjgardner/demap.el][demap.el]]
(straight-use-package 'demap)
;; [[https://github.com/cadadr/elisp][org-variable-pitch]]
(straight-use-package 'org-variable-pitch)
(add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)
;;[[https://github.com/duckwork/titlecase.el][titlecase.el]]
(straight-use-package 'titlecase)
(require 'emacs-launcher)
(require 'eshell-term)
(setq server-socket-dir "~/.guix-profile/bin/emacsclient")
(setq server-socket-dir "/run/user/1000/emacs")
;; [[https://github.com/MaximeWack/seriestracker][seriestracker]]
(straight-use-package 'seriestracker)
;;[[https://github.com/ifosch/keepass-mode][keepass-mode]]
(straight-use-package 'keepass-mode)
(require ' keepass-mode)
;; *** TODO setup a checker and auto downloader for fonts
;; Set further font and theme customizations
(set-face-attribute 'default nil
                    :font "Julia Mono"
                    :weight 'light
                    :height 110)
(load-theme 'HexDark t)

;; *** TODO setup a checker and auto downloader for espeak-cli
(defun espeak (text)
  "Speaks text by espeak"
  (interactive)
  (let* ((amplitude 100)
         (voice "en-us")
         (command (format "espeak -a %s -v %s \"%s\"" amplitude voice text)))
    (async-shell-command command))
  (switch-to-buffer-other-window "\*Async Shell Command\*")
  (delete-window))
(defun clean-emacs ()
  "Kills all unmodified buffers and closes all but the selected frame."
  (interactive)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (and (buffer-live-p buffer)
           (not (buffer-modified-p buffer))
           (kill-buffer buffer))))
  (delete-other-frames))

;; (espeak "Welcome to Emacs " name " Emacs loaded in, " time loaded)
(global-set-key (kbd "C-M-'") 'async-shell-command)
(setq espeak-tmp-file "~/.config/.tmp/espeak-tmp.txt")
(defun speak-region (reg-start reg-end)
  "Use espeak to say the selected region"
  (interactive "r")
  (write-region reg-start reg-end espeak-tmp-file)
  ;; (espeak espeak-tmp-file))
  (let* (;;(input (copy-region-as-kill))
         ;;(input (consult-yank-from-kill-ring))
         (amplitude 50)
         (voice "en-us")
         (speed 140)
         (pitch 99)
         (command (format "espeak -a %s -v %s -s %s -p %s -f %s" amplitude voice speed pitch espeak-tmp-file) ))
    (async-shell-command command))
  (switch-to-buffer-other-window "\*Async Shell Command\*")
  (delete-window))
(defun speak-region-melora (reg-start reg-end)
  "Use espeak to say the selected region"
  (interactive "r")
  (write-region reg-start reg-end espeak-tmp-file)
  ;; (espeak espeak-tmp-file))
  (let* (;;(input (copy-region-as-kill))
         ;;(input (consult-yank-from-kill-ring))
         (amp2 120)
         (voice2 "en-us")
         (speed2 140)
         (pitch2 90)
         (command2 (format "espeak -a %s -v %s -s %s -p %s -f %s" amp2 voice2 speed2 pitch2 espeak-tmp-file) ))
    (async-shell-command command2 "\*espeak2\*"))
  (switch-to-buffer-other-window "\*espeak2\*")
  (delete-window))
(defun speak-region-arolem (reg-start reg-end)
  "Use espeak to say the selected region"
  (interactive "r")
  (write-region reg-start reg-end espeak-tmp-file)
  ;; (espeak espeak-tmp-file))
  (let* (;;(input (copy-region-as-kill))
         ;;(input (consult-yank-from-kill-ring))
         (amp1 180)
         (voice1 "en-us")
         (speed1 140)
         (pitch1 30)
         (command1 (format "espeak -a %s -v %s -s %s -p %s -f %s" amp1 voice1 speed1 pitch1 espeak-tmp-file)))
    (async-shell-command command1 "\*espeak1\*"))
  (switch-to-buffer-other-window "\*espeak1\*")
  (delete-window))
(defun speak-region-dual (reg-start reg-end)
  "Use espeak to say the selected region"
  (interactive "r")
  (write-region reg-start reg-end espeak-tmp-file)
  ;; (espeak espeak-tmp-file))
  (let* (;;(input (copy-region-as-kill))
         ;;(input (consult-yank-from-kill-ring))
         (amp1 180)
         (voice1 "en-us")
         (speed1 140)
         (pitch1 30)
         (command1 (format "espeak -a %s -v %s -s %s -p %s -f %s" amp1 voice1 speed1 pitch1 espeak-tmp-file) )
         (amp2 120)
         (voice2 "en-us")
         (speed2 140)
         (pitch2 90)
         (command2 (format "espeak -a %s -v %s -s %s -p %s -f %s" amp2 voice2 speed2 pitch2 espeak-tmp-file) ))
    (async-shell-command command1 "\*espeak1\*")
    (async-shell-command command2 "\*espeak2\*"))
  (switch-to-buffer-other-window "\*espeak1\*")
  (delete-window))
(global-set-key (kbd "C-c M-s") 'speak-region)
(straight-use-package 'delight)
(delight '((emacs-lisp-mode ".el" :major)
           (rainbow-mode)
           (page-break-mode nil t)
           (aggressive-indent-mode nil t)))
;;
(straight-use-package 'geiser)
;;
(straight-use-package 'geiser-guile)
;;
(straight-use-package 'pdf-tools)
;;
(straight-use-package 'pdf-view-restore)
;;
(straight-use-package 'fireplace)
;;
(straight-use-package 'snow)
;;
(straight-use-package 'magit)
;;
(straight-use-package 'org)
;;
(straight-use-package 'transient)
;;
(straight-use-package 'zk)
;;
(straight-use-package 'org-download)
;;
(straight-use-package 'avy)
;;
(straight-use-package 'ace-link)
;;
(straight-use-package 'ace-window)
;;
(straight-use-package 'define-word)
;;
(straight-use-package 'yasnippet)
;;
(straight-use-package 'auto-yasnippet)
;;
(straight-use-package 'el2org)
;;
(straight-use-package 'outorg)
;;
(straight-use-package '(org-marginalia
                        :type git
                        :host github
                        :repo "alphapapa/org-marginalia"))
;;
(straight-use-package 'org-sticky-header)
;;
(straight-use-package '(snitch
                        :type git
                        :host github
                        :repo "mrmekon/snitch-el"))
;;
(straight-use-package 'minions)
;;
(straight-use-package 'pulse)
;;
(straight-use-package '(yequake
                        :type git
                        :host github
                        :repo "alphapapa/yequake"))
;;
(straight-use-package '(outshine
                        :type git
                        :host github
                        :repo "alphapapa/outshine"))
;;
(straight-use-package 'origami)
;;
(straight-use-package 'org-ql)
;;
(straight-use-package '(pulsar
                        :type git
                        :host gitlab
                        :repo "protesilaos/pulsar"))
;;
(use-package lin)



(defun espeak-file (file)
  "Speaks text by espeak"
  (interactive)
  (let* ((amplitude 100)
         (voice "en-us")
         (speed 145)
         (command (format "espeak -a %s -v %s -s %s -f %s" amplitude voice speed file)))
    (async-shell-command command))
  (switch-to-buffer-other-window "\*Async Shell Command\*")
  (delete-window))



(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


(defun mark-as-done-and-archive ()
  "Mark the state of an org-mode item as DONE, archive it, and
save the Org buffers."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree)
  (org-save-all-org-buffers))

(setq org-enforce-todo-dependencies t)
(setq org-pretty-entities t)
(setq org-hide-emphasis-markers t)
;;
(straight-use-package 'org-appear)
(add-hook 'org-mode-hook  'org-appear-mode)
(setq org-startup-with-inline-images t
      org-image-actual-width '(600))
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-log-done 'time)

(defun org-add-tag (new-tag)
  (org-set-tags (cons new-tag
                      (seq-remove (lambda (tag)
                                    (get-text-property 0 'inherited tag))
                                  (org-get-tags)))))

(defun schedule-today ()
  "Tag this item with `daily'."
  (interactive)
  (org-add-tag "daily")
  (save-buffer))

(setq org-agenda-start-on-weekday nil)

(setq org-deadline-warning-days 0)

(defvar org-capture-templates '())

(add-to-list 'org-capture-templates
             '("c" "Contact"
               entry
               (file "~/Documents/org/contacts.org")
               "* %(org-contacts-template-name)
:PROPERTIES:
:ADDRESS: %^{123 Fake St., City, ST 12345}
:PHONE: %^{555-555-5555}
:EMAIL: %(org-contacts-template-email)
:NOTE: %^{note}
:END:"))

(add-to-list 'org-capture-templates
             '("f" "Finished book"
               entry
               (file+headline "~/Documents/org/notes/books-read.org" "Books")
               "* %^{Title} -- %^{Author}\n%t\n"))

(add-to-list 'org-capture-templates
             '("r" "RPG prompt"
               entry
               (file "~/Documents/org/RPG-ideas.org")
               "* %?\n   %t\n"))

(add-to-list 'org-capture-templates
             '("s" "Subscribe to an RSS feed"
               plain
               (file "~/rss-feeds.org")
               "*** [[%^{Feed URL}][%^{Feed name}]]"))

(add-to-list 'org-capture-templates
             '("t" "Task"
               entry
               (file+headline org-index-file "Inbox")
               "* TODO %?\n"))

(setq org-directory "~/Documents/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))


(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat
       (org-file-path (format "archive/archive-%s.org" (format-time-string "%Y")))
       "::* From %s"))

(setq org-refile-targets `((,org-index-file :level . 1)
                           (,(org-file-path "deliveries.org") :level . 1)
                           (,(org-file-path "environment.org") :level . 1)
                           (,(org-file-path "goals.org") :level . 1)
                           (,(org-file-path "links.org") :level . 1)
                           (,(org-file-path "media.org") :level . 1)
                           (,(org-file-path "someday-maybe.org") :level . 1)
                           (,(org-file-path "work.org") :level . 1)))

(setq org-agenda-files (list org-index-file
                             (org-file-path "calendars")
                             (org-file-path "events.org")
                             (org-file-path "goals.org")
                             (org-file-path "habits.org")
                             (org-file-path "news.org")
                             (org-file-path "recurring-events.org")
                             (org-file-path "recurring-tasks.org")
                             (org-file-path "work.org")
                             (org-file-path "writing.org")))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)
(defun open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'open-index-file)
(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "M-n") 'org-capture-todo)

(defun org-insert-link-dwim ()
  "Like `org-insert-link' but with personal dwim preferences."
  (interactive)
  (let* ((point-in-link (org-in-regexp org-link-any-re 1))
         (clipboard-url (when (string-match-p "^http" (current-kill 0))
                          (current-kill 0)))
         (region-content (when (region-active-p)
                           (buffer-substring-no-properties (region-beginning)
                                                           (region-end)))))
    (cond ((and region-content clipboard-url (not point-in-link))
           (delete-region (region-beginning) (region-end))
           (insert (org-make-link-string clipboard-url region-content))
           (message clipboard-url))
          ((and clipboard-url (not point-in-link))
           (insert (org-make-link-string
                    clipboard-url
                    (read-string "title: "
                                 (with-current-buffer (url-retrieve-synchronously clipboard-url)
                                   (dom-text (car
                                              (dom-by-tag (libxml-parse-html-region
                                                           (point-min)
                                                           (point-max))
                                                          'title))))))))
          (t
           (call-interactively 'org-insert-link)))))
(defun enable-variable-pitch-mode ()
  (variable-pitch-mode 1))

(add-hook 'org-mode-hook
          (lambda ()
            (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
            (set-face-attribute 'org-table nil :inherit 'fixed-pitch)))
;;
(straight-use-package 'flyspell)
(add-hook 'git-commit-mode-hook 'orgtbl-mode)
(add-hook 'markdown-mode-hook 'orgtbl-mode)
(add-hook 'message-mode-hook 'orgtbl-mode)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;
(straight-use-package 'dired-hide-dotfiles)
(dired-hide-dotfiles-mode)
(define-key dired-mode-map "." 'dired-hide-dotfiles-mode)

(setq dired-kill-when-opening-new-dired-buffer t)

;;
(straight-use-package 'dired-open)
(setq dired-open-extensions
      '(
        ))

(setq dired-dwim-target t)

(setq dired-clean-up-buffers-too t)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'top)

;;
(straight-use-package 'async)
(dired-async-mode 1)

(defun kill-current-buffer ()
  "Kill the current buffer without prompting."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x K") 'kill-buffer)
(defun split-window-below-and-switch ()
  "Split the window horizontally, then switch to the new pane."
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))

(defun split-window-right-and-switch ()
  "Split the window vertically, then switch to the new pane."
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
;; (defadvice prompt-for-buffer (&rest _)
;;   :after '(split-window-right-and-switch split-window-below-and-switch)
;;   (consult-buffer))
(defun delete-window-and-rebalance ()
  "Delete the current window, then rebalance the remaining windows."
  (interactive)
  (delete-window)
  (balance-windows))

(global-set-key (kbd "C-x 2") 'split-window-below-and-switch)
(global-set-key (kbd "C-x 3") 'split-window-right-and-switch)
(global-set-key (kbd "C-x 0") 'delete-window-and-rebalance)

;;
(straight-use-package 'engine-mode)
(require 'engine-mode)

(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s"
  :keybinding "g")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s"
  :keybinding "s")

(defengine wikipedia
  "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
  :keybinding "w")

(defengine youtube
  "https://www.youtube.com/results?search_query=%s")

(engine-mode t)

(defun comment-or-uncomment-region-or-line ()
  "Comments or uncomments the region or the current line if
there's no active region."
  (interactive)
  (let (beg end)
    (if (region-active-p)
        (setq beg (region-beginning) end (region-end))
      (setq beg (line-beginning-position) end (line-end-position)))
    (comment-or-uncomment-region beg end)))

(defun reset-text-size ()
  (interactive)
  (text-scale-set 0))

;; Settings:

;; When opening a file, start searching at the user's home directory.
(setq default-directory "~/")



;; Call DELETE-TRAILING-WHITESPACE every time a buffer is saved.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Treat CamelCaseSubWords as separate words in every programming mode.
(add-hook 'prog-mode-hook 'subword-mode)

;; When opening a file, always follow symlinks.
(setq vc-follow-symlinks t)

;; When saving a file that starts with `#!', make it executable.
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; When saving a file in a directory that doesn't exist, offer
;; to (recursively) create the file's parent directories.
(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; Turn on transient-mark-mode to apply changes to region.
(transient-mark-mode t)


;; If some text is selected, and you type some text, delete the
;; selected text and start inserting your typed text.
(delete-selection-mode t)

;; If you save a file that doesn't end with a newline,
;; automatically append one.
(setq require-final-newline t)

;; Ask if you're sure that you want to close Emacs.
(setq confirm-kill-emacs 'y-or-n-p)

;;   (setq initial-scratch-message nil)

;; Add file sizes in human-readable units (KB, MB, etc) to dired buffers.
(setq-default dired-listing-switches "-alh")

;; "Don't ask `yes/no?', ask `y/n?'."
(fset 'yes-or-no-p 'y-or-n-p)


;; Turn on syntax highlighting whenever possible.
(global-font-lock-mode t)

;; When something changes a file, automatically refresh the
;; buffer containing that file so they can't get out of sync.
(global-auto-revert-mode t)

;; Keybindings:

;; Comment or uncomment a region by hitting M-;.
(global-set-key (kbd "M-;")
                'comment-or-uncomment-region-or-line)



;;
(straight-use-package '(logos
                        :type git
                        :host gitlab
                        :repo "protesilaos/logos"))
;;
(straight-use-package 'redacted)
;;
(straight-use-package 'topspace)
(global-topspace-mode 1)

;; * TODO fix this
;; (straight-use-package '(emacs-splash
;;                         :type git
;;                         :host github
;;                         :repo "rougier/emacs-splash"))
;; (require 'splash-screen)
;; (straight-use-package '(kisses
;;                         :type git
;;                         :host github
;;                         :repo "jsilve24/kisses"))
;; (require 'kisses)
;; (put-text-property 0 (length kisses-banner) 'face 'outline-1
;;                    kisses-banner)
;; ;; (setq initial-buffer-choice 'kisses-initial-buffer)

(setq prettify-symbols-mode t)
(setq user-full-name "Jerm"
      user-mail-address "glechisma.k@aleeas.com")
(setq auth-sources '("~/.authinfo.gpg")
      auth-source-cache-expiry nil)

(setq undo-limit 80000000
      auto-save-default t
      truncate-string-ellipsis "…"
      scroll-margin 2)

(display-time-mode 1)

(unless (string-match-p "^Power N/A" (battery))
  (display-battery-mode 1))
;;
(straight-use-package '(emacs-svg-icon
                        :type git
                        :host github
                        :repo "rougier/emacs-svg-icon"))
(straight-use-package '(svg-lib
                        :type git
                        :host github
                        :repo "rougier/svg-lib"))
(straight-use-package '(svg-tag-mode
                        :type git
                        :host github
                        :repo "rougier/svg-tag-mode"))
(straight-use-package '(org-outer-indent
                        :type git
                        :host github
                        :repo "rougier/org-outer-indent"))
(straight-use-package '(nano-sidebar
                        :type git
                        :host github
                        :repo "rougier/nano-sidebar"))
;; * TODO Setup
;; (straight-use-package '(mu4e-dashboard
;;                         :type git
;;                         :host github
;;                         :repo "rougier/mu4e-dashboard"))
(straight-use-package 'org-sidebar)
(straight-use-package 'dired-sidebar)
(straight-use-package '(unpackaged
                        :type git
                        :host github
                        :repo "alphapapa/unpackaged.el"))
(straight-use-package '(ox-elisp
                        :type git
                        :host github
                        :repo "alphapapa/ox-elisp"))
(straight-use-package 'frame-purpose)
(straight-use-package 'prism)
(straight-use-package 'topsy)
(straight-use-package 'org-web-tools)
(straight-use-package 'dogears)

(straight-use-package '(plz
                        :type git
                        :host github
                        :repo "alphapapa/plz.el"))

(straight-use-package '(ement
                        :type git
                        :host github
                        :repo "alphapapa/ement.el"))
(straight-use-package 'org-auto-expand)
(straight-use-package 'org-modern)
(straight-use-package 'cape)
(straight-use-package 'package-lint)
(straight-use-package 'diredfl)

(straight-use-package '(spookfox
                        :type git
                        :host github
                        :repo "bitspook/spookfox"
                        :file "spookfox.el"))
;; (require 'spookfox)
;; Revert Dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Use "y" and "n" to confirm/negate prompt instead of "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-save-file (expand-file-name "recentf" rational-config-var-directory))

;; Do not saves duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for an command history
(savehist-mode 1)
(straight-use-package 'all-the-icons)
(straight-use-package 'doom-modeline)
(straight-use-package 'doom-themes)
(straight-use-package 'elisp-demos)
(straight-use-package 'helpful)

;;;; Font

(defcustom rational-ui-default-font nil
  "The configuration of the `default' face.
Use a plist with the same key names as accepted by `set-face-attribute'.")

(when rational-ui-default-font
  (apply 'set-face-attribute 'default nil (cons :font rational-ui-default-font)))

;;;; Mode-Line

;; Start up the modeline after initialization is finished
(add-hook 'after-init-hook 'doom-modeline-init)

;; Configure `doom-modeline'
(customize-set-variable 'doom-modeline-height 15)
(customize-set-variable 'doom-modeline-bar-width 6)
(customize-set-variable 'doom-modeline-minor-modes t)
(customize-set-variable 'doom-modeline-buffer-file-name-style 'truncate-except-project)

;;;; Help Buffers

;; Make `describe-*' screens more helpful
(require 'helpful)
(define-key helpful-mode-map [remap revert-buffer] #'helpful-update)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key [remap describe-symbol] #'helpful-symbol)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key (kbd "C-h F") #'helpful-function)

;; Bind extra `describe-*' commands
(global-set-key (kbd "C-h K") #'describe-keymap)

;;;; Line Numbers

(defcustom rational-ui-line-numbers-enabled-modes
  '(conf-mode prog-mode)
  "Modes which should display line numbers."
  :type 'list
  :group 'rational)

(defcustom rational-ui-line-numbers-disabled-modes
  '(org-mode)
  "Modes which should not display line numbers.
Modes derived from the modes defined in
`rational-ui-line-number-enabled-modes', but should not display line numbers."
  :type 'list
  :group 'rational)

(defun rational-ui--enable-line-numbers-mode ()
  "Turn on line numbers mode.

Used as hook for modes which should display line numbers."
  (display-line-numbers-mode 1))

(defun rational-ui--disable-line-numbers-mode ()
  "Turn off line numbers mode.

Used as hook for modes which should not display line numebrs."
  (display-line-numbers-mode -1))

(defun rational-ui--update-line-numbers-display ()
  "Update configuration for line numbers display."
  (if rational-ui-display-line-numbers
      (progn
        (dolist (mode rational-ui-line-numbers-enabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'rational-ui--enable-line-numbers-mode))
        (dolist (mode rational-ui-line-numbers-disabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'rational-ui--disable-line-numbers-mode))
        (setq-default
         display-line-numbers-grow-only t
         display-line-numbers-type t
         display-line-numbers-width 2))
    (progn
      (dolist (mode rational-ui-line-numbers-enabled-modes)
        (remove-hook (intern (format "%s-hook" mode))
                     #'rational-ui--enable-line-numbers-mode))
      (dolist (mode rational-ui-line-numbers-disabled-modes)
        (remove-hook (intern (format "%s-hook" mode))
                     #'rational-ui--disable-line-numbers-mode)))))

(defcustom rational-ui-display-line-numbers nil
  "Whether line numbers should be enabled."
  :type 'boolean
  :group 'rational
  :set (lambda (sym val)
         (set-default sym val)
         (rational-ui--update-line-numbers-display)))

;;;; Elisp-Demos

;; also add some examples
(require 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
                                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(straight-use-package 'vertico)
(straight-use-package 'consult)
(straight-use-package 'orderless)
(straight-use-package 'marginalia)
(straight-use-package 'embark)

(defun rational-completion/minibuffer-backward-kill (arg)
  "When minibuffer is completing a file name delete up to parent
folder, otherwise delete a word"
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

;;;; Vertico

(require 'vertico)
(require 'vertico-directory "extensions/vertico-directory.el")

(with-eval-after-load 'evil
  (define-key vertico-map (kbd "C-j") 'vertico-next)
  (define-key vertico-map (kbd "C-k") 'vertico-previous)
  (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

;; Cycle back to top/bottom result when the edge is reached
(customize-set-variable 'vertico-cycle t)

;; Start Vertico
(vertico-mode 1)

;;;; Marginalia

;; Configure Marginalia
(require 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; Set some consult bindings
(global-set-key (kbd "C-s") 'consult-line)
(define-key minibuffer-local-map (kbd "C-r") 'consult-history)

(setq completion-in-region-function #'consult-completion-in-region)

;;;; Orderless

;; Set up Orderless for better fuzzy matching
(require 'orderless)
(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))
(setq completion-category-defaults nil)

;;;; Embark

(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") 'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)

(defcustom rational-windows-evil-style nil
  "When t, window movement bindings will be evil-style.")

(defcustom rational-windows-prefix-key "C-c w"
  "Configure the prefix key for `rational-windows' bindings.")

(winner-mode 1)

(define-prefix-command 'rational-windows-key-map)

(define-key 'rational-windows-key-map (kbd "u") 'winner-undo)
(define-key 'rational-windows-key-map (kbd "n") 'windmove-down)
(define-key 'rational-windows-key-map (kbd "p") 'windmove-up)
(define-key 'rational-windows-key-map (kbd "b") 'windmove-left)
(define-key 'rational-windows-key-map (kbd "f") 'windmove-right)

(global-set-key (kbd rational-windows-prefix-key) 'rational-windows-key-map)

(straight-use-package 'ws-butler)
(straight-use-package 'evil-nerd-commenter)

;; Set up ws-butler for trimming whitespace and line endings
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Set a global binding for better line commenting/uncommenting
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

;; parentheses
(electric-pair-mode 1) ; auto-insert matching bracket
(show-paren-mode 1)    ; turn on paren match highlighting

(customize-set-variable 'project-list-file (expand-file-name "projects" rational-config-var-directory))
;; [[https://github.com/nobiot/org-transclusion][org-transclusion]]
(use-package org-tranclusion
  :straight  (org-transclusion
              :type git
              :host github
              :repo "nobiot/org-transclusion"))
(define-key global-map (kbd "<f12>") #'org-transclusion-add)
(define-key global-map (kbd "C-c t") #'org-transclusion-mode)



;;; init.el ends here

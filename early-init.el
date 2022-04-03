;;; early-init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jerm

;; Author: Jerm <jerm@pop-os>
;; Keywords: 

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

;;; Commentary:

;; 

;;; Code:

;; Set garbage collection threshold to t0 5GB for faster startup
(setq gc-cons-threshold (* 50 1000 1000))

;; Always load the newest version of files on load
(setq load-prefer-newer t)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native comp asynchronous
  (setq native-comp-deferred-compilation t)

  ;; Set native-comp cache directory
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Disable =package.el= because using =straight.el= instead
(setq package-enable-at-startup nil)

;; Cleanup UI
;; Disable the tool bar
(push '(tool-bar-lines . 0) default-frame-alist)
;; Disable menu bar
(push '(menu-bar-lines . 0) default-frame-alist)
;; Disable scroll bar
(push '(vertical-scroll-bars) default-frame-alist)

;;; early-init.el ends here

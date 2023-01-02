;;; alossage.el --- View lossage with automatic refresh -*- lexical-binding: t -*-

;; Copyright (C) 2001 Riku Saikkonen

;; Author: Riku Saikkonen <Riku.Saikkonen@hut.fi>
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Version: 1.0
;; Keywords: help
;; URL: https://github.com/KarimAziev/alossage
;; Package-Requires: ((emacs "24.3"))

;; This file is *NOT* part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;; USA

;;; Commentary:
;;;
;;; Like the standard Emacs function `view-lossage', but with
;;; automatic refresh of lossage information. This package is meant
;;; for teaching Emacs to others: with it, onlookers can see what keys
;;; you press while using Emacs.
;;;
;;; The normal way to use this package is to turn on alossage-mode
;;; (with M-x alossage-mode), and then view the *Auto-Lossage*
;;; buffer in another window (use C-x 3) or frame (resize it to about
;;; 10 characters wide). M-x alossage-make-frame is a simple way
;;; to activate the package when you can display multiple frames.
;;;
;;; Please note that, as with the normal `view-lossage', this mode
;;; displays all the keys you press in Emacs, including passwords and
;;; such. You should probably turn it off when writing passwords or
;;; other sensitive information into Emacs. This mode (and
;;; `view-lossage') can display up to 100 previous keystrokes, so you
;;; may want to type that much before re-enabling it.

;;; Change Log:
;;; $Id: alossage.el,v 1.7 2001/01/20 15:25:37 rjs Exp $
;;;
;;; Version 0.9 (January 20th, 2001)
;;;  * Initial release.
;;; Version 1.0 (2022)
;;;  * Replace auto-lossage prefix with alossage
;;;  * Change rendering

;;; Code:


(defvar alossage-buffer-name "*Auto-Lossage*")

(defun alossage-update ()
  "Update *Auto-Lossage* buffer with current lossage.
Also scrolls a window displaying the buffer so that the most
recent keys are visible.

Normally called from `post-command-hook' when
`alossage-mode' is active."
  (with-current-buffer (get-buffer-create alossage-buffer-name)
    (delete-region (point-min)
                   (point-max))
    (goto-char (point-min))
    (let ((comment-start ";; ")
          (comment-column 24))
      (insert (concat " "
                      (mapconcat (lambda (key)
                                   (cond ((and (consp key)
                                               (null (car key)))
                                          (format ";; %s\n"
                                                  (if
                                                      (symbolp (cdr
                                                                key))
                                                      (cdr key)
                                                    "anonymous-command")))
                                         ((or (integerp key)
                                              (symbolp key)
                                              (listp key))
                                          (single-key-description key))
                                         (t
                                          (prin1-to-string key nil))))
                                 (recent-keys 'include-cmds)
                                 " ")))
      (goto-char (point-min))
      (while (not (eobp))
        (comment-indent)
        (forward-line 1))
      (buffer-disable-undo)
      (set-buffer-modified-p nil)
      (let ((win (get-buffer-window (current-buffer) 'visible)))
        (if win
            (save-selected-window
              (select-window win)
              (goto-char (point-max))
              (recenter -1)))))))

(defvar alossage-mode nil
  "Non-nil means that automatic updating of lossage is enabled.
Should be changed only by the function `alossage-mode'.")

;;;###autoload
(defun alossage-mode (arg)
  "Toggle automatic updating of lossage.
When automatic updating is enabled, lossage is updated into
the buffer *Auto-Lossage* after every command.
With negative ARG, turn off the updating.
With positive ARG, turn it on."
  (interactive "P")
  (let ((newval (if (null arg)
                    (not alossage-mode)
                  (> (prefix-numeric-value arg) 0))))
    (cond (newval                       ; turn on the mode
           (add-hook 'post-command-hook #'alossage-update))
          (t                            ; turn off the mode
           (remove-hook 'post-command-hook #'alossage-update)))
    (setq alossage-mode newval)))

;;;###autoload
(defun alossage-buffer ()
  "Make a new buffer displaying automatically updated lossage.
Turns on `alossage-mode' and displays the generated
*Auto-Lossage* buffer in a newly created frame.
The new frame will be 10 characters wide and as high as the
current frame."
  (interactive)
  (alossage-mode 1)
  (with-current-buffer (get-buffer-create alossage-buffer-name)
    (unless (get-buffer-window (current-buffer))
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun alossage-make-frame ()
  "Make a new frame displaying automatically updated lossage.
Turns on `alossage-mode' and displays the generated
*Auto-Lossage* buffer in a newly created frame.
The new frame will be 10 characters wide and as high as the
current frame."
  (interactive)
  (alossage-mode 1)
  (with-current-buffer (get-buffer-create alossage-buffer-name)
    (make-frame `((width . 10)
                  (height . ,(frame-height))))))

;;; alossage.el ends here
(provide 'alossage)
;;; alossage.el ends here
;; dcd.el --- Interface to the dcd cd-player
;; Author: Jean-Philippe Theberge
;; Created: 09/03/2000
;; Version: 0.1
;; Keywords: dcd, cd player

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Copyright (c) 1998 - 1999 Free Software Foundation, Inc.
;;
;; This file is not part of GNU Emacs. :-(
;;
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;;
;;  You can get DCD (Dave's Cd Player) by evaluating this:
;;  (copy-file
;;   "/anonymous@metalab.unc.edu:/pub/Linux/apps/sound/cdrom/cli/dcd-0.80.tar.gz"
;;   "/tmp")

;;; Code:
(defconst cd-cdlist-filename "~/.emacs-dcd")

(defun cd-dcd (&optional arg)
  (shell-command-to-string (concat "dcd " (if arg
                                              (if (symbolp arg)
                                                  (prin1-to-string arg)
                                                arg)))))

(defun cd-prompt-for-cd-info ()
  (interactive)
  (let* ((current (cd-get-current-diskid))
         (title (read-from-minibuffer "Title: "))
         (artist (read-from-minibuffer "Artist: "))
         (number-of-tracks (let ((i (cd-dcd "i")))
                             (string-match "(\\([0-9]+\\)" i)
                             (string-to-number (substring i (match-beginning 1)(match-end 1)))))
         (count 0)
         (L nil))
    (while (< count number-of-tracks)
      (progn
        (setq count (+ count 1))
        (setq L (append (list (read-from-minibuffer (concat "Track #" (number-to-string count) ": "))) L))))
    (save-excursion
      (find-file cd-cdlist-filename)
      (goto-char (point-max))
      (insert "\n")
      (insert (prin1-to-string (list current title artist (reverse L))))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun cd-get-info-from-web-database ()
  (interactive)
  (let* ((coding-system-for-read 'binary)
         (coding-system-for-write 'binary)
         (host "www.cdindex.org")
         (path (concat "/cgi-bin/cdi/get.pl?id=" (cd-dcd 'x)))
         (tmp-buffer "*cd-temp-buffer*")
         (http (open-network-stream
                "cd-info-retrieval-process"
                tmp-buffer
                host
                80))
         (pbuf (process-buffer http)))
    (process-send-string
     http (concat "GET " path " HTTP/1.0\r\n\r\n"))
    (while (eq (process-status http) 'open)
      (sleep-for 1))
    (save-excursion
      (switch-to-buffer tmp-buffer)
      (goto-char (point-min))
      (let* ((artist (progn
                      (when (re-search-forward "^Artist: +\\(.*\\)$" nil t)
                        (buffer-substring (match-beginning 1)(match-end 1)))))
             (album (progn
                      (when (re-search-forward "^Album: +\\(.*\\)$" nil t)
                        (buffer-substring (match-beginning 1)(match-end 1)))))
             (tracklist (let ((L nil))
                          (while (re-search-forward "^Track[0-9]+: +\\(.*\\)$" nil t)
                            (setq L (append (list (buffer-substring (match-beginning 1)(match-end 1))) L)))
                          (reverse L)))
             (info (list (cd-dcd 'x) artist album tracklist)))
        (kill-buffer (current-buffer))
        (if (null album) nil
          (progn
            (save-excursion
              (find-file cd-cdlist-filename)
              (goto-char (point-max))
              (insert "\n")
              (insert (prin1-to-string info))
              (save-buffer)
              (kill-buffer (current-buffer)))
            info))))))

(defun cd-get-info-from-file ()
  (save-excursion
    (find-file cd-cdlist-filename)
    (let ((x nil)
          (info (cd-get-current-diskid)))
      (ignore-errors (while (not (equal (car x) info))
                      (setq x (read (current-buffer)))))
      (kill-buffer (current-buffer))
      (if (equal (car x) info) x nil))))

(defun cd-show-cd-info ()
  (interactive)
  (let ((info (cd-get-info-from-file))
        (count 0))
    (switch-to-buffer "*CD*")
    (goto-char (point-min))
    (erase-buffer)
    (insert (concat (cadr info) " - "  (caddr info) "\n\n"))
    (map 'list (lambda (x)
                 (setq count (+ count 1))
                 (insert (concat " " (number-to-string count) ") " x "\n"))) (car (last info)))))

(defun cd-get-current-diskid ()
  (cd-dcd "x"))

(defun cd-play ()
  (interactive)
  (cd-dcd)
  (let ((info (or (cd-get-info-from-file)(cd-get-info-from-web-database))))
    (if info
        (message (concat "Playing... " (cadr info) " - "  (caddr info)))
      (message "Unknow CD.  Use M-x cd-prompt-for-cd-info to create the entry"))))

(defun cd-play-track (track)
  (interactive "sTrack: ")
  (cd-dcd track))

(defun cd-stop ()
  (interactive)
  (cd-dcd "s"))

(defun cd-eject ()
  (interactive)
  (cd-dcd 'e))

(provide 'dcd)

;;; dcd.el ends here

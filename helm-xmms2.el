;;; helm-xmms2.el --- control xmms2 from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 Rémi Vanicat

;; Author: Rémi Vanicat      <vanicat@debian.org>

;; Package-Requires: ((dash "2.11.0"))
;; Version: 0.0.1
;; Keywords: music tools

;; helm-xmms2 has only been tested with Emacs 24

;; helm-xmms2 is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.
;;
;; helm-xmms2 is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Invoking the `helm-xmms2' command will run a helm session where you
;; can select a song to add to xmms2 current playlist.  For now two
;; action are available: f1 to insert after currently playing song,
;; and f2 to append at end of the playlist.

;;; Code:

;;;###autoload
(defcustom helm-xmms2-command "xmms2"
  "Name of the command used to run xmms2.

Must be in path."
  :type '(string))

(defun helm-xmms2-collection-collect-candidates ()
  "Collect candidates for helm."
  (let ((pattern (mapcar (lambda (s)
                           (if (string-match "[:~><]" s)
                               s
                             (concat "~" s)))
                         (split-string helm-pattern)))
        (process-connection-type ()))
    (apply #'start-process "xmms2" helm-buffer helm-xmms2-command "search" pattern)))

(defun helm-xmms2-collection-candidate-transformer (candidates)
  "Filter CANDIDATES for displaying them."
  (cl-loop for candidate in candidates
           for str = (if (consp candidate)
                         (car candidate)
                       candidate)
           if (string-match "^\\([0-9]*?\\) *| \\(.*?\\) *| \\(.*?\\) *| \\(.*?\\) *$" str)
           collect (let ((id (match-string 1 str))
                         (artist (match-string 2 str))
                         (album (match-string 3 str))
                         (title (match-string 4 str)))
                     (cons (format "%-20s: %-20s: %s" artist album title)
                           (list id artist album title)))))

(defun helm-xmms2-playlist-collect-candidates ()
  "Collect playlist candidates for helm."
  (let ((pattern (mapcar (lambda (s)
                           (if (or (string-match "[:~]" s)
                                   (string-match "^[0-9]+$" s))
                               s
                             (concat "~" s)))
                         (split-string helm-pattern)))
        (process-connection-type ()))
    (apply #'start-process "xmms2" helm-buffer helm-xmms2-command "list" pattern)))

(defun helm-xmms2-playlist-candidate-transformer (candidates)
  "Filter CANDIDATES for displaying them."
  (cl-loop for candidate in candidates
           for str = (if (consp candidate)
                         (car candidate)
                       candidate)
           if (string-match "^\\(  \\|->\\)\\[\\([0-9]+\\)/\\([0-9]+\\)\\] \\(.*\\) - \\(.*\\) (..:..)$" str)
           collect (let ((pos (match-string 2 str))
                         (id (match-string 3 str))
                         (current (string= "->" (match-string 1 str)))
                         (artist (match-string 4 str))
                         (title (match-string 5 str)))
                     (cons (format "%s%-3s: %-20s: %s" (match-string 1 str) pos artist title)
                           (list id pos artist title)))))

(defun helm-xmms2-append (candidate &optional next)
  "Append CANDIDATE to the xmms2 playlist.

If NEXT is non-nil, add it as next played song instead"
  (let* ((ids (or (-when-let (it (helm-marked-candidates))
                    (cl-loop for candidate in it
                             for start = t then ()
                             if (not start) collect "OR"
                             collect (format "#%s" (car candidate))))
                  (list (format "#%s" (car candidate))))))
    (if next
        (apply #'call-process helm-xmms2-command () (get-buffer "*Message*") () "add" "-n" ids)
      ((apply #'call-process helm-xmms2-command () (get-buffer "*Message*") () "add" ids)))))

(defun helm-xmms2-insert (candidate)
  "Insert CANDIDATE as next to be played song."
  (helm-xmms2-append candidate t))

(defmacro helm-xmms2-command (command)
  (let ((command-name (intern (concat "helm-xmms2-"(symbol-name command) )))
        (xmms2-arg (downcase (symbol-name command))))
    `(defun ,command-name (&optional unused)
       (interactive)
       (call-process helm-xmms2-command () 0 () ,xmms2-arg))))

(helm-xmms2-command play)
(helm-xmms2-command pause)
(helm-xmms2-command toggle)
(helm-xmms2-command stop)
(helm-xmms2-command next)
(helm-xmms2-command prev)

(defun helm-xmms2-goto (candidate)
  (call-process helm-xmms2-command () 0 () "jump" (cadr candidate)))

(defun helm-xmms2-remove (candidate)
  (let* ((poss (or (mapcar (lambda (candidate) (format "%s" (cadr candidate))) (helm-marked-candidates))
                   (list (format "%s" (cadr candidate))))))
    (apply #'call-process helm-xmms2-command () 0 () "remove" poss)))

(defvar helm-source-xmms2-collection
  (helm-build-async-source
      "Xmms2 Collection"
    :header-name "Xmms2 (C-c ? Help)"
    :candidates-process 'helm-xmms2-collection-collect-candidates
    :candidate-transformer 'helm-xmms2-collection-candidate-transformer
    :candidate-number-limit 9999
    :requires-pattern 3
    :action (helm-make-actions "insert next" #'helm-xmms2-insert
                               "append at end" #'helm-xmms2-append)))

(defvar helm-source-xmms2-playlist
  (helm-build-async-source
      "Xmms2 Playlist"
    :header-name "Xmms2 (C-c ? Help)"
    :candidates-process 'helm-xmms2-playlist-collect-candidates
    :candidate-transformer 'helm-xmms2-playlist-candidate-transformer
    :candidate-number-limit 9999
    :action (helm-make-actions "jump" #'helm-xmms2-goto
                               "remove" #'helm-xmms2-remove
                               "toggle" #'helm-xmms2-toggle
                               "play" #'helm-xmms2-play
                               "pause" #'helm-xmms2-pause
                               "stop" #'helm-xmms2-stop
                               "next" #'helm-xmms2-next
                               "prev" #'helm-xmms2-prev)))

;;;###autoload
(defun helm-xmms2 ()
  "Use helm to control xmms2."
  (interactive)
  (helm :sources '(helm-source-xmms2-collection helm-source-xmms2-playlist)))

(provide 'helm-xmms2)

;;; helm-xmms2.el ends here

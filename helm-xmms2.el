;;; helm-xmms2 --- control xmms2 from Emacs

;; Copyright (C) 2014 Rémi Vanicat

;; Author: Rémi Vanicat      <vanicat@debian.org>

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

(defun helm-xmms2-collect-candidates ()
  "Collect candidates for helm."
  (let ((pattern (mapcar (lambda (s)
                           (if (string-match "[:~]" s)
                               s
                             (concat "~" s)))
                         (split-string helm-pattern)))
        (process-connection-type ()))
    (prog1
        (apply #'start-process "xmms2" helm-buffer "xmms2" "search" pattern)
      ())))

(defun helm-xmms2-filter-one-by-one (candidate)
  "Filter CANDIDATE for displaying them."
  (when (string-match "^\\([0-9]*?\\) *| \\(.*?\\) *| \\(.*?\\) *| \\(.*?\\) *$" candidate)
    (let ((id (match-string 1 candidate))
          (artist (match-string 2 candidate))
          (album (match-string 3 candidate))
          (title (match-string 4 candidate)))
      (cons (format "%-20s: %-20s: %s" artist album title)
            (list id artist album title)))))

(defun helm-xmms2-append (candidate &optional next)
  "Append CANDIDATE to the xmms2 playlist.

If NEXT is non-nil, add it as next played song instead"
  (let* ((ids (or (mapcar (lambda (candidate) (format "#%s" (car candidate))) (helm-marked-candidates))
                  (list (format "#%s" (car candidate)))))
         (args (if next
                   (cons "-n" ids)
                 ids)))
    (apply #'call-process "xmms2" () 0 () "add" args)))

(defun helm-xmms2-insert (candidate)
  "Insert CANDIDATE as next to be played song."
  (helm-xmms2-append candidate t))

(defmacro helm-xmms2-command (command)
  (let ((command-name (intern (concat "helm-xmms2-"(symbol-name command) )))
        (xmms2-arg (downcase (symbol-name command))))
    `(defun ,command-name (&optional unused)
       (interactive)
       (call-process "xmms2" () 0 () ,xmms2-arg))))

(helm-xmms2-command play)
(helm-xmms2-command pause)
(helm-xmms2-command toggle)
(helm-xmms2-command stop)
(helm-xmms2-command next)
(helm-xmms2-command prev)

(defvar helm-source-xmms2
  (helm-build-async-source
      "Xmms2"
    :header-name "Xmms2 (C-c ? Help)"
    :candidates-process 'helm-xmms2-collect-candidates
    :filter-one-by-one 'helm-xmms2-filter-one-by-one
    :candidate-number-limit 9999
    :requires-pattern 3
    :action (helm-make-actions "insert next" #'helm-xmms2-insert
                               "append at end" #'helm-xmms2-append)))

(defun helm-xmms2 ()
  "Use helm to manipulate xmms2."
  (interactive)
  (helm :sources '(helm-source-xmms2)))

(provide 'helm-xmms2)

;;; helm-xmms2.el ends here

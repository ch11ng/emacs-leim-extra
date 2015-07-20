;;; ibus-table-to-quail-rules.el --- Convert IBus  -*- lexical-binding: t -*-
;;;                                  table to Quail rules

;; Copyright (C) 2015 Chris Feng

;; Author: Chris Feng <chris.w.feng@gmail.com>
;; Keywords: i18n

;; This file is not part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file should be run as a Emacs script with an ibus table data as the
;; only argument. Currently, only the TABLE entry is used.

;;; Code:

(defun ibus-table->quail-rules (ibus-table)
  "Return quail rules from IBus table data."
  (let ((data
         (with-temp-buffer
           (message "Reading %s..." ibus-table)
           (insert-file-contents ibus-table)
           (goto-char (point-min))
           (delete-region (point-min) (search-forward "\nBEGIN_TABLE\n"))
           (goto-char (point-max))
           (delete-region (search-backward "\nEND_TABLE\n") (point-max))
           (mapcar 'split-string (split-string (buffer-string) "\n" t))))
        (hash (make-hash-table :test 'equal))
        rules)
    (message "Converting...")
    (mapc (lambda (i)
            (let ((key (elt i 0))
                  (translation (elt i 1))
                  (frequency (string-to-int (elt i 2))))
              (when (= 1 (length translation))
                (setq translation (elt translation 0)))
              (dotimes (j (1- (length key)))
                (let ((key* (substring key 0 (1+ j))))
                  (puthash key*
                           (nconc (gethash key* hash)
                                  `((,translation . ,frequency)))
                           hash)))
              (puthash key (nconc (gethash key hash) (list translation))
                       hash)))
          data)
    (message "Finishing...")
    (maphash (lambda (key val)
               (setq rules
                     (nconc rules
                            `((,key
                               ,(let ((translation
                                       (mapcar (lambda (i)
                                                 (if (listp i)
                                                     (car i)
                                                   i))
                                               (sort val
                                                     (lambda (a b)
                                                       (if (nlistp a)
                                                           t
                                                         (if (nlistp b)
                                                             nil
                                                           (> (cdr a)
                                                              (cdr b)))))))))
                                  (if (and (= 1 (length translation))
                                           (not (stringp (car translation))))
                                      (car translation)
                                    (vconcat translation))))))))
             hash)
    rules))

(print (ibus-table->quail-rules (car argv)))

;;; ibus-table-to-quail-rules.el ends here

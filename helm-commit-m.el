;;; helm-commit-m.el --- commit-m with helm interface

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-commit-m
;; Version: 0.01
;; Package-Requires: ((helm-core "1.7.7") (cl-lib "0.5") (s "1.10.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; helm interface of commit-m(http://commit-m.minamijoyo.com/).

;;; Code:

(eval-when-compile
  (defvar url-http-end-of-headers))

(require 'helm)
(require 'url)
(require 'cl-lib)
(require 's)

(cl-defstruct helm-commit-m--data
  message project url)

(defconst helm-commit-m--endpoint
  "http://commit-m.minamijoyo.com/commits/search")

(defsubst helm-commit-m--construct-url (keyword page)
  (format "%s?keyword=%s&page=%d"
          helm-commit-m--endpoint (url-encode-url keyword) page))

(defsubst helm-commit-m--remove-text-nodes (nodes)
  (cl-remove-if-not #'listp nodes))

(defun helm-commit-m--retrieve-entries (parsed)
  (cdr
   (assoc-default
    'tbody
    (assoc-default
     'table
     (helm-commit-m--remove-text-nodes
      (assoc-default
       'div
       (cddr (assoc-default 'body parsed))))))))

(defun helm-commit-m--retrieve-message (msg-node)
  (cl-loop for node in (cddr msg-node)
           if (listp node)
           concat (propertize (cl-caddr node) 'face 'font-lock-keyword-face)
           else
           concat node))

(defun helm-commit-m--retrieve-project (proj-node)
  (let* ((inner-nodes (cddr proj-node))
         (a-node (assoc 'a inner-nodes)))
    (car (last a-node))))

(defun helm-commit-m--retrieve-commit-url (url-node)
  (let* ((inner-nodes (cddr url-node))
         (a-node (assoc 'a inner-nodes))
         (a-attrs (cadr a-node)))
    (assoc-default 'href a-attrs)))

(defun helm-commit-m--parse-entries (entries)
  (cl-loop for entry in entries
           for tds = (helm-commit-m--remove-text-nodes (cddr entry))
           for message = (helm-commit-m--retrieve-message (cl-first tds))
           for project = (helm-commit-m--retrieve-project (cl-second tds))
           for commit-url = (helm-commit-m--retrieve-commit-url (cl-third tds))
           collect (make-helm-commit-m--data
                    :message (s-trim-right message) :project project :url commit-url)))

(defun helm-commit-m--action-insert (data)
  (with-helm-current-buffer
    (insert (helm-commit-m--data-message data))))

(defun helm-commit-m--action-browse-url (data)
  (browse-url (helm-commit-m--data-url data)))

(defun helm-commit-m--decorate-message (data)
  (format "%-30s| %s"
          (helm-commit-m--data-project data)
          (helm-commit-m--data-message data)))

(defun helm-commit-m--construct-source (keyword page candidates)
  (helm-build-sync-source (format "commit-m [Keyword: %s Page: %d]" keyword page)
    :candidates (cl-loop for candidate in candidates
                         collect
                         (cons (helm-commit-m--decorate-message candidate)
                               candidate))
    :action '(("Browse commit URL" . helm-commit-m--action-browse-url)
              ("Insert message" . helm-commit-m--action-insert))
    :volatile t))

;;;###autoload
(defun helm-commit-m (keyword page)
  (interactive
   (list (read-string "Keyword: ")
         (prefix-numeric-value current-prefix-arg)))
  (when (string= keyword "")
    (error "Error: Keyword is empty!!"))
  (let ((url (helm-commit-m--construct-url keyword page)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (let* ((entries (helm-commit-m--retrieve-entries
                       (libxml-parse-html-region (point) (point-max))))
             (candidates (helm-commit-m--parse-entries entries)))
        (helm :sources (list (helm-commit-m--construct-source keyword page candidates))
              :buffer "*helm commit-m*")))))

(provide 'helm-commit-m)

;;; helm-commit-m.el ends here

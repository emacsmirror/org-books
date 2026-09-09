;;; org-books.el --- Reading list management with Org mode and helm   -*- lexical-binding: t -*-

;; Copyright (C) 2017-2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.3.2
;; Package-Requires: ((enlive "0.0.1") (s "1.11.0") (helm "2.9.2") (helm-org "1.0") (dash "2.14.1") (org "9.3") (emacs "25"))
;; URL: https://github.com/lepisma/org-books
;; Keywords: outlines

;;; Commentary:

;; org-books.el is a tool for managing reading list in an Org mode file.
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'enlive)
(require 'json)
(require 'helm)
(require 'helm-org)
(require 'org)
(require 's)
(require 'subr-x)
(require 'url)
(require 'url-parse)

(defgroup org-books nil
  "Org reading list management."
  :group 'org)

(defcustom org-books-file nil
  "File for keeping reading list."
  :type 'file
  :group 'org-books)

(defcustom org-books-add-to-top t
  "Should add new books as the first item under a heading?"
  :type 'boolean
  :group 'org-books)

(defcustom org-books-file-depth 2
  "The max depth for adding book under headings."
  :type 'integer
  :group 'org-books)

(defcustom org-books-duplicate-check-properties
  '("URL" "GOODREADS" "AMAZON" "ISBN" "GOOGLE_BOOKS" "WIKIPEDIA")
  "Property names checked, in addition to matching title and
author, when looking for a possible duplicate before adding a new
book. See `org-books-add-book'."
  :type '(repeat string)
  :group 'org-books)

(defcustom org-books-url-pattern-dispatches
  '(("^\\(www\\.\\)?amazon\\." . org-books-get-details-amazon)
    ("^\\(www\\.\\)?goodreads\\.com" . org-books-get-details-goodreads)
    ("openlibrary\\.org" . org-books-get-details-isbn)
    ("books\\.google\\." . org-books-get-details-google-books)
    ("wikipedia\\.org" . org-books-get-details-wikipedia))
  "Pairs of url patterns and functions taking url and returning
book details. Check documentation of `org-books-get-details' for
return structure from these functions."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-books)

(defun org-books--get-json (url)
  "Parse JSON data from given URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (json-read)))

(defun org-books--clean-str (text)
  "Clean TEXT to remove extra whitespaces."
  (s-trim (s-collapse-whitespace text)))

(defun org-books--html-decode-entities (text)
  "Decode common HTML entities found in TEXT.

This is meant for text pulled out of raw contexts (like inline
JSON embedded in a page) that libxml does not entity-decode for
us, since some sites (e.g. Goodreads) double-escape such data."
  (dolist (pair '(("&amp;" . "&") ("&quot;" . "\"") ("&#39;" . "'")
                  ("&apos;" . "'") ("&lt;" . "<") ("&gt;" . ">"))
                text)
    (setq text (replace-regexp-in-string (regexp-quote (car pair)) (cdr pair) text t t))))

(defun org-books--get-ld-json-objects (page-node)
  "Return list of parsed JSON-LD objects embedded in PAGE-NODE."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (delq nil
          (mapcar
           (lambda (el)
             (when (string= (enlive-attr el 'type) "application/ld+json")
               (condition-case nil
                   (json-read-from-string (enlive-text el))
                 (error nil))))
           (enlive-get-elements-by-tag-name page-node 'script)))))

(defun org-books--find-ld-json-book (objects)
  "Find a schema.org Book entry within OBJECTS.

OBJECTS is a list of parsed JSON-LD values, as returned by
`org-books--get-ld-json-objects'. Descends into \"@graph\" lists
where needed."
  (cl-labels ((search-obj (obj)
                (cond
                 ((and (hash-table-p obj) (equal (gethash "@type" obj) "Book")) obj)
                 ((hash-table-p obj) (search-obj (gethash "@graph" obj)))
                 ((listp obj) (cl-some #'search-obj obj)))))
    (cl-some #'search-obj objects)))

(defun org-books-get-details-amazon-authors (page-node)
  "Return author names for amazon PAGE-NODE.

PAGE-NODE is the return value of `enlive-fetch' on the page url."
  (or (mapcar #'enlive-text (enlive-query-all page-node [.a-section .author .contributorNameID]))
      (mapcar #'enlive-text (enlive-query-all page-node [.a-section .author > a]))))

(defun org-books-get-details-amazon (url)
  "Get book details from amazon URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books--clean-str (enlive-text (enlive-get-element-by-id page-node "productTitle"))))
         (author (s-join ", " (org-books-get-details-amazon-authors page-node))))
    (if (not (string-equal title ""))
        (list title author `(("AMAZON" . ,url))))))

(defun org-books-get-details-goodreads--ld-json (page-node url)
  "Get book details for goodreads PAGE-NODE using its embedded JSON-LD data.

This is the primary strategy since Goodreads keeps a full,
untruncated author list here (the on-page contributor list is
often cut short behind a \"...more\" toggle), and it avoids
accidentally picking up an author's name a second time from an
unrelated \"About the author\" widget elsewhere on the page."
  (let ((book (org-books--find-ld-json-book (org-books--get-ld-json-objects page-node))))
    (when book
      (let* ((raw-authors (gethash "author" book))
             (authors (if (hash-table-p raw-authors) (list raw-authors) raw-authors))
             (title (org-books--clean-str
                     (org-books--html-decode-entities (or (gethash "name" book) ""))))
             (author (org-books--clean-str
                      (org-books--html-decode-entities
                       (s-join ", " (delq nil (mapcar (lambda (a) (and (hash-table-p a) (gethash "name" a)))
                                                       authors)))))))
        (unless (string-equal title "")
          (list title author `(("GOODREADS" . ,url))))))))

(defun org-books-get-details-goodreads--scrape (page-node url)
  "Get book details for goodreads PAGE-NODE by scraping displayed elements.

Fallback for when JSON-LD data is not present. The contributor
query is scoped to the metadata section's contributor list so it
does not also match the (differently purposed) author bio widget
further down the page, which shares the same CSS class and would
otherwise duplicate the primary author's name."
  (let* ((title (org-books--clean-str (enlive-text (enlive-query page-node [.Text__title1]))))
         (author (org-books--clean-str
                  (s-join ", " (mapcar #'enlive-text
                                        (enlive-query-all
                                         page-node
                                         [.BookPageMetadataSection__contributor .ContributorLink__name]))))))
    (unless (string-equal title "")
      (list title author `(("GOODREADS" . ,url))))))

(defun org-books-get-details-goodreads (url)
  "Get book details from Goodreads URL."
  (let ((page-node (enlive-fetch url)))
    (or (org-books-get-details-goodreads--ld-json page-node url)
        (org-books-get-details-goodreads--scrape page-node url))))

(defun org-books-get-url-from-isbn (isbn)
  "Make and return openlibrary url from ISBN."
  (concat "https://openlibrary.org/api/books?bibkeys=ISBN:" isbn "&jscmd=data&format=json"))

(defun org-books-get-details-google-books (url)
  "Get book details from Google Books URL."
  (let* ((page-node (enlive-fetch url))
         (raw-title (org-books--clean-str (enlive-text (enlive-query page-node [title])))))
    (when (string-match "^\\(.*\\) - \\(.+\\) - Google Books$" raw-title)
      (list (match-string 1 raw-title)
            (match-string 2 raw-title)
            `(("GOOGLE_BOOKS" . ,url))))))

(defun org-books-get-details-wikipedia (url)
  "Get book details from Wikipedia URL."
  (condition-case nil
      (let* ((parsed-url (url-generic-parse-url url))
             (host (url-host parsed-url))
             (path (car (split-string (url-filename parsed-url) "?"))))
        (when (string-match "^/wiki/\\(.+\\)$" path)
          (let* ((article (match-string 1 path))
                 (api-url (concat "https://" host "/api/rest_v1/page/summary/" article))
                 (json-object-type 'hash-table)
                 (json-array-type 'list)
                 (json-key-type 'string)
                 (json (org-books--get-json api-url))
                 (title (gethash "title" json)))
            (when (and title (not (string-empty-p title)))
              (list title "" `(("WIKIPEDIA" . ,url)))))))
    (error nil)))

(defun org-books--get-page-title (url)
  "Fetch page at URL and return HTML title tag content, or nil on failure."
  (condition-case nil
      (let* ((page-node (enlive-fetch url))
             (title (enlive-text (enlive-query page-node [title]))))
        (when title (org-books--clean-str title)))
    (error nil)))

(defun org-books-get-details-isbn (url)
  "Get book details from openlibrary ISBN response from URL."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (org-books--get-json url))
         (isbn (car (hash-table-keys json)))
         (data (gethash isbn json))
         (title (gethash "title" data))
         (author (gethash "name" (car (gethash "authors" data)))))
    (list title author `(("ISBN" . ,url)))))

(defun org-books-get-details (url)
  "Fetch book details from given URL.

Return a list of three items: title (string), author (string) and
an alist of properties to be applied to the org entry. If the url
is not supported, throw an error."
  (let ((output 'no-match)
        (url-host-string (url-host (url-generic-parse-url url))))
    (cl-dolist (pattern-fn-pair org-books-url-pattern-dispatches)
      (when (s-matches? (car pattern-fn-pair) url-host-string)
        (setq output (funcall (cdr pattern-fn-pair) url))
        (cl-return)))
    (unless (eq output 'no-match)
      output)))

(defun org-books-create-file (file-path)
  "Write initialization stuff in a new file at FILE-PATH."
  (interactive "FFile: ")
  (if (file-exists-p file-path)
      (message "There is already a file present, skipping.")
    (with-temp-file file-path
      (insert "#+TITLE: Reading List\n"
              "#+AUTHOR: " (replace-regexp-in-string "" " " user-full-name) "\n\n"
              "#+TODO: READING NEXT | READ\n\n"))))

(defun org-books-all-authors ()
  "Return a list of authors in the `org-books-file'."
  (with-current-buffer (find-file-noselect org-books-file)
    (->> (org-property-values "AUTHOR")
       (-reduce-from (lambda (acc line) (append acc (s-split "," line))) nil)
       (mapcar #'s-trim)
       (-distinct)
       (-sort #'s-less-p))))

(defun org-books-entry-p ()
  "Tell if current entry is an org-books entry."
  (if (org-entry-get nil "AUTHOR") t))

(defun org-books-get-closed-time ()
  "Return closed time of the current entry."
  (let ((ent-body (buffer-substring-no-properties (org-entry-beginning-position) (org-entry-end-position))))
    (if (string-match org-closed-time-regexp ent-body)
        (parse-time-string (match-string-no-properties 1 ent-body)))))

(defun org-books-map-entries (func &optional match scope &rest skip)
  "Similar to `org-map-entries' but only walks on org-books entries.

Arguments FUNC, MATCH, SCOPE and SKIP follow their definitions
from `org-map-entries'."
  (with-current-buffer (find-file-noselect org-books-file)
    (let ((ignore-sym (gensym)))
      (-remove-item ignore-sym
                    (apply #'org-map-entries
                           (lambda ()
                             (if (org-books-entry-p)
                                 (if (functionp func) (funcall func) (funcall (list 'lambda () func)))
                               ignore-sym))
                           match scope skip)))))

(defun org-books--entry-duplicate-p (title author props)
  "Tell if the org-books entry at point looks like a duplicate of a
new entry defined by TITLE, AUTHOR and PROPS.

A match on the value of any property named in
`org-books-duplicate-check-properties' shared with PROPS counts
as a duplicate, as does a case-insensitive match on both title
and author."
  (or (cl-some (lambda (prop-name)
                 (let ((entry-val (org-entry-get nil prop-name))
                       (new-val (cdr (assoc prop-name props))))
                   (and entry-val new-val (string-equal entry-val new-val))))
               org-books-duplicate-check-properties)
      (let ((entry-title (org-get-heading t t t t))
            (entry-author (org-entry-get nil "AUTHOR")))
        (and entry-title entry-author
             (string-equal (downcase (org-books--clean-str entry-title))
                            (downcase (org-books--clean-str title)))
             (string-equal (downcase (org-books--clean-str entry-author))
                            (downcase (org-books--clean-str author)))))))

(defun org-books--find-duplicate (title author props)
  "Return the marker of an existing entry in `org-books-file' that
looks like a duplicate of TITLE, AUTHOR and PROPS, or nil if none
is found."
  (-first #'identity
          (org-books-map-entries
           (lambda ()
             (when (org-books--entry-duplicate-p title author props)
               (point-marker))))))

(defun org-books--get-active-books (&optional todo-keyword)
  "Return books that are currently active. Each item returned is
a pair of book name and position of the headline. Activity is
assumed, by default, to be marked by READING TODO state."
  (let ((active-todo-keyword "READING"))
    (org-books-map-entries
     (lambda ()
       (cons
        (substring-no-properties (org-get-heading) (+ 1 (length (or todo-keyword active-todo-keyword))))
        (point)))
     (format "TODO=\"%s\"" (or todo-keyword active-todo-keyword)))))

(defun org-books-visit-book-log ()
  "Ask to pick a book from currently active one and position
cursor to add log entry."
  (let ((active-books (org-books--get-active-books)))
    (if (null active-books)
        (message "No books active at the moment.")
      (let ((picked-book
             (helm :sources (helm-build-sync-source "Active books"
                              :candidates active-books)
                   :buffer "*helm active books*")))
        (find-file org-books-file)
        (goto-char picked-book)
        (unless (re-search-forward "^*+ Log$" nil t)
          (org-insert-heading-after-current)
          (org-do-demote)
          (insert "Log\n"))))))

;;;###autoload
(defun org-books-cliplink ()
  "Clip link from clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (org-books-add-url url)))

;;;###autoload
(defun org-books-add-url (url)
  "Add book from web URL."
  (interactive "sUrl: ")
  (let ((details (org-books-get-details url)))
    (if details
        (let* ((title (nth 0 details))
               (author (nth 1 details))
               (props (nth 2 details))
               (completion-ignore-case t)
               ;; When author is missing (e.g. Wikipedia), confirm title and ask for author
               (final-title (if (string-empty-p author)
                                (read-string "Book Title: " title)
                              title))
               (final-author (if (string-empty-p author)
                                 (s-join ", " (completing-read-multiple "Author(s): " (org-books-all-authors)))
                               author)))
          (org-books-add-book final-title final-author props))
      ;; When the url parsing or fetching fails, we ask user manually for
      ;; basic details while setting the URL property to the originally
      ;; given url.
      (message "Error in fetching url. Please enter details manually or retry.")
      (let* ((completion-ignore-case t)
             (page-title (org-books--get-page-title url))
             (title (read-string "Book Title: " page-title))
             (authors-str (s-join ", " (completing-read-multiple "Author(s): " (org-books-all-authors)))))
        (org-books-add-book title authors-str `(("URL" . ,url)))))))

;;;###autoload
(defun org-books-add-isbn (isbn)
  "Add book from ISBN."
  (interactive "sISBN: ")
  (org-books-add-url (org-books-get-url-from-isbn isbn)))

(defun org-books-format (level title author &optional props)
  "Return details as an org headline entry.

LEVEL specifies the headline level. TITLE goes as the main text.
AUTHOR and properties from PROPS go as org-property."
  (with-temp-buffer
    (org-mode)
    (insert (make-string level ?*) " " title "\n")
    (org-set-property "AUTHOR" author)
    (org-set-property "ADDED" (format-time-string "[%Y-%02m-%02d]"))
    (dolist (prop props)
      (org-set-property (car prop) (cdr prop)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-books--insert (level title author &optional props)
  "Insert book template at current position in buffer.

Formatting is specified by LEVEL, TITLE, AUTHOR and PROPS as
described in docstring of `org-books-format' function."
  (insert (org-books-format level title author props)))

(defun org-books--insert-at-pos (pos title author &optional props)
  "Goto POS in current buffer, insert a new entry and save buffer.

TITLE, AUTHOR and PROPS are formatted using `org-books-format'."
  (org-content)
  (goto-char pos)
  (let ((level (or (org-current-level) 0)))
    (org-books-goto-place)
    (insert "\n")
    (org-books--insert (+ level 1) title author props)
    (save-buffer)))

(defun org-books-goto-place ()
  "Move to the position where insertion should happen."
  (if org-books-add-to-top
      (let ((level (or (org-current-level) 0))
            (bound (save-excursion (org-get-next-sibling))))
        (if (re-search-forward (format "^\\*\\{%s\\}" (+ level 1)) bound t)
            (forward-line -1)))
    (if (org-get-next-sibling)
        (forward-line -1)))
  (goto-char (line-end-position)))

(defun org-books-get-headers ()
  "Return list of categories under which books can be filed.

Each item in list is a pair of title (propertized) and marker
specifying the position in the file."
  (let ((helm-org-headings-max-depth org-books-file-depth))
    (mapcar (lambda (it)
              (cons it (get-text-property 0 'helm-realvalue it)))
            (helm-org--get-candidates-in-file org-books-file helm-org-headings-fontify t nil t))))

;;;###autoload
(defun org-books-add-book (title author &optional props)
  "Add a book (specified by TITLE and AUTHOR) to the `org-books-file'.

Optionally apply PROPS. If an entry that looks like a duplicate
(matching one of `org-books-duplicate-check-properties', or the
same title and author) already exists, ask for confirmation
before adding another one."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (read-string "Book Title: ")
      (s-join ", " (completing-read-multiple "Author(s): " (org-books-all-authors))))))
  (cond
   ((not org-books-file) (message "org-books-file not set"))
   ((and (org-books--find-duplicate title author props)
         (not (y-or-n-p (format "\"%s\" by %s looks like it is already in %s. Add it anyway? "
                                 title author (file-name-nondirectory org-books-file)))))
    (message "org-books: skipped adding \"%s\", already present." title))
   (t
    (save-excursion
      (with-current-buffer (find-file-noselect org-books-file)
        (let ((headers (org-books-get-headers)))
          (if headers
              (helm :sources (helm-build-sync-source "org-book categories"
                               :candidates (mapcar (lambda (h) (cons (car h) (marker-position (cdr h)))) headers)
                               :action (lambda (pos) (org-books--insert-at-pos pos title author props)))
                    :buffer "*helm org-books add*")
            (goto-char (point-max))
            (org-books--insert 1 title author props)
            (save-buffer))))))))

;;;###autoload
(defun org-books-rate-book (rating)
  "Apply RATING to book at current point."
  (interactive "nRating (stars 1-5): ")
  (if (> rating 0)
      (org-set-property "RATING" (s-repeat rating ":star:"))))

(provide 'org-books)
;;; org-books.el ends here

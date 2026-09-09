;;; org-books.el --- Reading list management with Org mode   -*- lexical-binding: t -*-

;; Copyright (C) 2017-2026 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.4.0
;; Package-Requires: ((org "9.3") (emacs "25"))
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
(require 'dom)
(require 'json)
(require 'org)
(require 'org-refile)
(require 'seq)
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
  "Pairs of url patterns and functions taking (URL CALLBACK) and
calling CALLBACK (asynchronously, without blocking Emacs) with
book details or nil. Check documentation of `org-books-get-details'
for details on the CALLBACK argument and the expected result."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-books)

(defcustom org-books-fetch-timeout 15
  "Seconds to wait for a URL fetch to complete before giving up."
  :type 'integer
  :group 'org-books)

(defun org-books--defer (thunk)
  "Call THUNK (a 0-argument function) shortly, outside of the
current call stack.

A fetch's success/failure callback runs synchronously from deep
inside the network process filter that delivered it. Driving
interactive UI (`read-string', `completing-read' and friends)
directly from there is unreliable -- it can hang depending on
what buffer/window happened to be active when the request went
out. Routing the interactive continuation through this function
first lets it run as a fresh, ordinary top-level event instead."
  (run-at-time 0 nil thunk))

(defun org-books--url-retrieve-async (url on-success on-error)
  "Fetch URL without blocking Emacs.

Calls ON-SUCCESS with the response buffer (point placed right
after the HTTP headers) on success, or ON-ERROR with a
human-readable reason string on failure or timeout. Exactly one
of the two is called, exactly once. The response buffer, if any,
is killed right after its callback returns."
  (let* (done
         (settle (lambda (fn arg buffer)
                   (unless done
                     (setq done t)
                     (unwind-protect (funcall fn arg)
                       (when (buffer-live-p buffer) (kill-buffer buffer))))))
         (timer (run-at-time org-books-fetch-timeout nil
                              (lambda () (funcall settle on-error "timed out" nil)))))
    (url-retrieve
     url
     (lambda (status)
       (cancel-timer timer)
       (let ((buffer (current-buffer))
             (err (plist-get status :error)))
         (cond
          (err (funcall settle on-error (format "%S" err) buffer))
          ((not (progn (goto-char (point-min))
                       (search-forward-regexp "\n[\t\n ]*\n+" nil t)))
           (funcall settle on-error "malformed response" buffer))
          (t (funcall settle on-success buffer buffer)))))
     nil t)))

(defun org-books--fetch-html-async (url on-success on-error)
  "Fetch and parse the HTML page at URL without blocking Emacs.

ON-SUCCESS is called with the parsed page node. ON-ERROR is
called with a reason string on failure."
  (org-books--url-retrieve-async
   url
   (lambda (buffer)
     (with-current-buffer buffer
       (decode-coding-region (point) (point-max) 'utf-8)
       (funcall on-success (libxml-parse-html-region (point) (point-max)))))
   on-error))

(defun org-books--fetch-json-async (url on-success on-error)
  "Fetch and parse the JSON document at URL without blocking Emacs.

ON-SUCCESS is called with the parsed value. ON-ERROR is called
with a reason string on failure."
  (org-books--url-retrieve-async
   url
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string))
         (funcall on-success (json-read)))))
   on-error))

(defun org-books--clean-str (text)
  "Clean TEXT to remove extra whitespaces."
  (string-trim (replace-regexp-in-string "[ \t\n\r]+" " " text)))

(defun org-books--html-decode-entities (text)
  "Decode common HTML entities found in TEXT.

This is meant for text pulled out of raw contexts (like inline
JSON embedded in a page) that libxml does not entity-decode for
us, since some sites (e.g. Goodreads) double-escape such data."
  (dolist (pair '(("&amp;" . "&") ("&quot;" . "\"") ("&#39;" . "'")
                  ("&apos;" . "'") ("&lt;" . "<") ("&gt;" . ">"))
                text)
    (setq text (replace-regexp-in-string (regexp-quote (car pair)) (cdr pair) text t t))))

(defun org-books--dom-has-class-p (node class)
  "Tell if NODE's class attribute has CLASS as one of its
space-separated class names.

This is an exact match against individual class tokens, unlike
`dom-by-class', whose MATCH argument is a regexp tested against
the whole class attribute as one string -- so `dom-by-class' can
false-positive on an unrelated class that merely contains CLASS
as a substring (e.g. \"author\" also matching a class named
\"authorNameColumn\", which is a real, observed false match on
Amazon's product pages)."
  (let ((class-attr (dom-attr node 'class)))
    (and class-attr (member class (split-string class-attr)))))

(defun org-books--dom-by-class (node class)
  "Return elements within NODE (self-inclusive) that have CLASS as
one of their space-separated class names. See
`org-books--dom-has-class-p'."
  (dom-search node (lambda (n) (org-books--dom-has-class-p n class))))

(defun org-books--dom-descendants-by-class (nodes class)
  "Return elements with class CLASS found within the subtree of any
node in NODES.

Mirrors a CSS descendant-combinator selector like \".a .b\" when
NODES is itself the result of a previous `org-books--dom-by-class'
call -- scoping matches this way (rather than searching the whole
page) matters: some sites reuse the same class name for an
unrelated widget elsewhere on the page, and a page-wide search
would wrongly pick that up too."
  (seq-mapcat (lambda (node) (org-books--dom-by-class node class)) nodes))

(defun org-books--dom-direct-children-by-tag (node tag)
  "Return NODE's direct, non-text children that are elements of TAG."
  (seq-filter (lambda (child) (and (consp child) (eq (dom-tag child) tag)))
              (dom-non-text-children node)))

(defun org-books--get-ld-json-objects (page-node)
  "Return list of parsed JSON-LD objects embedded in PAGE-NODE."
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string))
    (delq nil
          (mapcar
           (lambda (el)
             (when (string= (dom-attr el 'type) "application/ld+json")
               (condition-case nil
                   ;; `dom-text' (not `dom-texts') since we want the script
                   ;; tag's raw content -- `dom-texts' deliberately skips it.
                   (json-read-from-string (dom-text el))
                 (error nil))))
           (dom-by-tag page-node 'script)))))

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

PAGE-NODE is a parsed HTML page, as returned by
`libxml-parse-html-region'."
  (let ((authors (org-books--dom-descendants-by-class
                  (org-books--dom-descendants-by-class (list page-node) "a-section")
                  "author")))
    (or (mapcar #'dom-texts (org-books--dom-descendants-by-class authors "contributorNameID"))
        (mapcar #'dom-texts (seq-mapcat (lambda (a) (org-books--dom-direct-children-by-tag a 'a)) authors)))))

(defun org-books-get-details-amazon (url callback)
  "Get book details from amazon URL asynchronously.

Calls CALLBACK with the result list, or nil if not found or the
fetch failed."
  (org-books--fetch-html-async
   url
   (lambda (page-node)
     (let* ((title (org-books--clean-str
                    (dom-texts (car (dom-by-id page-node "\\`productTitle\\'")))))
            (author (string-join (org-books-get-details-amazon-authors page-node) ", ")))
       (funcall callback
                (unless (string-equal title "")
                  (list title author `(("AMAZON" . ,url)))))))
   (lambda (_reason) (funcall callback nil))))

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
                       (string-join (delq nil (mapcar (lambda (a) (and (hash-table-p a) (gethash "name" a)))
                                                       authors))
                                    ", ")))))
        (unless (string-equal title "")
          (list title author `(("GOODREADS" . ,url))))))))

(defun org-books-get-details-goodreads--scrape (page-node url)
  "Get book details for goodreads PAGE-NODE by scraping displayed elements.

Fallback for when JSON-LD data is not present. The contributor
query is scoped to the metadata section's contributor list so it
does not also match the (differently purposed) author bio widget
further down the page, which shares the same CSS class and would
otherwise duplicate the primary author's name."
  (let* ((title (org-books--clean-str (dom-texts (car (org-books--dom-by-class page-node "Text__title1")))))
         (author (org-books--clean-str
                  (string-join (mapcar #'dom-texts
                                        (org-books--dom-descendants-by-class
                                         (org-books--dom-by-class page-node "BookPageMetadataSection__contributor")
                                         "ContributorLink__name"))
                               ", "))))
    (unless (string-equal title "")
      (list title author `(("GOODREADS" . ,url))))))

(defun org-books-get-details-goodreads (url callback)
  "Get book details from Goodreads URL asynchronously.

Calls CALLBACK with the result list, or nil if not found or the
fetch failed."
  (org-books--fetch-html-async
   url
   (lambda (page-node)
     (funcall callback
              (or (org-books-get-details-goodreads--ld-json page-node url)
                  (org-books-get-details-goodreads--scrape page-node url))))
   (lambda (_reason) (funcall callback nil))))

(defun org-books-get-url-from-isbn (isbn)
  "Make and return openlibrary url from ISBN."
  (concat "https://openlibrary.org/api/books?bibkeys=ISBN:" isbn "&jscmd=data&format=json"))

(defun org-books-get-details-google-books (url callback)
  "Get book details from Google Books URL asynchronously.

Calls CALLBACK with the result list, or nil if not found or the
fetch failed."
  (org-books--fetch-html-async
   url
   (lambda (page-node)
     (let ((raw-title (org-books--clean-str (dom-texts (car (dom-by-tag page-node 'title))))))
       (funcall callback
                (when (string-match "^\\(.*\\) - \\(.+\\) - Google Books$" raw-title)
                  (list (match-string 1 raw-title)
                        (match-string 2 raw-title)
                        `(("GOOGLE_BOOKS" . ,url)))))))
   (lambda (_reason) (funcall callback nil))))

(defun org-books-get-details-wikipedia (url callback)
  "Get book details from Wikipedia URL asynchronously.

Calls CALLBACK with the result list, or nil if not found or the
fetch failed."
  (condition-case nil
      (let* ((parsed-url (url-generic-parse-url url))
             (host (url-host parsed-url))
             (path (car (split-string (url-filename parsed-url) "?"))))
        (if (string-match "^/wiki/\\(.+\\)$" path)
            (let* ((article (match-string 1 path))
                   (api-url (concat "https://" host "/api/rest_v1/page/summary/" article)))
              (org-books--fetch-json-async
               api-url
               (lambda (json)
                 (let ((title (gethash "title" json)))
                   (funcall callback
                            (when (and title (not (string-empty-p title)))
                              (list title "" `(("WIKIPEDIA" . ,url)))))))
               (lambda (_reason) (funcall callback nil))))
          (funcall callback nil)))
    (error (funcall callback nil))))

(defun org-books--get-page-title-async (url callback)
  "Fetch page at URL and call CALLBACK with its HTML title tag
content, or nil on failure.

CALLBACK is invoked via `org-books--defer', since callers
typically follow up with interactive prompts."
  (org-books--fetch-html-async
   url
   (lambda (page-node)
     (let ((title (dom-texts (car (dom-by-tag page-node 'title)))))
       (org-books--defer (lambda () (funcall callback (when title (org-books--clean-str title)))))))
   (lambda (_reason) (org-books--defer (lambda () (funcall callback nil))))))

(defun org-books-get-details-isbn (url callback)
  "Get book details from openlibrary ISBN response from URL asynchronously.

Calls CALLBACK with the result list, or nil if the ISBN was not
found or the fetch failed."
  (org-books--fetch-json-async
   url
   (lambda (json)
     (let* ((isbn (car (hash-table-keys json)))
            (data (and isbn (gethash isbn json)))
            (title (and data (gethash "title" data))))
       (funcall callback
                (when (and data title)
                  (list title
                        (string-join (mapcar (lambda (a) (gethash "name" a)) (gethash "authors" data)) ", ")
                        `(("ISBN" . ,url)))))))
   (lambda (_reason) (funcall callback nil))))

(defun org-books-get-details (url callback)
  "Fetch book details from given URL asynchronously, without
blocking Emacs.

Calls CALLBACK with a list of three items: title (string), author
(string) and an alist of properties to be applied to the org
entry, or with nil if the url is unsupported or the lookup
failed. CALLBACK is invoked via `org-books--defer', so it is safe
to show interactive prompts (as `org-books-add-url' does) from
inside it. See `org-books-url-pattern-dispatches' for how urls
are matched to a resolver function -- resolver functions take
(URL CALLBACK) and must call CALLBACK exactly once."
  (let ((url-host-string (url-host (url-generic-parse-url url)))
        (matched nil)
        (deferred-callback (lambda (result) (org-books--defer (lambda () (funcall callback result))))))
    (cl-dolist (pattern-fn-pair org-books-url-pattern-dispatches)
      (when (string-match-p (car pattern-fn-pair) url-host-string)
        (setq matched t)
        (funcall (cdr pattern-fn-pair) url deferred-callback)
        (cl-return)))
    (unless matched
      (funcall deferred-callback nil))))

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
    (seq-sort #'string-lessp
              (seq-uniq
               (mapcar #'string-trim
                       (seq-reduce (lambda (acc line) (append acc (split-string line ",")))
                                   (org-property-values "AUTHOR")
                                   nil))))))

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
      (delq ignore-sym
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
  (seq-find #'identity
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
      (let* ((completion-ignore-case t)
             (choice (completing-read "Book: " (mapcar #'car active-books) nil t))
             (picked-book (cdr (assoc choice active-books))))
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
  "Add book from web URL.

The lookup runs in the background so Emacs stays responsive while
it is in flight; you will be prompted once details (or a fetch
failure) come back."
  (interactive "sUrl: ")
  (message "org-books: fetching book details from %s..." url)
  (org-books-get-details
   url
   (lambda (details)
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
                                  (string-join (completing-read-multiple "Author(s): " (org-books-all-authors)) ", ")
                                author)))
           (org-books-add-book final-title final-author props))
       ;; When the url parsing or fetching fails, we ask user manually for
       ;; basic details while setting the URL property to the originally
       ;; given url.
       (message "org-books: could not fetch details for %s. Please enter details manually or retry." url)
       (org-books--get-page-title-async
        url
        (lambda (page-title)
          (let* ((completion-ignore-case t)
                 (title (read-string "Book Title: " page-title))
                 (authors-str (string-join (completing-read-multiple "Author(s): " (org-books-all-authors)) ", ")))
            (org-books-add-book title authors-str `(("URL" . ,url))))))))))

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

Each item in list is a pair of title (an outline path string) and
buffer position specifying the location in the file."
  (let ((org-refile-targets `((,org-books-file :maxlevel . ,org-books-file-depth)))
        (org-refile-use-outline-path t)
        (org-refile-use-cache nil))
    (mapcar (lambda (target) (cons (car target) (nth 3 target)))
            (org-refile-get-targets))))

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
      (string-join (completing-read-multiple "Author(s): " (org-books-all-authors)) ", "))))
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
              (let* ((completion-ignore-case t)
                     (choice (completing-read "Category: " (mapcar #'car headers) nil t))
                     (pos (cdr (assoc choice headers))))
                (org-books--insert-at-pos pos title author props))
            (goto-char (point-max))
            (org-books--insert 1 title author props)
            (save-buffer))))))))

;;;###autoload
(defun org-books-rate-book (rating)
  "Apply RATING to book at current point."
  (interactive "nRating (stars 1-5): ")
  (if (> rating 0)
      (org-set-property "RATING" (apply #'concat (make-list rating ":star:")))))

(provide 'org-books)
;;; org-books.el ends here

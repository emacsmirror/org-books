;; Tests

(load-file "org-books.el")

(require 'f)
(require 's)

(defun files-equal (file-a file-b)
  (string-equal (f-read-text file-a 'utf-8)
                (f-read-text file-b 'utf-8)))

(ert-deftest test-goodreads ()
  (let* ((url "https://www.goodreads.com/book/show/23754.Preludes_Nocturnes")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "The Sandman, Vol. 1: Preludes & Nocturnes"))
    (should (string-equal (second res) "Neil Gaiman, Sam Kieth, Mike Dringenberg, Malcolm Jones III, Todd Klein, Karen Berger, Daniel Vozzo"))))

(defun org-books-test--parse-fixture (file-path)
  "Parse an HTML fixture at FILE-PATH into an enlive page node."
  (enlive-parse (f-read-text file-path 'utf-8)))

(ert-deftest test-goodreads-ld-json-no-duplicate-author ()
  "Author list should come from the (untruncated) JSON-LD data, and
should not include the primary author twice even though the page
also has an unrelated \"About the author\" widget sharing the same
CSS class as the contributor list."
  (let* ((page-node (org-books-test--parse-fixture "./test/files/goodreads-sample.html"))
         (res (org-books-get-details-goodreads--ld-json page-node "https://example.com/book")))
    (should (string-equal (first res) "The Sandman, Vol. 1: Preludes & Nocturnes"))
    (should (string-equal (second res) "Neil Gaiman, Sam Kieth, Mike Dringenberg, Malcolm Jones III, Todd Klein, Karen Berger, Daniel Vozzo"))))

(ert-deftest test-goodreads-scrape-fallback-no-duplicate-author ()
  "Without JSON-LD data, the CSS-based fallback should still avoid
picking up the primary author's name a second time from the
unrelated \"About the author\" widget, even though the on-page
contributor list is truncated (missing the \"...more\" authors)."
  (let* ((page-node (org-books-test--parse-fixture "./test/files/goodreads-sample-no-ld-json.html"))
         (res (org-books-get-details-goodreads--scrape page-node "https://example.com/book")))
    (should (string-equal (first res) "The Sandman, Vol. 1: Preludes & Nocturnes"))
    (should (string-equal (second res) "Neil Gaiman, Sam Kieth"))))

(ert-deftest test-goodreads-prefers-ld-json-over-scrape ()
  (let* ((page-node (org-books-test--parse-fixture "./test/files/goodreads-sample.html"))
         (res (org-books-get-details-goodreads--scrape page-node "https://example.com/book")))
    ;; Sanity check that the fixture used for the ld-json test would, on its
    ;; own, still trip the fallback's duplicate/truncation issues -- i.e.
    ;; that the ld-json path is genuinely doing the better job, not just
    ;; agreeing with a fallback that already handles this fixture fine.
    (should (string-equal (second res) "Neil Gaiman, Sam Kieth, Mike Dringenberg"))))

(ert-deftest test-html-decode-entities ()
  (should (string-equal (org-books--html-decode-entities "Preludes &amp; Nocturnes")
                         "Preludes & Nocturnes"))
  (should (string-equal (org-books--html-decode-entities "Tom &amp; Jerry&#39;s")
                         "Tom & Jerry's"))
  (should (string-equal (org-books--html-decode-entities "no entities here")
                         "no entities here")))

(ert-deftest test-clean-str ()
  (should (string-equal (org-books--clean-str "  Karen  Berger  ") "Karen Berger"))
  (should (string-equal (org-books--clean-str "Single") "Single")))

(ert-deftest test-amazon ()
  (let* ((url "https://www.amazon.com/Organization-Man-William-H-Whyte/dp/0812218191")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "The Organization Man"))
    (should (string-equal (second res) "William H. Whyte, Joseph Nocera"))))

(ert-deftest test-amazon-with-author-page ()
  (let* ((url "https://www.amazon.com/Elements-Programming-Style-2nd/dp/0070342075")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "The Elements of Programming Style, 2nd Edition"))
    (should (string-equal (second res) "Brian W. Kernighan, P. J. Plauger"))))

(ert-deftest test-isbn ()
  (let* ((isbn "0517149257")
	       (res (org-books-get-details (org-books-get-url-from-isbn isbn))))
    (should (string-equal (first res) "The Ultimate Hitchhiker's Guide"))
    (should (string-equal (second res) "Douglas Adams"))))

(ert-deftest test-google-books ()
  (let* ((url "https://books.google.co.in/books/about/About_Face.html?id=4c4XBAAAQBAJ&redir_esc=y")
         (res (org-books-get-details url)))
    (should (string-match "About Face" (first res)))
    (should (string-match "Alan Cooper" (second res)))))

(ert-deftest test-wikipedia-fermat ()
  (let* ((url "https://en.wikipedia.org/wiki/Fermat%27s_Last_Theorem_(book)")
         (res (org-books-get-details url)))
    (should (string-match "Fermat" (first res)))
    (should (string-equal (second res) ""))))

(ert-deftest test-wikipedia-tuesdays ()
  (let* ((url "https://en.wikipedia.org/wiki/Tuesdays_with_Morrie")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "Tuesdays with Morrie"))
    (should (string-equal (second res) ""))))

(ert-deftest test-wikipedia-pride ()
  (let* ((url "https://en.wikipedia.org/wiki/Pride_and_Prejudice")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "Pride and Prejudice"))
    (should (string-equal (second res) ""))))

(ert-deftest test-wikipedia-god-of-small-things ()
  (let* ((url "https://en.wikipedia.org/wiki/The_God_of_Small_Things")
         (res (org-books-get-details url)))
    (should (string-equal (first res) "The God of Small Things"))
    (should (string-equal (second res) ""))))

(ert-deftest test-find-duplicate-by-property ()
  (let* ((pre-file "./test/files/duplicate-test-pre.org")
         (org-books-file (make-temp-file "org-books-test" nil ".org" (f-read-text pre-file 'utf-8))))
    (unwind-protect
        (progn
          (should (org-books--find-duplicate
                   "Some Other Title" "Some Other Author"
                   '(("GOODREADS" . "https://www.goodreads.com/book/show/999"))))
          (should-not (org-books--find-duplicate
                       "Some Other Title" "Some Other Author"
                       '(("GOODREADS" . "https://www.goodreads.com/book/show/111")))))
      (f-delete org-books-file))))

(ert-deftest test-find-duplicate-by-title-author ()
  (let* ((pre-file "./test/files/duplicate-test-pre.org")
         (org-books-file (make-temp-file "org-books-test" nil ".org" (f-read-text pre-file 'utf-8))))
    (unwind-protect
        (progn
          ;; Case-insensitive match on title + author, even with no shared property.
          (should (org-books--find-duplicate "existing book" "some author" nil))
          (should-not (org-books--find-duplicate "A Totally Different Book" "Some Author" nil)))
      (f-delete org-books-file))))

(ert-deftest test-add-book-duplicate-declined ()
  "When the user declines to add a probable duplicate, the normal
category-picking/insertion flow should never run."
  (let* ((pre-file "./test/files/duplicate-test-pre.org")
         (org-books-file (make-temp-file "org-books-test" nil ".org" (f-read-text pre-file 'utf-8)))
         (helm-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
                  ((symbol-function 'helm) (lambda (&rest _) (setq helm-called t))))
          (org-books-add-book "Existing Book" "Some Author")
          (should-not helm-called))
      (f-delete org-books-file))))

(ert-deftest test-add-book-duplicate-confirmed ()
  "When the user confirms adding anyway, the normal insertion flow
still runs."
  (let* ((pre-file "./test/files/duplicate-test-pre.org")
         (org-books-file (make-temp-file "org-books-test" nil ".org" (f-read-text pre-file 'utf-8)))
         (helm-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
                  ((symbol-function 'helm) (lambda (&rest _) (setq helm-called t))))
          (org-books-add-book "Existing Book" "Some Author")
          (should helm-called))
      (f-delete org-books-file))))

(ert-deftest test-add-book-no-duplicate-skips-prompt ()
  "A genuinely new book should not trigger the duplicate prompt at all."
  (let* ((pre-file "./test/files/duplicate-test-pre.org")
         (org-books-file (make-temp-file "org-books-test" nil ".org" (f-read-text pre-file 'utf-8)))
         (prompted nil)
         (helm-called nil))
    (unwind-protect
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) (setq prompted t) t))
                  ((symbol-function 'helm) (lambda (&rest _) (setq helm-called t))))
          (org-books-add-book "A Brand New Book" "Nobody Yet")
          (should-not prompted)
          (should helm-called))
      (f-delete org-books-file))))

(ert-deftest test-basic-insertion ()
  ;; The expected fixture assumes drawers get indented under their heading,
  ;; which Org only does when `org-adapt-indentation' is non-nil. Newer Org
  ;; versions (9.7+) default this to nil, so bind it explicitly to keep this
  ;; test independent of the Org version/config it happens to run under.
  (let* ((org-adapt-indentation t)
         (pre-file "./test/files/insert-test-pre.org")
         (post-file "./test/files/insert-test-post.org")
         (org-books-file (make-temp-file "org-books-test" nil ".org" (f-read-text pre-file 'utf-8))))
    (with-current-buffer (find-file-noselect org-books-file)
      (org-books--insert-at-pos (point-max) "Book Title" "Book Author" '(("ADDED" . "[2020-05-10]"))))
    (should (files-equal org-books-file post-file))
    (f-delete org-books-file)))

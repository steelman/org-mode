;;; ox-jekyll.el --- Export org files to Jekyll static site generator

;; Copyright (C) 2011-2015 Free Software Foundation, Inc.

;; Author: Łukasz Stelmach <stlman at poczta dot fm>
;; Keywords: 

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a backend to export Org files as HTML
;; for further processing by Jekyll static site generator.
;; See Org manual for more information.

;;; Code:

;;; Dependencies

(require 'ox)

;;; Define Back-End

(org-export-define-derived-backend 'jekyll 'html
  :menu-entry
  '(?h 1
       ((?J "As HTML buffer (Jekyll)" org-jekyll-export-as-html)
        (?j "As HTML file (Jekyll)" org-jekyll-export-to-html)))

  :translate-alist
  '((template . org-jekyll-html-template)
    (src-block . org-jekyll-html-src-block))

  :options-alist
  '((:jekyll-layout "LAYOUT" nil "post" t)
    (:jekyll-categories "CATEGORIES" nil nil t))
  ;; TODO: tags, as soon as I learn how they differ from categories
  ;; https://github.com/jekyll/jekyll/issues/6853
  )

(defun org-jekyll-export-as-html
    "TODO: Doc"
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (org-export-to-buffer 'jekyll "*Org JEKYLL Export*"
    async subtreep visible-only body-only ext-plist (lambda () (html-mode))))

(defun org-jekyll-export-to-html
    "TODO: Doc"
  (&optional async subtreep visible-only body-only ext-plist)
  (interactive)
  (let ((file (org-export-output-file-name ".html" subtreep)))
    (org-export-to-file 'beamer file
      async subtreep visible-only body-only ext-plist)))

(defun org-jekyll-html-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to a highlight block for Jekyll.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (message "org-jekyll-html-src-block")
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let ((lang (org-element-property :language src-block))
          (caption (org-export-get-caption src-block))
          (code (org-html-format-code src-block info))
          (label (let ((lbl (org-element-property :name src-block)))
                   (if (not lbl) ""
                     (format " id=\"%s\""
                             (org-export-solidify-link-text lbl))))))
      (if (not lang) (format "<pre class=\"example\"%s>\n%s</pre>" label code)
        (format
         "<div class=\"org-src-container\">\n%s%s\n</div>"
         (if (not caption) ""
           (format "<label class=\"org-src-name\">%s</label>"
                   (org-export-data caption info)))
         (format "\n{%% highlight %s %%}\n%s{%% endhighlight %%}" lang code))))))

(defun org-jekyll-html-template (contents info)
  "Return complete document for Jekyll, i.e. the HTML contents
without the <head> and <body> tags but prepended with a front
matter for Jekyll."
  (let ((title (org-export-data (plist-get info :title) info))
        (date (org-export-data (plist-get info :date) info))
	(categories (org-export-data  (plist-get info :jekyll-categories) info))
        (now (format-time-string "%FT%T%z" (current-time)))
        last_mod)
    (setq last_mod (if (and (stringp date) (/= 0 (length date)))
                       now
                     nil))
    (setq date (if (and (stringp date) (/= 0 (length date)))
                   date
                 (with-current-buffer (plist-get info :input-buffer)
                   (save-excursion
                     (goto-char (point-min))
                     (insert "#+DATE: " now "\n")))
                 now))
    (concat
     (format
     "---
title: %s
date: %s
layout: %s
%s%s---\n"
     title date (plist-get info :jekyll-layout)
     (if (< 0 (length categories))
	 (concat "categories:\n  - "
			 (mapconcat 'identity
				    (split-string categories "," t "\\s-+")
				    "\n  - ")	      
			 "\n")
       "")
     (when last_mod (concat "last_modified_at: " last_mod "\n")))
     contents)))

(defun org-html-publish-to-jekyll-html (plist filename pub-dir)
  "Publish an org file to HTML for postprocessing in Jekyll.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'jekyll filename
		      (concat "." (or (plist-get plist :html-extension)
				      org-html-extension "html"))
		      plist pub-dir))

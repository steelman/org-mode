;;; org-geo.el --- geolocation utilities for Org.

;; Copyright (C) 2010 Łukasz Stelmach

;; Author: Łukasz Stelmach <lukasz.stelmach@iem.pw.edu.pl>
;; Created: 2010-09-29
;; Version: 0.1

;; This file is not (yet) part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation version 2.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a copy of the GNU General Public License, search the Internet,
;; or write to the Free Software Foundation, Inc., 59 Temple Place,
;; Suite 330, Boston, MA 02111-1307 USA

;;; Commentary:
;; org-geo.el provides utility functions to support geographic
;; information stored in the GEO property.

;; Loading the module makes org-mode buffers accept links with
;; geographic coordinates to OpenStreetMap and Google Maps, as well as
;; the geo URIs (see: http://geouri.org). GEO property is set on the
;; node that a URI is dropped on. Note that if one drops above the
;; first node in a buffer then the the property is set on that node.

;; Note: a link dropped must be of a link type in terms of DND system
;; not just a piece of text, however, support for plain text
;; containing coordinates is also possible.

;;; Usage:
;;; (require 'org-geo)

;; Regular expression to verify geo URI. See RFC 5870, section
;; 3.3. It may suboptimal considering regular expression engine but
;; it is 100% RFC compliant. However, it doesn't as yet provide
;; coordinate range verification.
(defconst org-geo-uri-regexp
  (let* ((alphanum "[A-Za-z0-9]")
	 (p-unreserved "[\[\]:&+$]")
	 (pct-encoded "%[[:xdigit:]][[:xdigit:]]")

	 (mark "[-_.!~*'()]")
	 (unreserved (concat "\\(?:" alphanum "\\|" mark "\\)"))
	 (pnum "[0-9]+\\(?:\.[0-9]+\\)?")
	 (num (concat "-?" pnum))
	 (labeltext (concat "\\(?:" alphanum "\\|-\\)+"))

	 (pvalue (concat "\\(?:"
			 p-unreserved "\\|"
			 unreserved "\\|"
			 pct-encoded "\\)+" ))
	 (pname labeltext)
	 (parameter (concat ";" pname "\\(:?=" pvalue "\\)?"))
	 (uval pnum)
	 (uncp (concat ";u=" uval))
	 (crslabel (concat "\\(?:wgs84\\|" labeltext "\\)"))
	 (crsp (concat ";crs=" crslabel))
	 (p (concat
	     "\\(" crsp "\\)?"
	     "\\(" uncp "\\)?"
	     "\\(" parameter "\\)*" ))

	 (coord-c num)
	 (coord-b num)
	 (coord-a num)
	 (coordinates (concat "\\(" coord-a "\\),"
			      "\\(" coord-b "\\)"
			      "\\(," coord-c "\\)?"))
	 (geo-path (concat coordinates p))
	 (geo-scheme "geo"))
    (concat geo-scheme ":" geo-path)))

(defun org-geo-dnd-set-geo-property (uri action)
  "Receive URI from Emacs' DND system and set GEO property of the
current node to a geo URI with coordiantes extracted from the
dropped URI. If a geo URI is dropped then use it directly. We
support links from OpenStreetMap and Google Maps."
  (save-excursion
    (unless (outline-previous-heading)
      (search-forward-regexp (concat "^" org-outline-regexp)))
    (org-set-property "GEO"
      (cond
					; OpenStreetMap
       ((string-match
	 "^http://\\(?:www\.\\)openstreetmap\.org/" uri)
	(let (lat lon)
	  (dolist (p (split-string (cadr (split-string uri "\?")) "&" ))
	    (cond
	     ((string-match "lat=\\(-?[.0-9]+\\)" p)
	      (setq lat (match-string 1 p)))
	     ((string-match "lon=\\(-?[.0-9]+\\)" p)
	      (setq lon (match-string 1 p)))))
	  (concat "geo:" lat "," lon)))
					; Google Maps
       ((string-match
	 "^http://maps\.google\.com/.*ll=\\(-?[.0-9]+\\),\\(-?[.0-9]+\\)" uri)
	(concat "geo:" (match-string 1 uri) "," (match-string 2 uri)))
					; geo URI
       ((string-match (concat "^" org-geo-uri-regexp) uri) uri)))))

(defun org-geo-dnd-support ()
  (org-set-local
   'dnd-protocol-alist
   (append
    (list (cons (concat "^\\(http://"
			"\\(?:\\(?:www\.\\)?openstreetmap\.org"
			"\\|"
			"maps\.google\.com\\)"
			"\\|" org-geo-uri-regexp "\\)")
		'org-geo-dnd-set-geo-property))
    'dnd-protocol-alist)))

(add-hook 'org-mode-hook 'org-geo-dnd-support)

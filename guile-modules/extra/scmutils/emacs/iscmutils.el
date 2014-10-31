;;;; iscmutils.el --- scmutils mode with images

;; adapted from imaxima.el
;; Copyright (C) 2001, 2002, 2003, 2004 Jesper Harder

;; Author: Jesper Harder <harder@ifa.au.dk>
;; Created: 14 Nov 2001
;; Version: 0.9
;; Location: <http://purl.org/harder/imaxima.html>
;; Keywords: maxima

;; Copyright (C) 2006 Stephen Eglen

;; $Id: imaxima.el,v 1.5 2005/10/15 07:39:56 yasube Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;

;;; Commentary:
;;
;;
;; The command `iscmutils' (M-x iscmutils) provides a simple comint
;; derived CLI mode.
;;
;; Installation:
;;
;; Set the variable scmutils-dir to the directory where scmtuils' load.scm can be found:

(defvar scmutils-dir "./")

;; this package calls guile with command line arguments to 
;; load scmutils and the file emacs-show.guile, which causes 
;; the commands "show-expression" or "se" to output tex that
;; can be formatted by emacs.

;;
;; * Get the LaTeX package `breqn' from <ftp://ftp.ams.org/pub/tex/>.
;;   Install the package where TeX can find it -- e.g. install in your
;;   texmf or local.texmf tree and run texhash.
;;
;;   (With teTeX a suitable place for the *.sty and *.sym files could
;;   be "/usr/share/texmf/tex/latex/breqn/" and
;;   "/usr/share/texmf/doc/latex/breqn/" for the documentation.)
;;
;; * Put (autoload 'iscmutils "iscmutils" "Image support for scmutils." t)
;;   in your .emacs file.


;; changes from imaxima.el:
;;
;; will pass through any output after ^D control character
;;
;; changed default image type to postscript.
;; necessary to prevent asynchrony between images and plain text
;; in output buffer.
;;
;; changed default scale factor from 1.0 to 1.2

;;; Code:

;; modified to remove eval-when-compile form. Surrounding
;; the eval-when-compile prevernts iscmutils from running
;; properly in the xemacs on cygwin environment.
(require 'advice)

(require 'comint)
(require 'cl)

;; XEmacs stuff

(defalias 'iscmutils-image-type-available-p
  (if (fboundp 'image-type-available-p)
      'image-type-available-p
    'featurep))

(defalias 'iscmutils-display-pixel-width
  (if (fboundp 'display-pixel-width)
      'display-pixel-width
    'device-pixel-width))

(defalias 'iscmutils-display-pixel-height
  (if (fboundp 'display-pixel-height)
      'display-pixel-height
    'device-pixel-height))

(defalias 'iscmutils-display-mm-width
  (if (fboundp 'display-mm-width)
      'display-mm-width
    'device-mm-width))

(defalias 'iscmutils-display-mm-height
  (if (fboundp 'display-mm-height)
      'display-mm-height
    'device-mm-height))

(defalias 'iscmutils-get-window-width
  (if (featurep 'xemacs)
      'iscmutils-get-window-width-xemacs
    'iscmutils-get-window-width-emacs))

(defalias 'iscmutils-color-values
  (if (fboundp 'color-values)
      'color-values
    '(lambda (color) (color-rgb-components
		      (if (stringp color)
			  (make-color-specifier color)
			color)))))

(defun iscmutils-get-bg-color ()
  (if (featurep 'xemacs)
      (face-property 'default 'background)
    (frame-parameter nil 'background-color)))

(defun iscmutils-get-fg-color ()
  (if (featurep 'xemacs)
      (face-property 'default 'foreground)
    (frame-parameter nil 'foreground-color)))

;; XEmacs doesn't have subst-char-in-string (sigh!).

(defun iscmutils-subst-char-in-string (fromchar tochar string &optional inplace)
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))


(defconst iscmutils-mouse2 (if (featurep 'xemacs)
			   [button2]
			 [mouse-2]))

(defconst iscmutils-mouse3 (if (featurep 'xemacs)
			   [button3]
			 [mouse-3]))

;; Options

(defgroup iscmutils nil
  "Image support for Scmutils."
  :version "21.1"
  :link '(url-link "http://purl.org/harder/iscmutils.html")
  :link '(custom-manual "(iscmutils)")
  :prefix "iscmutils-"
  :group 'scmutils)

(defvar process-connection-type-flag
  (if (eql system-type 'darwin) t nil))

(defvar iscmutils-image-types '(png postscript jpeg tiff))

(defcustom iscmutils-image-type 'postscript		; avoids asynchrony
  "Image type to used in Scmutils buffer."
  :group 'iscmutils
  :type (cons 'choice
	      (mapcar (lambda (type) (list 'const type))
		      (remove-if-not 'iscmutils-image-type-available-p
				     iscmutils-image-types))))

(defcustom iscmutils-pt-size 11
  "*Point size used in LaTeX."
  :group 'iscmutils
  :type '(choice (const 9)
		 (const 10)
		 (const 11)
		 (const 12)))

(defcustom iscmutils-fnt-size "normalsize"
  "*Default size of font."
  :group 'iscmutils
  :type '(choice (const "small")
		 (const "normalsize")
		 (const "large")
		 (const "Large")
		 (const "LARGE")
		 (const "huge")
		 (const "Huge")))

(defcustom iscmutils-scale-factor 1.2
  "*All images are scaled by this factor."
  :group 'iscmutils
  :type 'number)

(defcustom iscmutils-label-color "red"
  "*Color used in output labels."
  :group 'iscmutils
  :type '(color))

(defcustom iscmutils-equation-color (iscmutils-get-fg-color)
  "*Color used for equations."
  :group 'iscmutils
  :type '(color))

(defcustom iscmutils-bg-color nil
    "Background color of iscmutils buffer."
    :group 'iscmutils
    :type '(choice (color)
		   (const :tag "None" nil)))

(defcustom iscmutils-fg-color nil
    "Foreground color of iscmutils buffer."
    :group 'iscmutils
    :type '(choice (color)
		   (const :tag "None" nil)))

(defcustom iscmutils-latex-preamble ""
  "*String inserted at the start of the document preamble.
This can be used to change, say, the document font.
E.g. `\\usepackage{concrete}' will use the Euler math fonts."
  :group 'iscmutils
  :type '(string))

(defcustom iscmutils-max-scale 0.85
  "Maximum amount of scaling allowed to fit wide equations in the buffer.
nil means no scaling at all, t allows any scaling."
  :group 'iscmutils
  :type 'number)

(defcustom iscmutils-linearize-flag t
  "Non-nil means that equations too wide to fit in the buffer are linearized."
  :type '(boolean)
  :group 'iscmutils)

(defcustom iscmutils-use-scmutils-mode-flag nil
  "Non-nil means that the major mode from `scmutils.el' is used."
  :type '(boolean)
  :group 'iscmutils)

(defcustom iscmutils-scmutils-program "guile"
  "Scmutils executable."
  :group 'iscmutils
  :type '(string))


(defcustom iscmutils-initex-option "-ini"
  "Option passed to TeX to start initex."
  :group 'iscmutils
  :type '(string))

(defcustom iscmutils-tex-program "latex"
  "TeX executable."
  :group 'iscmutils
  :type '(string))

(defcustom iscmutils-gs-program "gs"
  "Ghostscript executable."
  :group 'iscmutils
  :type '(string))

(defcustom iscmutils-gs-options '("-q" "-dNOPAUSE"
				"-dSAFER"
				"-dDELAYSAFER"
				"-DNOPLATFONTS" "-dTextAlphaBits=4"
				"-dGraphicsAlphaBits=4")
  "Options passed to gs for conversion from EPS."
  :group 'iscmutils
  :type '(repeat string))

(defcustom iscmutils-dvips-program "dvips"
  "Dvips executable."
  :group 'iscmutils
  :type '(string))

(defcustom iscmutils-dvips-options '("-E" "-R")
  "Options passed to dvips for conversion from DVI to EPS."
  :group 'iscmutils
  :type '(repeat string))

(defcustom iscmutils-tmp-dir
  (cond ((featurep 'xemacs)
	 (temp-directory))
	((eql system-type 'cygwin)
	 "/tmp/")
	(t temporary-file-directory))
  "*Directory used for temporary TeX and image files."
  :type '(directory)
  :group 'iscmutils)

(defcustom iscmutils-startup-hook nil
  "A hook called at startup.
This hook is called after iscmutils has started Scmutils."
  :group 'iscmutils
  :type 'hook)

(defcustom iscmutils-exit-hook nil
  "Hook called when exiting iscmutils."
  :group 'iscmutils
  :type 'hook)

(defvar iscmutils-tmp-subdir ""
  "Subdirectory for temporary files.")

;(defcustom iscmutils-lisp-file 
;  (if (eq system-type 'windows-nt)
;      (iscmutils-subst-char-in-string ?\\ ?/ (locate-library "iscmutils.lisp"))
;    (locate-library "iscmutils.lisp"))
;  "Location of `iscmutils.lisp'."
;  :group 'iscmutils
;  :type '(file))

(defcustom iscmutils-scmutils-options
  (if (eq system-type 'windows-nt)
      "-eval (user::run)"
    (concat "-l " scmutils-dir
	    "load.scm -l "		; load scmutils
	    scmutils-dir
	    "emacs/emacs-show.guile"))	; use emacs-show-expression
  "Arguments passed to guile."
  :group 'iscmutils
  :type '(string))

(defface iscmutils-latex-error-face
  '((t (:foreground "Blue" :underline t)))
  "Face used for LaTeX errors."
  :group 'iscmutils)

(defvar iscmutils-image-creators
  '((postscript nil)
    (png ("-sDEVICE=png16m"))
    (jpeg ("-sDEVICE=jpeg"))
    (tiff ("-sDEVICE=tiffpack")))
  "Define functions for generating images.
Argument list is passed to gs.")

(defvar iscmutils-resolution nil
  "Screen resolution where rendering started.
Cons-cell of x and y resolution, given in
dots per inch.  Buffer-local to rendering buffer.")
(make-variable-buffer-local 'iscmutils-resolution)

(defvar iscmutils-output ""
  "Accumulator for `iscmutils-filter'.")

(defvar iscmutils-gs-output ""
  "Accumulator for `iscmutils-gs-filter'.")

(defvar iscmutils-process nil)
(defvar iscmutils-gs-process nil)
(defvar iscmutils-gs-computing-p nil)
(defvar iscmutils-gs-7.05-is-broken nil)

(defvar iscmutils-error-map (make-sparse-keymap)
  "Keymap for mouse clicks on LaTeX errors.")

(defvar iscmutils-old-bg-color nil
  "Old background color.")

(defvar iscmutils-old-fg-color nil
  "Old foreground color.")

(defvar iscmutils-file-counter 0
  "Counter used for naming temp files.")

;; This piece of TeX is `mylatex.ltx' by David Carlisle.  The license is:
;;
;; "There are no restrictions on the distribution or modification of
;; this file, except that other people should not attempt to alter
;; the master copy on the ctan archives."

(defconst iscmutils-mylatex
"\\makeatletter\\let\\MYLATEXdocument\\document
\\let\\MYLATEXopenout\\openout\\def\\document{\\endgroup
{\\setbox\\z@\\hbox{\\normalfont% normal
{\\ifx\\large\\@undefined\\else\\large\\fi
\\ifx\\footnotesize\\@undefined\\else\\footnotesize\\fi}%
{\\bfseries\\itshape}% bold and bold italic
{\\itshape}\\ttfamily\\sffamily}}%
\\let\\document\\MYLATEXdocument\\let\\openout\\MYLATEXopenout
\\makeatother\\everyjob\\expandafter{\\the\\everyjob
\\begingroup\\listfiles\\expandafter\\MYLATEXcustomised\\@dofilelist
\\endgroup}\\@addtofilelist{.}\\catcode`\\\\=13\\relax
\\catcode`\\#=12\\relax\\catcode`\\ =9\\relax\\dump}
\\def\\openout#1 {\\g@addto@macro\\MYLATEXopens{\\immediate\\openout#1 }}
\\let\\MYLATEXopens\\@empty\\def\\MYLATEXbegin{\\begin{document}}
\\def\\MYLATEXcomment{mylatex}\\def\\MYLATEXcustomised#1#2#3\\typeout#4{%
\\typeout{CUSTOMISED FORMAT. Preloaded files:^^J\\@spaces\\@spaces.}#3}
{\\catcode`\\^^M=\\active\\catcode`\\/=0 %
/catcode`\\\\=13 /gdef\\{/catcode`/\\=0 /catcode`/^^M=13   /catcode`/%=9 ^^M}%
/long/gdef^^M#1^^M{/def/MYLATEXline{#1}%
/ifx/MYLATEXline/MYLATEXcomment/let/MYLATEXbegin/relax%
/let/MYLATEXline/relax/fi/ifx/MYLATEXline/MYLATEXbegin%
/catcode`/^^M=5/relax/let^^M/par/catcode`/#=6/relax%
/catcode`/%=14/relax/catcode`/ =10/relax%
/expandafter/MYLATEXopens/expandafter/MYLATEXbegin%
/else/expandafter^^M/fi}}\\expandafter\\input\\endinput%"
  "TeX code for dumping a format file.")

;;
;; Geometry
;;

(defun iscmutils-get-geometry (buffer)
  "Transfer display geometry parameters from current display.
Those are put in local variable `iscmutils-resolution'.  Calculation is done
in source buffer specified by BUFF."
  (let (res)
    (with-current-buffer buffer
      (setq res (cons (/ (* 25.4 (iscmutils-display-pixel-width))
			 (iscmutils-display-mm-width))
		      (/ (* 25.4 (iscmutils-display-pixel-height))
			 (iscmutils-display-mm-height)))))
    (setq iscmutils-resolution res)))

(defun iscmutils-get-window-width-xemacs ()
  "Return window width in mm.
XEmacs verson."
  (/ (* (window-text-area-pixel-width) (iscmutils-display-mm-width))
     (iscmutils-display-pixel-width)))

(defun iscmutils-get-window-width-emacs ()
  "Return window width in mm.
Emacs version."
  (/ (* (- (window-width) 1) (frame-char-width))
     (/ (float (iscmutils-display-pixel-width))
	(iscmutils-display-mm-width))))

(defun iscmutils-bp-to-mm (bp)
  "Convert PostScript big points to mm.  BP is size in big points."
  (* bp 0.352778))

(defun iscmutils-color-to-rgb (str)
  "Convert color name STR to rgb values understood by TeX."
  (mapcar '(lambda (x) (/ x 65535.0)) (iscmutils-color-values str)))

(defmacro iscmutils-with-temp-dir (dir &rest body)
  "Change to DIR temporarily and execute BODY."
  (let ((wd (make-symbol "wd")))
    `(let ((,wd  default-directory))
       (cd ,dir)
       (unwind-protect
	   (progn
	     ,@body)
	 (cd ,wd)))))

;;
;; Gs stuff
;;

(defun iscmutils-gs-filter (process str)
  "Set `iscmutils-gs-computing-p' to t when gs is done."
  (setq iscmutils-gs-output (concat iscmutils-gs-output str))
  (when (string-match "GS\\(<[0-9+]\\)?>" iscmutils-gs-output)
    (setq iscmutils-gs-computing-p nil)
    (setq iscmutils-gs-output "")))

(defun iscmutils-gs-wait ()
  "Wait for gs to finish."
  (while (and iscmutils-gs-computing-p
	      (eq (process-status iscmutils-gs-process) 'run))
    (accept-process-output iscmutils-gs-process 1)
))

(defun iscmutils-start-gs ()
  "Start Ghostscript as an asynchronyous process."
  ;; Are we using the broken GNU Ghostscript 7.05?
  (setq iscmutils-gs-7.05-is-broken
	(string-match "\\(GNU\\|ESP\\) Ghostscript 7.05"
		      (shell-command-to-string
		       (concat iscmutils-gs-program " --help"))))
  (let* (output
	 (type (cadr (assq iscmutils-image-type iscmutils-image-creators)))
	 (gs-args (append iscmutils-gs-options
			  type
			  (list (format "-r%gx%g" (car iscmutils-resolution)
					(cdr iscmutils-resolution))))))
    (when (processp iscmutils-gs-process)
      (delete-process iscmutils-gs-process))
    (setq iscmutils-gs-computing-p t)
    (condition-case nil
	(setq iscmutils-gs-process (apply 'start-process "iscmutils-gs"
					" *iscmutils gs output*"
					iscmutils-gs-program gs-args))
      (error (error
	      "Sorry, Ghostscript could not be started.  Please check
that you have gs in your path or customize the value of
`iscmutils-gs-program' (current values is \"%s\").
%s"
	      iscmutils-gs-program
	      (if (iscmutils-image-type-available-p 'postscript)
		  "If Ghostscript isn't installed you can set `iscmutils-image-type' to `ps'."
		;; don't offer this advice in XEmacs, which doesn't support ps.
		""))))
    (set-process-filter iscmutils-gs-process 'iscmutils-gs-filter)
    (iscmutils-gs-wait)
    (process-kill-without-query iscmutils-gs-process nil)
    (unless (eq (process-status iscmutils-gs-process) 'run)
      (setq output (shell-command-to-string (concat iscmutils-gs-program " -h")))
      (cond
       ((null (string-match (car type) output))
	(error
	 "Your version Ghostscript does not appear to support the image type %s.
The command \"gs -h\" lists the available devices.
You can change the image type in `iscmutils-image-type' or the device name
associated with an image type in `imaxma-image-creators'" (car type)))
       (t (error
	   "Some of the options passed to Ghostscript are probably not supported
by your version.  In particular \"-dTextAlphaBits=4\" and \"-dGraphicsAlphaBits=4\"
are not supported by gs 5.5 or earlier.  Please edit `iscmutils-gs-options'"))))))

(defun iscmutils-extract-bb (filename)
  "Extract EPS bounding box vector from FILENAME.
Returns a list of bounding box, width, and height."
  (with-temp-buffer
    (insert-file-contents-literally filename nil 0 1024 t)
    (goto-char (point-min))
    (when (search-forward-regexp "%%BoundingBox:\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)\
 +\\([-+]?[0-9.]+\\)" nil t)
      (let ((bb
	    (vector
	     (floor (string-to-number (match-string 1)))
	     (floor (string-to-number (match-string 2)))
	     (ceiling (string-to-number (match-string 3)))
	     (ceiling (string-to-number (match-string 4))))))
      (list bb
	    (- (aref bb 2) (aref bb 0))
	    (- (aref bb 3) (aref bb 1)))))))

(defun iscmutils-eps-scale (file bb scale)
  "Scale the eps image in FILE with factor SCALE.
BB is the bounding box of the image.  Returns a list of new bounding
box, width, and height."
  (multiple-value-bind (llx lly urx ury) (append bb nil)
    (let ((x (round (* (- urx llx) scale)))
          (y (round (* (- ury lly) scale)))
	  (buff (find-file-noselect file)))
      (unwind-protect
	  (with-current-buffer buff
	    (goto-char (point-min))
	    (search-forward "%%BoundingBox")
	    (delete-region (line-beginning-position) (line-end-position))
	    (insert (format "%%%%BoundingBox: 0 0 %d %d\n" x y))
	    (search-forward "%%EndComments")
	    (forward-line)
	    (insert "%%BeginProcSet: iscmutils 1 0\ngsave\n")
	    (insert (format "%f %f translate\n"
			    (- (* llx scale))
			    (- (* lly scale))))
	    (insert (format "%f %f scale\n" scale scale))
	    (insert "%%EndProcSet\n")
	    (goto-char (point-max))
	    (insert "\ngrestore")
	    (save-buffer))
	(kill-buffer buff))
      (list (vector 0 0 x y) x y))))

(defun iscmutils-latex ()
  "Convert Scmutils buffer to LaTeX.
Thic command does not work in XEmacs."
  (interactive)
  (let (pos2 label (pos (make-marker))
	     (buf (generate-new-buffer "*iscmutils-latex*"))
	     (oldbuf (current-buffer)))
    (set-buffer buf)
    (insert-buffer oldbuf)
    ;; Remove copyright notice
    (goto-char (point-min))
    (search-forward "(C1)" nil t 2)
    (search-backward "(C1)" nil t)
    (delete-region (point-min) (point))
    (goto-char (point-min))
    (insert "\\documentclass[leqno]{article}
\\usepackage{verbatim}
\\usepackage[cmbase]{flexisym}
\\usepackage{breqn}
\\setkeys{breqn}{compact}
\\newcommand{\\ifrac}[2]{\\frac{#1}{#2}}
\\newcommand{\\ifracd}[2]{\\frac{#1}{#2}}
\\newcommand{\\ifracn}[2]{\\frac{#1}{#2}}
\\newcommand{\\isubscript}[2]{{#1}_{#2}}
\\newcommand{\\iexpt}[2]{{#1}^{#2}}
\\newcommand{\\isqrt}[1]{\\sqrt{#1}}
\\begin{document}\n")
;;     (while (and (not (eobp))
;; 		(setq pos (next-single-property-change (point) 'display)))
;;       (goto-char pos)
;;       (insert "\\end{verbatim}\n\n")
;;       (setq pos (copy-marker (next-single-property-change (point) 'display)))
;;       (remove-text-properties (point) pos '(display nil))
;;       (setq pos2 (point))
;;       (re-search-forward "(\\([^)]*\\))")
;;       (setq label (match-string 1))
;;       (delete-region pos2 (point))
;;       (insert (format "\\begin{dmath}[number={%s}]\n" label))
;;       (goto-char pos)
;;       (insert "\\end{dmath}\n\n\\begin{verbatim}"))
;;     (goto-char (point-max))
;;     (insert "\n\\end{verbatim}\n\\end{document}")
    (while (not (eobp))
      (let* ((region-start (copy-marker (point)))
	     (region-end (copy-marker (next-single-property-change (point) 'display nil (point-max))))
	     (text-prop (get-text-property region-start 'display)))
	(if text-prop
	    (progn
	      (goto-char region-start)
	      (remove-text-properties region-start region-end '(display nil))
	      (goto-char region-end)
	      (goto-char region-start)
	      (re-search-forward "(\\([^)]*\\))")
	      (setq label (match-string 1))
	      (delete-region region-start (point))
	      (goto-char region-start)
	      (insert (format "\\begin{dmath}[number={%s}]\n" label))
	      (goto-char region-end)
	      (insert "\\end{dmath}\n\n"))
	  (progn
	    (goto-char region-start)
	    (insert "\\begin{verbatim}")
	    (goto-char region-end)
	    (insert "\\end{verbatim}\n\n")))))
    (insert "\n\\end{document}")
    (switch-to-buffer-other-window buf)
    (latex-mode)))

(defun iscmutils-process-sentinel (process event)
  "Process sentinel for Scmutils process."
  (message "Process %s %s" process event)
  (unless (eq (process-status process) 'run)
    (iscmutils-clean-up)))

(defun iscmutils-ps-to-image (psfilename filename bb width height)
  "Convert eps file PSFILENAME to a bitmap image file FILENAME.
BB is the bounding box for eps image.  WIDTH and HEIGHT are the
dimensions of the image."
  (setq iscmutils-gs-computing-p t)
  (when (eq system-type 'windows-nt)
    (setq psfilename (iscmutils-subst-char-in-string ?\\ ?/ psfilename))
    (setq filename (iscmutils-subst-char-in-string ?\\ ?/ filename)))
  (process-send-string iscmutils-gs-process
		       (format
			(if iscmutils-gs-7.05-is-broken
			    "clear /iscmutils-state save def \
<< /PageSize [%d %d] /PageOffset [%d %d] /OutputFile (%s) >> \
setpagedevice (%s) run iscmutils-state restore\n"
			  "clear \
<< \
/PageSize [%d %d] /PageOffset [%d %d] /OutputFile (%s) \
>> setpagedevice [save] (%s) (r) file cvx \
systemdict /.runandhide known revision 700 ge and {.setsafe {.runandhide}} if \
stopped {handleerror quit} if count 1 ne {quit} if \
cleardictstack 0 get restore\n")
			width
			height
			(- (aref bb 0))
			(aref bb 1)
			filename
			psfilename))
  (iscmutils-gs-wait))

(defun iscmutils-make-image (str)
  "Make image from STR."
  (let* ((filename (expand-file-name
		    (number-to-string (incf iscmutils-file-counter))
		    iscmutils-tmp-subdir))
	 (psfilename (concat filename ".ps"))
	 (label "*"))
    (when (string-match "\\(\\([^]*\\)\\)" str)
      (setq label (match-string 2 str))
      (setq str (replace-match "" t t str 1)))
    (iscmutils-tex-to-dvi str label (concat filename ".tex"))
    (iscmutils-dvi-to-ps filename)
    (if (not (file-exists-p psfilename))
	(iscmutils-latex-error str filename)
      (multiple-value-bind (bb  width height)
	    (iscmutils-extract-bb psfilename)
	  (let ((ratio (/  (iscmutils-get-window-width)
			   (iscmutils-bp-to-mm width))))
	    (when (< ratio 1.0)
	      ;; image is wider than the buffer
	      (if (and iscmutils-max-scale
			 (or (eq iscmutils-max-scale t)
			     (> ratio iscmutils-max-scale)))
		  ;; scale image
		  (multiple-value-setq (bb width height)
		    (iscmutils-eps-scale psfilename bb ratio))
		(when iscmutils-linearize-flag
		  ;; linearize image
		  (iscmutils-tex-to-dvi str label (concat filename ".tex") t)
		  (iscmutils-dvi-to-ps filename)
		  (multiple-value-setq (bb width height)
		    (iscmutils-extract-bb psfilename))))))
	  (unless (eq iscmutils-image-type 'postscript)
	    (iscmutils-ps-to-image psfilename filename bb width height))
	  (cond ((featurep 'xemacs)
		 (when (eq system-type 'windows-nt)
		   ;;(setq filename (iscmutils-subst-char-in-string ?\\ ?/ filename))
		   ;; FIXME:
		   ;; Ghostscript on Windows doesn't flush the image to the file.
		   ;; So we have to kill the process and restart.  What a kludge!
		   (kill-process iscmutils-gs-process)
		   (iscmutils-start-gs))
		 (setq str (concat " " str))
		 (set-text-properties 0 (length str)
				      `(begin-glyph
					,(make-glyph (vector iscmutils-image-type
							     :file filename))) str)
		 (add-text-properties 1 (length str) '(invisible t) str)
		 str)
		(t
		 (propertize (concat "(" label ") " str) 'display
			     (if (eq iscmutils-image-type 'postscript)
				 (create-image psfilename
					       'postscript nil
					       :pt-width width :pt-height height
					       :bounding-box bb :ascent 'center
					       :mask '(heuristic (color-values iscmutils-bg-color)))
			       (create-image filename
					     iscmutils-image-type nil
					     :ascent 'center
					     :mask '(heuristic
						     (color-values iscmutils-bg-color)))))))))))


(defun iscmutils-latex-error (str filename)
  "Make clickable error message.
STR is offending LaTeX expression.  FILENAME is name of the LaTeX file."
  (let* ((msg "LaTex error in: ")
	(delim (if (featurep 'xemacs)
		   "; " "\n"))
	iscmutils-error-2
	iscmutils-error-3
	(error-text (concat "mouse-2: view LaTeX error log" delim
					     "mouse-3: view LaTeX source")))
    (fset 'iscmutils-error-2
	  `(lambda ()
	     (interactive)
	     (view-file-other-window (concat ,filename ".log"))))
    (fset 'iscmutils-error-3
	  `(lambda ()
	     (interactive)
	     (view-file-other-window (concat ,filename ".tex"))))
    (define-key iscmutils-error-map iscmutils-mouse2 'iscmutils-error-2)
    (define-key iscmutils-error-map [(return)] 'iscmutils-error-2)
    (define-key iscmutils-error-map iscmutils-mouse3 'iscmutils-error-3)
    (define-key iscmutils-error-map [(meta return)] 'iscmutils-error-3)
    (set-text-properties 0 14 `(face iscmutils-latex-error-face
				     mouse-face highlight
				     help-echo ,error-text
				     keymap ,iscmutils-error-map)
			 msg)
    (concat msg str)))


(defun iscmutils-dump-tex ()
  "Dump a TeX format file preloaded with the required packages."
  (with-temp-file (expand-file-name "mylatex.ltx" iscmutils-tmp-subdir)
    (insert iscmutils-mylatex))
  (with-temp-file (expand-file-name "format.tex" iscmutils-tmp-subdir)
    (insert
     ;;"\\batchmode\n"
     (format "\\documentclass[%dpt,leqno]{article}\n" iscmutils-pt-size)
     iscmutils-latex-preamble
     "\\usepackage{color}\n"
     "\\usepackage{exscale}\n"
     "\\usepackage[cmbase]{flexisym}\n"
     "\\usepackage{breqn}\n"
     "\\setkeys{breqn}{compact}\n"
     "\\setlength{\\textheight}{200cm}\n"
     ;; define \boxed from amsmath.sty
     "\\makeatletter
      \\newcommand{\\boxed}[1]{\\fbox{\\m@th$\\displaystyle#1$}}
\\newcommand{\\operatorname}[1]{%
\\mathop{\\relax\\kern\\z@\\operator@font{#1}}}
      \\makeatother
      \\newcommand{\\ifrac}[2]{\\frac{#1}{#2}}
      \\newcommand{\\ifracd}[2]{\\frac{#1}{#2}}
      \\newcommand{\\ifracn}[2]{\\frac{#1}{#2}}
      \\newcommand{\\isubscript}[2]{{#1}_{#2}}
      \\newcommand{\\iexpt}[2]{{#1}^{#2}}
      \\newcommand{\\isqrt}[1]{\\sqrt{#1}}\n
      \\nofiles
      \\begin{document}
      \\end{document}"))
  (iscmutils-with-temp-dir
   iscmutils-tmp-subdir
   (apply 'call-process iscmutils-tex-program nil nil nil
	  (list iscmutils-initex-option "&latex" "mylatex.ltx"
		(format "\\input{%s}" "format.tex")))))

(defun iscmutils-tex-to-dvi (str label filename &optional linear)
"Run LaTeX on STR.
Argument LABEL is used as equation label.  FILENAME is used for
temporary files.  Use linearized form if LINEAR is non-nil."
  (with-temp-file filename
    (insert
     ;;"\\batchmode\n"
     (format "\\documentclass[%dpt,leqno]{article}\n" iscmutils-pt-size)
     "\n% mylatex\n"
     (format "\\setlength{\\textwidth}{%dmm}\n"
	     (round (/ (iscmutils-get-window-width)
		       iscmutils-scale-factor)))
     (if linear
	 (concat
	  ;; braces in both denominator and numerator
	  "\\renewcommand{\\ifrac}[2]{\\left(#1\\right)/\\left(#2\\right)}"
	  ;; only braces denominator
	  "\\renewcommand{\\ifracd}[2]{#1/\\left(#2\\right)}"
	  ;; only braces in numerator
          "\\renewcommand{\\ifracn}[2]{\\left(#1\\right)/#2}"
          "\\renewcommand{\\isubscript}[2]{\\mathrm{subscript}\\left(#1,#2\\right)}"
          "\\renewcommand{\\iexpt}[2]{\\mathrm{expt}\\left(#1,#2\\right)}"
	  "\\renewcommand{\\isqrt}[1]{\\left(#1\\right)^{1/2}}\n")
       "")
     "\\begin{document}\n"
     (apply 'format "\\pagecolor[rgb]{%f,%f,%f}"
	    (iscmutils-color-to-rgb (iscmutils-get-bg-color)))
     "\\pagestyle{empty}\n"
     (format "\\begin{%s}\n" iscmutils-fnt-size)
     (apply 'format "\\color[rgb]{%f,%f,%f}"
	    (iscmutils-color-to-rgb iscmutils-label-color))
     "\\tt"
     (format "\\begin{dmath}[number={%s}]\n" label)
     (apply 'format "\\color[rgb]{%f,%f,%f}"
	    (iscmutils-color-to-rgb iscmutils-equation-color))
     str "\\end{dmath}"
     (format "\\end{%s}" iscmutils-fnt-size)
     "\\end{document}"))
    (iscmutils-with-temp-dir iscmutils-tmp-subdir
      (apply 'call-process iscmutils-tex-program nil nil nil
	     (list "&mylatex" filename))))

(defun iscmutils-dvi-to-ps (filename)
  "Convert dvi file FILENAME to PostScript."
  (let ((dvips-args (append
		     iscmutils-dvips-options
		     (list "-x" (format "%s" (* iscmutils-scale-factor 1000))
			   "-y" (format "%s" (* iscmutils-scale-factor 1000))
			   (concat filename ".dvi") "-o"))))
    (iscmutils-with-temp-dir iscmutils-tmp-subdir
      (apply 'call-process iscmutils-dvips-program nil nil nil dvips-args))))

(defun iscmutils-clean-up ()
  "Kill gs process, delete temporary files and restore colors if applicable."
  (interactive)
  (ignore-errors
    (kill-process iscmutils-gs-process))
  (mapc 'delete-file (directory-files iscmutils-tmp-subdir t "^[^.].*"))
  (delete-directory iscmutils-tmp-subdir)
  (if (featurep 'xemacs)
      (ad-deactivate 'comint-output-filter)
    ;; restore frame colors in Emacs
    (when iscmutils-fg-color
      (modify-frame-parameters
       nil (list (cons 'foreground-color iscmutils-old-fg-color))))
    (when iscmutils-bg-color
      (modify-frame-parameters
       nil (list (cons 'background-color iscmutils-old-bg-color)))))
  (run-hooks 'iscmutils-exit-hook))

(defun iscmutils-filter (str)
  "Parse output from Scmutils and make image from TeX parts.
Argument STR contains output received from Scmutils."
;;; Uncomment to debug:
;;;  (with-current-buffer (get-buffer-create "*iscmutils-work*")
;;;   (insert str))
  (let* ((len (length str)))
    (if (zerop len)
	""
      (setq iscmutils-output (concat iscmutils-output str))
      (let ((lastchar (aref str (1- len))))
	(when (and (char-equal lastchar ?\n) (> len 1))
	  (setq lastchar (aref str (- len 2))))
	(cond
	 ;; Plain text
	 ((string-match "\\`[^]+\\'" iscmutils-output)
	  (prog1 iscmutils-output
	    (setq iscmutils-output "")))
;	 ((char-equal lastchar ?)
	 ((string-match "" iscmutils-output)
	  (string-match "\\(\\([^]*\\)\\([^]*\\)*\\)" iscmutils-output)
	  (let ((prompt (concat (match-string 3 iscmutils-output)
				(replace-match "" t t iscmutils-output 1)))
		(output "")
		(rest (match-string 2 iscmutils-output))
		text match)
	    (setq iscmutils-output "")
	    (message "Processing Scmutils output...")
	    (while (string-match "\\(\\([^]*\\)\\([^]*\\)\\)"
				 rest)
	      (setq text (match-string 2 rest))
	      (setq match (match-string 3 rest))
	      (setq rest (replace-match "" t t rest 1))
	      (setq output (concat output text (iscmutils-make-image match))))
	    (message "Processing Scmutils output...done")
	    (concat output rest prompt)))
	 ;; Special prompt, question.
	 ((char-equal lastchar ?)
	  (string-match "\\([^]*\\)" iscmutils-output)
	  (prog1 (iscmutils-make-image (match-string 1 iscmutils-output))
	    (setq iscmutils-output "")))
	 (t ""))))))

(eval-when-compile
  (ignore-errors
    (require 'scmutils)))

(defun iscmutils-setup-preoutput-filter ()
  "Set up `comint-preoutput-filter-functions' or the equivalent."
  (cond ((featurep 'xemacs)
	 ;; XEmacs does not have comint-preoutput-filter-functions, so
	 ;; we have to advice comint-output-filter instead
	 (defadvice comint-output-filter (before preoutput-filter)
	   "Run comint-preoutput-filter-functions."
	   (ad-set-arg 1 (iscmutils-filter (ad-get-arg 1))))
	 (ad-activate 'comint-output-filter))
	(t
	 (make-local-variable 'comint-preoutput-filter-functions)
	 ;; This doesn't work due to a bug in comint.el
	 ;; (add-hook 'comint-preoutput-filter-functions 'iscmutils-filter nil t)
	 (add-hook 'comint-preoutput-filter-functions 'iscmutils-filter t))))

(defun iscmutils-change-color (buf)
  "Change background and foreground color if applicable.
BUF is iscmutils buffer."
  (cond
   ((featurep 'xemacs)
    (when iscmutils-bg-color
      (set-face-background 'default iscmutils-bg-color buf))
    (when iscmutils-fg-color
      (set-face-foreground 'default iscmutils-fg-color buf)))
   (t
    (when iscmutils-bg-color
      (setq iscmutils-old-bg-color (frame-parameter nil 'background-color))
      (modify-frame-parameters
       nil (list (cons 'background-color iscmutils-bg-color))))
    (when iscmutils-fg-color
      (setq iscmutils-old-fg-color (frame-parameter nil 'foreground-color))
      (modify-frame-parameters
       nil (list (cons 'foreground-color iscmutils-fg-color)))))))

(defun iscmutils-setup ()
  "Image support for scmutils.el."
  (let ((mbuf (process-buffer inferior-scmutils-process)))
    (with-current-buffer mbuf
      (iscmutils-change-color mbuf)
      (iscmutils-get-geometry mbuf)
      (iscmutils-dump-tex)
      (unless (eq iscmutils-image-type 'postscript)
	(iscmutils-start-gs))
      (add-hook 'kill-buffer-hook 'iscmutils-clean-up t t)
      (iscmutils-setup-preoutput-filter)
      (scmutils-single-string
       (format "block(load(\"%s\"), linenum:0)$\n" iscmutils-lisp-file))
      ;; scmutils mode tries to run inferior-scmutils-mode-hook twice
      ;; due to changes made in 5.9.2 release. To prevent this,
      ;; the following hook must be removed earlier than before.
      ;; y.honda
      (remove-hook 'inferior-scmutils-mode-hook 'iscmutils-setup)
      (goto-char (point-max)))))

(defun iscmutils ()
  "Image support for Scmutils.
\"display2d:true\" in Scmutils turns images off, \"display2d:iscmutils\"
turns them on.  Set `iscmutils-use-scmutils-mode-flag' to t to use
`scmutils.el'."
  (interactive)
  (unless (iscmutils-image-type-available-p iscmutils-image-type)
    (error "Your version of Emacs does not support the image type %s"
	   iscmutils-image-type))
  (unless window-system
    (error "Cannot run iscmutils in terminal mode"))
;  (unless iscmutils-lisp-file
;    (error "The file iscmutils.lisp could not be found.
;Please customize the option `iscmutils-lisp-file'."))
  (setq iscmutils-file-counter 0)
  (make-directory
   (setq iscmutils-tmp-subdir
	 ;; For some reason TeX doesn't grok underscores in file names
	 (iscmutils-subst-char-in-string ?_ ?=
	    (make-temp-name (expand-file-name "iscmutils" iscmutils-tmp-dir)))))
  (set-file-modes iscmutils-tmp-subdir 448) ; 700 in octal
  (let ((process-connection-type process-connection-type-flag))
    (if iscmutils-use-scmutils-mode-flag
	(progn
	  (add-hook 'inferior-scmutils-mode-hook 'iscmutils-setup t)
	  (scmutils)
	  (remove-hook 'inferior-scmutils-mode-hook 'iscmutils-setup))
      (setq iscmutils-output "")
      (let ((mbuf
	     (apply 'make-comint
	      "iscmutils"
	      iscmutils-scmutils-program
	      nil
	      (split-string
	       iscmutils-scmutils-options))))
	(save-excursion
	  (set-buffer mbuf)
	  (setq iscmutils-process (get-buffer-process mbuf))
	  (iscmutils-get-geometry mbuf)
	  (iscmutils-change-color mbuf)
	  (iscmutils-dump-tex)
	  (set-process-sentinel iscmutils-process 'iscmutils-process-sentinel)
	  (iscmutils-setup-preoutput-filter)
	  (unless (eq iscmutils-image-type 'postscript)
	    (iscmutils-start-gs)))
	(when (eq system-type 'windows-nt)
	  (comint-send-string
	   mbuf
	   (format "block(load(\"%s\"), linenum:0)$\n" iscmutils-lisp-file)))
      (switch-to-buffer mbuf))))
  (run-hooks 'iscmutils-startup-hook))

(defcustom iscmutils-print-tex-file "imax"
  "Name of the LaTeX file name to be created by `iscmutils-print-buffer'.
Do not include \".tex\" suffix.  This file will be stored in the
directory `iscmutils-temp-dir'."
  :group 'iscmutils
  :type 'string)

(defcustom iscmutils-print-tex-command 
  "latex %s; dvips -o imax.ps %s; gv imax.ps"
  ;;"latex %s; xdvi %s"
  ;;"latex %s; dvipdf %s.dvi imax.pdf; open imax.pdf" for Mac OS X users.
  "Command to run LaTeX on the file created by `iscmutils-print-buffer'.
In the string %s is replaced by the name of the tex file. e.g.
\"latex %s; xdvi %s\"
"
  :group 'iscmutils
  :type 'string)

(defun iscmutils-print-buffer ()
  "Run LaTeX on the current buffer and show output.
See `iscmutils-print-tex-command' for how latex is run on the latex output."
  (interactive "")
  (let (( tex-file (concat iscmutils-tmp-dir 
			   iscmutils-print-tex-file ".tex"))
	(buf)
	(cmd)
	)
    
    (iscmutils-latex)
    (write-file tex-file)
    (setq buf (current-buffer))
    
    ;; Convert all %s into the tex file name.
    (setq cmd iscmutils-print-tex-command)
    (while (string-match "%s" cmd)
      (setq cmd (replace-match iscmutils-print-tex-file t nil cmd)))

    (shell-command cmd)
    (kill-buffer buf)			;kill the temp tex buffer
    ))

(provide 'iscmutils)

;;; iscmutils.el ends here

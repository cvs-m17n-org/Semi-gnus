AC_DEFUN(AC_DEFINE_GNUS_PRODUCT_NAME,
 [echo $ECHO_N "defining gnus product name... $ECHO_C"
  AC_CACHE_VAL(EMACS_cv_GNUS_PRODUCT_NAME,[EMACS_cv_GNUS_PRODUCT_NAME=$1])
  GNUS_PRODUCT_NAME=${EMACS_cv_GNUS_PRODUCT_NAME}
  AC_MSG_RESULT(${GNUS_PRODUCT_NAME})
  AC_SUBST(GNUS_PRODUCT_NAME)])

AC_DEFUN(AC_CHECK_EMACS,
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
  dnl environment variable to 't'.  Lets undo the damage.
  test "$EMACS" = t && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS; unset ac_cv_prog_XEMACS;

  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [[EMACS=emacs, mule...]]],
   [if test "$withval" = yes -o -z "$withval"; then
      AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)
    else
      AC_CHECK_PROG(EMACS, $withval, $withval, emacs)
    fi])
  AC_ARG_WITH(xemacs,
   [  --with-xemacs=XEMACS    compile with XEMACS [[XEMACS=xemacs]]],
   [if test "$withval" = yes -o -z "$withval"; then
      AC_CHECK_PROG(XEMACS, xemacs, xemacs, xemacs)
    else
      AC_CHECK_PROG(XEMACS, $withval, $withval, xemacs)
    fi
    EMACS=$XEMACS],
   [XEMACS=xemacs
    test -z "$EMACS" && AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)])
  AC_SUBST(EMACS)
  AC_SUBST(XEMACS)])

AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS}' -batch -eval '\''(let ((x '${elisp}')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	eval ${EMACS}' -batch -eval '\''(let ((x '${elisp}')) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil "'${OUTPUT}'" nil 5))'\' >& AC_FD_CC 2>&1
	retval=`cat ${OUTPUT}`
	echo "=> ${retval}" >& AC_FD_CC 2>&1
	rm -f ${OUTPUT}
	EMACS_cv_SYS_$1=$retval
])
$1=${EMACS_cv_SYS_$1}
if test -z "$3"; then
	AC_MSG_RESULT($$1)
fi
])

AC_DEFUN(AC_CHECK_EMACS_FLAVOR,
 [AC_MSG_CHECKING([what a flavor does $EMACS have])

  dnl Ignore cache.
  unset EMACS_cv_SYS_flavor;

  AC_EMACS_LISP(flavor,
    (if (featurep (quote xemacs))\
        \"XEmacs\"\
      \"Emacs\"),
    "noecho")
  if test $EMACS_cv_SYS_flavor = XEmacs; then
    EMACS_FLAVOR=xemacs
  else
    EMACS_FLAVOR=emacs
  fi
  AC_MSG_RESULT($EMACS_cv_SYS_flavor)])

AC_DEFUN(AC_PATH_LISPDIR, [
  AC_CHECK_EMACS_FLAVOR
  if test "$prefix" = NONE; then
	AC_MSG_CHECKING([prefix for your Emacs])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),"noecho")
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT($prefix)
  fi
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      Where to install lisp files
                          (for XEmacs package, use --with-packagedir instead)],
    lispdir=${withval})
  AC_MSG_CHECKING([where lisp files should go])
  if test -z "$lispdir"; then
    dnl Set default value
    theprefix=$prefix
    if test "$theprefix" = NONE; then
	theprefix=$ac_default_prefix
    fi
    if test "$EMACS_FLAVOR" = "xemacs"; then
	datadir="\$(prefix)/lib"
	lispdir="\$(datadir)/${EMACS_FLAVOR}/site-packages/lisp/${GNUS_PRODUCT_NAME}"
    else
	lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lisp/${GNUS_PRODUCT_NAME}"
    fi
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
	   if test "$EMACS_FLAVOR" = "xemacs"; then
	       lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-packages/lisp/${GNUS_PRODUCT_NAME}"
	   else
	       lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp/${GNUS_PRODUCT_NAME}"
	   fi
	   break
	fi
    done
  fi
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_RESULT([$lispdir
         (it will be ignored when \"make install-package[[-ja]]\" is done)])
  else
    AC_MSG_RESULT([$lispdir])
  fi
  AC_SUBST(lispdir)
])

AC_DEFUN(AC_PATH_ETCDIR, [
  AC_ARG_WITH(etcdir,[  --with-etcdir=DIR       Where to install etc files], etcdir=${withval})
  AC_MSG_CHECKING([where etc files should go])
  if test -z "$etcdir"; then
    dnl Set default value.
    if test "$EMACS_FLAVOR" = "xemacs"; then
      etcdir="\$(lispdir)/../../etc"
    else
    etcdir="\$(lispdir)/../etc"
    fi
  fi
  AC_MSG_RESULT($etcdir)
  AC_SUBST(etcdir)
])

dnl 
dnl This is a bit on the "evil hack" side of things.  It is so we can
dnl have a different default infodir for XEmacs.  A user can still specify
dnl someplace else with '--infodir=DIR'.
dnl
AC_DEFUN(AC_PATH_INFO_DIR, [
  AC_MSG_CHECKING([where the TeXinfo docs should go])
  dnl Set default value.  This must be an absolute path.
  if test "$infodir" = "\${prefix}/info"; then
    if test "$EMACS_FLAVOR" = "xemacs"; then
      info_dir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-packages/info"
    else
      info_dir="\$(prefix)/info"
    fi
  else
    info_dir=$infodir
  fi
  AC_MSG_RESULT([$info_dir
         (it will be ignored when \"make install-package[[-ja]]\" is done)])
  AC_SUBST(info_dir)
])

dnl
dnl This will set the XEmacs command line options to be slightly different
dnl from the Emacs ones.  If building with XEmacs the options will be
dnl "-batch -no-autoloads..." to give a much cleaner build environment.
dnl
AC_DEFUN(AC_SET_BUILD_FLAGS, [
  AC_MSG_CHECKING([which options to pass on to (X)Emacs])
  if test "x$FLAGS" = "x"; then
    if test "$EMACS_FLAVOR" = "xemacs"; then
      FLAGS="-batch -no-autoloads -l \$(srcdir)/dgnushack.el"
    else
      FLAGS="-batch -q -no-site-file -l \$(srcdir)/dgnushack.el"
    fi
  else
    FLAGS=$FLAGS
  fi
  AC_MSG_RESULT($FLAGS)
  AC_SUBST(FLAGS)
])

dnl
dnl Check whether a function exists in a library
dnl All '_' characters in the first argument are converted to '-'
dnl
AC_DEFUN(AC_EMACS_CHECK_LIB, [
if test -z "$3"; then
	AC_MSG_CHECKING(for $2 in $1)
fi
library=`echo $1 | tr _ -`
AC_EMACS_LISP($1,(progn (fmakunbound (quote $2)) (condition-case nil (progn (require (quote $library)) (fboundp (quote $2))) (error (prog1 nil (message \"$library not found\"))))),"noecho")
if test "${EMACS_cv_SYS_$1}" = nil; then
	EMACS_cv_SYS_$1=no
fi
if test "${EMACS_cv_SYS_$1}" = t; then
	EMACS_cv_SYS_$1=yes
fi
HAVE_$1=${EMACS_cv_SYS_$1}
AC_SUBST(HAVE_$1)
if test -z "$3"; then
	AC_MSG_RESULT($HAVE_$1)
fi
])

dnl
dnl Perform sanity checking and try to locate the W3 package
dnl
AC_DEFUN(AC_CHECK_W3, [
AC_MSG_CHECKING(for acceptable W3 version)

dnl Ignore cache.
unset EMACS_cv_ACCEPTABLE_W3;
unset EMACS_cv_SYS_w3_dir;
unset EMACS_cv_SYS_w3_forms;

AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_W3,[
AC_EMACS_CHECK_LIB(w3_forms, w3-form-encode-xwfu,"noecho")
if test "${HAVE_w3_forms}" = yes; then
	EMACS_cv_ACCEPTABLE_W3=yes
else
	EMACS_cv_ACCEPTABLE_W3=
fi

if test "${EMACS_cv_ACCEPTABLE_W3}" = yes; then
	AC_EMACS_LISP(w3_dir,(file-name-directory (locate-library \"w3-forms\")),"noecho")
	EMACS_cv_ACCEPTABLE_W3=$EMACS_cv_SYS_w3_dir
fi
])
   AC_ARG_WITH(w3,[  --with-w3=DIR           Specify where to find the w3 package], [ EMACS_cv_ACCEPTABLE_W3=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   W3=${EMACS_cv_ACCEPTABLE_W3}
   AC_SUBST(W3)
   if test -z "${EMACS_cv_ACCEPTABLE_W3}"; then
	AC_MSG_RESULT(not found)
   else
	AC_MSG_RESULT(${W3})
   fi
])

dnl
dnl Perform sanity checking and try to locate the W3 package
dnl
AC_DEFUN(AC_CHECK_URL, [
AC_MSG_CHECKING(for acceptable URL version)

dnl Ignore cache.
unset EMACS_cv_ACCEPTABLE_URL;
unset EMACS_cv_SYS_url_dir;
unset EMACS_cv_SYS_url;

AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_URL,[
AC_EMACS_CHECK_LIB(url, url-retrieve, "noecho")
if test "${HAVE_url}" = yes; then
	EMACS_cv_ACCEPTABLE_URL=yes
else
	EMACS_cv_ACCEPTABLE_URL=
fi

if test "${EMACS_cv_ACCEPTABLE_URL}" = yes; then
	AC_EMACS_LISP(url_dir,(file-name-directory (locate-library \"url\")),"noecho")
	EMACS_cv_ACCEPTABLE_URL=$EMACS_cv_SYS_url_dir
fi
])
   AC_ARG_WITH(url,[  --with-url=DIR          Specify where to find the url package], [ EMACS_cv_ACCEPTABLE_URL=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   URL=${EMACS_cv_ACCEPTABLE_URL}
   AC_SUBST(URL)
   if test -z "${EMACS_cv_ACCEPTABLE_URL}"; then
	AC_MSG_RESULT(not found)
   else
	AC_MSG_RESULT(${URL})
   fi
])

dnl
dnl Perform checking available fonts: Adobe Bembo, Adobe Futura and 
dnl Bitstream Courier.
dnl

AC_DEFUN(GNUS_CHECK_FONTS, [
test "$LATEX" = t && LATEX=
test "$LATEX" || AC_PATH_PROGS(LATEX, latex, no)
AC_MSG_CHECKING(for available fonts)
AC_ARG_WITH(fonts,[  --with-fonts            Assume all fonts required are available],[USE_FONTS="$withval"])
WITH_FONTS_bembo='%'
WITHOUT_FONTS_bembo=
WITH_FONTS_pfu='%'
WITHOUT_FONTS_pfu=
WITH_FONTS_bcr='%'
WITHOUT_FONTS_bcr=
if test -z "${USE_FONTS}"; then
  if test "${LATEX}" = no; then
	:
  else
    OUTPUT=./conftest-$$
    echo '\nonstopmode\documentclass{article}\usepackage{bembo}\begin{document}\end{document}' > ${OUTPUT}
    if ${LATEX} ${OUTPUT} </dev/null >& AC_FD_CC 2>&1  ; then  
      if test -z "${USE_FONTS}"; then
	USE_FONTS="Adobe Bembo"
      else
	USE_FONTS="${USE_FONTS}, Adobe Bembo"
      fi
      WITH_FONTS_bembo=
      WITHOUT_FONTS_bembo='%'
    fi
    echo '\nonstopmode\documentclass{article}\begin{document}{\fontfamily{pfu}\fontsize{10pt}{10}\selectfont test}\end{document}' > ${OUTPUT}
    if retval=`${LATEX} ${OUTPUT} </dev/null 2>& AC_FD_CC`; then
      if echo "$retval" | grep 'Some font shapes were not available' >& AC_FD_CC 2>&1  ; then  
	:
      else
	 if test -z "${USE_FONTS}"; then
	  USE_FONTS="Adobe Futura"
	 else
	  USE_FONTS="${USE_FONTS}, Adobe Futura"
	 fi
	 WITH_FONTS_pfu=
	 WITHOUT_FONTS_pfu='%'
      fi
    fi
    echo '\nonstopmode\documentclass{article}\begin{document}{\fontfamily{bcr}\fontsize{10pt}{10}\selectfont test}\end{document}' > ${OUTPUT}
    if retval=`${LATEX} ${OUTPUT} </dev/null 2>& AC_FD_CC`; then
      if echo "$retval" | grep 'Some font shapes were not available' >& AC_FD_CC 2>&1  ; then  
	:
      else
	 if test -z "${USE_FONTS}"; then
	  USE_FONTS="Bitstream Courier"
	 else
	  USE_FONTS="${USE_FONTS}, Bitstream Courier"
	 fi
	 WITH_FONTS_bcr=
	 WITHOUT_FONTS_bcr='%'
      fi
    fi
    rm -f ${OUTPUT} ${OUTPUT}.aux ${OUTPUT}.log ${OUTPUT}.dvi
  fi
elif test "${USE_FONTS}" = yes ; then
  WITH_FONTS_bembo=
  WITHOUT_FONTS_bembo='%'
  WITH_FONTS_pfu=
  WITHOUT_FONTS_pfu='%'
  WITH_FONTS_bcr=
  WITHOUT_FONTS_bcr='%'
fi
AC_SUBST(WITH_FONTS_bembo)
AC_SUBST(WITHOUT_FONTS_bembo)
AC_SUBST(WITH_FONTS_pfu)
AC_SUBST(WITHOUT_FONTS_pfu)
AC_SUBST(WITH_FONTS_bcr)
AC_SUBST(WITHOUT_FONTS_bcr)
if test -z "${USE_FONTS}" ; then
  USE_FONTS=no
fi
USE_FONTS=`echo "${USE_FONTS}" | sed 's/,\([[^,]]*\)$/ and\1/'`
AC_MSG_RESULT(${USE_FONTS})
if test "${USE_FONTS}" = yes ; then
  USE_FONTS='Set in Adobe Bembo, Adobe Futura and Bitstream Courier.'
elif test "${USE_FONTS}" = no ; then
  USE_FONTS=''
else
  USE_FONTS="Set in ${USE_FONTS}."
fi
AC_SUBST(USE_FONTS)
])

AC_DEFUN(AC_EXAMINE_PACKAGEDIR,
 [dnl Examine PACKAGEDIR.
  AC_EMACS_LISP(PACKAGEDIR,
    (let ((dirs (append\
		 (if (and (boundp (quote configure-package-path))\
			  (listp configure-package-path))\
		     (delete \"\" configure-package-path))\
		 (if (boundp (quote early-packages))\
		     (append\
		      (if early-package-load-path early-packages)\
		      (if late-package-load-path late-packages)\
		      (if last-package-load-path last-packages)))))\
	  package-dir)\
      (while (and dirs (not package-dir))\
	(if (file-directory-p (car dirs))\
	    (setq package-dir (car dirs)\
		  dirs (cdr dirs))))\
      (or package-dir \"\")),
    "noecho")])

AC_DEFUN(AC_PATH_PACKAGEDIR,
 [dnl Check for PACKAGEDIR.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([where the XEmacs package is])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test "$withval" != yes -a -n "$withval"; then
	PACKAGEDIR=$withval
      else
	AC_EXAMINE_PACKAGEDIR
      fi],
      AC_EXAMINE_PACKAGEDIR)
    if test -z "$PACKAGEDIR"; then
      AC_MSG_RESULT(not found)
    else
      AC_MSG_RESULT($PACKAGEDIR)
    fi
  else
    PACKAGEDIR=
  fi
  AC_SUBST(PACKAGEDIR)])

AC_DEFUN(AC_ADD_LOAD_PATH,
 [dnl Check for additional load path.
  AC_ARG_WITH(addpath,
   [  --with-addpath=PATH     search Emacs-Lisp libraries with PATH
                          use colons to separate directory names],
   [if test "$withval" != yes -a -n "$withval"; then
      AC_MSG_CHECKING([where to find the additional elisp libraries])
      ADDITIONAL_LOAD_PATH=$withval
      AC_MSG_RESULT($ADDITIONAL_LOAD_PATH)
    fi],
    ADDITIONAL_LOAD_PATH=)
  AC_SUBST(ADDITIONAL_LOAD_PATH)])

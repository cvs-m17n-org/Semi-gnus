# serial 1

AC_DEFUN(AM_PATH_LISPDIR,
 [# If set to t, that means we are running in a shell under Emacs.
  # If you have an Emacs named "t", then use the full path.
  test "$EMACS" = t && EMACS=
  test "$EMACS" || AC_PATH_PROGS(EMACS, emacs xemacs, no)
  if test $EMACS != "no"; then
    AC_MSG_CHECKING([where .elc files should go])
    dnl Set default value
    lispdir="\$(datadir)/emacs/site-lisp"
    if test "x$prefix" = "xNONE"; then
      if test -d $ac_default_prefix/share/emacs/site-lisp; then
	lispdir="\$(prefix)/share/emacs/site-lisp"
      else
	if test -d $ac_default_prefix/lib/emacs/site-lisp; then
	  lispdir="\$(prefix)/lib/emacs/site-lisp"
	fi
      fi
    else
      if test -d $prefix/share/emacs/site-lisp; then
	lispdir="\$(prefix)/share/emacs/site-lisp"
      else
	if test -d $prefix/lib/emacs/site-lisp; then
	  lispdir="\$(prefix)/lib/emacs/site-lisp"
	fi
      fi
    fi
    AC_MSG_RESULT($lispdir)
  fi
  AC_SUBST(lispdir)])

dnl AC_EMACS_LIST AC_XEMACS_P AC_PATH_LISPDIR and AC_EMACS_CHECK_LIB
dnl are stolen from w3.
dnl AC_PATH_LISPDIR obsoletes AM_PATH_LISPDIR.

AC_DEFUN(AC_EMACS_LISP, [
elisp="$2"
if test -z "$3"; then
	AC_MSG_CHECKING(for $1)
fi
AC_CACHE_VAL(EMACS_cv_SYS_$1,[
	OUTPUT=./conftest-$$
	echo ${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x) (prin1-to-string x)) nil \"${OUTPUT}\"))" >& AC_FD_CC 2>&1  
	${EMACS} -batch -eval "(let ((x ${elisp})) (write-region (if (stringp x) (princ x 'ignore) (prin1-to-string x)) nil \"${OUTPUT}\"nil 5))" >& AC_FD_CC 2>&1
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

AC_DEFUN(AC_XEMACS_P, [
  AC_MSG_CHECKING([if $EMACS is really XEmacs])
  AC_EMACS_LISP(xemacsp,(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\") ,"noecho")
  XEMACS=${EMACS_cv_SYS_xemacsp}
  EMACS_FLAVOR=emacs
  if test "$XEMACS" = "yes"; then
     EMACS_FLAVOR=xemacs
  fi
  AC_MSG_RESULT($XEMACS)
  AC_SUBST(XEMACS)
  AC_SUBST(EMACS_FLAVOR)
])

AC_DEFUN(AC_PATH_LISPDIR, [
  AC_XEMACS_P
  if test "$prefix" = "NONE"; then
	AC_MSG_CHECKING([prefix for your Emacs])
	AC_EMACS_LISP(prefix,(expand-file-name \"..\" invocation-directory),"noecho")
	prefix=${EMACS_cv_SYS_prefix}
	AC_MSG_RESULT($prefix)
  fi
  AC_ARG_WITH(lispdir,[  --with-lispdir=DIR      Where to install lisp files], lispdir=${withval})
  AC_MSG_CHECKING([where .elc files should go])
  if test -z "$lispdir"; then
    dnl Set default value
    theprefix=$prefix
    if test "x$theprefix" = "xNONE"; then
	theprefix=$ac_default_prefix
    fi
    lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lisp"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp"
	   break
	fi
    done
  fi
  AC_MSG_RESULT($lispdir)
  AC_SUBST(lispdir)
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
AC_EMACS_LISP($1,(progn (fmakunbound '$2) (condition-case nil (progn (require '$library) (fboundp '$2)) (error (prog1 nil (message \"$library not found\"))))),"noecho")
if test "${EMACS_cv_SYS_$1}" = "nil"; then
	EMACS_cv_SYS_$1=no
fi
if test "${EMACS_cv_SYS_$1}" = "t"; then
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
AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_W3,[
AC_EMACS_CHECK_LIB(w3_forms, w3-form-encode-xwfu,"noecho")
if test "${HAVE_w3_forms}" = "yes"; then
	EMACS_cv_ACCEPTABLE_W3=yes
else
	EMACS_cv_ACCEPTABLE_W3=no
fi

if test "${EMACS_cv_ACCEPTABLE_W3}" = "yes"; then
	AC_EMACS_LISP(w3_dir,(file-name-directory (locate-library \"w3-forms\")),"noecho")
	EMACS_cv_ACCEPTABLE_W3=$EMACS_cv_SYS_w3_dir
fi
])
   AC_ARG_WITH(w3,[  --with-w3=DIR           Specify where to find the w3 package], [ EMACS_cv_ACCEPTABLE_W3=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   W3=${EMACS_cv_ACCEPTABLE_W3}
   AC_SUBST(W3)
   AC_MSG_RESULT("${W3}")
])

dnl Copyright (C) 1999 NISHIDA Keisuke <knishida@ring.aist.go.jp>
dnl
dnl This program is free software; you can redistribute it and/or modify
dnl it under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2, or (at your option)
dnl any later version.
dnl
dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl GNU General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this program; if not, write to the Free Software
dnl Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
dnl 02111-1307, USA.

AC_DEFUN(AM_PATH_LISPDIR,
 [dnl #
  dnl # Check Emacs
  dnl #
  AC_ARG_WITH(emacs,
    [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, xemacs...]],
    [case "${withval}" in
       yes)	EMACS= ;;
       no)	AC_MSG_ERROR([emacs is not available]) ;;
       *)	EMACS=${withval} ;;
     esac], EMACS=)
  if test "x$EMACS" = "xt" -o "x$EMACS" = x; then
    AC_PATH_PROGS(EMACS, emacs xemacs mule, no)
    if test $EMACS = no; then
      AC_MSG_ERROR(you should install Emacs first)
    fi
  fi
  dnl #
  dnl # Check Emacs directories
  dnl #
  AC_MSG_CHECKING([where emacs files are in])
  EMACS_BASENAME="`echo x$EMACS | sed -e 's/x//' -e 's/^.*\///'`"
  if test "x$emacsdir" = x; then
    if test "x$prefix" = "xNONE"; then
      prefix=$ac_default_prefix
    fi
    emacsdir="\$(datadir)/emacs"
    case "$EMACS_BASENAME" in
    emacs|emacs-*)
      if test -d $prefix/lib/emacs; then
	emacsdir="$prefix/lib/emacs"
      fi
      if test -d $prefix/share/emacs; then
	emacsdir="$prefix/share/emacs"
      fi
      ;;
    xemacs|xemacs-*)
      if test -d $prefix/lib/xemacs; then
	emacsdir="$prefix/lib/xemacs"
      fi
      if test -d $prefix/share/xemacs; then
	emacsdir="$prefix/share/xemacs"
      fi
      ;;
    mule|mule-*)
      if test -d $prefix/lib/emacs; then
	emacsdir="$prefix/lib/emacs"
      fi
      if test -d $prefix/share/emacs; then
	emacsdir="$prefix/share/emacs"
      fi
      if test -d $prefix/lib/mule; then
	emacsdir="$prefix/lib/mule"
      fi
      if test -d $prefix/share/mule; then
	emacsdir="$prefix/share/mule"
      fi
      ;;
    esac
  fi
  AC_MSG_RESULT($emacsdir)
  AC_SUBST(emacsdir)
  dnl #
  dnl # Check Emacs site-lisp directories
  dnl #
  AC_ARG_WITH(lispdir,
    [  --with-lispdir=DIR      emacs lisp files go to DIR [guessed]],
    [case "${withval}" in
       yes)	lispdir= ;;
       no)	AC_MSG_ERROR(lispdir is not available) ;;
       *)	lispdir=${withval} ;;
     esac], lispdir=)
  AC_MSG_CHECKING([where .elc files should go])
  if test "x$lispdir" = x; then
    lispdir="$emacsdir/site-lisp"
    if test -d $emacsdir/lisp; then
      lispdir="$emacsdir/lisp"
    fi
    case "$EMACS_BASENAME" in
    xemacs|xemacs-*)
      lispdir="$lispdir/lookup"
      ;;
    esac
  fi
  AC_MSG_RESULT($lispdir)
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

AC_DEFUN(AC_XEMACS_P, [
  AC_MSG_CHECKING([if $EMACS is really XEmacs])
  AC_EMACS_LISP(xemacsp,(if (string-match \"XEmacs\" emacs-version) \"yes\" \"no\") ,"noecho")
  XEMACSP=${EMACS_cv_SYS_xemacsp}
  EMACS_FLAVOR=emacs
  if test "$XEMACSP" = "yes"; then
     EMACS_FLAVOR=xemacs
  fi
  AC_MSG_RESULT($XEMACSP)
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
AC_EMACS_LISP($1,(progn (fmakunbound (quote $2)) (condition-case nil (progn (require (quote $library)) (fboundp (quote $2))) (error (prog1 nil (message \"$library not found\"))))),"noecho")
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
	EMACS_cv_ACCEPTABLE_W3=$EMACS_cv_SYS_w3_forms
fi
])
   AC_ARG_WITH(w3,[  --with-w3=DIR           Specify where to find the w3 package], [ EMACS_cv_ACCEPTABLE_W3=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   W3=${EMACS_cv_ACCEPTABLE_W3}
   AC_SUBST(W3)
   AC_MSG_RESULT("${W3}")
])

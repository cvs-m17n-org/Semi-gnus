AC_DEFUN(AC_DEFINE_GNUS_PRODUCT_NAME,
 [echo $ac_n "defining gnus product name... $ac_c"
  AC_CACHE_VAL(EMACS_cv_GNUS_PRODUCT_NAME,[EMACS_cv_GNUS_PRODUCT_NAME=$1])
  GNUS_PRODUCT_NAME=${EMACS_cv_GNUS_PRODUCT_NAME}
  AC_MSG_RESULT(${GNUS_PRODUCT_NAME})
  AC_SUBST(GNUS_PRODUCT_NAME)])

AC_DEFUN(AC_CHECK_EMACS,
 [dnl Check for Emacsen.

  dnl Apparently, if you run a shell window in Emacs, it sets the EMACS
  dnl environment variable to 't'.  Lets undo the damage.
  test x$EMACS = xt && EMACS=

  dnl Ignore cache.
  unset ac_cv_prog_EMACS; unset ac_cv_prog_XEMACS;

  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, mule...]],
   [if test x$withval = xyes -o x$withval = x; then
      AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)
    else
      AC_CHECK_PROG(EMACS, $withval, $withval, emacs)
    fi])
  AC_ARG_WITH(xemacs,
   [  --with-xemacs=XEMACS    compile with XEMACS [XEMACS=xemacs]],
   [if test x$withval = xyes -o x$withval = x; then
      AC_CHECK_PROG(XEMACS, xemacs, xemacs, xemacs)
    else
      AC_CHECK_PROG(XEMACS, $withval, $withval, xemacs)
    fi
    EMACS=$XEMACS],
   [XEMACS=xemacs
    test x$EMACS = x &&\
      AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)])
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
    (cond ((featurep (quote xemacs)) \"XEmacs\")\
          ((boundp (quote MULE)) \"MULE\")\
          (t \"FSF Emacs\")),
    "noecho")
  case $EMACS_cv_SYS_flavor in
  XEmacs)
    EMACS_FLAVOR=xemacs;;
  MULE)
    EMACS_FLAVOR=mule;;
  *)
    EMACS_FLAVOR=emacs;;
  esac
  AC_MSG_RESULT($EMACS_cv_SYS_flavor)])

AC_DEFUN(AC_PATH_LISPDIR, [
  AC_CHECK_EMACS_FLAVOR
  if test "$prefix" = "NONE"; then
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
    if test "x$theprefix" = "xNONE"; then
	theprefix=$ac_default_prefix
    fi
    lispdir="\$(datadir)/${EMACS_FLAVOR}/site-lisp/${GNUS_PRODUCT_NAME}"
    for thedir in share lib; do
	potential=
	if test -d ${theprefix}/${thedir}/${EMACS_FLAVOR}/site-lisp; then
	   lispdir="\$(prefix)/${thedir}/${EMACS_FLAVOR}/site-lisp/${GNUS_PRODUCT_NAME}"
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

dnl Ignore cache.
unset EMACS_cv_ACCEPTABLE_W3;
unset EMACS_cv_SYS_w3_dir;
unset EMACS_cv_SYS_w3_forms;

AC_CACHE_VAL(EMACS_cv_ACCEPTABLE_W3,[
AC_EMACS_CHECK_LIB(w3_forms, w3-form-encode-xwfu,"noecho")
if test "${HAVE_w3_forms}" = "yes"; then
	EMACS_cv_ACCEPTABLE_W3=yes
else
	EMACS_cv_ACCEPTABLE_W3=
fi

if test "x${EMACS_cv_ACCEPTABLE_W3}" = "xyes"; then
	AC_EMACS_LISP(w3_dir,(file-name-directory (locate-library \"w3-forms\")),"noecho")
	EMACS_cv_ACCEPTABLE_W3=$EMACS_cv_SYS_w3_dir
fi
])
   AC_ARG_WITH(w3,[  --with-w3=DIR           Specify where to find the w3 package], [ EMACS_cv_ACCEPTABLE_W3=`( cd $withval && pwd || echo "$withval" ) 2> /dev/null` ])
   W3=${EMACS_cv_ACCEPTABLE_W3}
   AC_SUBST(W3)
   if test "x${EMACS_cv_ACCEPTABLE_W3}" = "x"; then
	AC_MSG_RESULT(not found)
   else
	AC_MSG_RESULT(${W3})
   fi
])

AC_DEFUN(AC_EXAMINE_PACKAGEDIR,
 [dnl Examine PACKAGEDIR.
  AC_EMACS_LISP(PACKAGEDIR,
    (let (package-dir)\
      (if (boundp (quote early-packages))\
	  (let ((dirs (delq nil (append (if early-package-load-path\
					    early-packages)\
					(if late-package-load-path\
					    late-packages)\
					(if last-package-load-path\
					    last-packages)))))\
	    (while (and dirs (not package-dir))\
	      (if (file-directory-p (car dirs))\
		  (setq package-dir (car dirs)\
			dirs (cdr dirs))))))\
      (or package-dir \"\")),
    "noecho")])

AC_DEFUN(AC_PATH_PACKAGEDIR,
 [dnl Check for PACKAGEDIR.
  if test ${EMACS_FLAVOR} = xemacs; then
    AC_MSG_CHECKING([where the XEmacs package is])
    AC_ARG_WITH(packagedir,
      [  --with-packagedir=DIR   package DIR for XEmacs],
      [if test x$withval != xyes -a x$withval != x; then
	PACKAGEDIR=$withval
      else
	AC_EXAMINE_PACKAGEDIR
      fi],
      AC_EXAMINE_PACKAGEDIR)
    if test x$PACKAGEDIR = x; then
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
   [if test x$withval != xyes -a x$withval != x; then
      AC_MSG_CHECKING([where to find the additional elisp libraries])
      ADDITIONAL_LOAD_PATH=$withval
      AC_MSG_RESULT($ADDITIONAL_LOAD_PATH)
    fi],
    ADDITIONAL_LOAD_PATH=)
  AC_SUBST(ADDITIONAL_LOAD_PATH)])

AC_DEFUN(AC_DEFINE_GNUS_PRODUCT_NAME,
 [dnl Defining gnus product name.
  GNUS_PRODUCT_NAME=$1])

AC_DEFUN(AC_CHECK_EMACS,
 [dnl Check for Emacsen.
  AC_ARG_WITH(emacs,
   [  --with-emacs=EMACS      compile with EMACS [EMACS=emacs, mule...]],
   [if test x$withval = xyes -o x$withval = xt -o x$withval = x; then
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
    test x$EMACS = xt -o x$EMACS = x &&\
      AC_CHECK_PROGS(EMACS, emacs xemacs mule, emacs)])
  AC_SUBST(EMACS)
  AC_SUBST(XEMACS)])

AC_DEFUN(AC_PATH_LISPDIR,
 [dnl Check for LISPDIR.
  AC_MSG_CHECKING([where lisp files should go])
  AC_ARG_WITH(lispdir,
   [  --with-lispdir=DIR      Where to install lisp files
                          (for XEmacs package, use --with-packagedir instead)],
   lispdir=$withval,
   [emacsdir=$datadir/emacs
    test x$prefix = xNONE && prefix=$ac_default_prefix
    case `echo x$EMACS | sed -e 's/x//' -e 's/^.*\///'` in
    emacs|emacs-*)
      if test -d $prefix/share/emacs; then
        emacsdir=$prefix/share/emacs
      elif test -d $prefix/lib/emacs; then
        emacsdir=$prefix/lib/emacs
      fi;;
    xemacs|xemacs-*)
      if test -d $prefix/lib/xemacs; then
        emacsdir=$prefix/lib/xemacs
      elif test -d $prefix/share/xemacs; then
        emacsdir=$prefix/share/xemacs
      fi;;
    mule|mule-*)
      if test -d $prefix/share/mule; then
        emacsdir=$prefix/share/mule
      elif test -d $prefix/lib/mule; then
        emacsdir=$prefix/lib/mule
      elif test -d $prefix/share/emacs; then
        emacsdir=$prefix/share/emacs
      elif test -d $prefix/lib/emacs; then
        emacsdir=$prefix/lib/emacs
      fi;;
    esac
    lispdir=$emacsdir/site-lisp/$GNUS_PRODUCT_NAME])
  AC_MSG_RESULT([$lispdir
         (it will be ignored when \"make install-package[[-ja]]\" is done)])
  AC_SUBST(lispdir)])

AC_DEFUN(AC_PATH_PACKAGEDIR,
 [dnl Check for PACKAGEDIR.
  AC_ARG_WITH(packagedir,
   [  --with-packagedir=DIR   package DIR for XEmacs],
   [if test x$withval != xyes -a x$withval != x; then
      AC_MSG_CHECKING([where the package should go])
      PACKAGEDIR=$withval
      AC_MSG_RESULT($PACKAGEDIR)
    fi],
    PACKAGEDIR=)
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

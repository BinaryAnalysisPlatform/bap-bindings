AC_INIT([BAP C Bindings], [0.1-beta], [ivg@ieee.org])
AC_CONFIG_FILES([Makefile test/Makefile examples/Makefile])


AC_ARG_VAR(LIBRARY, [Specify library to build])
AC_ARG_VAR(OCAMLFIND, [A path to ocamlfind])

if test -z "${LIBRARY}"; then
   LIBRARY=bap
fi

if test -z "${OCAMLFIND}"; then
   OCAMLFIND=`which ocamlfind`
fi

OCAMLDIR=`${OCAMLFIND} ocamlopt -where`

OSTYPE=`${OCAMLFIND} ocamlc -config | awk '/^os_type:/ {print $2}'`
SYSTEM=`${OCAMLFIND} ocamlc -config | awk '/^system:/ {print $2}'`
EXTDLL=`${OCAMLFIND} ocamlc -config | awk '/^ext_dll:/ {print $2}'`
OCAMLCC=`${OCAMLFIND} ocamlc -config | awk '/^bytecomp_c_compiler/ {for(i=2;i<=NF;i++) printf "%s " ,$i}'`

AC_OUTPUT
AC_SUBST(OSTYPE)
AC_SUBST(SYSTEM)
AC_SUBST(EXTDLL)
AC_SUBST(OCAMLCC)

echo "OCAMLFIND: $OCAMLFIND"
echo "OSTYPE: $OSTYPE"
echo "OCAMLDIR: $OCAMLDIR"

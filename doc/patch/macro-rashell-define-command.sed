s/@table @emph/@table @code/
/^[[:space:]]*See /{
  s/[`]\([^']*\)[']/@code{\1}/g
}
s/^@lisp//
s/^@end lisp//
/FLAG-STRING)/i\
@table @code
/(OPTION-NAME/s/^/@item /
/where the following properties are allowed/i\
@end table


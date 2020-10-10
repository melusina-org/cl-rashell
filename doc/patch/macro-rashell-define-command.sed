s/@table @emph/@table @code/
/^[[:space:]]*See /{
  s/[`]\([^']*\)[']/@code{\1}/g
}

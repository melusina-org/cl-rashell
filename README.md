# Rashell – Resilient replicant Shell Programming Library for Common Lisp

The **Rashell** package defines primitives which combine ease of use
with the ability to write maintainable and resilient programs
leveraging the full power of UNIX. These primitives implements common
patterns to interact with UNIX utilities as subprocesses. These
patterns usually yield a string or a sequence of lines, they will also
adequately report error conditions on subprocesses.

[![Continuous Integration](https://github.com/melusina-org/cl-rashell/actions/workflows/continuous-integration.yaml/badge.svg)](https://github.com/melusina-org/cl-rashell/actions/workflows/continuous-integration.yaml)

*This software is Copyright © 2017–2023 Michaël Le Barbier and
is distributed under the terms described in the LICENSE file.*


## Compatibility

The **Rashell** package is beta software and is currently only
compatible with the SBCL implementation of Common Lisp.


## Documentation

The **Rashell** package comes with documentation in [PDF][doc-pdf],
[HTML][doc-html] and [INFO][doc-info] format.

  [doc-html]: https://melusina-org.github.io/asset/cl-rashell/rashell.html
  [doc-pdf]: https://melusina-org.github.io/asset/cl-rashell/rashell.pdf
  [doc-info]: https://melusina-org.github.io/asset/cl-rashell/rashell.info


## Future Plans

- Add support for other Common Lisp implementations, probably using
  **external-program** as an interface.
- Add support for temporary files and directories with **mktemp**.
- Add support for further common utilities in additional
  packages.

Interesting utilities are maybe SCMs such as git, whereas maybe only a
few well selected functions would fit. Programs like **tar**,
**cpio**, building programs **make** or **bmake** and the like could
also be general enough to fit well in **Rashell**'s additional
packages.

# lisp.sh — Interact with Lisp systems

# Rashell (https://github.com/melusina-org/cl-rashell)
# This file is part of Rashell.
#
# Copyright © 2017–2023 Michaël Le Barbier
# All rights reserved.

# This file must be used under the terms of the MIT License.
# This source file is licensed as described in the file LICENSE, which
# you should have received as part of this distribution. The terms
# are also available at https://opensource.org/licenses/MIT

lisp_batch()
{
    sbcl --noinform\
         --no-userinit\
         --no-sysinit\
         --non-interactive\
         --disable-debugger\
         "$@"
}

# End of file `lisp.sh'

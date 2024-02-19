#!/bin/bash

bookname="${1:-""}"
file_out="${2:-""}"

# Move to kindle.sh directory (even from symlink).
cd "$(dirname "$( readlink -f "$0"; )")" || exit
Rscript show_book_clippings.R "$bookname" "$file_out"

#!/bin/sh
#
#   create input file to oui database
#
grep "(base 16)" ../doc/standards-oui.ieee.org | sed 's/[(]base 16[)]//g' | \
    awk '{ sub("\r","",$NF); printf("{16#%s, \"", $1); for (i=2; i < NF; i++) { printf("%s ", $i) }; printf("%s\"}.\n", $NF); }' > oui.term

#!/usr/bin/awk -f
#
# usage: awk -v f1=keyfield -f arrange_horizontally.awk files...
#
# This script groups successive lines by a key specified with f1,
# stores them as a sequence of data, and displays the sequence
# in the same line with the key. See the following example for
# the behavior of this script.
#
# (input)
#     A 1 X
#     A 2 Y
#     A 3 Z
#     B 4 X
#     B 5 Y
# (output)
#     A 1 X 2 Y 3 Z
#     B 4 X 5 Y
# 
# initialize
BEGIN{
    if(ARGC < 2){
	print "usage: arrange_horizontally.awk -v f1=n files..." > "/dev/stderr";
        exit;
    }
    default_field = 1;
    prevkey = "";
    buf = "";
}

# set the default field
f1==0{ f1=default_field; }

# rearrange
{
    # different key: print out and clear buffer
    if($f1!=prevkey && NR>1){
	print buf
        buf = "";
    }

    # add the current line to buffer
    prevkey = $f1;
    buf = buf " " $0;
}

# the last block
END{
    print buf
}

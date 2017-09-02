#!/usr/bin/awk -f
#
# usage: awk -v f1=keyfield -f arrange_horizontally.awk files...
#
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

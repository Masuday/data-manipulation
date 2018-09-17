#!/usr/bin/awk -f
#
# usage: awk -v f1=keyfield -v f2=datafield -f extract_data.awk keyfile datafiles
#
# This awk script print only a line with a specific keyword in a particular field
# for supplied data files. This behavior is similar to the "join" command but
# the user doesn't have to sort the input lines. The keywords are defined in a keyfile
# and the user can specify which field has a keyword in the keyfile using "-v f1=n".
# The user can also specify which field will be checked in the data files, using
# "-v f2=n". The default values of f1 and f2 are both 1.
#
# initialization
BEGIN{
    if(ARGC < 2){
	print " usage: awk -v f1=keyfield -v f2=datafield -f extract_data.awk keyfile datafile..." > "/dev/stderr"
	exit;
    }
    keyfile=ARGV[1];
    default_f1=1;
    default_f2=1;
}

# set the default field
f1==0{ f1=default_f1; }
f2==0{ f2=default_f2; }

# save keywords
FILENAME==keyfile{
    KEY[$f1]=1;
    next;
}

# extract lines with a keyword
{
    if(KEY[$f2]>0){
        print;
    }
}

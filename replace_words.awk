#!/usr/bin/awk -f
#
# usage: awk -f replace_words.awk listfile mainfiles...
#
# This awk script replaces a word in a text file with a new word. Pairs of the original words
# and the new words are stored in a list file. In the list file, each row has 2 fields separated
# by one or more spaces and the 1st field has the original word and the next field has
# the corresponding new word. This script is useful especially when the replace list is
# very long.
#

# initialization
BEGIN{
    if(ARGC<2){
        print "usage: replace_words.awk listfile mainfiles..." > "/dev/stderr";
        exit;
    }
    listfile = ARGV[1];
}

# read the list file
FILENAME==listfile{
    KEY[$1]=$2;
    next;
}

# read the main files
{
    for(i=1;i<=NF;i++){
        if(KEY[$i]){
            $i=KEY[$i];
	}
    }
    print $0;
}

#!/bin/bash
# supposed to reside in /usr/local/bin/el-backup/common.sh

# to be sourced by bash-scripts

# reading CONFIG FILE
# usage: readConf <fileName> <several variable names>
function readConf {
local confFile=$1
# echo "debug readConf: confFile=$confFile"
shift
local allowedVars="$@" # only allowed variable names will be processed
# echo "debug readConf: allowedVars=\"$allowedVars\""

local forbidden=" #@%*()<>~'\"{}[]\$"
while IFS== read -r key val ; do
    var=${key//["$forbidden"]}
    if [[ " ${allowedVars[@]} " =~ " $var " ]]; then
    case $var in
	password)
	    eval "${var}=\"${val}\"";;
	sysFiles) # many space-separated files
	    forbidden="#@%*()<>~'\"{}[]\$"
	    val=${val//["$forbidden"]}
	    eval "${var}=\"${val}\"";;
	localDir|remoteDir|users|maxMicro|maxMeso)
	    val=${val//["$forbidden"]}
	    eval "${var}=\"${val}\"";;
	*)
	    echo "invalid conf parameter: \"$var\"";;
    esac
    else
	if ! [ -z $var ]; then
	    echo "conf parameter: \"$var\" is not supposed to be defined in \"$confFile\""
	fi
    fi
done < <(grep -v "^#" $confFile)
}

### end of the block:  CONFIG FILE

function textFileP {
if [[ ${nonTextFiles[*]} =~ "$1" ]]
then
    return 0
else
    typeOf=`file -i "$1" | cut -d' ' -f2`
    if [[ $typeOf =~ ^text/.*$ ]]
    then
	echo "$1" >> $NTFs
	nonTextFiles="$nonTextFiles $1"
	return 0
    fi
fi
}
    
function oneFile {
typeOf=`file -i "$1" | cut -d' ' -f2`
if [[ $typeOf =~ ^text/.*$ ]] && [[ "$1" -nt $PBD ]]
then
    echo "$1 is newer than $PBD"
    echo "$1 is a text file"
    echo "$1" >> ${local_dir}${MMM}/root.list
fi
}

# Typical usage:  collect directory I-J-K [list of many file names]
function collect {
local local_dir=$1
shift
local MMM=$1
shift

local PBD=${local_dir}backup.version

# it is possible to mark some files in current directory as "temporary":
local NTFs=${local_dir}.temp.files
if [ -f $NTFs ]
then
    local twoWeeksAgo=`mktemp`
    touch -d `date -I --date=" 2 weeks ago "` $twoWeeksAgo
    if [[ $NTFs -ot $twoWeeksAgo ]]; then
	rm $NTFs
    else
	nonTextFiles=$(<"$NTFs")
    fi
fi

while(($#)) ; do
   oneFile $1
   shift
done
}

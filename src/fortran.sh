#!/bin/bash

# Takes four arguments
# $1 Routine name
# $2 variable type
# $3 variable precision
# $4 variable dimension
function get_routine_name {
    local name="$_ROUTINE_NAME"
    [ ! -z "$1" ] && name="${name}_$1"
    [ ! -z "$2" ] && name="${name}_${2:0:1}"
    [ ! -z "$3" ] && name="${name}_$3"
    [ ! -z "$4" ] && name="${name}_$4D"
    _ps "$name"
}

function _ps {
    local nl=0
    if [ "x${1:0:3}" == "x-nl" ]; then
	nl=1
	[ ${#1} -gt 3 ] && nl=${1:3}
	shift
    fi
    printf "%s" "$@"
    if [ $nl -gt 0 ]; then
	for i in `seq 1 $nl` ; do
	    _nl
	done
    fi
}

function _psnl { _ps -nl "$@" ; }

function _nl { printf '\n' ; }

function get_precisions {
    if [ "$1" == "complex" ]; then
	_ps "dp"
    elif [ "$1" == "real" ]; then
	_ps "sp dp"
    elif [ "$1" == "integer" ]; then
	_ps "is il"
    elif [ "$1" == "logical" ]; then
	_ps "none"
    elif [ "$1" == "character" ]; then
	_ps "none"
    else
	exit 1
    fi
}

declare -A _var
_var[complex]=cz 
_var[real]=sd 
_var[integer]=il 
_var[logical]=b
_var[character]=a

function get_variable_short {
    if [ "$2" == "dp" ]; then
	local p=1
    elif [ "$2" == "il" ]; then
	local p=1
    else
	local p=0
    fi
    local a="${_var[$1]}"
    _ps "${a:$p:1}"
}

function get_dimensions {
    if [ "$1" == "complex" ]; then
	_ps "0 1 2"
    elif [ "$1" == "real" ]; then
	_ps "0 1 2"
    elif [ "$1" == "integer" ]; then
	_ps "0 1 2"
    elif [ "$1" == "logical" ]; then
	_ps "0 1 2"
    elif [ "$1" == "character" ]; then
	_ps "1"
    else
	exit 1
    fi
}

function var_dimension {
    local d=$1
    if [ "$d" -gt 0 ]; then
	local tmp=""
	for i in `seq 1 $d` ; do
	    tmp="$tmp,:"
	done
	_ps "(${tmp:1})"
    fi
}

function add_var_declaration {
    local n="" ; local t=""
    local d="0" ; local p=""
    local s="" ; local p=""
    local extra="" ; local alloc=0
    local newline=1 ; local check=1
    while [ $# -gt 0 ]; do
	# Process what is requested
	local opt="$1"
	case $opt in
	    --*) opt=${opt:1} ;;
	esac
	shift
	case $opt in
	    -nocheck)         check=0 ;;
	    -nonewline)       newline=0 ;;
            -name)            n="$1" ; shift ;;
            -logical|-log)    t="logical" ;;
            -int|-integer)    t="integer" ;;
            -r|-real)         t="real"  ;;
            -c|-complex)      t="complex" ;;
            -char|-character) 
		case $1 in
		    --*) opt="(len=*)" ;;
		    -*) opt="(len=*)" ;;
		    *) opt="$1" ; shift ;;
		esac
		t="character$opt"
		;;
            -type)            t="type($1)" ; shift ;;
            -dimension)  [ "$1" != "0" ] && [ "$1" != "none" ] && d="$1" ; shift ;;
            -size)       [ "$1" != "0" ] && s="$1" ; shift ;;
            -precision)  [ "$1" != "none" ] && p="$1" ; shift ;;
            -opt|-optional)   extra="$extra, optional" ;;
            -alloc|-allocatable)   alloc=1 ; extra="$extra, allocatable" ;;
            -pointer)   alloc=1 ; extra="$extra, pointer" ;;
	    -target)   extra="$extra, target" ;;
            -in)   extra="$extra, intent(in)"  ;;
            -out)   extra="$extra, intent(out)"  ;;
            -inout)   extra="$extra, intent(inout)"  ;;
	esac
    done
    d=$(var_dimension $d)
    if [ -z "$d" -a $check -eq 1 ]; then
	# If the dimension is zero...
	extra="${extra//, allocatable/}"
	extra="${extra//, pointer/}"
    fi
    if [ ! -z "$s" ]; then
	d="($s)"
    fi	
    [ ! -z "$p" ] && \
	p="($p)"
    _ps "$t$p$extra :: $n$d"
    [ $newline -eq 1 ] && printf "\n"
}

function add_var_allocation {
    local n=""
    local d=0
    local s_var=""
    local dealloc=0
    while [ $# -gt 0 ]; do
	# Process what is requested
	local opt="$1"
	case $opt in
	    --*) opt=${opt:1} ;;
	esac
	shift
	case $opt in
            -name)      n="$1" ; shift ;;
	    -dimension) d="$1" ; shift ;;
            -size-of)   s_var="$1" ; shift ;;
            -deallocate)dealloc=1 ;;
	esac
    done
    [ $d -eq 0 ] && return 0
    # We do not allocate
    if [ "$dealloc" -eq 1 ]; then
	echo "    deallocate($n)"
	return 0
    fi
    [ $d -eq 0 ] && return 0
    local bound=""
    for i in `seq 1 $d` ; do
	bound="$bound,ubound($s_var,$i)"
    done
    bound="${bound:1}"
    echo "    allocate($n($bound))"
}
    

#!/bin/bash


# We will create a bash script for automating the generation of 
# routines for wrapper functions...
interface_file="dictionary_interface.inc"
code_file="dictionary_funcs.inc"

rm -f $interface_file $code_file

source fortran.sh

types="complex real integer"

_ROUTINE_NAME=dict

current_routine_name=""
function new_function {
    # $1 is the routine name
    # $@ is the argument list
    local result="$1" ; shift
    local name="$1" ; shift
    local args=""
    for arg in $@ ; do
	args="$args, $arg"
    done
    current_routine_name="$name"
    echo "$result function $name(${args:2})"
}

function end_function {
    echo ""
    echo "  end function $current_routine_name"
    echo ""
    current_routine_name=""
}

function new_routine {
    # $1 is the routine name
    # $@ is the argument list
    local name="$1" ; shift
    local args=""
    for arg in $@ ; do
	args="$args, $arg"
    done
    current_routine_name="$name"
    echo "  subroutine $name(${args:2})"
}

function end_routine {
    echo ""
    echo "  end subroutine $current_routine_name"
    echo ""
    current_routine_name=""
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
    

function add_new_line {
    echo ""
}

function create_key_v {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		new_function "type(dict)" "$(get_routine_name kv $typ $prec $dim)" key var
		add_var_declaration -name key -char --in
		add_var_declaration -name var --$typ  --in --dimension $dim --precision $prec
		echo "$current_routine_name = new_d_key(key)"
		echo "call assign($current_routine_name,val)"
		
		end_function
	    done
	done
    done
    new_function "type(dict)" "$(get_routine_name kv_var)" key var
    add_var_declaration -name key -char --in
    add_var_declaration -name var --type var --in
    echo "$current_routine_name = new_d_key(key)"
    echo "$current_routine_name = val"
    end_function
}

function create_key_v_interface {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		echo "module procedure $(get_routine_name kv $typ $prec $dim)"
	    done
	done
    done
    echo "module procedure $(get_routine_name kv_var)"
}


function create_key_vp {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		new_function "type(dict)" "$(get_routine_name kvp $typ $prec $dim)" key var
		add_var_declaration -name key -char --in
		add_var_declaration -name var --$typ  --in --dimension $dim --precision $prec
		echo "$current_routine_name = new_d_key(key)"
		echo "call associate($current_routine_name,val)"
		
		end_function
	    done
	done
    done
    #new_function "type(dict)" "$(get_routine_name kv_var)" key var
    #add_var_declaration -name key -char --in
    #add_var_declaration -name var --type var --in
    #echo "$current_routine_name = new_d_key(key)"
    #echo "$current_routine_name = val"
    #end_function
}

function create_key_vp_interface {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		echo "module procedure $(get_routine_name kvp $typ $prec $dim)"
	    done
	done
    done
    #echo "module procedure $(get_routine_name kv_var)"
}



echo "  interface operator( .KV. )" >> $interface_file
# This we will obsolete... The user has to find other means to retrieve the ID and manipulate...
# At least for now!
#create_put_var id >> $code_file
create_key_v_interface >> $interface_file
create_key_v >> $code_file
echo "  end interface operator( .KV. )" >> $interface_file

echo "  interface operator( .KPV. )" >> $interface_file
create_key_vp_interface >> $interface_file
create_key_vp >> $code_file
echo "  end interface operator( .KPV. )" >> $interface_file

#!/bin/bash


# We will create a bash script for automating the generation of 
# routines for wrapper functions...
interface_file="dictionary_interface.inc"
code_file="dictionary_funcs.inc"

rm -f $interface_file $code_file

source fortran.sh

types="complex real integer logical"

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
    _psnl "$result function $name(${args:2})"
}

function end_function {
    _psnl "end function $current_routine_name"
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
    _psnl "subroutine $name(${args:2})"
}

function end_routine {
    _psnl "end subroutine $current_routine_name"
    current_routine_name=""
}

    
function create_key_v {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		new_function "type(dict)" "$(get_routine_name kv $typ $prec $dim)" key val
		add_var_declaration -name key -char --in
		add_var_declaration -name val --$typ  --in --dimension $dim --precision $prec
		_psnl "$current_routine_name = new_d_key(key)"
		_psnl "call assign($current_routine_name%first%value,val)"
		
		end_function
	    done
	done
    done
    typ=character
    for prec in $(get_precisions $typ) ; do
        # Insert this code in the file...
	[ "$prec" == "default" ] && prec=""
	for dim in $(get_dimensions $typ) ; do
	    new_function "type(dict)" "$(get_routine_name kv $typ $prec $dim)" key val
	    add_var_declaration -name key -char --in
	    add_var_declaration -name val --$typ  --in --nocheck--dimension $dim --precision $prec
	    _psnl "$current_routine_name = new_d_key(key)"
	    _psnl "call assign($current_routine_name%first%value,val)"
	    
	    end_function
	done
    done

    #new_function "type(dict)" "$(get_routine_name kv_val)" key val
    #add_var_declaration -name key -char --in
    #add_var_declaration -name val --type var --in
    #_psnl "$current_routine_name = new_d_key(key)"
    #_psnl "call assign($current_routine_name%first%value,val)"
    #end_function
}

function create_key_v_interface {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		_psnl "module procedure $(get_routine_name kv $typ $prec $dim)"
	    done
	done
    done
    #_psnl "module procedure $(get_routine_name kv_val)"
}


function create_key_vp {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		new_function "type(dict)" "$(get_routine_name kvp $typ $prec $dim)" key val
		add_var_declaration -name key -char --in
		add_var_declaration -name val --$typ  --in --dimension $dim --precision $prec
		_psnl "$current_routine_name = new_d_key(key)"
		_psnl "call associate($current_routine_name%first%value,val)"
		
		end_function
	    done
	done
    done
    typ=character
    for prec in $(get_precisions $typ) ; do
        # Insert this code in the file...
	[ "$prec" == "default" ] && prec=""
	for dim in $(get_dimensions $typ) ; do
	    new_function "type(dict)" "$(get_routine_name kvp $typ $prec $dim)" key val
	    add_var_declaration -name key -char --in
	    add_var_declaration -name val --$typ  --in --nocheck --dimension $dim --precision $prec
	    _psnl "$current_routine_name = new_d_key(key)"
	    _psnl "call associate($current_routine_name%first%value,val)"
	    end_function
	done
    done

    #new_function "type(dict)" "$(get_routine_name kvp_val)" key val
    #add_var_declaration -name key -char --in
    #add_var_declaration -name val --type var --in
    #_psnl "$current_routine_name = new_d_key(key)"
    #_psnl "call associate($current_routine_name%first%value,val)"
    #end_function
}

function create_key_vp_interface {
    local tmp=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		_psnl "module procedure $(get_routine_name kvp $typ $prec $dim)"
	    done
	done
    done
    #_psnl "module procedure $(get_routine_name kvp_val)"
}



_psnl "  interface operator( .KV. )" >> $interface_file
# This we will obsolete... The user has to find other means to retrieve the ID and manipulate...
# At least for now!
#create_put_var id >> $code_file
create_key_v_interface >> $interface_file
create_key_v >> $code_file
_psnl "  end interface operator( .KV. )" >> $interface_file

_psnl "  interface operator( .KP. )" >> $interface_file
create_key_vp_interface >> $interface_file
create_key_vp >> $code_file
_psnl "  end interface operator( .KP. )" >> $interface_file

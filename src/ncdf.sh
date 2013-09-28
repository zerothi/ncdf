#!/bin/bash


# We will create a bash script for automating the generation of 
# routines for wrapper functions...
interface_file="ncdf_interface.f90"
code_file="ncdf_code.f90"

rm -f $interface_file $code_file

source fortran.sh

types="complex real integer"

_ROUTINE_NAME=ncdf

current_routine_name=""
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
    echo "    use netcdf"
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

function add_node_participate {
    echo "    if ( .not. ncdf_participate(ncdf) ) return"
}

function add_define_check {
    if [ "$1" == "define" ]; then
	echo "    call ncdf_redef(ncdf)"
    elif [ "$1" == "data" ]; then
	echo "    call ncdf_enddef(ncdf)"
    else
	exit 1
    fi
}


function create_put_var {
    local tmp="" ; local c_head="" ; local cc=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		c_head="" ; cc=""
		if [ $dim -ne 0 ]; then
		    c_head="count stride"
		    cc=",count=count,stride=stride"
		fi
		if [ "$1" == "name" ]; then
		    new_routine "$(get_routine_name put_var $typ $prec $dim)_name" ncdf name var start $c_head
		    add_var_declaration -name name -char --in
		    tmp=""
		else
		    new_routine "$(get_routine_name put_var $typ $prec $dim)" ncdf id var start $c_head
		    if [ "$typ" == "complex" ]; then
			add_var_declaration -name id -int --in --size 2
		    else
			add_var_declaration -name id -int --in
		    fi
		fi
		echo "     module procedure $current_routine_name" >> $interface_file
		add_var_declaration -name ncdf --type hNCDF --inout
		add_var_declaration -name var    --$typ   --in --dimension $dim --precision $prec
		add_var_declaration -name start  -int --in -opt --dimension 1
		if [ ! -z "$c_head" ]; then
		    for var in $c_head ; do
			add_var_declaration -name $var  -int --in -opt --dimension 1
		    done
		fi
		# We go to local variables:
		if [ "$1" == "name" ]; then
		    add_var_declaration -name id -int
		fi
		if [ "$typ" == "complex" ]; then
		    add_var_declaration -name rvar  -r --dimension $dim --allocatable --precision $prec
		fi
		add_new_line
		# add participation...
		add_node_participate
		add_new_line
		add_define_check data
		add_new_line

		if [ "$typ" == "complex" ]; then
		    add_var_allocation -name rvar --dimension $dim --size-of var
		    if [ "$1" != "name" ]; then
			tmp="(1)"
		    else
			echo "    call ncdf_inq_var(ncdf,'Re'//name,id=id)"
		    fi
		    echo "    rvar = real(var,$prec)"
		    echo "    call ncdf_err(nf90_put_var(ncdf%id, id$tmp, rvar,start=start$cc), &"
		    if [ "$1" == "name" ]; then
			echo "         'Saving the real part of '//name//', ${dim}D'//' in file: '//ncdf)"
		    else
			echo "         'Saving the real part, ${dim}D'//' in file: '//ncdf)"
		    fi
		    add_new_line
		    if [ "$1" != "name" ]; then
			tmp="(2)"
		    else
			echo "    call ncdf_inq_var(ncdf,'Im'//name,id=id)"
		    fi
		    if [ "$prec" == "sp" ]; then
			echo "    rvar = aimag(var)"
		    else
			echo "    rvar = dimag(var)"
		    fi
		    echo "    call ncdf_err(nf90_put_var(ncdf%id, id$tmp, rvar,start=start$cc), &"
		    if [ "$1" == "name" ]; then
			echo "         'Saving the imaginary part of '//name//', ${dim}D'//' in file: '//ncdf)"
		    else
			echo "         'Saving the imaginary part, ${dim}D'//' in file: '//ncdf)"
		    fi
		    add_new_line
		    add_var_allocation --name rvar --dimension $dim --deallocate
		else
		    if [ "$1" == "name" ]; then
			echo "    call ncdf_inq_var(ncdf,name,id=id)"
		    fi
		    echo "    call ncdf_err(nf90_put_var(ncdf%id, id, var,start=start$cc), &"
		    if [ "$1" == "name" ]; then
			echo "         'Saving $typ variable, '//name//', ${dim}D'//' in file: '//ncdf)"
		    else
			echo "         'Saving $typ variable, ${dim}D'//' in file: '//ncdf)"
		    fi
		fi
		end_routine
	    done
	done
    done
}

function create_get_var {
    local tmp="" ; local c_head="" ; local cc=""
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in $(get_dimensions $typ) ; do
		c_head="" ; cc=""
		if [ $dim -ne 0 ]; then
		    c_head="count stride"
		    cc=",count=count,stride=stride"
		fi
		if [ "$1" == "name" ]; then
		    new_routine "$(get_routine_name get_var $typ $prec $dim)_name" ncdf name var start $c_head
		    add_var_declaration -name name -char --in
		    tmp=""
		else
		    new_routine "$(get_routine_name get_var $typ $prec $dim)" ncdf id var start $c_head
		    if [ "$typ" == "complex" ]; then
			add_var_declaration -name id -int --in --size 2
		    else
			add_var_declaration -name id -int --in
		    fi
		fi
		echo "     module procedure $current_routine_name" >> $interface_file
		add_var_declaration -name ncdf --type hNCDF --inout
		add_var_declaration -name var    --$typ   --out --dimension $dim --precision $prec
		add_var_declaration -name start  -int --in -opt --dimension 1
		if [ ! -z "$c_head" ]; then
		    add_var_declaration -name count  -int --in -opt --dimension 1
		    add_var_declaration -name stride -int --in -opt --dimension 1
		fi
		# We go to local variables:
		if [ "$1" == "name" ]; then
		    add_var_declaration -name id -int
		fi
		if [ "$typ" == "complex" ]; then
		    add_var_declaration -name rvar  -r --dimension $dim --allocatable --precision $prec
		fi
		add_new_line
		# add participation...
		add_node_participate
		add_new_line
		add_define_check data
		add_new_line

		if [ "$typ" == "complex" ]; then
		    add_var_allocation -name rvar --dimension $dim --size-of var
		    if [ "$1" != "name" ]; then
			tmp="(1)"
		    else
			echo "    call ncdf_inq_var(ncdf,'Re'//name,id=id)"
		    fi

		    echo "    call ncdf_err(nf90_get_var(ncdf%id, id$tmp, rvar,start=start$cc), &"
		    if [ "$1" == "name" ]; then
			echo "         'Retrieving real part of variable, '//name//', ${dim}D'//' in file: '//ncdf)"
		    else
			echo "         'Retrieving real part, ${dim}D'//' in file: '//ncdf)"
		    fi
		    echo "    var = cmplx(rvar,0_$prec,$prec)"
		    add_new_line
		    if [ "$1" != "name" ]; then
			tmp="(2)"
		    else
			echo "    call ncdf_inq_var(ncdf,'Im'//name,id=id)"
		    fi
		    echo "    call ncdf_err(nf90_get_var(ncdf%id, id$tmp, rvar,start=start$cc), &"
		    if [ "$1" == "name" ]; then
			echo "         'Retrieving imaginary part of variable, '//name//', ${dim}D'//' in file: '//ncdf)"
		    else
			echo "         'Retrieving imaginary part, ${dim}D'//' in file: '//ncdf)"
		    fi
		    echo "    var = cmplx(real(var,$prec),rvar,$prec)"
		    add_new_line
		    add_var_allocation --name rvar --dimension $dim --deallocate
		else
		    if [ "$1" == "name" ]; then
			echo "    call ncdf_inq_var(ncdf,name,id=id)"
		    fi
		    echo "    call ncdf_err(nf90_get_var(ncdf%id, id, var,start=start$cc), &"
		    if [ "$1" == "name" ]; then
			echo "         'Retrieving $typ variable, '//name//', ${dim}D'//' in file: '//ncdf)"
		    else
			echo "         'Retrieving $typ variable ${dim}D'//' in file: '//ncdf)"
		    fi
		fi
		end_routine
	    done
	done
    done
}

function create_put_gatt {
    local tmp=""
    for typ in $types ; do
	[ "$typ" == "complex" ] && continue
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in 0 1 ; do # Attributes (only scalar or vector)
		new_routine "$(get_routine_name put_gatt $typ $prec $dim)" ncdf name att
		echo "     module procedure $current_routine_name" >> $interface_file
		add_var_declaration -name ncdf -type hNCDF --inout
		add_var_declaration -name name -char --in
		add_var_declaration -name att --$typ --in --dimension $dim --precision $prec
		add_new_line
		# add participation...
		add_node_participate
		add_new_line
		add_define_check define
		add_new_line

		echo "    call ncdf_err(nf90_put_att(ncdf%id, NF90_GLOBAL, name, att), &"
		echo "         'Saving global $typ attribute: '//trim(name)//' ${dim}D'//' in file: '//ncdf)"
		end_routine
	    done
	done
    done

    # Create the character
    new_routine "$(get_routine_name put_gatt char "" "")" ncdf name att
    echo "     module procedure $current_routine_name" >> $interface_file
    add_var_declaration -name ncdf -type hNCDF --inout
    add_var_declaration -name name -char --in
    add_var_declaration -name att --char --in
    add_new_line
    # add participation...
    add_node_participate
    add_new_line
    add_define_check define
    add_new_line
    
    echo "    call ncdf_err(nf90_put_att(ncdf%id, NF90_GLOBAL, name, att), &"
    echo "         'Saving global character attribute: '//trim(name)//' in file: '//ncdf)"
    end_routine

}


function create_put_att {
    local tmp=""
    for typ in $types ; do
	[ "$typ" == "complex" ] && continue
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in 0 1 ; do # Attributes (only scalar or vector)
		new_routine "$(get_routine_name put_att $typ $prec $dim)" ncdf var name att
		echo "     module procedure $current_routine_name" >> $interface_file
		add_var_declaration -name ncdf -type hNCDF --inout
		add_var_declaration -name var  -char --in
		add_var_declaration -name name -char --in
		add_var_declaration -name att --$typ --in --dimension $dim --precision $prec
		add_var_declaration -name id --int
		add_new_line
		# add participation...
		add_node_participate
		add_new_line
		add_define_check define
		add_new_line

		echo "    call ncdf_err(nf90_inq_varid(ncdf%id, trim(var), id), &"
		echo "         'Saving (inq) $typ attribute: '//trim(name)//' ${dim}D'//' in file: '//ncdf)"
		echo "    call ncdf_err(nf90_put_att(ncdf%id, id, name, att), &"
		echo "         'Saving $typ attribute: '//trim(name)//' ${dim}D'//' in file: '//ncdf)"
		end_routine
	    done
	done
    done

    # Create the character
    new_routine "$(get_routine_name put_att char "" "")" ncdf var name att
    echo "     module procedure $current_routine_name" >> $interface_file
    add_var_declaration -name ncdf -type hNCDF --inout
    add_var_declaration -name var  -char --in
    add_var_declaration -name name -char --in
    add_var_declaration -name att --char --in
    add_var_declaration -name id --int
    add_new_line
    # add participation...
    add_node_participate
    add_new_line
    add_define_check define
    add_new_line
    
    echo "    call ncdf_err(nf90_inq_varid(ncdf%id, trim(var), id), &"
    echo "         'Saving (inq) character attribute: '//trim(name)//' in file: '//ncdf)"
    echo "    call ncdf_err(nf90_put_att(ncdf%id, id, name, att), &"
    echo "         'Saving character attribute: '//trim(name)//' in file: '//ncdf)"
    end_routine

}

function create_get_gatt {
    local tmp=""
    new_routine "$(get_routine_name get_gatt char "" "")" ncdf name att
    echo "     module procedure $current_routine_name" >> $interface_file
    add_var_declaration -name ncdf -type hNCDF --inout
    add_var_declaration -name name -char --in
    add_var_declaration -name att --char --out --dimension 0
    add_new_line
    add_node_participate
    add_new_line

    echo "    call ncdf_err(nf90_get_att(ncdf%id, NF90_GLOBAL, name, att), &"
    echo "         'Retrieving character attribute: '//trim(name)//' in file: '//ncdf)"
    end_routine
    for typ in $types ; do
	if [ $typ == "complex" ]; then
	    continue
	fi
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in 0 1 ; do # Attributes (only scalar or vector)
		new_routine "$(get_routine_name get_gatt $typ $prec $dim)" ncdf name att
		echo "     module procedure $current_routine_name" >> $interface_file
		add_var_declaration -name ncdf -type hNCDF --inout
		add_var_declaration -name name -char --in
		add_var_declaration -name att --$typ   --out --dimension $dim --precision $prec
		add_new_line
		# add participation...
		add_node_participate
		add_new_line

		echo "    call ncdf_err(nf90_get_att(ncdf%id, NF90_GLOBAL, name, att), &"
		echo "         'Retrieving $typ attribute: '//trim(name)//' ${dim}D'//' in file: '//ncdf)"
		end_routine
	    done
	done
    done
}

function create_get_att {
    local tmp=""
    new_routine "$(get_routine_name get_att char "" "")" ncdf var name att
    echo "     module procedure $current_routine_name" >> $interface_file
    add_var_declaration -name ncdf -type hNCDF --inout
    add_var_declaration -name var  -char --in
    add_var_declaration -name name -char --in
    add_var_declaration -name att --char --out --dimension 0
    add_var_declaration -name id  --int
    add_new_line
    add_node_participate
    add_new_line

    echo "    call ncdf_err(nf90_inq_varid(ncdf%id, trim(var), id), &"
    echo "         'Retrieving id from '//trim(var)//' character attribute: '//trim(name)//' in file: '//ncdf)"
    echo "    call ncdf_err(nf90_get_att(ncdf%id, id, name, att), &"
    echo "         'Retrieving from '//trim(var)//' character attribute: '//trim(name)//' in file: '//ncdf)"
    end_routine
    for typ in $types ; do
	if [ $typ == "complex" ]; then
	    continue
	fi
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    for dim in 0 1 ; do # Attributes (only scalar or vector)
		new_routine "$(get_routine_name get_att $typ $prec $dim)" ncdf var name att
		echo "     module procedure $current_routine_name" >> $interface_file
		add_var_declaration -name ncdf -type hNCDF --inout
		add_var_declaration -name var  -char  --in
		add_var_declaration -name name -char --in
		add_var_declaration -name att --$typ --out --dimension $dim --precision $prec
		add_var_declaration -name id  --int
		add_new_line
		# add participation...
		add_node_participate
		add_new_line

		echo "    call ncdf_err(nf90_inq_varid(ncdf%id, trim(var), id), &"
		echo "         'Retrieving id from '//trim(var)//' $typ attribute: '//trim(name)//' in file: '//ncdf)"
		echo "    call ncdf_err(nf90_get_att(ncdf%id, id, name, att), &"
		echo "         'Retrieving from '//trim(var)//' $typ attribute: '//trim(name)//' ${dim}D'//' in file: '//ncdf)"
		end_routine
	    done
	done
    done
}

function create_def_var {
    local tmp="" ; local type=type
    for typ in real complex ; do
	type=type
	new_routine "$(get_routine_name def_var $typ logical)" ncdf name type dims atts compress_lvl fill
	echo "     module procedure $current_routine_name" >> $interface_file
	add_var_declaration -name ncdf         -type hNCDF --inout
	add_var_declaration -name name         -char       --in
	if [ "$typ" == "real" ]; then
	    add_var_declaration -name type     -int        --in
	elif [ "$typ" == "complex" ]; then
	    add_var_declaration -name type     -logical    --in
	fi
	add_var_declaration -name dims         -char       --in --optional --dimension 1
	add_var_declaration -name atts         -type dict  --in --optional
	add_var_declaration -name compress_lvl -int        --in --optional
	add_var_declaration -name fill         -logical    --in --optional
        # Local variables
	add_var_declaration -name id -int
	# Local variables
	if [ "$typ" == "complex" ]; then
	    add_var_declaration -name ltype -int
	    type=ltype
	fi
	add_new_line
	add_node_participate
	add_new_line
	if [ "$typ" == "complex" ]; then
	    echo "    if ( type .eqv. NF90_DOUBLE_COMPLEX ) then"
	    echo "       ltype = NF90_DOUBLE"
	    echo "    else"
	    echo "       ltype = NF90_FLOAT"
	    echo "    end if"
	    add_new_line
	fi
	if [ "$typ" == "complex" ]; then
	    echo "    if ( type .eqv. NF90_DOUBLE_COMPLEX ) then"
	    echo "       ltype = NF90_DOUBLE"
	    echo "    else"
	    echo "       ltype = NF90_FLOAT"
	    echo "    end if"
	    add_new_line
	    echo "    call ncdf_def_var_generic(ncdf, 'Re'//name, ltype, dims, id, atts=atts, compress_lvl=compress_lvl)"
                # If the 3 argument is 0 it will specify the fill value!
	    echo "    if ( present(fill) ) then"
	    echo "       if ( fill ) then"
    # A value of 1 means that it should not fill values in the
	    echo "          call ncdf_err(nf90_def_var_fill(ncdf%id,id, 1, 0), &"
	    echo "              'Setting the variable '//trim(name)//' to NOFILL in file '//ncdf)"
	    echo "       else"
    # Simply initialize to 0 fill...
	    echo "          call ncdf_err(nf90_def_var_fill(ncdf%id,id, 0, 0), &"
	    echo "              'Setting the variable '//trim(name)//' to FILL in file '//ncdf)"
	    echo "       end if"
	    echo "    end if"

	    echo "    call ncdf_def_var_generic(ncdf, 'Im'//name, ltype, dims, id, atts=atts, compress_lvl=compress_lvl)"
	        # If the 3 argument is 0 it will specify the fill value!
	    echo "    if ( present(fill) ) then"
	    echo "       if ( fill ) then"
    # A value of 1 means that it should not fill values in the
	    echo "          call ncdf_err(nf90_def_var_fill(ncdf%id,id, 1, 0), &"
	    echo "              'Setting the variable '//trim(name)//' to NOFILL in file '//ncdf)"
	    echo "       else"
    # Simply initialize to 0 fill...
	    echo "          call ncdf_err(nf90_def_var_fill(ncdf%id,id, 0, 0), &"
	    echo "              'Setting the variable '//trim(name)//' to FILL in file '//ncdf)"
	    echo "       end if"
	    echo "    end if"
	else
	    echo "    call ncdf_def_var_generic(ncdf, name, type, dims, id, atts=atts, compress_lvl=compress_lvl)"
	    # If the 3 argument is 0 it will specify the fill value!
	    echo "    if ( present(fill) ) then"
	    echo "       if ( fill ) then"
    # A value of 1 means that it should not fill values in the
	    echo "          call ncdf_err(nf90_def_var_fill(ncdf%id,id, 1, 0), &"
	    echo "              'Setting the variable '//trim(name)//' to NOFILL in file '//ncdf)"
	    echo "       else"
    # Simply initialize to 0 fill...
	    echo "          call ncdf_err(nf90_def_var_fill(ncdf%id,id, 0, 0), &"
	    echo "              'Setting the variable '//trim(name)//' to FILL in file '//ncdf)"
	    echo "       end if"
	    echo "    end if"
	fi
	end_routine
    done
    return 0

    for typ in logical integer ; do
	for prec in $(get_precisions $typ) ; do
	    # Insert this code in the file...
	    [ "$prec" == "default" ] && prec=""
	    new_routine "$(get_routine_name def_var $typ $prec $dim)" ncdf name type dims atts compress_lvl fill
	    echo "     module procedure $current_routine_name" >> $interface_file
	    add_var_declaration -name ncdf         -type hNCDF --inout
	    add_var_declaration -name name         -char       --in
	    add_var_declaration -name type         -int        --in
	    add_var_declaration -name dims         -char       --in --optional --dimension 1
	    add_var_declaration -name atts         -type dict  --in --optional
	    add_var_declaration -name compress_lvl -int        --in --optional
	    add_var_declaration -name fill         -$typ       --in --optional               --precision $prec
	    # Local variables
	    if [ "$typ" == "complex" ]; then
		add_var_declaration -name ltype -int
	    fi
	    add_var_declaration -name id -int

	    add_new_line
	    add_node_participate
	    add_new_line
	    if [ "$typ" == "complex" ]; then
		echo "    if ( type .eqv. NF90_DOUBLE_COMPLEX ) then"
		echo "       ltype = NF90_DOUBLE"
		echo "    else"
		echo "       ltype = NF90_FLOAT"
		echo "    end if"
		add_new_line
		echo "    call ncdf_def_var_generic(ncdf, 'Re'//name, ltype, dims, id, atts=atts, compress_lvl=compress_lvl)"
                # If the 3 argument is 0 it will specify the fill value!
		echo "    if ( present(fill) ) then"
		echo "       call ncdf_err(nf90_def_var_fill(ncdf%id, id, 0, fill), &"
		echo "          'Setting the fill value on variable Re'//trim(name)//' in file '//ncdf)"
		echo "    end if"
		echo "    call ncdf_def_var_generic(ncdf, 'Im'//name, ltype, dims, id, atts=atts, compress_lvl=compress_lvl)"
	        # If the 3 argument is 0 it will specify the fill value!
		echo "    if ( present(fill) ) then"
		echo "       call ncdf_err(nf90_def_var_fill(ncdf%id, id, 0, fill), &"
		echo "          'Setting the fill value on variable Im'//trim(name)//' in file '//ncdf)"
		echo "    end if"
	    else
		echo "    call ncdf_def_var_generic(ncdf, name, type, dims, id, atts=atts, compress_lvl=compress_lvl)"
	    # If the 3 argument is 0 it will specify the fill value!
		echo "    if ( present(fill) ) then"
		echo "       call ncdf_err(nf90_def_var_fill(ncdf%id, id, 0, fill), &"
		echo "          'Setting the fill value on variable '//trim(name)//' in file '//ncdf)"
		echo "    end if"
	    fi
	    end_routine
	done
    done
}

echo "  interface ncdf_put_var" >> $interface_file
# This we will obsolete... The user has to find other means to retrieve the ID and manipulate...
# At least for now!
#create_put_var id >> $code_file
create_put_var name >> $code_file
echo "  end interface ncdf_put_var" >> $interface_file

echo "  interface ncdf_get_var" >> $interface_file
#create_get_var id >> $code_file
create_get_var name >> $code_file
echo "  end interface ncdf_get_var" >> $interface_file

echo "  interface ncdf_put_gatt" >> $interface_file
create_put_gatt >> $code_file
echo "  end interface ncdf_put_gatt" >> $interface_file

echo "  interface ncdf_get_gatt" >> $interface_file
create_get_gatt >> $code_file
echo "  end interface ncdf_get_gatt" >> $interface_file

echo "  interface ncdf_put_att" >> $interface_file
create_put_att >> $code_file
echo "  end interface ncdf_put_att" >> $interface_file

echo "  interface ncdf_get_att" >> $interface_file
create_get_att >> $code_file
echo "  end interface ncdf_get_att" >> $interface_file

echo "  interface ncdf_def_var" >> $interface_file
create_def_var >> $code_file
echo "  end interface ncdf_def_var" >> $interface_file

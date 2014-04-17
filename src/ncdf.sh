#!/bin/bash

var_dir=../lib/fvar
source $var_dir/settings.sh
[ -e $var_dir/current_settings.sh ] && source $var_dir/current_settings.sh

# Override any special settings in this file
[ -e settings.sh ] && source settings.sh

# The different variable types used in this (long does not exist)
vars=(h s d c z i)

declare -A c_to_r
c_to_r["c"]=s
c_to_r["z"]=d

declare -A has_att
for v in ${vars[@]} ; do
    has_att[$v]=1
done
has_att["h"]=0
has_att["c"]=0
has_att["z"]=0
has_att["l"]=0

# Create the interface files
{
# Variable creation
for sub in put get ; do
_psnl "interface ncdf_${sub}_var"
for v in ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do 
	_psnl "module procedure ${sub}_var_${v}${d}_name"
    done
done
_psnl "end interface ncdf_${sub}_var"
done
# Global/local attribute set
for ssub in put get ; do
for sub in ${ssub}_gatt ${ssub}_att ; do
_psnl "interface ncdf_${sub}"
for v in ${vars[@]} ; do
    [ ${has_att[$v]} -eq 0 ] && continue
    for d in `seq 0 ${N[$v]}` ; do 
	# Attributes does not allow dimensions larger than 1
	[ $d -gt 1 ] && continue
	_psnl "module procedure ${sub}_${v}${d}"
    done
done
_psnl "end interface ncdf_${sub}"
done
done
} > ncdf_interface.inc

{
_psnl "#undef VAR_PREC"
for v in ${vars[@]} ; do
    _psnl "#define VAR_TYPE ${name[$v]}"
    for d in `seq 0 ${N[$v]}` ; do
	if [ $d -eq 0 ]; then
	    _psnl "#define DIMS"
	else
	    _psnl "#define DIMS , dimension$(dim_to_size $d)"
	fi
	_psnl "#define VAR $v$d"
	_psnl "#define DIM $d"
	if [ "$v" == "c" ] || [ "$v" == "z" ]; then
	    _psnl "#define IS_COMPLEX"
	    _psnl "#define REAL_TYPE ${name[${c_to_r[$v]}]}"
	else
	    _psnl "#undef IS_COMPLEX"
	fi
        # Attributes only allowed for dimensions larger than 1
	if [ $d -le 1 ] && [ ${has_att[$v]} -eq 1 ]; then
	    _psnl '#include "ncdf_att_inc.inc"'
	fi
	_psnl '#include "ncdf_var_inc.inc"'
	_psnl "#undef REAL_TYPE"
	_psnl "#undef VAR"
	_psnl "#undef DIM"
	_psnl "#undef DIMS"
    done
    _psnl "#undef VAR_TYPE"
done
} > ncdf_funcs.inc
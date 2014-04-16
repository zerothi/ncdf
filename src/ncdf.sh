#!/bin/bash

var_dir=../lib/fvar
source $var_dir/settings.sh
[ -e $var_dir/current_settings.sh ] && source $var_dir/current_settings.sh

# Override any special settings in this file
source settings.sh

# The different settings used in this
vars=(s d c z i l)

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
for ssub in put set ; do
for sub in ${ssub}_gatt ${ssub}_att ; do
_psnl "interface ncdf_${sub}"
for v in ${vars[@]} ; do
    for d in `seq 0 ${N[$v]}` ; do 
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
	_psnl '#include "ncdf_funcs_inc.inc"'
	_psnl "#undef VAR"
	_psnl "#undef DIM"
	_psnl "#undef DIMS"
	if [ "$v" == "c" ] || [ "$v" == "z" ]; then
	    # it is complex
	    _psnl "#def IS_COMPLEX"
	else
	    _psnl "#undef IS_COMPLEX"
	fi
    done
    _psnl "#undef VAR_TYPE"
done
} > ncdf_funcs.inc
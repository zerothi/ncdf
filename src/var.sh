#!/bin/bash

# Create the input for the var.f90
types="real complex integer logical"

# The type inclusion 
# type ::
type_file="var_type.inc"

# The routine declarations 
# interface <>
#   module procedure <1>
#   module procedure <1>
# end interface
mod_file="var_mods.inc"

# Functions under contains
func_file="var_funcs.inc"

# File for delete
delete_file="var_delete.inc"

# File for nullification
nullify_file="var_nullify.inc"

files="$type_file $mod_file $func_file $delete_file $nullify_file"

# Clean up
rm -f $files

source fortran.sh

function check_size {
    local v=$1
    local data=$2
    local dim=$3
    for i in `seq 1 $dim` ; do
	_ps "size($v,$i) == size($data,$i)"
	if [ $i -lt $dim ]; then
	    _ps " .and. &"
	    _nl
	fi
    done
}


# First we create all the types
{
for typ in $types ; do
    for prec in $(get_precisions $typ) ; do

        # create the type
	add_var_declaration -$typ -precision $prec \
	    -nocheck -pointer -nonewline
	
	print_comma=0
	
	for dim in $(get_dimensions $typ) ; do
	    [ $print_comma -eq 1 ] && _ps ", " >> $type_file
	    _ps "$(get_variable_short $typ $prec)$dim$(var_dimension $dim)=>null()"
	    print_comma=1
	done
	
	_nl

    done
done
# We only allow 1D chars
_ps "character, pointer :: a1(:)" ; _nl
} >> $type_file



# Create the function interface
{
# operators (functions)
# point does not work (overloading => is apparently not viable)
for int in assignment assign associate ; do
    if [ $int == "assignment" ]; then
	_ps "interface assignment(=)" ; _nl
    else
	_ps "interface $int" ; _nl
    fi
    for typ in $types ; do
	for prec in $(get_precisions $typ) ; do
	    
	    for dim in $(get_dimensions $typ) ; do
		_ps "  module procedure ${int}_get_$(get_variable_short $typ $prec)$dim"
		_nl
		_ps "  module procedure ${int}_set_$(get_variable_short $typ $prec)$dim"
		_nl
	    done
	    
	done
    done
    typ=character
    prec=$(get_precisions $typ)
    dim=$(get_dimensions $typ)
    if [ $int == "associate" ]; then
	typ=t
    else
	_ps "  module procedure ${int}_get_$(get_variable_short $typ $prec)$dim"
	_nl
	_ps "  module procedure ${int}_set_$(get_variable_short $typ $prec)$dim"
	_nl
    fi
    _ps 'end interface'
    _nl ; _nl
done
} >> $mod_file


####################### Routines ########################

# We want functions on the basis of
#   ret = associate(lhs,rhs,overwrite=)

 ############ SET ##########
{
for typ in $types ; do
 for prec in $(get_precisions $typ) ; do
  for dim in $(get_dimensions $typ) ; do

      tmp_func() {
	  local i
	  if [ $1 -eq 0 ]; then
	      _ps "allocate(this%$v)" ; _nl
	  else
	      _ps "allocate(this%$v("
	      for i in `seq 1 $1` ; do
		  _ps "size(rhs,$i)"
		  [ $i -lt $1 ] && _ps ","
		  done
	      _ps "))" ; _nl
	  fi
      }

      # Here we create the function for the assignment overload
      int=assignment_set
      {
	  v=$(get_variable_short $typ $prec)$dim
	  sub_name=${int}_$(get_variable_short $typ $prec)$dim
	  _ps "subroutine $sub_name(this,rhs)" ; _nl
	  
          # this variable
	  add_var_declaration -type var -inout -name this
          # assignment variable
	  add_var_declaration -nocheck -$typ -precision $prec \
	      -dimension $dim -in -name rhs
	  
	  # check whether the data is the same
	  _ps "if (.not. (this%t=='$v'"
	  if [ $dim -gt 0 ]; then
	      _ps ".and. &" ; _nl
	      check_size "this%$v" rhs $dim
	  fi
	  _ps ") ) then" ; _nl
          # No matter what we will delete (better handling later)
	  _ps "call delete(this)" ; _nl
	  _ps "this%t = '$v'" ; _nl
	  # Add allocation statement
	  tmp_func $dim
	  _ps "end if" ; _nl

	  _ps "this%$v = rhs" ; _nl
	  
	  _ps "end subroutine $sub_name" ; _nl
      } >> .tmp1

      int=assign_set
      {
	  
	  v=$(get_variable_short $typ $prec)$dim
	  sub_name=${int}_$(get_variable_short $typ $prec)$dim
	  _ps "function $sub_name(this,rhs,overwrite) result(ret)" ; _nl
	  	  
          # this variable
  	  add_var_declaration -type var -inout -name this
          # assignment variable
	  add_var_declaration -nocheck -$typ -precision $prec \
	      -dimension $dim -in -name rhs

	  add_var_declaration -logical -in -optional -name overwrite
	  add_var_declaration -logical -name ret	  

	  # Check the data type
	  _ps "ret = this%t == '$v'" ; _nl
	  if [ $dim -gt 0 ]; then
	      _ps "if (ret) then" ; _nl
	      _ps "ret = "
	      check_size "this%$v" rhs $dim ; _nl
	      _ps "end if" ; _nl
	  fi

          # Check whether we may overwrite the data...
	  _ps "if (present(overwrite)) then" ; _nl
	  _ps "if (overwrite .and. .not. ret) then" ; _nl
	  _ps "call delete(this)" ; _nl # we can safely delete
	  _ps "ret = .true." ; _nl
	  _ps "this%t = '$v'" ; _nl
	  tmp_func $dim
	  _ps "end if" ; _nl
	  _ps "end if" ; _nl
	  _ps "if ( .not. ret ) return" ; _nl
	  
	      # We know know that we may overwrite the data...
	  _ps "this%$v = rhs" ; _nl
	  #_ps "ret = .true." ; _nl
	  
	  _ps "end function $sub_name" ; _nl
      } >> .tmp2
  done
 done
done

# Here we create the function for the assignment overload
typ=character
prec=$(get_precisions $typ)
dim=$(get_dimensions $typ)

int=assignment_set
{
    v=$(get_variable_short $typ $prec)$dim
    sub_name=${int}_$(get_variable_short $typ $prec)$dim
    _ps "subroutine $sub_name(this,rhs)" ; _nl
    
          # this variable
    add_var_declaration -type var -inout -name this
          # assignment variable
    add_var_declaration -nocheck -$typ -precision $prec \
	-in -name rhs
    add_var_declaration -int -name i
    
	  # check whether the data is the same
    _ps "if (.not. (this%t=='$v'.and.len_trim(rhs)==size(this%$v,1)) ) then"
    _nl
          # No matter what we will delete (better handling later)
    _ps "call delete(this)" ; _nl
    _ps "this%t = '$v'" ; _nl
	  # Add allocation statement
    _ps "allocate(this%$v(len_trim(rhs)))" ; _nl
    _ps "end if" ; _nl
    
    _ps "do i = 1 , len_trim(rhs)" ; _nl
    _ps "this%$v(i) = rhs(i:i)" ; _nl
    _ps "end do" ; _nl
    
    _ps "end subroutine $sub_name" ; _nl
} >> .tmp1

int=assign_set
{
    v=$(get_variable_short $typ $prec)$dim
    sub_name=${int}_$(get_variable_short $typ $prec)$dim
    _ps "function $sub_name(this,rhs,overwrite) result(ret)" ; _nl
    
          # this variable
    add_var_declaration -type var -inout -name this
          # assignment variable
    add_var_declaration -nocheck -$typ -precision $prec \
        -in -name rhs
    
    add_var_declaration -logical -in -optional -name overwrite
    add_var_declaration -logical -name ret
    add_var_declaration -int -name i
    
	  # Check the data type
    _ps "ret = this%t == '$v'" ; _nl
    if [ $dim -gt 0 ]; then
	_ps "if (ret) then" ; _nl
	_ps "ret = "
	for i in `seq 1 $dim` ; do
	    _ps "len_trim(rhs)==size(this%$v,$i)" ; _nl
	done
	_ps "end if" ; _nl
    fi

          # Check whether we may overwrite the data...
    _ps "if (present(overwrite)) then" ; _nl
    _ps "if (overwrite .and. .not. ret) then" ; _nl
    _ps "call delete(this)" ; _nl # we can safely delete
    _ps "ret = .true." ; _nl
    _ps "this%t = '$v'" ; _nl
    _ps "allocate(this%$v(len_trim(rhs)))" ; _nl
    _ps "end if" ; _nl
    _ps "end if" ; _nl
    _ps "if ( .not. ret ) return" ; _nl
    
    # We know know that we may overwrite the data...
    _ps "do i = 1 , len_trim(rhs)" ; _nl
    _ps "this%$v(i) = rhs(i:i)" ; _nl
    _ps "end do" ; _nl
    
    _ps "end function $sub_name" ; _nl
} >> .tmp2

cat .tmp1 ; rm .tmp1
cat .tmp2 ; rm .tmp2

# Add character

} >> $func_file

# Create the set functions
int=associate_set
{
    for typ in $types ; do
     for prec in $(get_precisions $typ) ; do
      for dim in $(get_dimensions $typ) ; do
	  v=$(get_variable_short $typ $prec)$dim
	  sub_name=${int}_$(get_variable_short $typ $prec)$dim
	  _ps "function $sub_name(this,rhs) result(ret)" ; _nl
	  
	   # this variable
	  add_var_declaration -type var -inout -name this
	  
	  add_var_declaration -nocheck -$typ -precision $prec \
	      -target -in -dimension $dim -name rhs

	  add_var_declaration -logical -name ret

	  _ps "call nullify(this)" ; _nl
	  _ps "this%t = '$v'" ; _nl
	  _ps "this%$v => rhs" ; _nl

	  _ps "ret = .true." ; _nl
	  
	  _ps "end function $sub_name" ; _nl
      done
     done
    done
} >> $func_file

 ############ SET ##########

 ############ GET ##########

# Create the get
for int in assignment_get assign_get ; do
{
    for typ in $types ; do
     for prec in $(get_precisions $typ) ; do
      for dim in $(get_dimensions $typ) ; do
	  v=$(get_variable_short $typ $prec)$dim
	  sub_name=${int}_$(get_variable_short $typ $prec)$dim
	  if [ $int == "assignment_get" ]; then
	      _ps "subroutine "
	  else
	      _ps "function "
	  fi

           # Add the input
	  _ps "$sub_name(lhs,this)"
	  if [ $int == "assign_get" ]; then
	      _ps " result(ret)" ; _nl
	      add_var_declaration -logical -name ret
	  else
	      _nl
	  fi

	  add_var_declaration -nocheck -$typ -precision $prec \
	      -out -dimension $dim -name lhs

	   # this variable
	  add_var_declaration -type var -in -name this
	  
	  if [ $int == "assign_get" ]; then
	      _ps "ret = this%t/='$v'.or..not.associated(this%$v)" ; _nl
	      _ps "if ( .not. ret ) return" ; _nl
	      for i in `seq 1 $dim` ; do
		  _ps "ret = ret .and. size(this%$v,$i)==size(lhs,$i)" ; _nl
	      done
	      [ $dim -gt 0 ] && _ps "if ( .not. ret ) return" && _nl
	  else
	      _ps "if(this%t/='$v'.or..not.associated(this%$v)) stop 'Type is not consistent'" ; _nl
	  fi
	  
	  _ps "lhs = this%$v" ; _nl
	  
	  if [ $int == "assign_get" ]; then
	      _ps "end function $sub_name" ; _nl
	  else
	      _ps "end subroutine $sub_name" ; _nl
	  fi
      done
     done
    done
} >> $func_file
done

# Here we create the function for the assignment overload
typ=character
prec=$(get_precisions $typ)
dim=$(get_dimensions $typ)

int=assignment_get
{
    v=$(get_variable_short $typ $prec)$dim
    sub_name=${int}_$(get_variable_short $typ $prec)$dim
    _ps "subroutine $sub_name(lhs,this)" ; _nl
    add_var_declaration -nocheck -$typ -precision $prec \
	-out -name lhs

	   # this variable
    add_var_declaration -type var -in -name this
    add_var_declaration -int -name i
	  
    _ps "if(this%t/='$v'.or..not.associated(this%$v)) stop 'Type is not consistent'" ; _nl
    
    _ps "lhs = ' '" ; _nl
    _ps "do i = 1 , size(this%$v,1)" ; _nl
    _ps "lhs(i:i) = this%$v(i)" ; _nl
    _ps "end do" ; _nl

    _ps "end subroutine $sub_name" ; _nl
} >> $func_file

int=assign_get
{
    v=$(get_variable_short $typ $prec)$dim
    sub_name=${int}_$(get_variable_short $typ $prec)$dim
    _ps "function $sub_name(lhs,this) result(ret)" ; _nl
    add_var_declaration -nocheck -$typ -precision $prec \
	-out -name lhs

	   # this variable
    add_var_declaration -type var -in -name this
    add_var_declaration -logical -name ret
    add_var_declaration -int -name i

    _ps "ret = this%t/='$v'.or..not.associated(this%$v)" ; _nl
    _ps "if ( .not. ret ) return" ; _nl
    for i in `seq 1 $dim` ; do
	_ps "ret = ret .and. len(lhs)>=size(this%$v,$i)" ; _nl
    done
    [ $dim -gt 0 ] && _ps "if ( .not. ret ) return" && _nl

    _ps "lhs = ' '" ; _nl
    _ps "do i = 1 , size(this%$v,1)" ; _nl
    _ps "lhs(i:i) = this%$v(i)" ; _nl
    _ps "end do" ; _nl

    _ps "end function $sub_name" ; _nl
} >> $func_file


int=associate_get
{
    for typ in $types ; do
     for prec in $(get_precisions $typ) ; do
      for dim in $(get_dimensions $typ) ; do
	  v=$(get_variable_short $typ $prec)$dim
	  sub_name=${int}_$(get_variable_short $typ $prec)$dim
	  _ps "function $sub_name(lhs,this) result(ret)" ; _nl
	  
	  add_var_declaration -nocheck -$typ -precision $prec \
	      -pointer -dimension $dim -name lhs

	   # this variable
	  add_var_declaration -type var -in -name this

	  add_var_declaration -logical -name ret

	  # Check the data type
	  _ps "ret = this%t == '$v'" ; _nl

          # Check whether we may overwrite the data...
	  _ps "if ( .not. ret ) return" ; _nl

	  _ps "lhs => this%$v" ; _nl

	  _ps "ret = .true." ; _nl

	  _ps "end function $sub_name" ; _nl
      done
     done
    done
} >> $func_file

########## GET #######


# Create the delete function
int=delete
{
    used=0
    for typ in $types ; do
     for prec in $(get_precisions $typ) ; do
      for dim in $(get_dimensions $typ) ; do
	  v=$(get_variable_short $typ $prec)$dim
	  if [ $used -eq 0 ]; then
	      used=1
	  else
	      _ps "else "
	  fi
	  _ps "if(this%t=='$v'.and.associated(this%$v))then" ; _nl
	  _ps "deallocate(this%$v)" ; _nl ; _ps "nullify(this%$v)" ; _nl
      done
     done
    done
    _ps "end if" ; _nl
} >> $delete_file

# Create the nullify function
int=nullify
{
    for typ in $types ; do
     for prec in $(get_precisions $typ) ; do
      for dim in $(get_dimensions $typ) ; do
	  v=$(get_variable_short $typ $prec)$dim
	  _ps "nullify(this%$v)" ; _nl
      done
     done
    done
    _ps "this%t='  '" ; _nl
} >> $nullify_file


#################### We create all comparisons operators ###############

# Notice that
# /= '==' .not. ==
# <= '==' .not. >
# >= '==' .not. <
declare -A opName
opName["=="]=eq
opName["=="]=eq
opName[">"]=gt
opName[">="]=ge
opName["<"]=lt
opName["<="]=le
for op in "==" ">" "<" ">=" "<=" ; do
{
 int=${opName[$op]}
 for typ in ${types//complex/} ; do
  for prec in $(get_precisions $typ) ; do
   for dim in $(get_dimensions $typ) ; do
       v=$(get_variable_short $typ $prec)$dim
       sub_name=${int}_l_$(get_variable_short $typ $prec)$dim
       
       _ps "function $sub_name(this,rhs) result(ret)" ; _nl
       add_var_declaration -type var -in -name this
       add_var_declaration -nocheck -$typ -precision $prec \
	   -in -dimension $dim -name rhs
       add_var_declaration -logical -name ret

       _ps "ret = this%t=='$v'" ; _nl
       _ps "if (.not. ret) return" ; _nl
       
       if [ $dim -gt 0 ]; then
	   _ps "ret = all(this%$v $op rhs)" ; _nl
       else
	   _ps "ret = this%$v $op rhs" ; _nl
       fi
       _ps "end function $sub_name" ; _nl

       sub_name=${int}_r_$(get_variable_short $typ $prec)$dim

       _ps "function $sub_name(lhs,this) result(ret)" ; _nl
       add_var_declaration -nocheck -$typ -precision $prec \
	   -in -dimension $dim -name lhs
       add_var_declaration -type var -in -name this
       add_var_declaration -logical -name ret

       _ps "ret = this%t=='$v'" ; _nl
       _ps "if (.not. ret) return" ; _nl

       if [ $dim -gt 0 ]; then
	   _ps "ret = all(lhs $op this%$v)" ; _nl
       else
	   _ps "ret = lhs $op this%$v" ; _nl
       fi

       _ps "end function $sub_name" ; _nl

   done
  done
 done
} >> $func_file
done
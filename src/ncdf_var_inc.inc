#include "settings.inc"
#if DIM > 0
#define __NC_COUNT ,count=count
subroutine ROUTINE3(put_var,VAR,name)(this,name,var,start,count)
  integer, intent(in), optional :: count(:)
#else
#define __NC_COUNT  
subroutine ROUTINE3(put_var,VAR,name)(this,name,var,start)
#endif
  type(hNCDF), intent(inout) :: this
  character(len=*), intent(in) :: name
  VAR_TYPE, intent(in)DIMS :: var
  integer, intent(in), optional :: start(:)
  integer :: id
#ifdef IS_COMPLEX
  REAL_TYPE, allocatable DIMS :: r
#endif
  if ( .not. ncdf_participate(this) ) return
  call ncdf_enddef(this)
#ifdef IS_COMPLEX
  ALLOC(r,var)
  r = real(var)
  call ncdf_inq_var(this,'Re'//name,id=id)
  call ncdf_err(nf90_put_var(this%id, id, r,start=start __NC_COUNT), &
     "Saving variable (VAR) Re"//trim(name)//' in file: '//this)
  call ncdf_inq_var(this,'Im'//name,id=id)
  r = aimag(var)
  call ncdf_err(nf90_put_var(this%id, id, r,start=start __NC_COUNT), &
       'Saving variable (VAR) Im'//trim(name)//' in file: '//this)
  deallocate(r)
#else
  call ncdf_inq_var(this,name,id=id)
  call ncdf_err(nf90_put_var(this%id, id, var,start=start __NC_COUNT), &
       'Saving variable (VAR) '//trim(name)//' in file: '//this)
#endif
end subroutine ROUTINE3(put_var,VAR,name)

subroutine ROUTINE3(get_var,VAR,name)(this, name, var, start, count, stride)
  type(hNCDF), intent(inout) :: this
  character(len=*), intent(in) :: name
  VAR_TYPE, intent(out)DIMS :: var
  integer, intent(in), optional :: start(:)
  integer, intent(in), optional :: count(:)
  integer, intent(in), optional :: stride(:)
  integer :: id
#ifdef IS_COMPLEX
  REAL_TYPE, allocatable DIMS :: r,i
#endif
#ifdef DIM == 0
  VAR_TYPE :: v(1)
  call ncdf_get_var(this,name,v,start=start,count=count,stride=stride)
  var = v(1)
#else
  if ( .not. ncdf_participate(this) ) return
  call ncdf_enddef(this)
#ifdef IS_COMPLEX
  ALLOC(r,var)
  ALLOC(i,var)
  call ncdf_inq_var(this,'Re'//name,id=id)
  call ncdf_err(nf90_get_var(this%id, id, r,start=start,count=count,stride=stride), &
       'Retrieving variable (VAR) Re'//trim(name)//' in file: '//this)
  call ncdf_inq_var(this,'Im'//name,id=id)
  call ncdf_err(nf90_get_var(this%id, id, i,start=start,count=count,stride=stride), &
       'Retrieving variable (VAR) Im'//trim(name)//' in file: '//this)
  var = cmplx(r,i)
  deallocate(r,i)
#else
  call ncdf_inq_var(this,name,id=id)
  call ncdf_err(nf90_get_var(this%id, id, var,start=start,count=count,stride=stride), &
       'Retrieving (VAR) variable, '//trim(name)//' in file: '//this)
#endif
#endif
end subroutine ROUTINE3(get_var,VAR,name)

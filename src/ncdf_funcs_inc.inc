#include "settings.inc"
subroutine ROUTINE3(put_var,VAR,name)(this,name,var,start)
  type(hNCDF), intent(inout) :: this
  character(len=*), intent(in) :: name
  VAR_TYPE, intent(in)DIMS :: var
  integer, intent(in), optional :: start(:)
  integer :: id
  if ( .not. ncdf_participate(this) ) return
  call ncdf_enddef(this)
#ifdef IS_COMPLEX
  ! we need TODO something here
  call ncdf_inq_var(this,'Re'//name,id=id)
  call ncdf_err(nf90_put_var(this%id, id, var,start=start), &
       'Saving variable (VAR) Re'//trim(name)//' in file: '//this)
  call ncdf_inq_var(this,'Im'//name,id=id)
  call ncdf_err(nf90_put_var(this%id, id, var,start=start), &
       'Saving variable (VAR) Im'//trim(name)//' in file: '//this)
#else
  call ncdf_inq_var(this,name,id=id)
  call ncdf_err(nf90_put_var(this%id, id, var,start=start), &
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
  if ( .not. ncdf_participate(this) ) return
  call ncdf_enddef(this)
#ifdef IS_COMPLEX
  ! we need TODO something here
  call ncdf_inq_var(this,'Re'//name,id=id)
  call ncdf_err(nf90_get_var(this%id, id, var,start=start,count=count,stride=stride), &
       'Saving variable (VAR) Re'//trim(name)//' in file: '//this)
  call ncdf_inq_var(this,'Im'//name,id=id)
  call ncdf_err(nf90_get_var(this%id, id, var,start=start,count=count,stride=stride), &
       'Saving variable (VAR) Im'//trim(name)//' in file: '//this)
#else
  call ncdf_inq_var(this,name,id=id)
  call ncdf_err(nf90_get_var(this%id, id, var,start=start,count=count,stride=stride), &
       'Retrieving (VAR) variable, '//trim(name)//' in file: '//this)
#endif
end subroutine ROUTINE3(get_var,VAR,name)

subroutine ROUTINE2(put_gatt,VAR)(this, name, att)
  type(hNCDF), intent(inout) :: this
  character(len=*), intent(in) :: name
  VAR_TYPE, intent(in)DIMS :: att
  if ( .not. ncdf_participate(this) ) return
  call ncdf_redef(this)
  call ncdf_err(nf90_put_att(this%id, NF90_GLOBAL, name, att), &
       'Saving global (VAR) attribute: '//trim(name)//' in file: '//this)
end subroutine ROUTINE2(put_gatt,VAR)

subroutine ROUTINE2(put_att,VAR)(this, var, name, att)
  type(hNCDF), intent(inout) :: this
  character(len=*), intent(in) :: var, name
  VAR_TYPE, intent(in)DIMS :: att
  integer :: id
  if ( .not. ncdf_participate(this) ) return
  call ncdf_redef(this)
  call ncdf_err(nf90_inq_varid(this%id, trim(var), id), &
              'Retrieving id from (VAR) '//trim(var)//' : '//trim(name)//' in file: '//this)
  call ncdf_err(nf90_put_att(this%id, id, name, att), &
       'Saving (VAR) '//trim(var)//' attribute: '//trim(name)//' in file: '//this)
end subroutine ROUTINE2(put_att,VAR)

subroutine ROUTINE2(get_att,VAR)(this, var, name, att)
  type(hNCDF), intent(inout) :: this
  character(len=*), intent(in) :: var, name
  VAR_TYPE, intent(out)DIMS :: att
  integer :: id
  if ( .not. ncdf_participate(this) ) return
  call ncdf_err(nf90_inq_varid(this%id, trim(var), id), &
              'Retrieving id from (VAR) '//trim(var)//' : '//trim(name)//' in file: '//this)
  call ncdf_err(nf90_get_att(this%id, id, name, att), &
       'Retrieving (VAR) '//trim(var)//' attribute: '//trim(name)//' in file: '//this)
end subroutine ROUTINE2(get_att,VAR)

subroutine ROUTINE2(get_gatt,VAR)(this, name, att)
  type(hNCDF), intent(inout) :: this
  character(len=*), intent(in) :: name
  VAR_TYPE, intent(out)DIMS :: att
  integer :: id
  if ( .not. ncdf_participate(this) ) return
  call ncdf_err(nf90_get_att(this%id, NF90_GLOBAL, name, att), &
       'Saving global (VAR) attribute: '//trim(name)//' in file: '//this)
end subroutine ROUTINE2(get_gatt,VAR)

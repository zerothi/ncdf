! Generic purpose variable as in any scripting language
! It has the power to transform into any variable at any time
! This means
module variable

  implicit none

  private 

  integer, parameter :: is = selected_int_kind(5)
  integer, parameter :: il = selected_int_kind(16)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)
  
  type :: var
     character(len=2) :: t = '  '
     include 'var_type.inc'
  end type var
  public :: var

!  public :: size
  public :: which
  public :: delete
  public :: nullify

  include 'var_mods.inc'
  public :: assign, associate

contains

  function which(this) result(t)
    type(var), intent(in) :: this
    character(len=2) :: t
    t = this%t
  end function which
    
  subroutine delete(this)
    type(var), intent(inout) :: this
    include 'var_delete.inc'
    this%t = '  '
  end subroutine delete

  subroutine nullify(this)
    type(var), intent(inout) :: this
    include 'var_nullify.inc'
    this%t = '  '
  end subroutine nullify

  include 'var_funcs.inc'

end module variable

#ifdef TEST
program test
  use variable

  type(var) :: va , vb
  real(dp) :: a, b(2), c(2,2)
  real(dp), pointer :: pa =>null(), pb(:)=>null(), pc(:,:)=>null()
  character(len=20) :: ca, cb
  logical :: success

  a = 1.0_dp
  b = 2._dp
  c = 3._dp

  call assign(va,a)
  print *,va%t,va%d0
  call associate(pa,va,success=success)
  if ( success ) then
     print *,'Success: ',pa
  end if

  call assign(va,b)
  call assign(b,va)
  print *,va%t,va%d1

  call assign(va,c)
  print *,va%t,va%d2

  call assign(va,1)
  print *,va%t,va%i0


  ca = 'hello world'
  call assign(va,ca)

  call assign(cb,va)
  print *,cb
  

end program test
#endif

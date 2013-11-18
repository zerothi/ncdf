! A simple dictionary module for a key-value based system...
! 
! This module has entirely been created by:
! Nick Papior Andersen, copyright 2012.
! nickpapior@gmail.com
!
! Only to be used for not-for-profit development/applications.
module dictionary

  use variable

  implicit none

  private

  integer, parameter :: is = selected_int_kind(5)
  integer, parameter :: il = selected_int_kind(16)
  integer, parameter :: sp = selected_real_kind(p=6)
  integer, parameter :: dp = selected_real_kind(p=15)


  public :: dict
  ! Create a dict type: 'key' .KV. 'val', 'key' .KP. 'pointer'
  public :: operator(.KV.), operator(.KP.)
  ! Retrieve the value of a list of dicts by searching for a key: type(dict)(:) .LU. 'a' (returns .VAL. from 'a'.KV.'val')
  ! 'Look-Up'
  public :: operator( .LU. ), operator( .LOOKUP. )
  ! Concatenates lists of dicts or just two dicts: type(dict)(:) //('a'.KV.'val')
  public :: operator( // )
  ! Retrieve the key of a dict: .KEY. type(dict)
  public :: operator(.KEY.)
  ! Retrieve the value of a dict: .VAL. type(dict)
  public :: operator(.VAL.)
  ! Compares two dict's
  public :: operator(.EQ.) ! Overloaded
  ! Compares two dict's and returns the negativ of .eq.
  public :: operator(.NE.) ! Overloaded
  ! Checks whether a character(*) or dict resides in a list of dicts
  public :: operator(.IN.)
  ! Checks whether a character(*) or dict does not reside in a list of dicts
  public :: operator(.NIN.)
  ! The unary operator of retrieving the next element
  public :: operator(.NEXT.)
  ! The unary operator of retrieving the next element
  public :: operator(.FIRST.)
  ! The unary operator of checkning for emptyness
  public :: operator(.EMPTY.)
  ! The unary operator of returning the hash value
  public :: operator(.HASH.), hash
  ! Return the length of the dictionary...
  public :: LEN
  public :: dict_copy, dict_print
  

  ! Internal variables for determining the maximum size of the dictionaries.
  ! We could consider changing this to a variable size string (m_string module)
  ! However, that will increase the dependencies and will most likely not yield
  ! a better interface.
  integer, parameter :: DICT_KEY_LENGTH   = 50
  
  ! A parameter returned if not found.
  character(len=DICT_KEY_LENGTH), parameter :: DICT_NOT_FOUND = 'ERROR: key not found'
  public :: DICT_NOT_FOUND

  ! We need to create a linked list to create arbitrarily long dictionaries...
  ! The dictionary entry is not visible outside.
  type :: d_entry
     private
     character(len=DICT_KEY_LENGTH)   :: key   = ' '
     type(var) :: value
     integer :: hash = 0
     type(d_entry), pointer :: next => null()
  end type d_entry
  
  type :: dict
     ! We will keep the dictionary private so that any coding
     ! has to use .KEY. and .VAL. etc.
     private
     type(d_entry), pointer :: first => null()
     integer :: len = 0
  end type dict
  
  ! If we ever need to extend it to deal with comparisons
  ! HASH-comparisons are MUCH faster...
  integer, parameter :: HASH_SIZE = 9857 ! a prime !
  integer, parameter :: HASH_MULT = 31
  
  ! Create a dictionary type from 
  include 'dictionary_interface.inc'

  ! Retrieve the value from a dictionary list by specifying the key...
  interface operator( .LU. )
     module procedure d_value_from_key
  end interface operator( .LU. )
  interface operator( .LOOKUP. )
     module procedure d_value_from_key
  end interface operator( .LOOKUP. )


  ! Concatenate dicts or list of dicts to list of dicts
  interface operator( // )
     module procedure d_cat_d
     module procedure ds_cat_d
     module procedure d_cat_ds
  end interface operator( // )

  
  ! Retrieve the key from a dictionary (unary)
  interface operator( .KEY. )
     module procedure key
  end interface operator( .KEY. )
  
  ! Retrieve the value from a dictionary (unary)
  interface operator( .VAL. )
     module procedure value
  end interface operator( .VAL. )

  ! Retrieve the hash value from a dictionary (unary)
  interface operator( .HASH. )
     module procedure hash
  end interface operator( .HASH. )

  ! Checks for two dicts to be equal (in both key and value)
  interface operator( .EQ. )
     module procedure d_eq_d
  end interface operator( .EQ. )

  ! Checks for two dicts to be not equal (in both key and value)
  interface operator( .NE. )
     module procedure d_ne_d
  end interface operator( .NE. )

  ! Checks for a dict to be in a dict list (uses .EQ. so in both key and value)
  interface operator( .IN. )
     module procedure char_in_d
  end interface operator( .IN. )

  ! Checks for a dict to not be in a dict list (uses .EQ. so not in both key and value)
  interface operator( .NIN. )
     module procedure char_nin_d
  end interface operator( .NIN. )

  ! Steps one time in the dictionary
  interface operator( .NEXT. )
     module procedure d_next
  end interface operator( .NEXT. )

  ! Retrieve the first of a dictionary
  interface operator( .FIRST. )
     module procedure d_first
  end interface operator( .FIRST. )

  ! Check whether the dictionary is empty...
  interface operator( .EMPTY. )
     module procedure d_empty
  end interface operator( .EMPTY. )

contains

  integer pure function hash_val(key)
    character(len=*), intent(in) :: key
    integer :: i
    hash_val = 0
    do i = 1 , min(DICT_KEY_LENGTH,len_trim(key))
       hash_val = hash_val + iachar(key(i:i))
    end do
    ! A hash has to be distinguished from the "empty"
    hash_val = 1 + mod(hash_val*HASH_MULT,HASH_SIZE)
  end function hash_val

  type(dict) pure function new_d_key(key)
    character(len=*), intent(in) :: key
    allocate(new_d_key%first)
    if ( len_trim(key) > DICT_KEY_LENGTH ) then
       new_d_key%first%key = key(1:DICT_KEY_LENGTH)
    else
       new_d_key%first%key = trim(key)
    end if
    new_d_key%first%hash = hash_val(key)
    new_d_key%len = 1
    nullify(new_d_key%first%next)
  end function new_d_key

  ! Retrieves the key value in a dictionary type (or a list)
  ! We expect that the key will only be called on single element dictionaries...
  character(len=DICT_KEY_LENGTH) function key(d)
    type(dict), intent(in) :: d
    key = d%first%key
  end function key

  ! Retrieves the value value in a dictionary type (or a list)
  type(var) function value(d)
    type(dict), intent(in) :: d
    value = d%first%value
  end function value

  ! Returns the hash value of the dictionary first item...
  integer function hash(d)
    type(dict), intent(in) :: d
    hash = d%first%hash
  end function hash

  ! Retrieves the value for a key in a list of dictionaries
  ! Will use .EQ. to tesh for their equivalence.
  type(var) function d_value_from_key(d,key)
    type(dict), intent(in) :: d
    character(len=*), intent(in) :: key
    type(dict) :: ld
    integer :: hash
    call delete(d_value_from_key)
    hash = hash_val(key)
    ld = .first. d
    search: do
       if ( .empty. ld ) exit search
       if (      hash  < .hash. ld ) then
          exit search
       else if ( hash  > .hash. ld ) then
          ! Do nothing... step
       else if ( hash == .hash. ld ) then
          if ( key .eq. .KEY. ld ) then
             d_value_from_key = .VAL. ld
             return
          end if
       end if
       ld = .next. ld
    end do search
  end function d_value_from_key

  ! Compares two dict types against each other
  ! Will do comparison, first by hash, and if that matches then
  ! for the key and value of the dict's
  logical function d_eq_d(d1,d2)
    type(dict), intent(in) :: d1,d2
    d_eq_d = (.hash. d1 == .hash. d2)
    if ( d_eq_d ) then
       d_eq_d = ( trim(.KEY. d1) .eq. trim(.KEY. d2) )! .and. &
       !( trim(.VAL. d1) .eq. trim(.VAL. d2) )
    end if
  end function d_eq_d

  ! Compares two dict types against each other
  ! The negative of .EQ.
  logical function d_ne_d(d1,d2)
    type(dict), intent(in) :: d1,d2
    d_ne_d = .not. (d1 .eq. d2)
  end function d_ne_d

  ! Compares a dict against a dict list.
  ! Will use .EQ. to tesh for their equivalence.
  ! Returns true in the case of 'd' is in 'ds'
  logical function char_in_d(char,d)
    character(len=*), intent(in) :: char
    type(dict), intent(in) :: d
    type(dict) :: ld
    integer :: hash
    hash = hash_val(char)
    ld = .first. d
    search: do
       if ( .empty. ld ) exit search
       if (      hash  < .hash. ld ) then
          exit search
       else if ( hash  > .hash. ld ) then
          ! Do nothing (quick step to next search)
       else if ( hash == .hash. ld ) then
          if ( char == (.key. ld) ) then
             char_in_d = .true.
             return
          end if
       end if
       ld = .next. ld
    end do search
    char_in_d = .false.
  end function char_in_d

  ! Compares a dict against a dict list.
  ! Will use .EQ. to test for their equivalence.
  ! Returns true in the case of 'd' is NOT in 'ds'
  logical function char_nin_d(char,d)
    character(len=*), intent(in) :: char
    type(dict), intent(in) :: d
    integer :: i
    char_nin_d = .not. (char.in.d)
  end function char_nin_d

  ! Concatenate two dictionaries to one dictionary...
  function d_cat_d(d1,d2)
    type(dict), intent(in) :: d1,d2
    type(dict) :: d_cat_d
    type(d_entry), pointer :: ladd,lnext
    call dict_copy(d1,d_cat_d)
    ladd => d2%first
    do 
       ! step ...
       lnext => ladd%next
       call d_insert(d_cat_d,ladd)
       if ( .not. associated(lnext) ) return
       ladd => lnext
    end do
  end function d_cat_d

  ! Concatenate a dictinoray and a list of dictionaries to one dictionary
  function d_cat_ds(d,ds)
    type(dict), intent(in) :: d,ds(:)
    type(dict) :: d_cat_ds
    integer :: i
    call dict_copy(d,d_cat_ds)
    do i = 1 , size(ds)
       ! Concatenate...
       d_cat_ds = d_cat_ds//ds(i)
    end do
  end function d_cat_ds
  function ds_cat_d(ds,d)
    type(dict), intent(in) :: ds(:),d
    type(dict) :: ds_cat_d
    integer :: i
    call dict_copy(d,ds_cat_d)
    do i = 1 , size(ds)
       ! The order does not matter...
       ds_cat_d = ds(i)//ds_cat_d
    end do
  end function ds_cat_d

  subroutine d_insert(d,entry)
    type(dict),    intent(inout) :: d
    type(d_entry), intent(inout), pointer :: entry
    type(d_entry), pointer :: search, prev
    nullify(prev)
    ! Initialize search...
    search => d%first
    ! The easy case...
    if ( search%hash > entry%hash ) then
       entry%next => d%first
       d%first => entry
       d%len = d%len + 1
       return
    else if ( search%hash == entry%hash ) then
       ! If the key already exists we will simply overwrite
       if ( search%key == entry%key ) then
          search%value = entry%value
          return
       end if
    end if
    search_loop: do 
       ! step...
       prev   => search
       ! step...
       search => prev%next
       if ( .not. associated(search) ) exit search_loop
       if ( search%hash > entry%hash ) then
          prev%next  => entry
          entry%next => search
          d%len = d%len + 1
          return
       else if ( search%hash == entry%hash ) then
          ! If the key already exists we will simply overwrite
          if ( search%key == entry%key ) then
             search%value = entry%value
             return
          end if
       end if
    end do search_loop
    prev%next => entry
    ! Increment length of the dictionary...
    d%len = d%len+1
    ! As we could insert from a dictionary we have to reset, to not do endless loops...
    nullify(entry%next)
  end subroutine d_insert

  ! Retrieve the length of the dictionary...
  pure function len(d)
    type(dict), intent(in) :: d
    integer :: len
    len = d%len
  end function len

  function d_next(d)
    type(dict), intent(in) :: d
    type(dict) :: d_next
    d_next%first => d%first%next
    d_next%len = len(d)-1
  end function d_next

  function d_empty(d)
    type(dict), intent(in) :: d
    logical :: d_empty
    d_empty = .not. associated(d%first)
  end function d_empty

  function d_first(d)
    type(dict), intent(in) :: d
    type(dict) :: d_first
    call dict_copy(d,d_first)
  end function d_first
  
  subroutine dict_copy(din,dcopy)
    type(dict), intent(in)  :: din
    type(dict), intent(out) :: dcopy
    dcopy%first => din%first
    dcopy%len = din%len
  end subroutine dict_copy

  subroutine dict_print(d)
    type(dict), intent(in)  :: d
    type(dict)  :: ld
    ld = .first. d
    d_loop: do 
       if ( .empty. ld ) exit d_loop
       write(*,*) trim(.key. ld) !,' : ',trim(.val. ld)
       ld = .next. ld
    end do d_loop
  end subroutine dict_print

  include 'dictionary_funcs.inc'

end module dictionary

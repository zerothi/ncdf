! This module has been fully created by:
! Nick Papior Andersen, nickpapior@gmail.com
! It is to be considered copyrighted and ONLY to be used for scientific purposes.

! Every dimension ID, variable ID and NetCDF file ID will now be tracked by
! this module. This will let you track all variables by name.
! Furthermore, the module will be developed to handle parallel writes using
! the MPI-communication layer without the user "knowing". One simply opens/creates
! the NetCDF file in parallel mode.

!
! Furthermore the inquiries, and returning variables has been abstracted to their
! type and size. Also all references should be done with names of variables, dimensions
! etc.
! Example. You have created a NetCDF file with the following variable:
!   Energy(MD): double
! If you need to read the variables content in 10:13 you will read by:
!   ncdf_get_var(ncdf,"Energy",E(1:4),start=(/10/))
!
! A specialized ncdf_[put|get]_var has been developed for complex variables.
! In ordinary NetCDF contents one has to define two variables, do a conversion
! for real and imaginary parts, etc.
! In this case we have created:
!   ncdf_get_var(ncdf,"ComplexEnergy",CE(1:4),start=(/10/))
! to read in the variables:
!     "ReComplexEnergy" and "ImComplexEnergy".
! This heavily relieves the burden of tracking content and imaginary double IDs for the 
! NetCDF interface.
! 
! NOTE: this limits the fetch of a COMPLEX variable, automatically converted to an
!       integer. The programmer needs to do this by other means.
!
! The routines for variables/attributes/dimensions are the following.
! Here an asterisk (*) denotes an optional argument. ALWAYS refer to
! optional arguments by keywords. ALWAYS.
!  - ncdf_inq_var
!     (ncdf,name,*exist,*id,*size)
!  - ncdf_get_var
!     (ncdf,name,*exist,*start,*count,*stride)
!  - ncdf_put_var
!     (ncdf,name,var,*start,*count,*stride)
!  - ncdf_inq_dim
!     (ncdf,name,*exist,*id,*len)
!  - ncdf_inq_att
!     (ncdf,var,name,*exist)
!  - ncdf_get_att
!     (ncdf,var,name,att<int,real,char>)
!  - ncdf_inq_gatt
!     (ncdf,name,*exist)
!  - ncdf_get_gatt
!     (ncdf,var,name,att<int,real,char>)
!  - ncdf_def_var
!     (ncdf,var,type,dims, 
!               *atts=(dictionary),*compress_lvl,*fill) (the fill variable is not implemented in NetCDF for now)

! The parallel access scheme is governed in this order:
!  a) open/create files with a wire
!     1. The wires IO_COMM and IO_PAR is used to determine which processors that
!        should be co-operating with the NetCDF file
!     2. TODO : implement an automatic send/recv with those not in the IO_PAR group...
!  b) open/create files with the optional "comm" flag.
!     1. The specified communicator is used to do IO-operations
!     2. 
!  c) open/create files with optional "parallel" flag in the routines
!     mean that the file are opened on each processor in NF90_SHARE mode.
!     1. This will make reading independent on each other, but still allow for some 
!        parallelization.



! A wrapper module for doing netcdf operations
! The idea is that this module should be able to do parallel IO when needed
! Currently it does not have this implemented, but it provides a wrapper basis
! which makes it easy.
module nf_ncdf

  ! Globalize the variables needed for generating the requested features
  use netcdf

  implicit none

  public

  ! Privatize used variables
  integer, private, parameter :: ih = selected_int_kind(4)
  integer, private, parameter :: is = selected_int_kind(9)
  integer, private, parameter :: il = selected_int_kind(18)
  integer, private, parameter :: sp = selected_real_kind(p=6)
  integer, private, parameter :: dp = selected_real_kind(p=15)

  ! The IONode setting
  logical, save :: IONode = .false.
  private :: IONode

  ! Local routines
  private :: ncdf_def_var_generic
  private :: cat_char_ncdf
  private :: cat_ncdf_char
  private :: ncdf_die

  ! We add a specific NetCDF handle for dealing with files
  type :: hNCDF
     ! The file-handle for the netCDF-file
     integer            :: id = -1
     ! Whether the file is handled parallely
     logical            :: parallel = .false.
     ! The mode of the file
     integer            :: mode
     ! If define < 0, then no enddef, or redefs will be performed
     ! If define == 0 then it is in define mode (needed for netCDF-3)
     ! If define == 1 then it is in data   mode (needed for netCDF-3)
     integer            :: define
     ! The name of the netCDF-file
     character(len=250) :: name = " "
     ! the group of the netCDF-file (i.e. a file within a file)
     character(len=NF90_MAX_NAME) :: grp = " "
     ! The communicator describing the parallel activity
     integer            :: comm = - 1
     ! Default compression level
     integer            :: comp_lvl = 0
  end type hNCDF

  ! Interface the concatenation
  interface operator (//)
     module procedure cat_char_ncdf
     module procedure cat_ncdf_char
  end interface operator (//)


  ! Interface for creating variables
  interface ncdf_def_var
     module procedure ncdf_def_var_integer
     module procedure ncdf_def_var_logical
  end interface ncdf_def_var
  private :: ncdf_def_var_integer
  private :: ncdf_def_var_logical


  ! Interface for acquiring information about a file...
  interface ncdf_inq
     module procedure ncdf_inq_ncdf
     module procedure ncdf_inq_name
  end interface ncdf_inq
  private :: ncdf_inq_name
  private :: ncdf_inq_ncdf


! Add new data types
! We need them to be logical due to the interface of the def_var.
! Otherwise they would have the same interface (due to the optional argument var_id)
  logical, parameter :: NF90_DOUBLE_COMPLEX = .true.  ! for true it is double
  logical, parameter :: NF90_FLOAT_COMPLEX  = .false. ! for false it is float

  ! This variable is used to enable the NOFILL on certain variables (for the interface to ncdf_def_var)
  logical, parameter :: NF90_VAR_NOFILL = .true.      ! for false it is float
  ! This variable is used to re-enable the FILL on certain variables (for the interface to ncdf_def_var)
  logical, parameter :: NF90_VAR_FILL   = .false.      ! for false it is float

  ! Added interface
#include "ncdf_interface.inc"

contains

! Every routine in this module needs NetCDF
! So it is sourrounded by this...

  subroutine ncdf_copy(ncdf,copy)
    type(hNCDF), intent(in) :: ncdf
    type(hNCDF), intent(out) :: copy
    copy = ncdf
  end subroutine ncdf_copy

  subroutine ncdf_init(ncdf,name,mode,parallel,comm,overwrite,compress_lvl)
#ifdef NCDF_PARALLEL
    use mpi
#endif
    type(hNCDF),       intent(inout)  :: ncdf
    character(len=*), optional, intent(in) :: name
    integer, optional, intent(in)     :: mode
    logical, optional, intent(in)     :: parallel
    integer, optional, intent(in)     :: comm
    logical, optional, intent(in)     :: overwrite
    integer, optional, intent(in)     :: compress_lvl
    integer :: format
    logical :: exist
#ifdef NCDF_PARALLEL
    integer :: MPIerror
#endif

    ncdf%id       = -1
    ncdf%parallel = .false.
    ncdf%define   = 0
    ncdf%mode     = 0
    ncdf%name     = " "
    if ( present(name) ) ncdf%name = name
    ncdf%grp      = " "
    ncdf%comp_lvl = 0
    if ( present(compress_lvl) ) ncdf%comp_lvl = compress_lvl

    if ( .not. present(mode) ) then
       ! Our default is the 64 bit offset files... 
       ! The best backwards compatibility format
       ncdf%mode = IOR(ncdf%mode,NF90_64BIT_OFFSET)
    end if


    ! If the parallel interface is not applied we need the communicator 
    ! to be negative
    ncdf%comm = -1 

    ! This will create the correct order
    if ( present(parallel) ) then
       ncdf%parallel = parallel
       ! The parallel flag is for the sequential parallel access !
       ! This will be reset to a zero mode if a communicator is supplied
       ! In this way we can have "parallel" access for reading purposes...
       if ( parallel ) then
          ! Check that the mode is not existing in the passed mode
          if ( present(mode) ) then
             if ( iand(NF90_SHARE,mode) /= NF90_SHARE ) &
                  ncdf%mode = NF90_SHARE
          else
             ncdf%mode = NF90_SHARE
          end if
       end if
    end if

    if ( present(comm) ) then
       if ( comm < 0 ) then
          ! If the communicator is negative we
          ! must assume that it is not parallel
          ncdf%comm = comm
#ifdef NCDF_PARALLEL
       else
          ! If the communicator is present, 
          ! so must the parallel execution...
          ncdf%parallel = .true.
          ncdf%comm = comm
          ! We cannot ask for no parallel access and supply a communicator (makes no sense)
          if ( present(parallel) ) then
             if ( .not. parallel ) then
                call ncdf_die("You cannot supply a communicator and request "//&
                     "NO parallel access. Please correct.")
             end if
          end if
          ! If a communicator is supplied we need to "delete" the NF90_SHARE we sat 
          ! "possibly" above
          ncdf%mode = 0
#endif
       end if
    end if
    
#ifdef NCDF_PARALLEL
    ! We need only do this if the file exists
    inquire(file=""//ncdf,exist=exist)
    if ( present(overwrite) .and. exist ) then
       exist = .not. overwrite
    end if
    if ( ncdf%parallel .and. exist ) then
       ! In case the implementation does not allow PCDF
       ! The = 0 is the standard with no parallel enabled...
       format = 0

       ! We need to figure out whether the file is 
       ! a NetCDF3 or NetCDF4 file...
       call ncdf_inq(""//ncdf,format=format)
       if ( ncdf%comm >= 0 ) then
          call MPI_Bcast(format,1,MPI_Integer,0,ncdf%comm,MPIerror)
       end if

       ! If the parallel flag has been set, we need to examine that
       ! We will follow the parallel flag (and limit to the parallel accesible processors)
       select case ( format )
       case ( NF90_FORMAT_CLASSIC )
          ncdf%mode = NF90_PNETCDF
       case ( NF90_FORMAT_64BIT )
          ncdf%mode = NF90_PNETCDF
       case ( NF90_FORMAT_NETCDF4, NF90_FORMAT_NETCDF4_CLASSIC )
          if ( present(mode) ) then
             if ( iand(NF90_MPIIO   ,mode) /= NF90_MPIIO &
                  .and. &
                  iand(NF90_MPIPOSIX,mode) /= NF90_MPIPOSIX) then
                ncdf%mode = NF90_MPIIO
             end if
          else
             ncdf%mode = NF90_MPIIO
          end if
       end select
    end if
#endif

    if ( present(mode) ) then
       ncdf%mode = IOR(ncdf%mode,mode)
    end if

#ifdef NCDF_PARALLEL
    ! If the user has forgotton the correct notation of the file format
    if ( iand(NF90_MPIIO,ncdf%mode) == NF90_MPIIO ) then
       ncdf%mode = IOR(ncdf%mode,NF90_NETCDF4)
    else if ( iand(NF90_MPIPOSIX,ncdf%mode) == NF90_MPIPOSIX ) then
       ncdf%mode = IOR(ncdf%mode,NF90_NETCDF4)
       if ( iand(NF90_64BIT_OFFSET,ncdf%mode) == NF90_64BIT_OFFSET ) &
            call ncdf_die("You have requested netCDF4 parallel "//&
            "IO together with a netCDF3 file.")
    else if ( iand(NF90_PNETCDF,ncdf%mode) == NF90_PNETCDF ) then
       if ( iand(NF90_NETCDF4,ncdf%mode) == NF90_NETCDF4 ) &
            call ncdf_die("You have requested netCDF3 parallel "//&
            "IO together with a netCDF4 file.")
    end if
#endif

  end subroutine ncdf_init

  subroutine ncdf_create(ncdf,filename,mode,overwrite,parallel,comm, &
       compress_lvl)
#ifdef NCDF_PARALLEL
    use mpi, only : MPI_INFO_NULL
#endif
    type(hNCDF),       intent(inout) :: ncdf    
    character(len=*),  intent(in)  :: filename
    integer, optional, intent(in)  :: mode
    logical, optional, intent(in)  :: overwrite
    logical, optional, intent(in)  :: parallel
    integer, optional, intent(in)  :: comm
    integer, optional, intent(in)  :: compress_lvl
    integer :: file_format
    logical :: exist

    call ncdf_init(ncdf,name=filename, &
         mode=mode,parallel=parallel,comm=comm, &
         overwrite=overwrite, &
         compress_lvl=compress_lvl)

    ! We need to correct the definition for netCDF-3 files
    inquire(file=ncdf%name,exist=exist)
    if ( present(overwrite) ) then
       if ( overwrite ) then
          exist = .false.
       end if
    end if
    ncdf%define = 0

    if ( .not. ncdf_participate(ncdf) ) return

    if ( exist ) then
       call ncdf_die("File: "//ncdf//" already exists!. "//&
            "Please delete the file (or request overwritting).")
    end if

    if ( ncdf%parallel .and. ncdf%comm >= 0 ) then
#ifdef NCDF_PARALLEL
       call ncdf_err(nf90_create(filename, ncdf%mode , ncdf%id, &
            comm = ncdf%comm, info=MPI_INFO_NULL), &
            "Creating file: "//ncdf//" with communicator")
#else
       call ncdf_err(-100,"Not compiled with communicater parallel")
#endif
    else if ( ncdf%parallel ) then
       call ncdf_err(nf90_create(filename, ncdf%mode , ncdf%id), &
            "Creating file: "//ncdf//" in parallel")
    else
       call ncdf_err(nf90_create(filename, ncdf%mode , ncdf%id), &
            "Creating file: "//ncdf)
    end if
    ! We could check for mode == NF90_SHARE in case of parallel...
    ! However, it does not make sense as the other is still correct, just slow

    ! In case the NetCDF format is a CDF4 file, we do not need to alter define
    ! modes
    call ncdf_inq(ncdf, format=file_format)
    select case ( file_format ) 
    case ( NF90_FORMAT_NETCDF4 )
       ! NetCDF4-classic still uses define/undefine
       ncdf%define = -1
    end select

  end subroutine ncdf_create

  subroutine ncdf_open(ncdf,filename,mode,parallel,comm,compress_lvl)
#ifdef NCDF_PARALLEL
    use mpi, only : MPI_INFO_NULL
#endif
    type(hNCDF),    intent(inout)   :: ncdf    
    character(len=*), intent(in)  :: filename
    integer, optional, intent(in) :: mode
    logical, optional, intent(in) :: parallel
    integer, optional, intent(in) :: comm
    integer, optional, intent(in) :: compress_lvl
    integer :: file_format
    logical :: exist

    ! Save the general information which should be accesible to all processors
    call ncdf_init(ncdf,name=filename,mode=mode, &
         parallel=parallel,comm=comm, &
         compress_lvl=compress_lvl)

    ! When we open a file, it will always be in data mode...
    ncdf%define = 1

    if ( .not. ncdf_participate(ncdf) ) return
    
    inquire(file=filename, exist=exist)
    if ( .not. exist ) then
       call ncdf_die("File: "//trim(filename)//" does not exist!. "//&
            "Please check your inqueries.")
    end if

    ! If we have not added a mode it must be for non-writing purposes
    if ( .not. present(mode) ) then
       ncdf%mode = IOR(ncdf%mode,NF90_NOWRITE)
    end if

    if ( ncdf%parallel .and. ncdf%comm >= 0 ) then
#ifdef NCDF_PARALLEL
       call ncdf_err(nf90_open(filename, ncdf%mode , ncdf%id, &
            comm = ncdf%comm, info=MPI_INFO_NULL), &
               "Opening file: "//ncdf//" with communicator")
#else
       call ncdf_err(-100,"Code not compiled with NCDF_PARALLEL")
#endif
    else if ( ncdf%parallel ) then
       call ncdf_err(nf90_open(filename, ncdf%mode , ncdf%id), &
            "Opening file: "//ncdf//" in parallel")
    else
       call ncdf_err(nf90_open(filename, ncdf%mode , ncdf%id), &
            "Opening file: "//ncdf)
    end if
    
    call ncdf_inq(ncdf, format=file_format)
    select case ( file_format ) 
    case ( NF90_FORMAT_NETCDF4 )
       ncdf%define = -1
    end select

  end subroutine ncdf_open

  subroutine ncdf_par_access(this,name,access)
    type(hNCDF), intent(inout) :: this
    character(len=*), intent(in), optional :: name
    integer, intent(in), optional :: access
#ifdef NCDF_PARALLEL
    integer :: i,N
    character(len=250) :: lname

    if ( .not. ncdf_participate(this) ) return
    if ( .not. present(access) ) return

    ! We will only allow a change of the variable
    ! access if the netcdf-file is parallel.
    if ( .not. this%parallel ) then
       return
    end if

    if ( present(name) ) then
       call var_par(name,access)
    else
       call ncdf_inq(this,vars=N)
       do i = 1 , N
          call ncdf_err(nf90_inquire_variable(this%id, i, name=lname))
          call var_par(lname,access)
       end do
    end if

  contains
    subroutine var_par(name,access) 
      character(len=*), intent(in) :: name
      integer, intent(in) :: access
      integer :: id
      call ncdf_inq_var(this,name,id=id)
      call ncdf_err(nf90_var_par_access(this%id, id, access), &
           'Changing par-access (VAR) '//trim(name)//' in file: '//this)
    end subroutine var_par
#endif
  end subroutine ncdf_par_access

  subroutine ncdf_close(ncdf)
    type(hNCDF), intent(inout) :: ncdf

    if ( .not. ncdf_participate(ncdf) ) return

    if ( ncdf%id < 0 ) return

    call ncdf_err(nf90_close(ncdf%id),"Closing NetCDF file: "//ncdf)
    
    ncdf%id = -1

  end subroutine ncdf_close

  subroutine ncdf_inq_ncdf(ncdf,dims,vars,atts,format,grps,exist)
    type(hNCDF), intent(in) :: ncdf
    integer, optional, intent(out) :: dims, vars, atts, format, grps
    logical, optional, intent(out) :: exist
    integer :: ldims, lvars, latts, lformat, lgrps
    integer, allocatable :: grp_id(:)
    
    if ( .not. ncdf_participate(ncdf) ) return

    ! A file-check has been requested...
    if ( present(exist) ) then
       inquire(file=ncdf%name,exist=exist)
       ! if it does not exist we simply return
       ! this ensures that the user can request all the information 
       ! at once
       if ( .not. exist ) then
          return
       end if
    end if

    call ncdf_err(nf90_inquire(ncdf%id,ldims,lvars,latts,formatNum=lformat), &
         "Inquiring file information "//ncdf)

    ! Copy over requested information...
    if ( present(dims) )   dims   = ldims
    if ( present(vars) )   vars   = lvars
    if ( present(atts) )   atts   = latts
    if ( present(format) ) format = lformat

    if ( present(grps) ) then
       if ( IAND(ncdf%mode,NF90_NETCDF4) == NF90_NETCDF4 ) then
          allocate(grp_id(50))
          call ncdf_err(nf90_inq_grps(ncdf%id,grps,grp_id), &
               "Inquiring file information "//ncdf)
          if ( grps > size(grp_id) ) then
             deallocate(grp_id)
             allocate(grp_id(grps))
             call ncdf_err(nf90_inq_grps(ncdf%id,grps,grp_id), &
                  "Inquiring file information "//ncdf)
             deallocate(grp_id)
          end if
       else
          grps = -1
       end if
    end if
    
  end subroutine ncdf_inq_ncdf

  subroutine ncdf_inq_name(name,dims,vars,atts,format,grps,exist)
    character(len=*), intent(in)   :: name
    integer, optional, intent(out) :: dims, vars, atts, format, grps
    logical, optional, intent(out) :: exist
    type(hNCDF) :: ncdf

    ! A file-check has been requested...
    if ( present(exist) ) then
       inquire(file=name,exist=exist)
       if ( .not. exist ) then
          return
       end if
    end if

    ! Open the file...
    call ncdf_open(ncdf,name,parallel=.false.)
    ! Do the inquiry...
    call ncdf_inq_ncdf(ncdf,dims=dims,vars=vars,atts=atts,grps=grps,&
         format=format)
    ! Close the file
    call ncdf_close(ncdf)

  end subroutine ncdf_inq_name
    
! Simplify the addition of any dimension
  subroutine ncdf_def_dim(ncdf,name,size)
    type(hNCDF),     intent(inout) :: ncdf
    character(len=*), intent(in)   :: name
    integer, intent(in)            :: size
    integer :: id

    if ( .not. ncdf_participate(ncdf) ) return

    ! ensure definition step 
    ! in case of netCDF-3 this will not change anything
    call ncdf_redef(ncdf)

    call ncdf_err(nf90_def_dim(ncdf%id, name, size, id),&
         "Defining dimension: "//trim(name)//" in file: "//ncdf)

  end subroutine ncdf_def_dim

! Simplify the renaming of any dimension
  subroutine ncdf_rename_var(ncdf,old_name,new_name)
    type(hNCDF),     intent(inout) :: ncdf
    character(len=*), intent(in)   :: old_name, new_name
    integer :: id

    if ( .not. ncdf_participate(ncdf) ) return

    call ncdf_redef(ncdf)

    call ncdf_inq_var(ncdf,old_name,id=id)

    call ncdf_err(nf90_rename_var(ncdf%id, id, new_name),&
         "Renaming variable: "//trim(old_name)//" to "//&
         trim(new_name)//" in file: "//ncdf)
    
  end subroutine ncdf_rename_var

! Simplify the renaming of any dimension
  subroutine ncdf_rename_dim(ncdf,old_name,new_name)
    type(hNCDF),     intent(inout) :: ncdf
    character(len=*), intent(in)   :: old_name, new_name
    integer :: id

    if ( .not. ncdf_participate(ncdf) ) return

    call ncdf_redef(ncdf)

    call ncdf_inq_dim(ncdf,old_name,id=id)

    call ncdf_err(nf90_rename_dim(ncdf%id, id, new_name),&
         "Renaming dimension: "//trim(old_name)//" to "//&
         trim(new_name)//" in file: "//ncdf)
    
  end subroutine ncdf_rename_dim

  subroutine ncdf_rename_att(ncdf,var,old_name,new_name)
    type(hNCDF),     intent(inout) :: ncdf
    character(len=*), intent(in)   :: var, old_name, new_name
    integer :: id

    if ( .not. ncdf_participate(ncdf) ) return

    call ncdf_redef(ncdf)

    call ncdf_inq_var(ncdf,var,id=id)

    call ncdf_err(nf90_rename_att(ncdf%id, id, old_name, new_name),&
         "Renaming variable ("//trim(var)//") attribute: "//trim(old_name)&
         //" to "//trim(new_name)//" in file: "//ncdf)
    
  end subroutine ncdf_rename_att

  subroutine ncdf_rename_gatt(ncdf,old_name,new_name)
    type(hNCDF),     intent(inout) :: ncdf
    character(len=*), intent(in)   :: old_name, new_name

    if ( .not. ncdf_participate(ncdf) ) return

    call ncdf_redef(ncdf)

    call ncdf_err(nf90_rename_att(ncdf%id, NF90_GLOBAL, old_name, new_name),&
         "Renaming global attribute: "//trim(old_name)&
         //" to "//trim(new_name)//" in file: "//ncdf)
    
  end subroutine ncdf_rename_gatt

! Simplify the addition of a variable...
! This routine *MUST* be called after ncdf_participate 
! (however, as it is a local routine the burden is ours, not the programmers) 
  subroutine ncdf_def_var_generic(this,name,type,dims,id,atts, &
       compress_lvl,shuffle, access)
    use iso_var_str
    use variable
    use dictionary
    type(hNCDF),      intent(inout) :: this
    character(len=*), intent(in)    :: name
    integer,          intent(in)    :: type
    character(len=*), intent(in)    :: dims(:)
    integer,          intent(out)   :: id
    type(dict),                      optional :: atts
    integer,          intent(in),    optional :: compress_lvl
    logical,          intent(in),    optional :: shuffle
    integer,          intent(in),    optional :: access
    type(dict) :: att
    type(var)  :: at_var
#ifdef NCDF_4
    integer :: loc_compress_lvl
    logical :: lshuffle
#endif
    integer :: iret, i, ldims(size(dims))
    character(len=100) :: key
    character(len=NF90_MAX_NAME) :: tmp

    call ncdf_redef(this)
    do i = 1 , size(dims)
       call ncdf_inq_dim(this,trim(dims(i)),id=ldims(i))
    end do

! Determine whether we have NetCDF 4 enabled, in that case do compression if asked for
#ifdef NCDF_4
    loc_compress_lvl = this%comp_lvl
    if ( present(compress_lvl) ) loc_compress_lvl = compress_lvl
    lshuffle = .true.
    if ( present(shuffle) ) lshuffle = shuffle
    ! Compression for parallel access is not allowed
    if ( this%comm > 0 ) loc_compress_lvl = 0
    if ( loc_compress_lvl > 0 ) then
       iret = nf90_def_var(this%id, name, type, ldims, id, &
            shuffle=lshuffle,deflate_level=loc_compress_lvl)
    else
       iret = nf90_def_var(this%id, name, type, ldims, id)
    end if
#else
! In case of NetCDF 3
       iret = nf90_def_var(this%id, name, type, ldims, id)
#endif
    call ncdf_err(iret,"Defining variable: "//trim(name)//" in file: "//this)

    if ( present(atts) ) then
       att = .first. atts
       att_loop: do 
          if ( .empty. att ) exit att_loop
          key = .key. att
          if ( key == "ATT_DELETE" ) then
             att = .next. att
             cycle
          end if
          call associate(at_var,att)
          ! currently we only allow the string to be performed
          if ( at_var%t /= "V0" ) then
             att = .next. att
             cycle
          end if
          tmp = at_var%v0
             
          iret = nf90_put_att(this%id, id, trim(key), tmp)

          call ncdf_err(iret, &
               "Saving attribute: "//trim(key)// &
               " to "//trim(name)//" in file: "//this)
          att = .next. att
       end do att_loop
       
       ! If the user adds this key, the dictionary will be deleted
       ! after usage...
       if ( "ATT_DELETE" .in. atts ) then
          call delete(atts)
       end if
       
    end if

    if ( present(access) ) then
       call ncdf_par_access(this,name=name,access=access)
    end if
    
  end subroutine ncdf_def_var_generic

  subroutine ncdf_def_var_integer(this, name, type, dims, &
       atts, compress_lvl, shuffle, fill, &
       access)
    use dictionary
    type(hNCDF), intent(inout) :: this
    character(len=*), intent(in) :: name
    integer, intent(in) :: type
    character(len=*), intent(in) :: dims(:)
    type(dict), optional :: atts
    integer, intent(in), optional :: compress_lvl
    logical, intent(in), optional :: shuffle, fill
    integer, intent(in), optional :: access
    integer :: id

    if ( .not. ncdf_participate(this) ) then
       ! in case the attributes are present, we
       ! still need to clean-up if asked
       if ( present(atts) ) then
          if ( 'ATT_DELETE' .in. atts ) then
             call delete(atts)
          end if
       end if

       return
    end if

    call ncdf_def_var_generic(this, name, type, dims, id, &
         atts=atts, compress_lvl=compress_lvl, shuffle=shuffle, &
         access=access)
    if ( present(fill) ) then
       if ( fill ) then
          call ncdf_err(nf90_def_var_fill(this%id,id, 1, 0), &
               "Setting the variable "//trim(name)//" to NOFILL in file "//this)
       else
          call ncdf_err(nf90_def_var_fill(this%id,id, 0, 0), &
               "Setting the variable "//trim(name)//" to FILL in file "//this)
       end if
    end if

  end subroutine ncdf_def_var_integer

  subroutine ncdf_def_var_logical(this, name, type, dims, &
       atts, compress_lvl, shuffle, fill, &
       access)
    use dictionary
    type(hNCDF), intent(inout) :: this
    character(len=*), intent(in) :: name
    logical, intent(in) :: type
    character(len=*), intent(in) :: dims(:)
    type(dict), optional :: atts
    integer, intent(in), optional :: compress_lvl
    logical, intent(in), optional :: shuffle, fill
    integer, intent(in), optional :: access
    integer :: id
    integer :: ltype

    if ( .not. ncdf_participate(this) ) then
       ! in case the attributes are present, we
       ! still need to clean-up if asked
       if ( present(atts) ) then
          if ( 'ATT_DELETE' .in. atts ) then
             call delete(atts)
          end if
       end if

       return
    end if

    if ( type .eqv. NF90_DOUBLE_COMPLEX ) then
       ltype = NF90_DOUBLE
    else
       ltype = NF90_FLOAT
    end if

    if ( type .eqv. NF90_DOUBLE_COMPLEX ) then
       ltype = NF90_DOUBLE
    else
       ltype = NF90_FLOAT
    end if

    call ncdf_def_var_generic(this, "Re"//trim(name), ltype, dims, id, &
         atts=atts, compress_lvl=compress_lvl, shuffle=shuffle, &
         access=access)
    if ( present(fill) ) then
       if ( fill ) then
          call ncdf_err(nf90_def_var_fill(this%id,id, 1, 0), &
               "Setting the variable "//trim(name)//" to NOFILL in file "//this)
       else
          call ncdf_err(nf90_def_var_fill(this%id,id, 0, 0), &
               "Setting the variable "//trim(name)//" to FILL in file "//this)
       end if
    end if
    call ncdf_def_var_generic(this, "Im"//trim(name), ltype, dims, id, &
         atts=atts, compress_lvl=compress_lvl, shuffle=shuffle, &
         access=access)
    if ( present(fill) ) then
       if ( fill ) then
          call ncdf_err(nf90_def_var_fill(this%id,id, 1, 0), &
               "Setting the variable "//trim(name)//" to NOFILL in file "//this)
       else
          call ncdf_err(nf90_def_var_fill(this%id,id, 0, 0), &
               "Setting the variable "//trim(name)//" to FILL in file "//this)
       end if
    end if

  end subroutine ncdf_def_var_logical

  subroutine ncdf_default(this,access,compress_lvl)
    type(hNCDF), intent(inout) :: this
    integer, intent(in), optional :: access, compress_lvl

    if ( .not. ncdf_participate(this) ) return

    if ( present(access) ) call ncdf_par_access(this,access=access)

#ifdef NCDF_4
    if ( present(compress_lvl) ) then
       if ( iand(NF90_NETCDF4,this%mode) == NF90_NETCDF4 ) then
          this%comp_lvl = compress_lvl
       end if
       if ( iand(NF90_CLASSIC_MODEL,this%mode) == NF90_CLASSIC_MODEL ) then
          this%comp_lvl = compress_lvl
       end if
    end if
#endif

  end subroutine ncdf_default

  subroutine ncdf_inq_var_def(ncdf,name,exist,id,size,atts)
    use dictionary
    type(hNCDF),      intent(inout) :: ncdf
    character(len=*), intent(in)    :: name
    logical, optional, intent(out)  :: exist
    integer, optional, intent(out)  :: id
    integer, optional, intent(out)  :: size(:)
    type(dict), optional, intent(inout) :: atts
    integer :: iret ! We need to retain any error message...
    integer :: lid, nids, i
    integer :: ldids(10) ! In case the user only wishes to read a sub-part of the size
    character(len=50) :: dim
    integer :: nAtts
    character(len=500) :: att_name,att
    logical :: lexist
    
    if ( .not. ncdf_participate(ncdf) ) return

    ! Figure out if the dimension exists
    iret = nf90_inq_varid(ncdf%id, trim(name), lid)
    ! The variable must exist
    lexist = iret == NF90_NOERR
    if ( present(exist) ) then
       exist = lexist
    else if ( .not. lexist ) then
       call ncdf_err(iret,"Retrieving information about: "//trim(name)//" in file: "//ncdf)
    end if
    
    ! If there is nothing to inquire: return
    if ( .not. lexist ) return

    if ( present(id) ) id = lid

    ! If the user has requested information about the size of the variable...
    if ( present(size) ) then
       call ncdf_err(nf90_inquire_variable(ncdf%id, lid, ndims=nids, dimids=ldids))
       do i = 1 , min(nids,ubound(size,1))
          call ncdf_err(nf90_inquire_dimension(ncdf%id,ldids(i),name=dim), &
               "Retrieving dimension name in inq_var for file: "//ncdf)
          ! Save the dimension size in array "size"
          call ncdf_inq_dim(ncdf,trim(dim),len=size(i))
       end do
    end if

    ! The user has requested information about the attributes associated...
    if ( present(atts) ) then
       call ncdf_err(nf90_inquire_variable(ncdf%id, lid, nAtts=nAtts), &
            "Retrieving number of associated attributes in inq_var for file: "//ncdf)
       do i = 1 , nAtts
          att_name = " "
          att      = " "
          call ncdf_err(nf90_inq_attname(ncdf%id, lid, i, att_name), &
               "Retrieving the attribute name for file: "//ncdf)

          ! TODO
          ! For the moment we can only retrieve attributes of characters...
          ! This should be leveraged as the dictionary can handle arbitrary values...
          call ncdf_err(nf90_get_att(ncdf%id, lid, trim(att_name), att), &
               "Retrieving the attribute value for file: "//ncdf)
          atts = atts//(trim(att_name).KV.trim(att))
       end do
    end if

  end subroutine ncdf_inq_var_def

  subroutine ncdf_inq_dim(ncdf,name,exist,id,len)
    type(hNCDF),      intent(inout) :: ncdf
    character(len=*), intent(in)    :: name
    logical, optional, intent(out)  :: exist
    integer, optional, intent(out)  :: id
    integer, optional, intent(out)  :: len
    integer :: iret ! We need to retain any error message...
    integer :: lid
    logical :: lexist
    
    if ( .not. ncdf_participate(ncdf) ) return

    ! Figure out if the dimension exists
    iret = nf90_inq_dimid(ncdf%id, trim(name), lid)
    lexist = iret == NF90_NOERR
    if ( present(exist) ) then
       exist = lexist
    else if ( .not. lexist ) then
       call ncdf_err(iret,"Retrieving information about: "//trim(name)//" in file: "//ncdf)
    end if

    ! If there is nothing to inquire: return
    if ( .not. lexist ) return

    if ( present(id) ) id = lid
    if ( present(len) ) then
       call ncdf_err(nf90_inquire_dimension(ncdf%id, lid, len=len), &
            "Retrieving length of dimension: "//trim(name)//" in file: "//ncdf)
    end if

  end subroutine ncdf_inq_dim

  subroutine ncdf_inq_gatt(ncdf,name,exist)
    type(hNCDF),      intent(inout) :: ncdf
    character(len=*), intent(in)    :: name
    logical, optional, intent(out)  :: exist
    integer :: iret ! We need to retain any error message...
    logical :: lexist
    
    if ( .not. ncdf_participate(ncdf) ) return

    ! Figure out if the dimension exists
    iret = nf90_inquire_attribute(ncdf%id, NF90_GLOBAL, trim(name))
    lexist = iret == NF90_NOERR
    if ( present(exist) ) then
       exist = lexist
    else if ( .not. lexist ) then
       call ncdf_err(iret,"Retrieving information about: "//trim(name)//" in file: "//ncdf)
    end if
  end subroutine ncdf_inq_gatt

  subroutine ncdf_inq_att(ncdf,var,name,exist)
    type(hNCDF),      intent(inout) :: ncdf
    character(len=*), intent(in)    :: var
    character(len=*), intent(in)    :: name
    logical, optional, intent(out)  :: exist
    integer :: iret ! We need to retain any error message...
    integer :: id
    logical :: lexist
    
    if ( .not. ncdf_participate(ncdf) ) return

    call ncdf_inq_var(ncdf,var,id=id)
    ! Figure out if the dimension exists
    iret = nf90_inquire_attribute(ncdf%id, id, trim(name))
    lexist = iret == NF90_NOERR
    if ( present(exist) ) then
       exist = lexist
    else if ( .not. lexist ) then
       call ncdf_err(iret,"Retrieving information about: "//trim(name)//" in file: "//ncdf)
    end if

  end subroutine ncdf_inq_att

  ! Delete attributes
  subroutine ncdf_del_att(ncdf,var,name)
    type(hNCDF),      intent(inout) :: ncdf
    character(len=*), intent(in)    :: var
    character(len=*), intent(in)    :: name
    integer :: iret ! We need to retain any error message...
    integer :: id

    call ncdf_redef(ncdf)    

    if ( .not. ncdf_participate(ncdf) ) return

    call ncdf_inq_var(ncdf,var,id=id)
    ! Figure out if the dimension exists
    iret = nf90_inquire_attribute(ncdf%id, id, trim(name))
    if ( iret == NF90_NOERR ) then
       call ncdf_err(nf90_del_att(ncdf%id, id, trim(name)), &
            "Deleting attribute: "//trim(name)//" for variable "//&
            trim(var)//" in file: "//ncdf)
    end if

  end subroutine ncdf_del_att

  subroutine ncdf_del_gatt(ncdf,name)
    type(hNCDF),      intent(inout) :: ncdf
    character(len=*), intent(in)    :: name
    integer :: iret ! We need to retain any error message...
    integer :: id

    call ncdf_redef(ncdf)
    
    if ( .not. ncdf_participate(ncdf) ) return
    
    ! Figure out if the dimension exists
    iret = nf90_inquire_attribute(ncdf%id, NF90_GLOBAL, trim(name))
    if ( iret == NF90_NOERR ) then
       call ncdf_err(nf90_del_att(ncdf%id, NF90_GLOBAL, trim(name)), &
            "Deleting global attribute: "//trim(name)//" in file: "//ncdf)
    end if

  end subroutine ncdf_del_gatt

  subroutine ncdf_fill(ncdf,fill,old_fill)
    type(hNCDF),       intent(inout) :: ncdf
    integer, optional, intent(in)    :: fill
    integer, optional, intent(out)   :: old_fill
    integer :: lf, lof

    ! option collect
    lf = NF90_NOFILL
    if ( present(fill) ) lf = fill

    call ncdf_err(nf90_set_fill(ncdf%id,lf, lof), &
         "Setting fill mode in file: "//ncdf)

    if ( present(old_fill) ) old_fill = lof

    if ( .not. present(fill) ) &
         call ncdf_err(nf90_set_fill(ncdf%id,lof, lf), &
         "Re-setting fill mode in file: "//ncdf)

  end subroutine ncdf_fill


! Use the netcdf_wrap.sh script to generate the needed code...
#include "ncdf_funcs.inc"

  subroutine ncdf_enddef(ncdf)
    type(hNCDF), intent(inout) :: ncdf
    ! A NetCDF4 file, (does not need define/data mode)
    if ( ncdf%define < 0 ) return
    ! Already in data mode:
    if ( ncdf%define == 1 ) return
    ncdf%define = 1
    if ( .not. ncdf_participate(ncdf) ) return
    call ncdf_err(nf90_enddef(ncdf%id),"End definition segment of file: "//ncdf)
  end subroutine ncdf_enddef

  subroutine ncdf_sync(ncdf)
    type(hNCDF), intent(in) :: ncdf
    if ( .not. ncdf_participate(ncdf) ) return
    ! We need (must) not sync when in definition mode...
    if ( ncdf%define == 0 ) return
    call ncdf_err(nf90_sync(ncdf%id),"File syncronization for file"//ncdf)
  end subroutine ncdf_sync

  subroutine ncdf_redef(ncdf)
    type(hNCDF), intent(inout) :: ncdf
    ! A NetCDF4 file, (does not need define/data mode)
    if ( ncdf%define < 0 ) return
    ! Already in define mode:
    if ( ncdf%define == 0 ) return
    ncdf%define = 0
    if ( .not. ncdf_participate(ncdf) ) return
    call ncdf_err(nf90_redef(ncdf%id),"Redef definition segment in file: "//ncdf)
  end subroutine ncdf_redef

! ################################################################
! ################# Specialized routines for handling ############
! ############ the different aspects of this module ##############
! ################################################################

! A simple error checker for NetCDF
  subroutine ncdf_err(status,msg)
    integer, intent(in) :: status
    character(len=*), optional, intent(in) :: msg

    ! We should never encounter this region of the code with out having enabled some 
    ! reading or writing of the NetCDF handle...
    ! if ( .not. ncdf_participate() ) return

    if (status .ne. nf90_noerr) then
       if (present(msg)) write(*,"(a)") trim(msg)
       write(*,*)
       write(*,"(a)") "Error occured in NCDF:"
       write(0,"(a)") "Error occured in NCDF:"
       write(*,"(a)") trim(nf90_strerror(status))
       write(0,"(a)") trim(nf90_strerror(status))
       write(*,"(a,tr1,i0)") "Status number:",status
       write(0,"(a,tr1,i0)") "Status number:",status
       call ncdf_die("NetCDF Error: Stopped due to error in NetCDF file")
    endif
  end subroutine ncdf_err


! ################################################################
! ################ Routines for handling the groups###############
! ############ of the NetCDF files. We allow this to #############
! ############ always be present due to reduction of #############
! ################## preprocessor flags ##########################
! ################################################################

  ! Create groups in a NetCDF4 file
  subroutine ncdf_def_grp(ncdf,name,grp)
    type(hNCDF), intent(in out) :: ncdf
    character(len=*), intent(in) :: name
    type(hNCDF), intent(out) :: grp
    
    if ( .not. ncdf_participate(ncdf) ) return

    ! Copy the information regarding the parent ncdf
    call ncdf_copy(ncdf,grp)
    ! Save the group name... (we save it with hiercharal notice /"grp1"/"grp2")
    grp%grp = trim(grp%grp)//"/"//trim(name)
    ! Create the group and return
    call ncdf_err(nf90_def_grp(ncdf%id,name,grp%id), &
         "Creating group "//trim(name)//" in file "//ncdf)

  end subroutine ncdf_def_grp

! ################################################################
! ################## End of group routines #######################
! ################################################################


! # Returns a logical determining the participation of the node
  function ncdf_participate(ncdf) result(participate)
    type(hNCDF), intent(in) :: ncdf
    logical :: participate
    ! In all cases this should be the correct way to do it
    ! If wire is attached, the correct parallel setting is created
    ! If a communicator is attached the parallel flag is also set
    participate = ncdf%parallel .or. IONode
  end function ncdf_participate


! These routines or functions are global available even if the NetCDF is not used...

! functions for concatenating strings and ncdf handles.
  character(len=300) function cat_char_ncdf(char,ncdf)
    character(len=*), intent(in) :: char
    type(hNCDF), intent(in) :: ncdf
    cat_char_ncdf = char//trim(ncdf%name)
  end function cat_char_ncdf

  character(len=300) function cat_ncdf_char(ncdf,char)
    type(hNCDF), intent(in) :: ncdf
    character(len=*), intent(in) :: char
    cat_ncdf_char = trim(ncdf%name)//char
  end function cat_ncdf_char

  subroutine ncdf_IONode(io_Node)
    logical, intent(in) :: IO_Node
    IONode = IO_Node
  end subroutine ncdf_IONode

  subroutine ncdf_print(ncdf)
#ifdef NCDF_PARALLEL
    use mpi
#endif
    type(hNCDF), intent(in) :: ncdf
    integer :: ndims,nvars,ngatts,file_format,ngrps
    integer :: Node,Nodes
#ifdef NCDF_PARALLEL
    integer :: MPIerror
#endif

    if ( .not. ncdf_participate(ncdf) ) return

    Node = 0
    Nodes = 1
#ifdef NCDF_PARALLEL
    if ( ncdf%comm >= 0 ) then
       call MPI_Comm_rank(ncdf%comm,Node,MPIerror)
       call MPI_Comm_size(ncdf%comm,Nodes,MPIerror)
    end if
#endif
    ! This will fail if it is not the 0th Node in the communicator
    ! For instance a subgroup in the Comm_World...
    if ( Node == 0 ) then
       write(*,"(a20,a)") "NetCDF filename:    ",trim(ncdf%name)
       if ( len_trim(ncdf%grp) /= 0 ) then
          write(*,"(a20,a)") "NetCDF group name:  ",trim(ncdf%grp)
       end if
       write(*,"(a20,i7)") "NetCDF ID:          ",ncdf%id
       if ( ncdf%parallel ) then
          write(*,"(a20,a)") "Parallel access:    ","True"
          write(*,"(a20,tr1,i0)") "Parallel processors:",Nodes
       else
          write(*,"(a20,a)") "Parallel access:    ","False"
       end if
       if ( ncdf%define == 0 ) then
          write(*,"(a20,a)") "In define-mode:     ","True"
       else if ( ncdf%define == 1 ) then
          write(*,"(a20,a)") "In define-mode:     ","False"
       end if
       call ncdf_inq(ncdf, dims=ndims, vars=nvars, atts=ngatts, &
            grps=ngrps, format=file_format)
       select case ( file_format ) 
       case ( NF90_FORMAT_CLASSIC )
          write(*,"(a20,a)") "File format:        ","Classic"
       case ( NF90_FORMAT_64BIT )
          write(*,"(a20,a)") "File format:        ","Classic 64Bit"
       case ( NF90_FORMAT_NETCDF4 )
          write(*,"(a20,a)") "File format:        ","NetCDF4"
          write(*,"(a20,i7)")"Default compression:",ncdf%comp_lvl
       case ( NF90_FORMAT_NETCDF4_CLASSIC )
          write(*,"(a20,a)") "File format:        ","NetCDF4 Classic format"
          write(*,"(a22,i7)")"Default compression:  ",ncdf%comp_lvl
       case default
          write(*,"(a20,a)") "File format:        ","Could not be determined"
       end select
       write(*,"(a20,i7)") "Number of dimensions:  ",ndims
       write(*,"(a20,i7)") "Number of variables:   ",nvars
       write(*,"(a20,i7)") "Number of attributes:  ",ngatts
       if ( ngrps >= 0 ) then
       write(*,"(a20,i7)") "Number of groups:      ",ngrps
       end if
       if ( iand(NF90_WRITE,ncdf%mode) == NF90_WRITE ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_WRITE"
       if ( iand(NF90_NOCLOBBER,ncdf%mode) == NF90_NOCLOBBER ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_NOCLOBBER"
       if ( iand(NF90_NOFILL,ncdf%mode) == NF90_NOFILL ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_NOFILL"
       if ( iand(NF90_64BIT_OFFSET,ncdf%mode) == NF90_64BIT_OFFSET ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_64BIT_OFFSET"
       if ( iand(NF90_LOCK,ncdf%mode) == NF90_LOCK ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_LOCK"
       if ( iand(NF90_SHARE,ncdf%mode) == NF90_SHARE ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_SHARE"
       if ( iand(NF90_NETCDF4,ncdf%mode) == NF90_NETCDF4 ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_NETCDF4"
       if ( iand(NF90_CLASSIC_MODEL,ncdf%mode) == NF90_CLASSIC_MODEL ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_CLASSIC_MODEL"
#ifdef NCDF_PARALLEL
       if ( iand(NF90_MPIIO,ncdf%mode) == NF90_MPIIO ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_MPIIO"
       if ( iand(NF90_MPIPOSIX,ncdf%mode) == NF90_MPIPOSIX ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_MPIPOSIX"
       if ( iand(NF90_PNETCDF,ncdf%mode) == NF90_PNETCDF ) &
            write(*,"(a20,a)") "NetCDF mode:        ","NF90_PNETCDF"
#endif
    end if
    
  end subroutine ncdf_print

! A standard die routine... It is not pretty... But it works...
! Recommended to be adapted!
  subroutine ncdf_die(str)
#ifdef NCDF_PARALLEL
    use mpi
#endif
    character(len=*), intent(in) :: str
#ifdef NCDF_PARALLEL
    integer :: MPIerror
#endif
    
    write(0,"(a)") trim(str)
    write(6,"(a)") trim(str)

#ifdef NCDF_PARALLEL
    call MPI_Abort(MPI_Comm_World,1,MPIerror)
#else
    call abort()
#endif
  end subroutine ncdf_die

end module nf_ncdf


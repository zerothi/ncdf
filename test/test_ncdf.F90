program test_ncdf
  use dictionary
  use nf_ncdf

  use tst_ncdf_util

  implicit none

  type(hNCDF) :: ncdf
  integer :: Node, Nodes, i
  character(len=1) :: ci
  type(dict) :: dic
#ifdef NCDF_PARALLEL
  integer :: MPIerror

  call MPI_Init(MPIerror)
  
  call MPI_Comm_rank(MPI_COMM_WORLD,Node,MPIerror)
  call MPI_Comm_size(MPI_COMM_WORLD,Nodes,MPIerror)
#else
  Node = 0
  Nodes = 1
#endif

!  i = nf90_set_log_level(3)
  call ncdf_IOnode(Node == 0)

  call goto_dir('first')
  write(*,*)
  call test_seq3()
  write(*,*)
  call test_seq4()
  write(*,*)
  call test_par3()
  write(*,*)
  call test_par4()
  write(*,*)
  call test_par4_ind()
  write(*,*)
  call chdir('..')

  write(ci,'(i1)') mod(Node,2)
  call goto_dir('IO'//ci)

  call test_seq3()
  call test_seq4()
  call test_par3()
  call test_par4()
  call test_par4_ind()
  call chdir('..')

#ifdef NCDF_PARALLEL
  call MPI_Finalize(MPIerror)
#endif

contains

  subroutine test_seq3()
    call show_where('In ncdf3 Sequential')
    call ncdf_create(ncdf,'NCDF3_seq.nc',mode=NF90_64BIT_OFFSET,overwrite=.true.)
    call ncdf_def_dim(ncdf,'x',1)
    call ncdf_def_dim(ncdf,'y',NF90_UNLIMITED)
    dic = ('unit'.kv.'m')//('Name'.kv.'What is this')
    call ncdf_def_var(ncdf,'v',NF90_DOUBLE,(/'x','y'/))!, atts=dic)
    call delete(dic)
    call ncdf_print(ncdf)
    do i = 1 , 10
       call ncdf_put_var(ncdf,'v',real(i,8),start=(/1,i/))
    end do
    ! redefining a NetCDF file after already ending the definition step is ONLY 
    ! allowed in NetCDF 3 formats...
    call ncdf_def_dim(ncdf,'z',2)
    dic = ('unit'.kv.'m')//('Name'.kv.'Height')
    dic = dic//('ATT_DELETE'.kv.1)
    call ncdf_def_var(ncdf,'h',NF90_DOUBLE,(/'z','y'/), &
         atts=('unit'.kv.'m')//('Name'.kv.'Height')//('ATT_DELETE'.kv.1))
    do i = 1 , 10
       call ncdf_put_var(ncdf,'h',(/real(i,8),real(i*2,8)/),start=(/1,i/))
    end do
    call ncdf_close(ncdf)
    call check_nc(''//ncdf)
  end subroutine test_seq3

  subroutine test_seq4()
    type(hNCDF):: grp1,grp2
#ifdef NCDF_4
    call show_where('In ncdf4 Sequential')
    call ncdf_create(ncdf,'NCDF4_seq.nc',mode=NF90_NETCDF4,overwrite=.true.)
    call ncdf_def_dim(ncdf,'x',1)
    call ncdf_def_dim(ncdf,'y',NF90_UNLIMITED)
    call ncdf_def_dim(ncdf,'z',2)
    call ncdf_def_var(ncdf,'v',NF90_DOUBLE,(/'x','y'/), &
         atts=('unit'.kv.'m')//('Name'.kv.'What is this'),compress_lvl=3)
    call ncdf_def_var(ncdf,'h',NF90_DOUBLE,(/'z','y'/), &
         atts=('unit'.kv.'m')//('Name'.kv.'Height'),compress_lvl=3)
    call ncdf_def_grp(ncdf,'info',grp1)
    call ncdf_def_dim(grp1,'j',3)
    call ncdf_def_var(grp1,'j',NF90_INT,(/'j'/))
    call ncdf_def_grp(grp1,'scndlevel',grp2)
    call ncdf_def_dim(grp2,'j',3)
    call ncdf_def_var(grp2,'j',NF90_INT,(/'j'/))
    call ncdf_print(ncdf)
    call ncdf_print(grp1)
    call ncdf_print(grp2)
    do i = 1 , 3
       call ncdf_put_var(grp1,'j',i,start=(/i/))
       call ncdf_put_var(grp2,'j',i,start=(/i/))
    end do
    do i = 1 , 10
       call ncdf_put_var(ncdf,'v',real(i,8),start=(/1,i/))
    end do
    do i = 1 , 10
       call ncdf_put_var(ncdf,'h',(/real(i,8),real(i*2,8)/),start=(/1,i/))
    end do
    call ncdf_close(ncdf)
    call check_nc(''//ncdf)
#endif
  end subroutine test_seq4

  subroutine test_par3()
#ifdef NCDF_PARALLEL3
    call show_where('In ncdf3 parallel')
    call ncdf_create(ncdf,'NCDF3_par.nc',mode=IOR(NF90_PNETCDF,NF90_64BIT_OFFSET),overwrite=.true.,comm=MPI_Comm_world)
    call ncdf_def_dim(ncdf,'x',1)
    call ncdf_def_dim(ncdf,'y',NF90_UNLIMITED)
    call ncdf_def_var(ncdf,'v',NF90_DOUBLE,(/'x','y'/), &
         atts=('unit'.kv.'m')//('Name'.kv.'What is this'))
    call ncdf_print(ncdf)
    do i = 1 , 9
       if ( mod(i,Nodes) == Node ) then
          call ncdf_put_var(ncdf,'v',real(i,8),start=(/1,i/))
       end if
    end do
    ! redefining a NetCDF file after already ending the definition step is ONLY 
    ! allowed in NetCDF 3 formats...
    call ncdf_def_dim(ncdf,'z',2)
    call ncdf_def_var(ncdf,'h',NF90_DOUBLE,(/'z','y'/), &
         atts=('unit'.kv.'m')//('Name'.kv.'Height'))
    do i = 1 , 9
       if ( mod(i,Nodes) == Node ) then
          call ncdf_put_var(ncdf,'h',(/real(i,8),real(i*2,8)/),start=(/1,i/))
       end if
    end do
    call ncdf_close(ncdf)
    call check_nc(''//ncdf)
#endif
  end subroutine test_par3

  subroutine test_par4()
#ifdef NCDF_PARALLEL
    integer, parameter :: N = 9
    call show_where('In ncdf4 parallel')
    call ncdf_create(ncdf,'NCDF4_par.nc', &
         mode=ior(NF90_MPIIO,NF90_CLASSIC_MODEL),overwrite=.true.,comm=MPI_Comm_World)
    call ncdf_def_dim(ncdf,'x',1)
    call ncdf_def_dim(ncdf,'y',NF90_UNLIMITED)
    call ncdf_def_dim(ncdf,'z',2)
    dic = ('unit'.kv.'m')//('Name'.kv.'What is this')
    call ncdf_def_var(ncdf,'v',NF90_DOUBLE,(/'x','y'/), &
         atts=dic)
    call delete(dic)
    dic = 'unit'.kv.'m'
    dic = dic//('Name'.kv.'Height')
    call ncdf_def_var(ncdf,'h',NF90_DOUBLE,(/'z','y'/), &
         atts=dic)
    call delete(dic)
    call ncdf_print(ncdf)
    call ncdf_default(ncdf,access=NF90_COLLECTIVE)

    do i = 1 , N
       if ( mod(i,Nodes) == Node ) then
          call ncdf_put_var(ncdf,'v',real(i,8),start=(/1,i/))
       end if
    end do
    ! not working in parallel
    i = N / Nodes
    if ( mod(N,Nodes) /= 0 .and. Node < N - i * Nodes ) then
       i = Node + 1
       call ncdf_put_var(ncdf,'v',real(-1,8),start=(/1,i/),count=(/0,0/))
    end if

    do i = 1 , N
       if ( mod(i,Nodes) == Node ) then
          call ncdf_put_var(ncdf,'h',(/real(i,8),real(i*2,8)/),start=(/1,i/),count=(/2/))
       end if
    end do
    i =  N / Nodes
    if ( mod(N,Nodes) /= 0 .and. Node < N - i * Nodes ) then
       i = Node + 1
       call ncdf_put_var(ncdf,'v',real(i,8),start=(/1,i/),count=(/0,0/))
    end if
    call ncdf_close(ncdf)
    call check_nc(''//ncdf)
#endif
  end subroutine test_par4

  subroutine test_par4_ind()
#ifdef NCDF_PARALLEL
    call show_where('In ncdf4 parallel, independent')
    call ncdf_create(ncdf,'NCDF4_par_ind.nc', &
         mode=ior(NF90_MPIIO,NF90_CLASSIC_MODEL),overwrite=.true.,comm=MPI_Comm_World)
    ! In case of parallel access, it cannot compress. (so it will 
    ! set this to zero inside)
    call ncdf_def_dim(ncdf,'x',1)
    call ncdf_def_dim(ncdf,'y',9)
    call ncdf_def_dim(ncdf,'z',2)
    dic = ('unit'.kv.'m')//('Name'.kv.'What is this')
    call ncdf_def_var(ncdf,'v',NF90_DOUBLE,(/'x','y'/), &
         atts=dic)
    call delete(dic)
    dic = 'unit'.kv.'m'
    dic = dic//('Name'.kv.'Height')
    call ncdf_def_var(ncdf,'h',NF90_DOUBLE,(/'z','y'/), &
         atts=dic)
    call delete(dic)
    call ncdf_default(ncdf,access=NF90_INDEPENDENT)
    call ncdf_print(ncdf)
    
    do i = 1 , 9
       if ( mod(i,Nodes) == Node ) then
          call ncdf_put_var(ncdf,'v',real(i,8),start=(/1,i/))
       end if
    end do
    do i = 1 , 9
       if ( mod(i,Nodes) == Node ) then
          call ncdf_put_var(ncdf,'h',(/real(i,8),real(i*2,8)/),start=(/1,i/),count=(/2/))
       end if
    end do
    call ncdf_close(ncdf)
    call check_nc(''//ncdf)
#endif
  end subroutine test_par4_ind

end program test_ncdf


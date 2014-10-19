program tst_ncdf_3
  use dictionary
  use nf_ncdf

  use tst_ncdf_utils

  implicit none

  type(hNCDF) :: ncdf
  integer :: Node, Nodes, i
  character(len=1) :: ci
  type(dict) :: dic
  logical :: assert

  call tst_mpi_init(Node,Nodes)

  ! Create the netcdf-file
  if ( Nodes > 1 ) then
     call ncdf_create(ncdf,'NCDF_par_3.nc',&
          mode=IOR(NF90_PNETCDF,NF90_64BIT_OFFSET), &
          overwrite=.true., comm=MPI_Comm_World)
  else
     call ncdf_create(ncdf,'NCDF_seq_3.nc',&
          mode=NF90_64BIT_OFFSET, overwrite=.true.)
  end if
  
  ! Define dimensions
  call ncdf_def_dim(ncdf,'x',1)
  call ncdf_def_dim(ncdf,'y',NF90_UNLIMITED)
  
  dic = ('unit'.kv.'m')//('Name'.kv.'What is this')
  call ncdf_def_var(ncdf,'v',NF90_DOUBLE,(/'x','y'/), atts=dic)
  call delete(dic) ! ensure deletion

  call ncdf_print(ncdf)
  do i = 1 , 10
     if ( mod(i,Nodes) == Node ) then
        call ncdf_put_var(ncdf,'v',real(i,8),start=(/1,i/))
     end if
  end do

  ! redefining a NetCDF file after already ending the definition step is ONLY 
  ! allowed in NetCDF 3 formats...
  call ncdf_def_dim(ncdf,'z',2)
  dic = ('unit'.kv.'m')//('Name'.kv.'Height')
  dic = dic//('ATT_DELETE'.kv.1)
  call ncdf_def_var(ncdf,'h',NF90_DOUBLE,(/'z','y'/), atts=dic)
  do i = 1 , 10
     if ( mod(i,Nodes) == Node ) then
        call ncdf_put_var(ncdf,'h',(/real(i,8),real(i*2,8)/),start=(/1,i/))
     end if
  end do

  ! We assert that the dimensions exist
  dic = ('x'.kv.1)//('y'.kv.10)
  call ncdf_assert(ncdf,assert,dims=dic)
  call delete(dic)
  if ( .not. assert ) then
     write(*,*) 'ASSERTION NOT FULFILLED'
  else
     write(*,*) 'Fulfilled assertion...'
  end if

  call ncdf_close(ncdf)
  call check_nc(''//ncdf)

  
  

#ifdef NCDF_PARALLEL
  call MPI_Finalize(Nodes)
#endif

end program tst_ncdf_3


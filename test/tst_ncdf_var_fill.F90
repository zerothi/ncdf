program tst_ncdf_var_fill

  use dictionary
  use netcdf_ncdf

  use tst_ncdf_utils

  implicit none

  character(len=30) :: fname

  type(hNCDF) :: ncdf
  integer :: Node, Nodes, comp_lvl
  
  type(dictionary_t) :: dic
  logical :: assert

  integer :: fill
  real(dp) :: tmp_fill
  complex(dp) :: tmp_fillc

  call tst_mpi_init(Node,Nodes)

  ! Create the netcdf-file
  if ( Nodes > 1 ) then
     fname = 'NCDF_fill_value_par_4.nc'
     call ncdf_create(ncdf,fname, &
          mode=ior(NF90_NETCDF4, NF90_MPIIO), overwrite=.true., &
          parallel=.true., &
          comm=MPI_Comm_World)
     ! parallel writes are not allowed with compression
     ! Offset positions are not well defined.
     comp_lvl = 0
  else
     fname = 'NCDF_fill_value_seq_4.nc'
     call ncdf_create(ncdf,fname, &
          mode=NF90_NETCDF4, overwrite=.true.)
     comp_lvl = 3
  end if

  call ncdf_def_dim(ncdf,'x',1)
  call ncdf_def_dim(ncdf,'y',NF90_UNLIMITED)
  call ncdf_def_dim(ncdf,'z',2)

  dic = ('unit'.kv.'m')//('Name'.kv.'What is this')
  call ncdf_def_var(ncdf,'v',NF90_DOUBLE,(/'x','y'/), &
       atts=dic,compress_lvl=comp_lvl)
  call ncdf_def_fill(ncdf,'v',2.0_dp)

  call ncdf_def_var(ncdf,'c',NF90_DOUBLE_COMPLEX,(/'x','y'/), &
       atts=dic,compress_lvl=comp_lvl)
  call ncdf_def_fill(ncdf,'c',(2.0_dp, 3.0_dp))
  call delete(dic)

  call ncdf_enddef(ncdf)

  ! print out the leveled netcdf
  call ncdf_print(ncdf)

  call ncdf_inq_var(ncdf, 'v', fill_value=tmp_fill, fill=fill)
  if ( abs(2._dp - tmp_fill) > 1.e-8 ) then
    write(*,*) "ASSERTION NOT FULFILLED   2 != ", tmp_fill, fill == NF90_FILL
    stop 9
  else
    write(*,*) 'Fulfilled assertion...', fill == NF90_FILL
  end if

  call ncdf_inq_var(ncdf, 'c', fill_value=tmp_fillc, fill=fill)
  if ( abs((2._dp, 3._dp) - tmp_fillc) > 1.e-8 ) then
    write(*,*) "ASSERTION NOT FULFILLED   2,3 != ", tmp_fillc, fill == NF90_FILL
    stop 9
  else
    write(*,*) 'Fulfilled assertion...', fill == NF90_FILL
  end if

  call ncdf_close(ncdf)

  call check_nc(fname)

#ifdef NCDF_PARALLEL
  call MPI_Finalize(Nodes)
#endif
  
end program


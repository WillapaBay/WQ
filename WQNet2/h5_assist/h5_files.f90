!DEC$ FREEFORM
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! HDF5 Interface Module: Files
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Written by Todd Steissberg, 2017
!
module h5_files
  !
  use, non_intrinsic  :: hdf5
  use, intrinsic      :: iso_c_binding ! provides: c_float, c_int, c_ptr
  use, non_intrinsic  :: h5_globals
  !
  implicit none
  !
  contains
  !
  ! ----- HDF5 Interface -----
  !
  !
  ! .....................................................................................................
  !
  ! Initialize HDF5
  ! .....................................................................................................
  logical function h5_init() result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_init" :: h5_init
    !
    ! This function opens the HDF5 interface and initializes constants,
    ! such as file access flags and data type identifiers.
    integer                             :: hdf_error ! HDF5 error flag
    !
    success = .false.
    !
    call h5open_f(hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Terminate HDF5
  ! .....................................................................................................
  logical function h5_terminate() result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_terminate" :: h5_terminate
    !
    ! This function closes the HDF5 interface and releases resources
    integer                             :: hdf_error ! HDF5 error flag
    !
    success = .false.
    !
    call h5close_f(hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  !
  ! ----- HDF5 Files -----
  !
  !
  ! .....................................................................................................
  !
  ! Open HDF5 File
  ! .....................................................................................................
  logical function h5_open_file(infile, H5_access_flag, file_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_open_file" :: h5_open_file
    !
    character(len=*), intent(in)        :: infile           ! HDF5 input file
    integer,          intent(in)        :: H5_access_flag   ! HDF5 file access flag (defined in module header)
    !                                                       ! Possible values: READ_ONLY, READ_WRITE, 
    !                                                       ! TRUNCATE, and CREATE_AND_READ_WRITE
    integer(hid_t), intent(out)         :: file_id          ! HDF5 file identifier to return
    integer                             :: hdf_error        ! HDF5 error flag
    logical                             :: is_ok            ! Did the function run without error?
    !
    success = .false.
    !
    ! Initialize HDF5
    is_ok = h5_init()
    !
    call h5fopen_f(trim(infile), H5_access_flag, file_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Close HDF5 File
  ! .....................................................................................................
  logical function h5_close_file(file_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_close_file" :: h5_close_file
    !
    integer(hid_t), intent(in)          :: file_id    ! File identifier of HDF5 file to close
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    call h5fclose_f(file_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Create HDF file for output
  ! .....................................................................................................
  logical function h5_create_file(filename, H5_access_flag, file_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_file" :: h5_create_file
    !
    character(len=*), intent(in)        :: filename         ! Name of HDF5 output file to create
    integer(hid_t), intent(in)          :: H5_access_flag   ! HDF5 file access flag (defined in module header)
    !                                                       ! Possible values: READ_ONLY, READ_WRITE, 
    !                                                       ! TRUNCATE, and CREATE_AND_READ_WRITE
    integer(hid_t), intent(out)         :: file_id          ! File identifier to return
    logical                             :: is_ok            ! Did the function run without error?
    integer                             :: hdf_error        ! HDF5 error flag
    !
    success = .false.
    !
    ! Initialize HDF5
    is_ok = h5_init()
    !
    ! Create an HDF5 file for output
    call h5fcreate_f(trim(filename), H5F_ACC_TRUNC_F, file_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
end module
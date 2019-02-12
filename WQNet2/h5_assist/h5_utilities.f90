!DEC$ FREEFORM
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! HDF5 Interface Module: Utilities
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Written by Todd Steissberg, 2017
!
module h5_utilities
  !
  use, non_intrinsic  :: hdf5
  use, intrinsic      :: iso_c_binding ! provides: c_float, c_int, c_ptr
  !
  implicit none
  !
  contains
  !
  ! ----- Helper Routines -----
  !
  subroutine error_handler(message)
    character(len=*), intent(in)        :: message
    integer, parameter                  :: error_log = 5
    character(len=*), parameter         :: error_log_filename = "h5_interface_error_log.txt"
    !
    open(error_log, file = error_log_filename)
    write(error_log, *) message
    close(error_log)
  end subroutine
  !
  ! .....................................................................................................
  !
  ! Get string data type, given a string length
  ! .....................................................................................................
  logical function h5_get_string_datatype(str_len, data_type) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_get_string_datatype" :: h5_get_string_datatype
    !
    integer(hsize_t), intent(in)        :: str_len    ! String length for data type
    integer(hid_t), intent(out)         :: data_type  ! Data type identifier
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    call h5tcopy_f(H5T_NATIVE_CHARACTER, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    call h5tset_size_f(data_type, str_len, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Get real data type
  ! .....................................................................................................
  logical function h5_get_real_datatype(data_type) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_get_real_datatype" :: h5_get_real_datatype
    !
    integer(hid_t), intent(out)         :: data_type  ! Data type identifier
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    call h5tcopy_f(H5T_NATIVE_REAL, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Get integer data type
  ! .....................................................................................................
  logical function h5_get_integer_datatype(data_type) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_get_integer_datatype" :: h5_get_integer_datatype
    !
    integer(hid_t), intent(out)         :: data_type  ! Data type identifier
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    call h5tcopy_f(H5T_NATIVE_INTEGER, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
end module
!DEC$ FREEFORM
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! HDF5 Interface Module: Attributes
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Written by Todd Steissberg, 2017
!
module h5_attributes
  !
  use, non_intrinsic  :: hdf5
  use, intrinsic      :: iso_c_binding ! provides: c_float, c_int, c_ptr
  use, non_intrinsic  :: h5_globals  
  use, non_intrinsic  :: h5_groups
  use, non_intrinsic  :: h5_datasets  
  use, non_intrinsic  :: h5_utilities  
  !
  implicit none
  !
  contains
  !
  !
  ! ----- Helper Functions -----
  !
  !
  ! .....................................................................................................
  !
  ! Get HDF5 attribute data type
  ! .....................................................................................................
  logical function h5_get_attribute_datatype(attri_id, data_type) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_get_attribute_datatype" :: h5_get_attribute_datatype
    !
    integer(hid_t), intent(in)          :: attri_id   ! Attribute identifier
    integer(hid_t), intent(out)         :: data_type  ! Data type flag    
    integer                             :: hdf_error  ! HDF5 error flag
    !      
    success = .false.
    !
    ! Get data type
    call h5aget_type_f (attri_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end
  !
  !
  ! ----- Open/Close/Create Attributes -----
  !
  !
  ! .....................................................................................................
  !
  ! Open HDF5 attribute
  ! .....................................................................................................
  logical function h5_open_attribute(obj_id, attri_name, attri_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_open_attribute" :: h5_open_attribute
    !
    integer(hid_t), intent(in)          :: obj_id     ! File, group, or dataset identifier
    character(len=*), intent(in)        :: attri_name ! Name of existing attribute
    integer(hid_t), intent(out)         :: attri_id   ! Attribute identifier
    logical                             :: is_exist   ! Indicates if attribute exists
    logical                             :: is_ok      ! Did the function run without error?
    integer                             :: hdf_error  ! HDF5 error flag
    !      
    success = .false.
    !
    ! Check if attribute exists
    call h5aexists_by_name_f(obj_id, '.', attri_name, is_exist, hdf_error)
    if (.not. is_exist) return
    !
    ! Open attribute
    call h5aopen_by_name_f(obj_id, '.', attri_name, attri_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Close HDF5 attribute
  ! .....................................................................................................
  logical function h5_close_attribute(attri_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_close_attribute" :: h5_close_attribute
    !
    integer(hid_t), intent(in)          :: attri_id   ! Attribute identifier
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    ! Close attribute
    call h5aclose_f(attri_id, hdf_error)
    if (hdf_error < 0) return
    !
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Create 1D HDF5 attribute (array or scalar)
  ! .....................................................................................................
  logical function h5_create_attribute(loc_id, attri_name, rank, data_type, nvals, attri_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_attribute" :: h5_create_attribute
    !
    integer(hid_t), intent(in)          :: loc_id       ! File, group, or dataset identifier
    character(len=*), intent(in)        :: attri_name   ! Name of existing attribute
    integer, intent(in)                 :: rank         ! Attribute rank (number of dimensions)
    integer(hid_t), intent(in)          :: data_type    ! Attribute data type
    integer(hid_t), intent(in)          :: nvals        ! Number of values in array
    integer(hid_t), intent(out)         :: attri_id     ! Attribute identifier
    integer(hid_t)                      :: dataspace_id ! Data space ID    
    integer                             :: hdf_error    ! HDF5 error flag
    integer(hsize_t), dimension(1:rank) :: data_dims    ! Attribute data dimensions
    !      
    success = .false.
    !
    ! Create a simple data space
    data_dims(1) = nvals
    call h5screate_simple_f(rank, data_dims, dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create attribute
    call h5acreate_f(loc_id, attri_name, data_type, dataspace_id, attri_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Create 1D HDF5 attribute (array or scalar): Character array type
  ! .....................................................................................................
  logical function h5_create_attribute_c(loc_id, attri_name, rank, str_len, nvals, attri_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_attribute_c" :: h5_create_attribute_c
    !
    integer(hid_t), intent(in)          :: loc_id       ! File, group, or dataset identifier
    character(len=*), intent(in)        :: attri_name   ! Name of existing attribute
    integer, intent(in)                 :: rank         ! Attribute rank (number of dimensions)
    integer(size_t), intent(in)         :: str_len      ! Attribute string length
    integer(hid_t), intent(in)          :: nvals        ! Number of values in array
    integer(hid_t), intent(out)         :: attri_id     ! Attribute identifier
    integer(hid_t)                      :: data_type    ! Attribute data type
    integer(hid_t)                      :: dataspace_id ! Data space ID
    integer                             :: hdf_error    ! HDF5 error flag
    integer(hsize_t), dimension(1:rank) :: data_dims    ! Attribute data dimensions
    !      
    success = .false.
    !
    data_dims(1) = nvals
    !
    ! Create the memory data type
    call h5tcopy_f(H5T_FORTRAN_S1, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set the memory size using the string length
    call h5tset_size_f(data_type, str_len, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create a simple data space
    call h5screate_simple_f(rank, data_dims, dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create attribute
    call h5acreate_f(loc_id, attri_name, data_type, dataspace_id, attri_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end
  !
  !
  ! ----- Read Attributes -----
  !
  !
  ! .....................................................................................................
  !
  ! Read HDF5 scalar attribute: Real type
  ! .....................................................................................................
  logical function h5_read_attri_scalar_r(attri_id, attri_value) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_attri_scalar_r" :: h5_read_attri_scalar_r
    !
    integer(hid_t), intent(in)          :: attri_id     ! Attribute identifier
    real(kind=c_float), intent(out)     :: attri_value  ! Attribute value
    integer(hsize_t), dimension(1)      :: data_dims    ! Data dimensions (returned by h5a_read_f)
    integer(hid_t)                      :: data_type    ! Data type flag
    integer                             :: hdf_error    ! HDF5 error flag
    logical                             :: is_ok        ! Did the function run without error?
    !      
    success = .false.
    !
    attri_value = 3.4e38
    !    
    ! Get data type
    is_ok = h5_get_attribute_datatype(attri_id, data_type)
    !
    ! Set dimensions = 1 for scalar data
    data_dims(1) = 1
    !
    ! Read attribute
    call h5aread_f(attri_id, data_type, attri_value, data_dims, hdf_error)
    if (hdf_error < 0) return
    !      
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Read HDF5 scalar attribute: Integer type
  ! .....................................................................................................
  logical function h5_read_attri_scalar_i(attri_id, attri_value) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_attri_scalar_i" :: h5_read_attri_scalar_i
    !
    integer(hid_t), intent(in)          :: attri_id     ! Attribute identifier
    integer(kind=c_int), intent(out)    :: attri_value  ! Attribute value
    integer(hsize_t), dimension(1)      :: data_dims    ! Data dimensions (returned by h5a_read_f)
    integer(hid_t)                      :: data_type    ! Data type flag
    integer                             :: hdf_error    ! HDF5 error flag
    logical                             :: is_ok        ! Did the function run without error?
    !      
    success = .false.
    !
    attri_value = 9999
    !    
    is_ok = h5_get_attribute_datatype(attri_id, data_type)
    if (.not. is_ok) return
    !
    ! Set dimensions = 1 for scalar data
    data_dims(1) = 1
    !
    ! Read attribute
    call h5aread_f(attri_id, data_type, attri_value, data_dims, hdf_error)
    if (hdf_error < 0) return
    !
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Read HDF5 scalar attribute: Character type
  ! .....................................................................................................
  logical function h5_read_attri_scalar_c(attri_id, attri_value) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_attri_scalar_c" :: h5_read_attri_scalar_c
    !
    integer(hid_t), intent(in)          :: attri_id     ! Attribute identifier
    character(len=*), intent(out)       :: attri_value  ! Attribute value to return
    integer(hsize_t), dimension(1)      :: data_dims    ! Data dimensions returned by h5a_read_f
    integer(hid_t)                      :: data_type    ! Data type flag
    integer                             :: hdf_error    ! HDF5 error flag
    logical                             :: is_ok        ! Did the function run without error?
    !      
    success = .false.
    !
    ! Get data type of existing attribute
    is_ok = h5_get_attribute_datatype(attri_id, data_type)
    if (.not. is_ok) return
    !    
    ! Set dimensions = 1 for scalar data
    data_dims(1) = 1
    !
    ! clear the string before reading it
    attri_value = " "    
    !
    ! Read attribute    
    call h5aread_f(attri_id, data_type, attri_value, data_dims, hdf_error)
    if (hdf_error < 0) return
    !
    !
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Get scalar attribute: character type
  ! .....................................................................................................
  function h5_get_attri_scalar_c(loc_id, attri_name) result(value)
    integer(hid_t),   intent(in)  :: loc_id       ! Location ID (file or group ID)    
    character(len=*), intent(in)  :: attri_name   ! name of attribute
    integer(hid_t)                :: attri_id
    character(len=:), allocatable :: value
    character(len=:), allocatable :: attri_value        
    logical                       :: is_ok 
    !
    ! open the attribute
    is_ok = h5_open_attribute(loc_id, attri_name, attri_id)
    !
    ! get the length and allocate space
    allocate(character(len=64)::attri_value)
    is_ok = h5_read_attri_scalar_c(attri_id, attri_value)
    !
    ! close the attribute
    is_ok = h5_close_attribute(attri_id)
    value = trim(attri_value)
  end function
  !
  ! .....................................................................................................
  !
  ! Read 1D HDF5 attribute: Real type
  ! .....................................................................................................
  logical function h5_read_attri_1d_array_r(attri_id, attri_array, nvals) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_attri_1d_array_r" :: h5_read_attri_1d_array_r
    !
    integer(hid_t), intent(in)          :: attri_id       ! Attribute identifier
    integer(hsize_t), intent(in)        :: nvals           ! Number of strings in array
    real(kind=c_float), &
      dimension(nvals), &
      intent(out), target               :: attri_array    ! Attribute array
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hsize_t), dimension(1)      :: data_dims      ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(size_t)                     :: data_type_size ! Size of data type
    integer                             :: i              ! Loop index for writing results
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    integer                             :: hdf_error      ! HDF5 error flag        
    !
    success = .false.
    !
    data_dims = (/nvals/)
    !
    ! Get the data type
    call h5aget_type_f(attri_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., 
    ! the string length of the stored dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create the memory data type
    call h5tcopy_f(data_type, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Read the data using a pointer
    f_ptr = C_LOC(attri_array(1))
    call h5aread_f(attri_id, memory_type, f_ptr, hdf_error)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 1D HDF5 attribute: Integer type
  ! .....................................................................................................
  logical function h5_read_attri_1d_array_i(attri_id, attri_array, nvals) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_attri_1d_array_i" :: h5_read_attri_1d_array_i
    !
    integer(hid_t), intent(in)          :: attri_id       ! Attribute identifier
    integer(hsize_t), intent(in)        :: nvals           ! Number of strings in array
    integer, &
      dimension(nvals), &
      intent(out), target               :: attri_array    ! Attribute array
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hsize_t), dimension(1)      :: data_dims      ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(size_t)                     :: data_type_size ! Size of data type
    integer                             :: i              ! Loop index for writing results
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    integer                             :: logfile = 111
    !
    success = .false.
    !
    data_dims = (/nvals/)
    !
    ! Get the data type
    call h5aget_type_f(attri_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., 
    ! the string length of the stored dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create the memory data type
    call h5tcopy_f(data_type, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Read the data using a pointer
    f_ptr = C_LOC(attri_array(1))
    call h5aread_f(attri_id, memory_type, f_ptr, hdf_error)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 1D HDF5 attribute: String type
  ! .....................................................................................................
  logical function h5_read_attri_1d_array_c(attri_id, attri_array, str_len, nvals) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_attri_1d_array_c" :: h5_read_attri_1d_array_c
    !
    ! Note: In .Net, use a StringBuilder object to pass the array of strings
    ! C# Example:
    ! var SB = new StringBuilder((int) nvals * str_len)
    ! SB = H5Interface.return_string_array(SB, ref str_len, ref nvals)
    ! Then parse SB to a string[] array in C#.
    !
    integer(hid_t), intent(in)          :: attri_id       ! Attribute identifier
    integer(size_t), intent(in)         :: str_len        ! Length of strings in array
    integer(hsize_t), intent(in)        :: nvals           ! Number of strings in array
    character(len=str_len), &
      dimension(nvals), &
      intent(out), target               :: attri_array    ! Attribute array
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hsize_t), dimension(1)      :: data_dims      ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(size_t)                     :: data_type_size ! Size of data type
    integer                             :: i              ! Loop index for writing results
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    logical                             :: is_ok          ! Did the function run without error?
    integer                             :: hdf_error      ! HDF5 error flag
    integer                             :: logfile = 109
    !
    success = .false.
    !
    data_dims = (/nvals/)
    !
    ! Open log file for output
    if (DEBUG) open(logfile, file = "h5_read_attri_1d_array_c")
    !
    ! Get the data type
    call h5aget_type_f(attri_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., 
    ! the string length of the stored dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Make sure the declared length is large enough
    if (data_type_size > str_len) then
       if (DEBUG) write(logfile,*) 'ERROR: Character LEN is too small'
       return
    endif
    !
    ! Create the memory data type
    call h5tcopy_f(H5T_FORTRAN_S1, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set the memory size using the string length
    call h5tset_size_f(memory_type, str_len, hdf_error)
    if (hdf_error == -1) return
    !
    ! Read the data using a pointer
    f_ptr = C_LOC(attri_array(1)(1:1))
    call h5aread_f(attri_id, memory_type, f_ptr, hdf_error)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    do i = 1, data_dims(1)
       if (DEBUG) write(logfile,'(A,"(",I0,"): ", A)') "data", i, trim(attri_array(i))
    end do
    !
    if (DEBUG) close(logfile)
    !
    success = .true.
  end function
  !
  !
  ! ----- Write Attributes
  !
  !
  ! .....................................................................................................
  !
  ! Write HDF5 scalar attribute: Real type
  ! .....................................................................................................
  logical function h5_write_attri_scalar_r(attri_id, attri_value) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_attri_scalar_r" :: h5_write_attri_scalar_r
    !
    integer(hid_t), intent(inout)       :: attri_id     ! Attribute identifier
    real(kind=c_float), intent(in)      :: attri_value  ! Attribute value
    integer(hsize_t), dimension(1)      :: data_dims    ! Data dimensions (returned by h5a_read_f)
    integer(hid_t)                      :: data_type    ! Data type flag
    integer                             :: hdf_error    ! HDF5 error flag
    logical                             :: is_ok        ! Did the function run without error?
    !      
    success = .false.
    !
    ! Get data type
    is_ok = h5_get_attribute_datatype(attri_id, data_type)
    !
    ! Set dimensions = 1 for scalar data
    data_dims(1) = 1
    !
    ! Read attribute
    call h5awrite_f(attri_id, data_type, attri_value, data_dims, hdf_error)
    if (hdf_error < 0) return
    !
    ! Close and release resources
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return
    !      
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Write HDF5 scalar attribute: Integer type
  ! .....................................................................................................
  logical function h5_write_attri_scalar_i(attri_id, attri_value) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_attri_scalar_i" :: h5_write_attri_scalar_i
    !
    integer(hid_t), intent(inout)       :: attri_id     ! Attribute identifier
    real(kind=c_float), intent(in)      :: attri_value  ! Attribute value
    integer(hsize_t), dimension(1)      :: data_dims    ! Data dimensions (returned by h5a_read_f)
    integer(hid_t)                      :: data_type    ! Data type flag
    integer                             :: hdf_error    ! HDF5 error flag
    logical                             :: is_ok        ! Did the function run without error?
    !      
    success = .false.
    !
    ! Get data type
    is_ok = h5_get_attribute_datatype(attri_id, data_type)
    !
    ! Set dimensions = 1 for scalar data
    data_dims(1) = 1
    !
    ! Read attribute
    call h5awrite_f(attri_id, data_type, attri_value, data_dims, hdf_error)
    if (hdf_error < 0) return
    !      
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Write HDF5 scalar attribute: String type
  ! .....................................................................................................
  logical function h5_write_attri_scalar_c(attri_id, attri_value, str_len) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_attri_scalar_c" :: h5_write_attri_scalar_c
    !
    integer(hid_t), intent(inout)       :: attri_id     ! Attribute identifier
    integer(size_t), intent(in)         :: str_len      ! Attribute string length
    character(kind=c_char, len=str_len), &
      intent(in)                        :: attri_value  ! Attribute value
    integer(hsize_t), dimension(1)      :: data_dims    ! Data dimensions (returned by h5a_read_f)
    integer(hid_t)                      :: data_type    ! Data type flag
    integer                             :: hdf_error    ! HDF5 error flag
    logical                             :: is_ok        ! Did the function run without error?
    !      
    success = .false.
    write(*,*) "I am here"
    !
    ! Get data type
    is_ok = h5_get_attribute_datatype(attri_id, data_type)
    !
    ! Set dimensions = 1 for scalar data
    data_dims(1) = 1
    !
    ! Read attribute
    call h5awrite_f(attri_id, data_type, attri_value, data_dims, hdf_error)
    if (hdf_error < 0) return
    !      
    success = .true.
  end
  !
  ! .....................................................................................................
  !
  ! Write 1D HDF5 attribute array: Real type
  ! .....................................................................................................
  logical function h5_write_attri_1d_array_r(attri_id, attri_array, nvals) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_attri_1d_array_r" :: h5_write_attri_1d_array_r
    !
    integer(hid_t), intent(in)          :: attri_id       ! Attribute identifier
    integer(hsize_t), intent(in)        :: nvals           ! Number of strings in array
    real(kind=c_float), &
      dimension(nvals), &
      intent(in), target                :: attri_array    ! Attribute array
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hsize_t), dimension(1)      :: data_dims      ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(size_t)                     :: data_type_size ! Size of data type
    integer                             :: i              ! Loop index for writing results
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    integer                             :: hdf_error      ! HDF5 error flag    
    !
    success = .false.
    !
    data_dims = (/nvals/)
    !
    ! Get the data type
    call h5aget_type_f(attri_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., 
    ! the string length of the stored dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create the memory data type
    call h5tcopy_f(data_type, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Read the data using a pointer
    f_ptr = C_LOC(attri_array(1))
    call h5awrite_f(attri_id, memory_type, f_ptr, hdf_error)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write 1D HDF5 attribute array: Integer type
  ! .....................................................................................................
  logical function h5_write_attri_1d_array_i(attri_id, attri_array, nvals) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_attri_1d_array_i" :: h5_write_attri_1d_array_i
    !
    integer(hid_t), intent(in)          :: attri_id       ! Attribute identifier
    integer(hsize_t), intent(in)        :: nvals           ! Number of strings in array
    integer(kind=c_int), &
      dimension(nvals), &
      intent(in), target                :: attri_array    ! Attribute array
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hsize_t), dimension(1)      :: data_dims      ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(size_t)                     :: data_type_size ! Size of data type
    integer                             :: i              ! Loop index for writing results
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    data_dims = (/nvals/)
    !
    ! Get the data type
    call h5aget_type_f(attri_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., 
    ! the string length of the stored dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create the memory data type
    call h5tcopy_f(data_type, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Read the data using a pointer
    f_ptr = C_LOC(attri_array(1))
    call h5awrite_f(attri_id, memory_type, f_ptr, hdf_error)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write 1D HDF5 attribute array: Character type
  ! .....................................................................................................
  logical function h5_write_attri_1d_array_c(attri_id, attri_array, str_len, nvals) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_attri_1d_array_c" :: h5_write_attri_1d_array_c
    !
    integer(hid_t), intent(in)          :: attri_id       ! Attribute identifier
    integer(size_t), intent(in)         :: str_len        ! String length in attri_array
    integer(hsize_t), intent(in)        :: nvals          ! Number of strings in array
    character(kind=c_char, len=str_len), &
      dimension(nvals), &
      intent(in), target                :: attri_array    ! Attribute array
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hsize_t), dimension(1)      :: data_dims      ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(size_t)                     :: data_type_size ! Size of data type
    integer                             :: i              ! Loop index for writing results
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    integer                             :: hdf_error      ! HDF5 error flag    
    !
    success = .false.
    !
    data_dims = (/nvals/)
    !
    ! Get the data type
    call h5aget_type_f(attri_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., 
    ! the string length of the stored dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Make sure the declared length is large enough
    if (data_type_size > str_len) then
       if (DEBUG) write(*,*) 'ERROR: Character LEN is to small'
       return
    endif
    !
    ! Create the memory data type
    call h5tcopy_f(H5T_FORTRAN_S1, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set the memory size using the string length
    call h5tset_size_f(memory_type, str_len, hdf_error)
    if (hdf_error == -1) return
    !
    ! Read the data using a pointer
    call h5awrite_f(attri_id, memory_type, attri_array, data_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  
end module
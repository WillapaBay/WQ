!DEC$ FREEFORM
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! HDF5 Interface Module: Datasets
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Written by Todd Steissberg, 2017
!
! HEC convention: 
!  Columns represent the time dimension and rows represent the space dimension.
!  Typically, a row of data (cross sections) is read or written for each 
!  time step during a simulation.
!
module h5_datasets
  !
  use, non_intrinsic  :: hdf5
  use, intrinsic      :: iso_c_binding ! provides: c_float, c_int, c_ptr
  use, non_intrinsic  :: h5_globals  
  use, non_intrinsic  :: h5_groups
  use, non_intrinsic  :: h5_utilities  
  !
  implicit none
  !
  contains
  !
  !
  ! ----- HDF5 Datasets -----
  !
  !
  ! .....................................................................................................
  !
  ! Open HDF5 dataset
  ! .....................................................................................................
  logical function h5_open_dataset(loc_id, dataset_name, dataset_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_open_dataset" :: h5_open_dataset
    !
    integer(hid_t), intent(in)          :: loc_id         ! Location ID (file or group ID),
    !                                                     ! where dataset_name may be located
    character(len=*), intent(in)        :: dataset_name   ! Name of dataset to open
    integer(hid_t), intent(out)         :: dataset_id     ! Dataset identifier to return    
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! open HDF5 dataset
    call h5dopen_f(loc_id, trim(dataset_name), dataset_id, hdf_error)
    if (hdf_error < 0) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Close HDF5 dataset
  ! .....................................................................................................
  logical function h5_close_dataset(dataset_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_close_dataset" :: h5_close_dataset
    !
    integer(hid_t), intent(in)          :: dataset_id ! Identifier of dataset to close
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    call h5dclose_f(dataset_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Check if HDF5 dataset exists via group name
  ! .....................................................................................................
  logical function h5_dataset_exists_in_groupname(file_id, group_name, dataset_name, link_exists) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_dataset_exists_in_groupname" :: h5_dataset_exists_in_groupname
    !
    integer(hid_t), intent(in)          :: file_id        ! Identifier of open file
    character(len=*), intent(in)        :: group_name     ! Name of group that should contain dataset
    character(len=*), intent(in)        :: dataset_name   ! Name of dataset to check
    logical, intent(out)                :: link_exists    ! Are you there?
    character(len=LONG_STR_LEN)         :: fullpath       ! Will contain full path to dataset, 
    !                                                     ! including group name    
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! Check if group (link) exists in and opened file
    call h5lexists_f(file_id, trim(group_name), link_exists, hdf_error)
    if (link_exists) then
      ! Check if dataset (link) exists in group
      fullpath = trim(group_name) // '/' // trim(dataset_name)
      call h5lexists_f(file_id, trim(fullpath), link_exists, hdf_error)
    else
      return
    end if
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Check if HDF5 dataset exists via group_id
  ! .....................................................................................................
  logical function h5_dataset_exists_in_group_id(group_id, dataset_name, link_exists) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_dataset_exists_in_group_id" :: h5_dataset_exists_in_group_id
    !
    integer(hid_t), intent(in)          :: group_id       ! Identifier of group that should contain dataset
    character(len=*), intent(in)        :: dataset_name   ! Name of dataset to check
    logical, intent(out)                :: link_exists    ! Are you there?    
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! Check if dataset (link) exists in an open group
    call h5lexists_f(group_id, trim(dataset_name), link_exists, hdf_error)
    !
    success = link_exists
  end function
  !
  ! .....................................................................................................
  !
  ! Get dataset dimensions and maximum dimensions
  ! .....................................................................................................
  logical function h5_get_dataset_dimensions(dataset_id, data_dims, max_dims, rank) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_get_dataset_dimensions" :: h5_get_dataset_dimensions
    !
    ! Note: data_dims and max_dims should be declared as long type when calling from C# or other languages
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset    
    integer, intent(in)                 :: rank           ! Rank of memory space
    integer(hsize_t), &
      dimension(1:rank), &
      intent(inout)                     :: data_dims      ! Dataset dimensions (set using dataset properties)
    integer(hsize_t), &
      dimension(1:rank), &
      intent(inout)                     :: max_dims       ! Maximum dataset dimensions (output)    
    integer(hid_t)                      :: dataspace_id   ! Data space identifier    
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! Get data space from dataset
    call h5dget_space_f(dataset_id, dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get dataset dimensions, data_dims. Ignoring max_dims.
    call h5sget_simple_extent_dims_f(dataspace_id, data_dims, max_dims, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Get dataset dimensions and maximum dimensions
  ! .....................................................................................................
  logical function h5_get_dataset_dimensions_cdecl(dataset_id, data_dims_ptr, max_dims_ptr, rank) result(success) bind(c, name="h5_get_dataset_dimensions_cdecl")
    !DEC$ ATTRIBUTES DLLEXPORT :: h5_get_dataset_dimensions_cdecl
    !
    integer(hid_t), intent(in)              :: dataset_id   ! Identifier of open dataset
    integer, intent(in)                     :: rank         ! Rank of memory space
    integer(hid_t)                          :: dataspace_id ! Data space identifier
    integer                                 :: hdf_error    ! HDF5 error flag
    type(c_ptr), value                      :: data_dims_ptr
    type(c_ptr), value                      :: max_dims_ptr
    integer(hsize_t), dimension(:), pointer :: data_dims
    integer(hsize_t), dimension(:), pointer :: max_dims
    call c_f_pointer(data_dims_ptr, data_dims, [rank])
    call c_f_pointer(max_dims_ptr, max_dims, [rank])
    !
    success = .false.
    !
    ! Get data space from dataset
    call h5dget_space_f(dataset_id, dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get dataset dimensions, data_dims. Ignoring max_dims.
    call h5sget_simple_extent_dims_f(dataspace_id, data_dims, max_dims, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Helper function: Get memory space and select hyperslab for a 2D dataset
  ! .....................................................................................................
  logical function h5_get_hyperspace_2d(dataset_id, startrow, endrow, startcol, endcol, dataspace_id, memspace_id, subset_dims) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_get_hyperspace_2d" :: h5_get_hyperspace_2d
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: endrow         ! Index of last row of subset
    integer(hsize_t), intent(in)        :: startcol       ! Index of first column of subset
    integer(hsize_t), intent(in)        :: endcol         ! Index of last column of subset
    integer(hid_t), intent(out)         :: dataspace_id   ! Data space identifier
    integer(hid_t), intent(out)         :: memspace_id    ! Memory space identifier
    integer, parameter                  :: memrank = 2    ! Rank of memory space
    integer(hsize_t), &
      dimension(1:memrank), &
      intent(out)                       :: subset_dims    ! Subset dimensions
    integer(hsize_t), &
      dimension(1:memrank)              :: data_dims      ! Dataset dimensions
    integer(hsize_t), &
      dimension(1:memrank)              :: max_dims       ! Maximum dataset dimensions (output)
    integer(hsize_t), &
      dimension(1:memrank)              :: offset         ! Dataset offset (where to start reading the data)
    integer(hsize_t)                    :: nrows_hdf      ! Number of HDF5 rows in subset
    integer(hsize_t)                    :: ncols_hdf      ! Number of HDF5 columns in subset    
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! Get data space from dataset
    call h5dget_space_f(dataset_id, dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get dataset dimensions, data_dims
    call h5sget_simple_extent_dims_f(dataspace_id, data_dims, max_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Compute extents of subset
    ncols_hdf = endcol - startcol + 1
    nrows_hdf = endrow - startrow + 1
    !
    ! If subset extends beyond the dataset boundaries, trim the excess
    ncols_hdf = min(ncols_hdf, data_dims(1))
    nrows_hdf = min(nrows_hdf, data_dims(2))
    !
    ! Set subset dimensions and offset (location of first point of subset in the dataset)
    subset_dims = (/ncols_hdf, nrows_hdf/)
    offset = (/startcol, startrow/)
    !
    ! Create simple memory space
    call h5screate_simple_f(memrank, subset_dims, memspace_id, hdf_error) ! Create memory space
    if (hdf_error == -1) return
    !
    ! Select hyperslab
    call h5sselect_hyperslab_f(dataspace_id, H5S_SELECT_SET_F, offset, subset_dims, hdf_error) ! Set dataspace size
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Helper function: Get memory space and select hyperslab for a 1D dataset
  ! .....................................................................................................
  logical function h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, subset_dims) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_get_hyperspace_1d" :: h5_get_hyperspace_1d
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first point in subset
    integer(hsize_t), intent(in)        :: nvals          ! Number values in subset
    integer(hid_t), intent(out)         :: memspace_id    ! Memory space identifier
    integer, parameter                  :: memrank = 1    ! Rank of memory space
    integer(hsize_t), &
      dimension(1:memrank), &
      intent(out)                       :: subset_dims    ! Subset dimensions
    integer(hsize_t), &
      dimension(1:memrank)              :: data_dims      ! Dataset dimensions
    integer(hsize_t), &
      dimension(1:memrank)              :: max_dims       ! Maximum dataset dimensions (output)
    integer(hid_t)                      :: dataspace_id   ! Data space identifier
    integer(hsize_t), &
      dimension(1:memrank)              :: offset         ! Dataset offset (where to start reading the data)
    integer(hsize_t)                    :: ncols_hdf      ! Number of HDF5 columns in subset    
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! Get data space from dataset
    call h5dget_space_f(dataset_id, dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get dataset dimensions, data_dims
    call h5sget_simple_extent_dims_f(dataspace_id, data_dims, max_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set subset dimensions and offset (location of first point of subset in the dataset)
    ! If subset extends beyond the dataset boundaries, trim the excess
    subset_dims(1) = min(nvals, data_dims(1))
    offset = (/startrow/)
    !
    ! Create simple memory space
    call h5screate_simple_f(memrank, subset_dims, memspace_id, hdf_error) ! Create memory space
    if (hdf_error == -1) return
    !
    ! Select hyperslab
    call h5sselect_hyperslab_f(dataspace_id, H5S_SELECT_SET_F, offset, subset_dims, hdf_error) ! Set dataspace size
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 2D HDF5 dataset: Real type
  !
  ! .....................................................................................................
  ! This function can read an entire dataset or a subset, including row and column vectors
  ! 
  ! Examples:
  ! Entire dataset, 100 rows x 50 columns: 
  !   startrow = 0, startcol = 0, nrows_hdf = 100, ncols_hdf = 50
  ! Subset of dataset at starting at (row, col) = (5,7), 30 rows x 20 columns: 
  !   startrow = 5, startcol = 7, nrows_hdf = 30, ncols_hdf = 20
  ! Row vector at index 5, 100 values (e.g. write all cross-section data for a time step):
  !   startrow = 5, startcol = 0, nrows_hdf = 1, ncols_hdf = 100
  ! Column vector at index 5, 100 values (e.g., write time series for a cross-section): 
  !   startrow = 0, startcol = 5, nrows_hdf = 100, ncols_hdf = 1
  ! .....................................................................................................
  logical function h5_read_dataset_2d_array_r(dataset_id, startrow, startcol, nrows_hdf, ncols_hdf, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_dataset_2d_array_r" :: h5_read_dataset_2d_array_r
    !
    integer(hid_t),   intent(in)        :: dataset_id     ! Identifier of open dataset
    integer,          intent(in)        :: startrow       ! Index of first row of subset
    integer,          intent(in)        :: startcol       ! Index of first column of subset
    integer,          intent(in)        :: nrows_hdf      ! Number of rows in HDF5 dataset (Fortran columns)     
    integer,          intent(in)        :: ncols_hdf      ! Number of columns in HDF5 dataset (Fortran rows)
    real(kind=c_float), &
      dimension(ncols_hdf, nrows_hdf), &
      intent(inout)                     :: hdf_array      ! Data array to read and return (must first be allocated by calling function)
    
    integer(hsize_t)                    :: startrow_h5t   ! Index of first row of subset(hsize_t)
    integer(hsize_t)                    :: startcol_h5t   ! Index of first column of subset(hsize_t)
    
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t)                    :: endcol         ! Index of last column of subset
    integer(hsize_t), dimension(2)      :: data_dims      ! Data array dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !    
    success = .false.
    
    startrow_h5t = startrow
    startcol_h5t = startcol
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    endrow = startrow_h5t + nrows_hdf - 1
    endcol = startcol_h5t + ncols_hdf - 1
    !is_ok = h5_get_hyperspace_2d(dataset_id, memspace_id, dataspace_id, startrow, endrow, startcol, endcol, data_dims)
    is_ok = h5_get_hyperspace_2d(dataset_id, startrow_h5t, endrow, startcol_h5t, endcol, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Read entire 2D dataset
    call h5dread_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close resources
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 2D HDF5 dataset: Integer type
  !
  ! .....................................................................................................
  ! This function can read an entire dataset or a subset, including row and column vectors
  ! 
  ! Examples:
  ! Entire dataset, 100 rows x 50 columns: 
  !   startrow = 0, startcol = 0, nrows_hdf = 100, ncols_hdf = 50
  ! Subset of dataset at starting at (row, col) = (5,7), 30 rows x 20 columns: 
  !   startrow = 5, startcol = 7, nrows_hdf = 30, ncols_hdf = 20
  ! Row vector at index 5, 100 values (e.g. write all cross-section data for a time step):
  !   startrow = 5, startcol = 0, nrows_hdf = 1, ncols_hdf = 100
  ! Column vector at index 5, 100 values (e.g., write time series for a cross-section): 
  !   startrow = 0, startcol = 5, nrows_hdf = 100, ncols_hdf = 1
  ! .....................................................................................................
  logical function h5_read_dataset_2d_array_i(dataset_id, startrow, startcol, nrows_hdf, ncols_hdf, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_dataset_2d_array_i" :: h5_read_dataset_2d_array_i
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: startcol       ! Index of first column of subset
    integer(hsize_t), intent(in)        :: nrows_hdf      ! Number of rows in HDF5 dataset (Fortran columns)     
    integer(hsize_t), intent(in)        :: ncols_hdf      ! Number of columns in HDF5 dataset (Fortran rows)
    integer(kind=c_int), &
      dimension(ncols_hdf, nrows_hdf), &
      intent(inout)                     :: hdf_array      ! Data array to read and return (must first be allocated by calling function)
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t)                    :: endcol         ! Index of last column of subset
    integer(hsize_t), dimension(2)      :: data_dims      ! Data array dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    endrow = startrow + nrows_hdf - 1
    endcol = startcol + ncols_hdf - 1
    !is_ok = h5_get_hyperspace_2d(dataset_id, memspace_id, dataspace_id, startrow, endrow, startcol, endcol, data_dims)
    is_ok = h5_get_hyperspace_2d(dataset_id, startrow, endrow, startcol, endcol, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Read entire 2D dataset
    call h5dread_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close dataset
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 1D HDF5 dataset: Real type
  ! .....................................................................................................
  logical function h5_read_dataset_1d_array_r(dataset_id, startrow, nvals, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_dataset_1d_array_r" :: h5_read_dataset_1d_array_r
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: nvals          ! Number of values in HDF5 dataset
    real(kind=c_float), &
      dimension(nvals), intent(inout)   :: hdf_array      ! Data array to read and return (must first be allocated by calling function)
    integer(hsize_t), dimension(1)      :: data_dims      ! Data array dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t)                    :: startcol       ! Index of first column of subset
    integer(hsize_t)                    :: endcol         ! Index of last column of subset
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    !is_ok = h5_get_hyperspace_1d(dataset_id, memspace_id, dataspace_id, startrow, nvals, data_dims)
    is_ok = h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Read entire 2D dataset
    call h5dread_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close dataset
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 1D HDF5 dataset: Double type
  ! .....................................................................................................
  logical function h5_read_dataset_1d_array_d(dataset_id, startrow, nvals, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_dataset_1d_array_d" :: h5_read_dataset_1d_array_d
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: nvals          ! Number of values in HDF5 dataset
    real(kind=8), &
      dimension(nvals), intent(inout)   :: hdf_array      ! Data array to read and return (must first be allocated by calling function)
    integer(hsize_t), dimension(1)      :: data_dims      ! Data array dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t)                    :: startcol       ! Index of first column of subset
    integer(hsize_t)                    :: endcol         ! Index of last column of subset
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    !is_ok = h5_get_hyperspace_1d(dataset_id, memspace_id, dataspace_id, startrow, nvals, data_dims)
    is_ok = h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Read entire 2D dataset
    call h5dread_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close dataset
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 1D HDF5 dataset: Integer type
  ! .....................................................................................................
  logical function h5_read_dataset_1d_array_i(dataset_id, startrow, nvals, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_dataset_1d_array_i" :: h5_read_dataset_1d_array_i
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: nvals          ! Number of values in HDF5 dataset
    real(kind=c_int), &
      dimension(nvals), intent(inout)   :: hdf_array      ! Data array to read and return (must first be allocated by calling function)
    integer(hsize_t), dimension(1)      :: data_dims      ! Data array dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t)                    :: startcol       ! Index of first column of subset
    integer(hsize_t)                    :: endcol         ! Index of last column of subset
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    !is_ok = h5_get_hyperspace_1d(dataset_id, memspace_id, dataspace_id, startrow, nvals, data_dims)
    is_ok = h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Read entire 2D dataset
    call h5dread_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close dataset
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Read 1D HDF5 dataset: String type
  ! .....................................................................................................
  logical function h5_read_dataset_1d_array_c(dataset_id, startrow, nvals, str_len, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_dataset_1d_array_c" :: h5_read_dataset_1d_array_c
    !
    ! Note: In .Net, use a StringBuilder object to pass the array of strings
    ! C# Example:
    ! var SB = new StringBuilder(nvals * str_len)
    ! SB = H5Interface.return_string_array(SB, ref str_len, ref nvals)
    ! Then parse SB to a string[] array in C#.
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Dataset identifier
    integer(hsize_t), intent(in)        :: startrow       ! Index of first value to read
    integer(hsize_t), intent(in)        :: nvals          ! Number of values to read
    integer(size_t), intent(in)         :: str_len        ! Length of strings in data array
    character(len=str_len), &
      dimension(nvals), &
      intent(out), target               :: hdf_array      ! Data array (should be allocated by caller)
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hsize_t), dimension(1:1)    :: data_dims      ! Data dimensions
    integer(hsize_t), dimension(1:1)    :: subset_dims      ! Data dimensions
    integer(hsize_t), dimension(1:1)    :: max_dims       ! Maximum dataset dimensions
    integer(hsize_t), dimension(1:1)    :: offset
    integer                             :: i              ! Array index, for writing results
    integer(size_t)                     :: data_type_size ! Size of data type
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data    
    integer                             :: hdf_error      ! HDF5 error flag
    integer                             :: logfile = 108
    !
    success = .false.
    !
    subset_dims = (/nvals/)
    offset = (/startrow/)
    !
    ! Open log file for output
    if (DEBUG) open(logfile, file = LOGFILE)
    !
    ! Get the data type
    call h5dget_type_f(dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., 
    ! the string length of the stored dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Make sure the declared length is large enough
    if (data_type_size > str_len) then
      if (DEBUG) write(logfile,*) 'ERROR: Character LEN is to small'
    endif
    !
    ! Get data space
    call h5dget_space_f(dataset_id, dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the dataset dimensions
    call h5sget_simple_extent_dims_f(dataspace_id, data_dims, max_dims, hdf_error)
    if (hdf_error == -1) return
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
    !call h5sselect_hyperslab_f(dataspace_id, H5S_SELECT_SET_F, offset, subset_dims, hdf_error) ! Set dataspace size
    f_ptr = C_LOC(hdf_array(1)(1:1))
    call h5dread_f(dataset_id, memory_type, f_ptr, hdf_error, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    do i = 1, data_dims(1)
       if (DEBUG) write(logfile,'(A,"(",I0,"): ", A)') "data", i, trim(hdf_array(i))
    end do
    !
    if (DEBUG) close(logfile)
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Create 2D HDF5 dataset (compression and chunking are optional)
  ! .....................................................................................................
  logical function h5_create_2d_dataset(loc_id, dataset_name, nrows_hdf, ncols_hdf, data_type, chunk_in_time, kind_nbytes, compressed, dataset_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_2d_dataset" :: h5_create_2d_dataset
    !
    integer, parameter                  :: rank = 2       ! Rank of the data set
    integer(hid_t), intent(in)          :: loc_id         ! File or group identifier, where dataset will be created
    character(len=*), intent(in)        :: dataset_name   ! Name of the dataset to create
    integer, intent(in)                 :: nrows_hdf      ! Number of rows in HDF5 dataset (C convention: [row, col])
    integer, intent(in)                 :: ncols_hdf      ! Number of columns in HDF5 dataset (C convention: [row, col])
    integer(hid_t), intent(in)          :: data_type      ! Data type for dataset (e.g., H5T_NATIVE_REAL)
    logical, intent(in)                 :: chunk_in_time  ! If true, set time dimension of the output dataset to unlimited. If false, set space dimension.
    integer, intent(in)                 :: kind_nbytes    ! Number of bytes of float and integer types
    logical, intent(in)                 :: compressed     ! If true, chunk and compress the dataset and set one dimension to unlimited
    integer(hid_t), intent(out)         :: dataset_id     ! Dataset identifier
    integer(hsize_t), dimension(1:rank) :: max_dims       ! Maximum dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: dataset_dims   ! Dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: chunk_dims     ! Chunk dimensions
    integer(hid_t)                      :: dataspace_id   ! Data space (memory) identifier
    integer(hid_t)                      :: plist_id       ! Property list identifier
    integer                             :: num_min        ! Used to set minimum dataset size
    integer                             :: num_max        ! Used to set maximum dataset size
    integer                             :: hdf_error      ! HDF error flag    
    integer                             :: nvalues_per_chunk ! HDF5 chunk size (number of values in chunk)
    integer                             :: dim1           ! Alias for ncols_hdf, for clarity below
    integer                             :: dim2           ! Alias for nrows_hdf, for clarity below
    integer, dimension(3)               :: today
    integer, dimension(3)               :: now
    !
    success = .false.
    !
    !if (DEBUG_THIS) call idate(today)   ! today(1)=day, (2)=month, (3)=year
    !if (DEBUG_THIS) call itime(now)     ! now(1)=hour, (2)=minute, (3)=second
    !if (DEBUG_THIS) write ( *, 1000 )  today(2), today(1), today(3), now
    !1000 format ( 'Date ', i2.2, '/', i2.2, '/', i4.4, '; time ', &
    !     i2.2, ':', i2.2, ':', i2.2 )
    !
    ! Set HDF chunk size in number of values (1MB / nbytes_per_value)
    nvalues_per_chunk = NBYTES_PER_CHUNK / kind_nbytes
    !
    ! Fudge down so rounding does not go over. For 4 byte data: 1,000,000 / 4 - 44 = 262100, which is Steve's value.
    ! (1/4 MB = 256 K = 262144 bytes)
    nvalues_per_chunk = nint(nvalues_per_chunk / 100.0) * 100
    !
    ! Initialize output dimensions. These will be adjusted below.
    dim1 = ncols_hdf ! The 1st dimension is the number of HDF columns, i.e., row length
    dim2 = nrows_hdf ! The 2nd dimension is the number of HDF rows, i.e., column length
    max_dims(1:2)     = (/dim1, dim2/)
    dataset_dims(1:2) = (/dim1, dim2/)
    !
    ! Create output property list
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    if (compressed) then
      !------------------------------------------------------------------------------------
      ! Set output dimensions (chunk_dims and max_dims) for compression and chunking
      ! Adapted by Todd Steissberg from Steve Piper's code
      !
      if (chunk_in_time) then 
        ! Chunk in time dimension (chunks will consist of one or more complete HDF5 rows)
        !
        ! Set chunk row length
        chunk_dims(1) = dim1 / NUM_CHUNKS_IN_SPACE
        chunk_dims(1) = max(chunk_dims(1), MIN_VALUES_IN_CHUNK) ! Ensure a minimum number of values along a chunk row
        chunk_dims(1) = min(chunk_dims(1), dim1)                   ! Ensure number of values in chunk row does not exceed number of columns
        num_max = max(nvalues_per_chunk, MIN_VALUES_IN_CHUNK)   ! Ensure minimum number of values
        !
        ! Set chunk column length
        chunk_dims(2) = num_max / chunk_dims(1)
        chunk_dims(2) = max(chunk_dims(2), 1)
        chunk_dims(2) = min(chunk_dims(2), dim2)
        !
        ! Set dimensions to one row and unlimited column length
        dataset_dims(2) = 1
        max_dims(2)     = H5S_UNLIMITED_F
      else
        ! Chunk in space dimension (chunks will consist of one or more complete HDF5 columns)
        !
        ! Set chunk column length
        chunk_dims(2) = dim2 / NUM_CHUNKS_IN_TIME
        chunk_dims(2) = max(chunk_dims(2), MIN_VALUES_IN_CHUNK)  ! Ensure a minimum number of values along a chunk row
        chunk_dims(2) = min(chunk_dims(2), dim2)                    ! Ensure number of values in chunk row does not exceed number of columns
        num_max = max(nvalues_per_chunk, MIN_VALUES_IN_CHUNK)    ! Ensure minimum number of values
        !
        ! Set chunk row length
        if (chunk_dims(2) /= 0) then
          chunk_dims(1) = num_max / chunk_dims(2)
        else
          chunk_dims(1) = 1
        end if
        chunk_dims(1) = max(chunk_dims(1), 1)
        chunk_dims(1) = min(chunk_dims(1), dim1)
        !
        ! Set dimensions to one column and unlimited row length
        dataset_dims(1) = 1
        max_dims(1)     = H5S_UNLIMITED_F
      end if
      !
      ! Set dataset chunking. Dataset must be chunked for compression.
      call h5pset_chunk_f(plist_id, rank, chunk_dims, hdf_error)
      if (hdf_error == -1) return
      !
      ! Set ZLIB / DEFLATE Compression
      call h5pset_deflate_f(plist_id, COMPRESSION_LEVEL, hdf_error)
      if (hdf_error == -1) return
      !
    end if
    !
    ! Create simple data space
    call h5screate_simple_f(rank, dataset_dims, dataspace_id, hdf_error, max_dims)
    if (hdf_error == -1) return
    !
    ! Create data set
    call h5dcreate_f(loc_id, dataset_name, data_type, dataspace_id, dataset_id, hdf_error, &
      dcpl_id=plist_id)
    if (hdf_error == -1) return
    !
    ! Release resources
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    ! 
    call h5pclose_f(plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Create 1D compressed HDF5 dataset
  ! .....................................................................................................
  logical function h5_create_compressed_1d_dataset(loc_id, dataset_name, nvals, &
      data_type, kind_nbytes, dataset_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_compressed_1d_dataset" :: h5_create_compressed_1d_dataset
    !
    integer, parameter                  :: rank = 1       ! Rank of the data set
    integer(hid_t), intent(in)          :: loc_id         ! File or group identifier, where dataset will be created
    character(len=*), intent(in)        :: dataset_name   ! Name of the dataset to create
    integer(hsize_t), intent(in)        :: nvals          ! Length of data array
    integer(hid_t), intent(inout)       :: data_type      ! Data type for dataset (e.g., H5T_NATIVE_REAL)
    integer, intent(in)                 :: kind_nbytes    ! Number of bytes of float and integer types
    integer(hid_t), intent(out)         :: dataset_id     ! Dataset identifier
    integer(hsize_t), dimension(1:rank) :: max_dims       ! Maximum dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: dataset_dims   ! Dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: chunk_dims     ! Chunk dimensions
    integer(hid_t)                      :: dataspace_id   ! Data space (memory) identifier
    integer(hid_t)                      :: plist_id       ! Property list identifier
    integer                             :: num_min        ! Used to set minimum dataset size
    integer                             :: num_max        ! Used to set maximum dataset size
    integer                             :: hdf_error      ! HDF error flag    
    integer, dimension(3)               :: today
    integer, dimension(3)               :: now
    integer                             :: nvalues_per_chunk ! HDF5 chunk size (number of values in chunk)
    !
    success = .false.
    !
    ! Set HDF chunk size in number of values (1MB / nbytes_per_value)
    nvalues_per_chunk = NBYTES_PER_CHUNK / kind_nbytes
    !
    ! Fudge down so rounding does not go over. For 4 byte data: 1,000,000 / 4 - 44 = 262100, which is Steve's value.
    ! (1/4 MB = 256 K = 262144 bytes)
    nvalues_per_chunk = nint(nvalues_per_chunk / 100.0) * 100
    !
    !------------------------------------------------------------------------------------
    ! Set output dimensions (chunk_dims and max_dims) for compression and chunking
    ! Adapted by Todd Steissberg from Steve Piper's code
    !
    ! Set chunk length
    chunk_dims(1) = nvals / NUM_CHUNKS_IN_ARRAY
    chunk_dims(1) = max(chunk_dims(1), MIN_VALUES_IN_CHUNK) ! Ensure a minimum number of values along a chunk row
    chunk_dims(1) = min(chunk_dims(1), nvals)                  ! Ensure number of values in chunk row does not exceed number of columns
    num_max = max(nvalues_per_chunk, MIN_VALUES_IN_CHUNK)   ! Ensure minimum number of values
    !
    ! Set dimensions to one row/column and unlimited column length
    dataset_dims(1) = 1 ! One row or column
    !dataset_dims(1) = nvals ! Alternatively, set to size it ultimately needs to be
    max_dims(1) = H5S_UNLIMITED_F ! Unlimited
    !max_dims(1) = nvals ! Alternatively, set max size to be limited to nvals
    !
    ! Create simple data space
    call h5screate_simple_f(rank, dataset_dims, dataspace_id, hdf_error, max_dims)
    if (hdf_error == -1) return
    !
    ! Create output property list
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set dataset chunking. Dataset must be chunked for compression.
    call h5pset_chunk_f(plist_id, rank, chunk_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set ZLIB / DEFLATE Compression
    call h5pset_deflate_f(plist_id, COMPRESSION_LEVEL, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create data set
    call h5dcreate_f(loc_id, dataset_name, data_type, dataspace_id, dataset_id, hdf_error, &
      dcpl_id=plist_id)
    if (hdf_error == -1) return
    !
    ! Release resources
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    ! 
    call h5pclose_f(plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Create 1D HDF5 dataset (chunking are optional)
  ! .....................................................................................................
  logical function h5_create_1d_dataset(loc_id, dataset_name, nvals, data_type, kind_nbytes, compressed, dataset_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_1d_dataset" :: h5_create_1d_dataset
    !
    integer, parameter                  :: rank = 1       ! Rank of the data set
    integer(hid_t), intent(in)          :: loc_id         ! File or group identifier, where dataset will be created
    character(len=*), intent(in)        :: dataset_name   ! Name of the dataset to create
    integer(hsize_t), intent(in)        :: nvals          ! Length of data array
    integer(hid_t), intent(inout)       :: data_type      ! Data type for dataset (e.g., H5T_NATIVE_REAL)
    integer, intent(in)                 :: kind_nbytes    ! Number of bytes of float and integer types
    logical, intent(in)                 :: compressed     ! If true, chunk and compress the dataset
    integer(hid_t), intent(out)         :: dataset_id     ! Dataset identifier
    integer(hsize_t), dimension(1:rank) :: max_dims       ! Maximum dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: dataset_dims   ! Dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: chunk_dims     ! Chunk dimensions
    integer(hid_t)                      :: dataspace_id   ! Data space (memory) identifier
    integer(hid_t)                      :: plist_id       ! Property list identifier
    integer                             :: num_min        ! Used to set minimum dataset size
    integer                             :: num_max        ! Used to set maximum dataset size
    integer                             :: hdf_error      ! HDF error flag    
    integer, dimension(3)               :: today
    integer, dimension(3)               :: now
    integer                             :: nvalues_per_chunk ! HDF5 chunk size (number of values in chunk)
    !
    success = .false.
    !
    ! Create output property list
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Optionally, chunk and compress the dataset
    if (compressed) then
      !------------------------------------------------------------------------------------
      ! Set output dimensions (chunk_dims and max_dims) for compression and chunking
      ! Adapted by Todd Steissberg from Steve Piper's code
      !
      ! Set HDF chunk size in number of values (1MB / nbytes_per_value)
      nvalues_per_chunk = NBYTES_PER_CHUNK / kind_nbytes
      !
      ! Fudge down so rounding does not go over. For 4 byte data: 1,000,000 / 4 - 44 = 262100, which is Steve's value.
      ! (1/4 MB = 256 K = 262144 bytes)
      nvalues_per_chunk = nint(nvalues_per_chunk / 100.0) * 100
      !
      ! Set chunk length
      chunk_dims(1) = nvals / NUM_CHUNKS_IN_SPACE
      chunk_dims(1) = max(chunk_dims(1), MIN_VALUES_IN_CHUNK) ! Ensure a minimum number of values along a chunk row
      chunk_dims(1) = min(chunk_dims(1), nvals)                  ! Ensure number of values in chunk row does not exceed number of columns
      num_max = max(nvalues_per_chunk, MIN_VALUES_IN_CHUNK)   ! Ensure minimum number of values
      !
      ! Set dimensions to one row/column and unlimited column length
      dataset_dims(1) = 1           ! Create the dataset with only one value
      max_dims(1) = H5S_UNLIMITED_F ! Set the maximum number of values to unlimited
      !
      ! Set dataset chunking. Dataset must be chunked for compression.
      call h5pset_chunk_f(plist_id, rank, chunk_dims, hdf_error)
      if (hdf_error == -1) return
      !
      ! Set ZLIB / DEFLATE Compression
      call h5pset_deflate_f(plist_id, COMPRESSION_LEVEL, hdf_error)
      if (hdf_error == -1) return
    else
      ! Set dimensions and maximum dimensions to the specified number of values
      dataset_dims(1) = nvals
      max_dims(1) = nvals
    end if
    !
    ! Create simple data space
    call h5screate_simple_f(rank, dataset_dims, dataspace_id, hdf_error, max_dims)
    if (hdf_error == -1) return
    !
    ! Create data set
    call h5dcreate_f(loc_id, dataset_name, data_type, dataspace_id, dataset_id, hdf_error, dcpl_id=plist_id)
    if (hdf_error == -1) return
    !
    ! Release resources
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    ! 
    call h5pclose_f(plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Create 1D HDF5 dataset (chunking are optional): Character type
  ! .....................................................................................................
  logical function h5_create_1d_dataset_c(loc_id, dataset_name, nvals, str_len, compressed, dataset_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_1d_dataset_c" :: h5_create_1d_dataset_c
    !
    integer, parameter                  :: rank = 1       ! Rank of the data set
    integer(hid_t), intent(in)          :: loc_id         ! File or group identifier, where dataset will be created
    character(len=*), intent(in)        :: dataset_name   ! Name of the dataset to create
    integer(hsize_t), intent(in)        :: nvals          ! Length of data array
    integer(hsize_t), intent(in)        :: str_len        ! String length in arrays
    integer(hid_t)                      :: data_type      ! Data type for dataset (e.g., H5T_NATIVE_REAL)
    logical, intent(in)                 :: compressed     ! If true, chunk and compress the dataset
    integer(hid_t), intent(out)         :: dataset_id     ! Dataset identifier
    integer                             :: kind_nbytes = 1! Number of bytes per character
    integer(hsize_t), dimension(1:rank) :: max_dims       ! Maximum dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: dataset_dims   ! Dataset dimensions (output)
    integer(hsize_t), dimension(1:rank) :: chunk_dims     ! Chunk dimensions
    integer(hid_t)                      :: dataspace_id   ! Data space (memory) identifier
    integer(hid_t)                      :: plist_id       ! Property list identifier
    integer                             :: num_min        ! Used to set minimum dataset size
    integer                             :: num_max        ! Used to set maximum dataset size
    integer                             :: hdf_error      ! HDF error flag    
    integer, dimension(3)               :: today
    integer, dimension(3)               :: now
    integer                             :: nvalues_per_chunk ! HDF5 chunk size (number of values in chunk)
    integer                             :: nbytes_per_string ! Number of bytes per string
    !
    success = .false.
    !
    ! Create the memory data type
    call h5tcopy_f(H5T_FORTRAN_S1, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set the memory size using the string length
    call h5tset_size_f(data_type, str_len, hdf_error)
    if (hdf_error == -1) return
    !
    ! Create output property list
    call h5pcreate_f(H5P_DATASET_CREATE_F, plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    ! Optionally, chunk and compress the dataset
    if (compressed) then
      !------------------------------------------------------------------------------------
      ! Set output dimensions (chunk_dims and max_dims) for compression and chunking
      ! Adapted by Todd Steissberg from Steve Piper's code
      !
      ! Set HDF chunk size in number of values (1MB / nbytes_per_value)
      !
      ! Number of values per chunk  = 1 MB / nbytes_per_string
      ! Each character is 1 byte, but character strings are aligned on 2 byte boundaries
      nbytes_per_string = kind_nbytes * str_len
      if (mod(nbytes_per_string, 2) /= 0) then
        nbytes_per_string = nbytes_per_string + 1
      end if
      nvalues_per_chunk = NBYTES_PER_CHUNK / nbytes_per_string
      !
      ! Fudge down so rounding does not go over. For 4 byte data: 1,000,000 / 4 - 44 = 262100, which is Steve's value.
      ! (1/4 MB = 256 K = 262144 bytes)
      nvalues_per_chunk = nint(nvalues_per_chunk / 100.0) * 100
      !
      ! Set chunk length
      chunk_dims(1) = nvals / NUM_CHUNKS_IN_SPACE
      chunk_dims(1) = max(chunk_dims(1), MIN_VALUES_IN_CHUNK) ! Ensure a minimum number of values along a chunk row
      chunk_dims(1) = min(chunk_dims(1), nvals)                  ! Ensure number of values in chunk row does not exceed number of columns
      num_max = max(nvalues_per_chunk, MIN_VALUES_IN_CHUNK)   ! Ensure minimum number of values
      !
      ! Set dimensions to one row/column and unlimited column length
      dataset_dims(1) = 1           ! Create the dataset with only one value
      max_dims(1) = H5S_UNLIMITED_F ! Set the maximum number of values to unlimited
      !
      ! Set dataset chunking. Dataset must be chunked for compression.
      call h5pset_chunk_f(plist_id, rank, chunk_dims, hdf_error)
      if (hdf_error == -1) return
      !
      ! Set ZLIB / DEFLATE Compression
      call h5pset_deflate_f(plist_id, COMPRESSION_LEVEL, hdf_error)
      if (hdf_error == -1) return
    else
      ! Set dimensions and maximum dimensions to the specified number of values
      dataset_dims(1) = nvals
      max_dims(1) = nvals
    end if
    !
    ! Create simple data space
    call h5screate_simple_f(rank, dataset_dims, dataspace_id, hdf_error, max_dims)
    if (hdf_error == -1) return
    !
    ! Create data set
    call h5dcreate_f(loc_id, dataset_name, data_type, dataspace_id, dataset_id, hdf_error, dcpl_id=plist_id)
    if (hdf_error == -1) return
    !
    ! Release resources
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    ! 
    call h5pclose_f(plist_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write 2D HDF5 dataset: Real type
  !
  ! .....................................................................................................
  ! This function can write an entire dataset or a subset, including row and column vectors
  ! 
  ! Examples:
  ! Entire dataset, 100 rows x 50 columns: 
  !   startrow = 0, startcol = 0, nrows_hdf = 100, ncols_hdf = 50
  ! Subset of dataset at starting at (row, col) = (5,7), 30 rows x 20 columns: 
  !   startrow = 5, startcol = 7, nrows_hdf = 30, ncols_hdf = 20
  ! Row vector at index 5, 100 values (e.g. write all cross-section data for a time step):
  !   startrow = 5, startcol = 0, nrows_hdf = 1, ncols_hdf = 100
  ! Column vector at index 5, 100 values (e.g., write time series for a cross-section): 
  !   startrow = 0, startcol = 5, nrows_hdf = 100, ncols_hdf = 1
  ! .....................................................................................................
  logical function h5_write_dataset_2d_array_r(dataset_id, startrow, startcol, nrows_hdf, ncols_hdf, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_dataset_2d_array_r" :: h5_write_dataset_2d_array_r
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: startcol       ! Index of first column of subset
    integer(hsize_t), intent(in)        :: nrows_hdf      ! Number of rows in HDF5 dataset (Fortran columns)     
    integer(hsize_t), intent(in)        :: ncols_hdf      ! Number of columns in HDF5 dataset (Fortran rows)
    real(kind=c_float), &
      dimension(ncols_hdf, nrows_hdf), &
      intent(inout)                     :: hdf_array      ! Data array to write and return (must first be allocated by calling function)
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t)                    :: endcol         ! Index of last column of subset
    integer(hsize_t), dimension(2)      :: data_dims      ! Data array dimensions
    integer(hsize_t), dimension(2)      :: dataset_dims   ! Dataset dimensions
    integer(hsize_t), dimension(2)      :: max_dims       ! Maximum dataset dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer, parameter                  :: rank = 2       ! Dataset rank
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get current dataset dimensions
    is_ok = h5_get_dataset_dimensions(dataset_id, dataset_dims, max_dims, rank)
    if (.not. is_ok) return
    !
    ! Extend dataset - before getting hyperslab
    endrow = startrow + nrows_hdf - 1
    endcol = startcol + ncols_hdf - 1
    if ((endcol + 1) > dataset_dims(1)) then
      dataset_dims(1) = endcol + 1
    end if
    if ((endrow + 1) > dataset_dims(2)) then
      dataset_dims(2) = endrow + 1
    end if
    !
    call h5dset_extent_f(dataset_id, dataset_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    !is_ok = h5_get_hyperspace_2d(dataset_id, memspace_id, dataspace_id, startrow, endrow, startcol, endcol, data_dims)
    is_ok = h5_get_hyperspace_2d(dataset_id, startrow, endrow, startcol, endcol, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Write data to dataset (region may be a subset of the entire dataset, e.g., a row or column vector)
    call h5dwrite_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close resources
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return

    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return

    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write 2D HDF5 dataset: Integer type
  !
  ! .....................................................................................................
  ! This function can write an entire dataset or a subset, including row and column vectors
  ! 
  ! Examples:
  ! Entire dataset, 100 rows x 50 columns: 
  !   startrow = 0, startcol = 0, nrows_hdf = 100, ncols_hdf = 50
  ! Subset of dataset at starting at (row, col) = (5,7), 30 rows x 20 columns: 
  !   startrow = 5, startcol = 7, nrows_hdf = 30, ncols_hdf = 20
  ! Row vector at index 5, 100 values (e.g. write all cross-section data for a time step):
  !   startrow = 5, startcol = 0, nrows_hdf = 1, ncols_hdf = 100
  ! Column vector at index 5, 100 values (e.g., write time series for a cross-section): 
  !   startrow = 0, startcol = 5, nrows_hdf = 100, ncols_hdf = 1
  ! .....................................................................................................
  logical function h5_write_dataset_2d_array_i(dataset_id, startrow, startcol, nrows_hdf, ncols_hdf, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_dataset_2d_array_i" :: h5_write_dataset_2d_array_i
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: startcol       ! Index of first column of subset
    integer(hsize_t), intent(in)        :: nrows_hdf      ! Number of rows in HDF5 dataset (Fortran columns)     
    integer(hsize_t), intent(in)        :: ncols_hdf      ! Number of columns in HDF5 dataset (Fortran rows)
    integer(kind=c_int), &
      dimension(ncols_hdf, nrows_hdf), &
      intent(inout)                     :: hdf_array      ! Data array to write and return (must first be allocated by calling function)
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t)                    :: endcol         ! Index of last column of subset
    integer(hsize_t), dimension(2)      :: data_dims      ! Data array dimensions
    integer(hsize_t), dimension(2)      :: dataset_dims   ! Dataset dimensions
    integer(hsize_t), dimension(2)      :: max_dims       ! Maximum dataset dimensions
    integer, parameter                  :: rank = 2       ! Dataset rank
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f(dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get current dataset dimensions
    is_ok = h5_get_dataset_dimensions(dataset_id, dataset_dims, max_dims, rank)
    if (.not. is_ok) return
    !
    ! Extend dataset - before getting hyperslab
    endrow = startrow + nrows_hdf - 1
    endcol = startcol + ncols_hdf - 1
    if ((endcol + 1) > dataset_dims(1)) then
      dataset_dims(1) = endcol + 1
    end if
    if ((endrow + 1) > dataset_dims(2)) then
      dataset_dims(2) = endrow + 1
    end if
    !
    call h5dset_extent_f(dataset_id, dataset_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    !is_ok = h5_get_hyperspace_2d(dataset_id, memspace_id, dataspace_id, startrow, endrow, startcol, endcol, data_dims)
    is_ok = h5_get_hyperspace_2d(dataset_id, startrow, endrow, startcol, endcol, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Write data to dataset (region may be a subset of the entire dataset, e.g., a row or column vector)
    call h5dwrite_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close resources
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return

    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return

    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write 1D HDF5 dataset: String type
  ! .....................................................................................................
  logical function h5_write_dataset_1d_array_c(dataset_id, startrow, nvals, str_len, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_dataset_1d_array_c" :: h5_write_dataset_1d_array_c
    !
    ! Note: In .Net, use a StringBuilder object to pass the array of strings
    ! C# Example:
    ! var SB = new StringBuilder(nvals * str_len)
    ! SB = H5Interface.return_string_array(SB, ref str_len, ref nvals)
    ! Then parse SB to a string[] array in C#.
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Dataset identifier
    integer(hsize_t), intent(in)        :: startrow       ! Index of first value to write
    integer(hsize_t), intent(in)        :: nvals          ! Number of values in data array
    integer(size_t), intent(in)         :: str_len        ! Length of strings in data array
    character(len=str_len), &
      dimension(nvals), &
      intent(inout), target             :: hdf_array      ! Data array (should be allocated by caller)
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hsize_t), dimension(1)      :: data_dims      ! Dimensions of input data array
    integer(hsize_t), dimension(1)      :: dataset_dims   ! Dimensions of the dataset
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(size_t)                     :: data_type_size ! Size of data type
    integer                             :: rank = 1       ! Rank of array and dataset
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    logical                             :: is_ok          ! Did the function run without error?
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! Set parameters to write the entire 1D array
    data_dims(1) = nvals
    !
    ! Get dataset type
    call h5dget_type_f(dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., the string length of the existing dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Make sure the declared length is large enough
    if (data_type_size > str_len) then
       if (DEBUG) write(*,*) 'ERROR: Character LEN is to small'
    endif
    !
    ! Get current dataset dimensions
    is_ok = h5_get_dataset_dimensions(dataset_id, dataset_dims, max_dims, rank)
    if (.not. is_ok) return
    !
    ! Extend if the data array is larger than the dataset, extend the dataset
    ! This must be done before getting the hyperslab
    if (nvals > dataset_dims(1)) then
      dataset_dims(1) = nvals
      call h5dset_extent_f(dataset_id, dataset_dims, hdf_error)
      if (hdf_error == -1) return
    end if
    !
    ! Select hyperslab and allocate memory space
    !is_ok = h5_get_hyperspace_1d(dataset_id, memspace_id, dataspace_id, index, nvals, data_dims)
    is_ok = h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Create the memory data type
    call h5tcopy_f(H5T_FORTRAN_S1, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set the memory size using the string length
    call h5tset_size_f(memory_type, str_len, hdf_error)
    if (hdf_error == -1) return
    !
    ! Write the data using a pointer
    !f_ptr = C_LOC(hdf_array(1)(1:1))
    !call h5dwrite_f(dataset_id, memory_type, f_ptr, data_dims, hdf_error, memspace_id, dataspace_id)
    call h5dwrite_f(dataset_id, memory_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write one value from a 1D string array
  ! .....................................................................................................
  logical function h5_write_dataset_1d_array_oneval_c(dataset_id, startrow, instring, str_len) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_dataset_1d_array_oneval_c" :: h5_write_dataset_1d_array_oneval_c
    !
    ! Note: In .Net, use a StringBuilder object to pass the array of strings
    ! C# Example:
    ! var SB = new StringBuilder(nvals * str_len)
    ! SB = H5Interface.return_string_array(SB, ref str_len, ref nvals)
    ! Then parse SB to a string[] array in C#.
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Dataset identifier
    integer(hsize_t), intent(in)        :: startrow       ! Row or column index in 2D array
    integer(size_t), intent(in)         :: str_len        ! Length of strings in data array
    character(len=str_len)              :: instring       ! String to write to dataset
    integer(hsize_t)                    :: nvals          ! Length of subset (one value only)
    character(len=str_len), &
      dimension(1)                      :: hdf_array      ! Data array to write
    integer(hid_t)                      :: data_type      ! Data type flag
    integer(hid_t)                      :: memory_type    ! Memory type flag
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer(hsize_t), dimension(1)      :: subset_dims    ! Dimensions of subset two write (one value only)
    integer(hsize_t), dimension(1)      :: dataset_dims   ! Dimensions of dataset (will be extended as necessary)
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer                             :: i              ! Array index, for writing results
    integer(size_t)                     :: data_type_size ! Size of data type
    type(C_PTR)                         :: f_ptr          ! Pointer to array to fetch data
    logical                             :: is_ok          ! Did the function run without error?
    integer                             :: hdf_error      ! HDF5 error flag
    !
    success = .false.
    !
    ! Set parameters to write only one string
    hdf_array(1) = instring
    nvals = 1
    subset_dims(1) = nvals
    !
    ! Get dataset type
    call h5dget_type_f(dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get the size of the data type, i.e., the string length of the existing dataset
    call h5tget_size_f(data_type, data_type_size, hdf_error)
    if (hdf_error == -1) return
    !
    ! Make sure the declared length is large enough
    if (data_type_size > str_len) then
       if (DEBUG) write(*,*) 'ERROR: Character LEN is to small'
    endif
    !
    ! Extend dataset - before getting hyperslab
    dataset_dims(1) = startrow + 1
    call h5dset_extent_f(dataset_id, dataset_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    !is_ok = h5_get_hyperspace_1d(dataset_id, memspace_id, dataspace_id, index, nvals, subset_dims)
    is_ok = h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, subset_dims)
    if (.not. is_ok) return
    !
    ! Create the memory data type
    call h5tcopy_f(H5T_FORTRAN_S1, memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Set the memory size using the string length
    call h5tset_size_f(memory_type, str_len, hdf_error)
    if (hdf_error == -1) return
    !
    ! Write the data using a pointer
    call h5dwrite_f(dataset_id, memory_type, hdf_array, subset_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(memory_type, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write 1D HDF5 dataset: Real type
  !
  ! .....................................................................................................
  ! This function can write an entire 1D dataset or a subset.
  !
  ! .....................................................................................................
  logical function h5_write_dataset_1d_array_r(dataset_id, startrow, nvals, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_dataset_1d_array_r" :: h5_write_dataset_1d_array_r
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: nvals      ! Number of rows in HDF5 dataset (Fortran columns)     
    real(kind=c_float), &
      dimension(nvals), &
      intent(inout)                     :: hdf_array      ! Data array to write and return (must first be allocated by calling function)
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t), dimension(1)      :: data_dims      ! Data array dimensions
    integer(hsize_t), dimension(1)      :: dataset_dims   ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer, parameter                  :: rank = 2       ! Dataset rank
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get current dataset dimensions
    is_ok = h5_get_dataset_dimensions(dataset_id, dataset_dims, max_dims, rank)
    if (.not. is_ok) return
    !
    ! Extend dataset - before getting hyperslab
    endrow = startrow + nvals - 1
    if ((endrow + 1) > dataset_dims(1)) then
      dataset_dims(1) = endrow + 1
    end if
    !
    call h5dset_extent_f(dataset_id, dataset_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    is_ok = h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Write data to dataset (region may be a subset of the entire dataset, e.g., a row or column vector)
    call h5dwrite_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Write 1D HDF5 dataset: Integer type
  !
  ! .....................................................................................................
  ! This function can write an entire 1D dataset or a subset.
  !
  ! .....................................................................................................
  logical function h5_write_dataset_1d_array_i(dataset_id, startrow, nvals, hdf_array) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_dataset_1d_array_i" :: h5_write_dataset_1d_array_i
    !
    integer(hid_t), intent(in)          :: dataset_id     ! Identifier of open dataset
    integer(hsize_t), intent(in)        :: startrow       ! Index of first row of subset
    integer(hsize_t), intent(in)        :: nvals          ! Number of rows in HDF5 dataset (Fortran columns)     
    integer(kind=c_int), &
      dimension(nvals), &
      intent(inout)                     :: hdf_array      ! Data array to write and return (must first be allocated by calling function)
    integer(hsize_t)                    :: endrow         ! Index of last row of subset
    integer(hsize_t), dimension(1)      :: data_dims      ! Data array dimensions
    integer(hsize_t), dimension(1)      :: dataset_dims   ! Dataset dimensions
    integer(hsize_t), dimension(1)      :: max_dims       ! Maximum dataset dimensions
    integer(hid_t)                      :: dataspace_id   ! Dataspace identifier
    integer(hid_t)                      :: memspace_id    ! Memory space identifier
    integer, parameter                  :: rank = 2       ! Dataset rank
    integer(hid_t)                      :: data_type      ! Data type of dataset, retrieved from existing dataset's properties
    integer                             :: hdf_error      ! HDF5 error flag
    logical                             :: is_ok          ! Did the function run without error?
    !
    success = .false.
    !
    ! Get dataset type
    call h5dget_type_f (dataset_id, data_type, hdf_error)
    if (hdf_error == -1) return
    !
    ! Get current dataset dimensions
    is_ok = h5_get_dataset_dimensions(dataset_id, dataset_dims, max_dims, rank)
    if (.not. is_ok) return
    !
    ! Extend dataset - before getting hyperslab
    endrow = startrow + nvals - 1
    if ((endrow + 1) > dataset_dims(1)) then
      dataset_dims(1) = endrow + 1
    end if
    !
    call h5dset_extent_f(dataset_id, dataset_dims, hdf_error)
    if (hdf_error == -1) return
    !
    ! Select hyperslab and allocate memory space
    is_ok = h5_get_hyperspace_1d(dataset_id, startrow, nvals, dataspace_id, memspace_id, data_dims)
    if (.not. is_ok) return
    !
    ! Write data to dataset (region may be a subset of the entire dataset, e.g., a row or column vector)
    call h5dwrite_f(dataset_id, data_type, hdf_array, data_dims, hdf_error, memspace_id, dataspace_id)
    if (hdf_error == -1) return
    !
    ! Close and release resources
    call h5tclose_f(data_type, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(memspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    call h5sclose_f(dataspace_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  !
  ! .....................................................................................................
  !
  ! Test if the type is a Fortran string
  ! .....................................................................................................
  !
  logical function is_string(dtype_id) result(isequal)
    integer(hid_t), intent(in) :: dtype_id
    integer                    :: hdf_error
    call h5tequal_f(dtype_id, H5T_FORTRAN_S1, isequal, hdf_error)
  end function is_string
  !
  !
  ! .....................................................................................................
  !
  ! Test if the type is a character
  ! .....................................................................................................
  !
  logical function is_character(dtype_id) result(isequal)
    integer(hid_t), intent(in) :: dtype_id
    integer                    :: hdf_error
    call h5tequal_f(dtype_id, H5T_NATIVE_CHARACTER, isequal, hdf_error)
  end function is_character
  !
  !
  ! .....................................................................................................
  !
  ! Test if the type is an integer
  ! .....................................................................................................
  !
  logical function is_integer(dtype_id) result(isequal)
    integer(hid_t), intent(in) :: dtype_id
    integer                    :: hdf_error
    call h5tequal_f(dtype_id, H5T_NATIVE_INTEGER, isequal, hdf_error)
  end function is_integer
  !
  !
  ! .....................................................................................................
  !
  ! Test if the type is a real
  ! .....................................................................................................
  !
  logical function is_real(dtype_id) result(isequal)
    integer(hid_t), intent(in) :: dtype_id
    integer                    :: hdf_error
    call h5tequal_f(dtype_id, H5T_NATIVE_REAL, isequal, hdf_error)
  end function is_real
  !
  !
  ! .....................................................................................................
  !
  ! Test if the type is a double
  ! .....................................................................................................
  !
  logical function is_double(dtype_id) result(isequal)
    integer(hid_t), intent(in) :: dtype_id
    integer                    :: hdf_error
    call h5tequal_f(dtype_id, H5T_NATIVE_DOUBLE, isequal, hdf_error)
  end function is_double
    
  !
  ! .....................................................................................................
  !
  ! Read integer field (column) from compound dataset
  !
  ! .....................................................................................................
  ! This function reads an integer column from an HDF5 compound dataset
  ! .....................................................................................................
  logical function h5_read_compound_integer(dataset_id, member_name, data_arr, nrows) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_compound_integer" :: h5_read_compound_integer
    integer(hid_t), intent(in)      :: dataset_id           ! HDF dataset identifier
    character(len=*), intent(in)    :: member_name                 ! Name of field in dataset
    integer, intent(in)             :: nrows                ! Number of rows in dataset
    integer, allocatable, intent(out), &
      dimension(:)                  :: data_arr             ! Data array from dataset
    integer(hsize_t)                :: field_type_size      ! Size of the type of the field
    integer(hid_t)                  :: mtype_id             ! Memory datatype identifier
    integer(hid_t)                  :: dtype_id             ! Dataset type identifier
    integer(hid_t)                  :: field_type_id        ! Identifier of type to read
    integer(size_t)                 :: offset               ! Data offset from start of row to member
    integer                         :: num_members          ! Number of members (columns) in dataset
    integer(hsize_t), dimension(1)  :: data_dims(1)         ! Dataset dimensions (number of rows)
    integer                         :: hdf_error            ! HDF error flag
    integer                         :: field_index          ! Index of field (column) to read
    logical                         :: is_ok                ! Did the function run without error?
    !
    success = .false.
    !
    ! Get data type ID
    call h5dget_type_f(dataset_id, dtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member index
    call h5tget_member_index_f(dtype_id, trim(member_name), field_index, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member type
    call h5tget_member_type_f(dtype_id, field_index, field_type_id, hdf_error) 
    if (hdf_error < 0) return
    !
    ! Get number of members
    call h5tget_nmembers_f(dtype_id, num_members, hdf_error) 
    if (hdf_error < 0) return
    !
    !call h5tget_size_f(H5T_NATIVE_INTEGER, field_type_size, hdf_error)
    call h5tget_size_f(field_type_id, field_type_size, hdf_error)
    if (hdf_error < 0) return
    !
    call h5tcreate_f(H5T_COMPOUND_F, field_type_size, mtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    offset = 0
    !call h5tinsert_f(mtype_id, trim(member_name), offset, H5T_NATIVE_INTEGER, hdf_error)
    call h5tinsert_f(mtype_id, trim(member_name), offset, field_type_id, hdf_error)
    if (hdf_error < 0) return
    !
    data_dims(1) = nrows
    allocate(data_arr(nrows))
    call h5dread_f(dataset_id, mtype_id, data_arr, data_dims, hdf_error)
    if (hdf_error < 0) return
    !
    success = .true.
    !
  end function h5_read_compound_integer
  !
  ! .....................................................................................................
  !
  ! Read real field (column) from compound dataset
  !
  ! .....................................................................................................
  ! This function reads an real column from an HDF5 compound dataset
  ! .....................................................................................................
  logical function h5_read_compound_real(dataset_id, member_name, data_arr, nrows) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_compound_real" :: h5_read_compound_real
    integer(hid_t), intent(in)      :: dataset_id           ! HDF dataset identifier
    character(len=*), intent(in)    :: member_name                 ! Name of field in dataset
    integer, intent(in)             :: nrows                ! Number of rows in dataset
    real(kind=4), allocatable, intent(out), &
      dimension(:)                  :: data_arr             ! Data array from dataset
    integer(hsize_t)                :: field_type_size      ! Size of the type of the field
    integer(hid_t)                  :: mtype_id             ! Memory datatype identifier
    integer(hid_t)                  :: dtype_id             ! Dataset type identifier
    integer(hid_t)                  :: field_type_id             ! Identifier of type to read
    integer(size_t)                 :: offset               ! Data offset from start of row to member
    integer                         :: num_members          ! Number of members (columns) in dataset
    integer(hsize_t), dimension(1)  :: data_dims(1)         ! Dataset dimensions (number of rows)
    integer                         :: hdf_error            ! HDF error flag
    integer                         :: field_index          ! Index of field (column) to read
    logical                         :: is_ok                ! Did the function run without error?
    !
    success = .false.
    !
    ! Get data type ID
    call h5dget_type_f(dataset_id, dtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member index
    call h5tget_member_index_f(dtype_id, trim(member_name), field_index, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member type
    call h5tget_member_type_f(dtype_id, field_index, field_type_id, hdf_error) 
    if (hdf_error < 0) return
    !
    ! Get number of members
    call h5tget_nmembers_f(dtype_id, num_members, hdf_error) 
    if (hdf_error < 0) return
    !
    !call h5tget_size_f(H5T_NATIVE_INTEGER, field_type_size, hdf_error)
    call h5tget_size_f(field_type_id, field_type_size, hdf_error)
    if (hdf_error < 0) return
    !
    call h5tcreate_f(H5T_COMPOUND_F, field_type_size, mtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    offset = 0
    call h5tinsert_f(mtype_id, trim(member_name), offset, field_type_id, hdf_error)
    if (hdf_error < 0) return
    !
    data_dims(1) = nrows
    allocate(data_arr(nrows))
    call h5dread_f(dataset_id, mtype_id, data_arr, data_dims, hdf_error)
    if (hdf_error < 0) return
    !
    success = .true.
    !
  end function h5_read_compound_real
  !
  ! .....................................................................................................
  !
  ! Read double field (column) from compound dataset
  !
  ! .....................................................................................................
  ! This function reads an double column from an HDF5 compound dataset
  ! .....................................................................................................
  logical function h5_read_compound_double(dataset_id, member_name, data_arr, nrows) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_compound_double" :: h5_read_compound_double
    integer(hid_t), intent(in)      :: dataset_id           ! HDF dataset identifier
    character(len=*), intent(in)    :: member_name                 ! Name of field in dataset
    integer, intent(in)             :: nrows                ! Number of rows in dataset
    real(kind=8), allocatable, intent(out), &
      dimension(:)                  :: data_arr             ! Data array from dataset
    integer(hsize_t)                :: field_type_size      ! Size of the type of the field
    integer(hid_t)                  :: mtype_id             ! Memory datatype identifier
    integer(hid_t)                  :: dtype_id             ! Dataset type identifier
    integer(hid_t)                  :: field_type_id        ! Identifier of type to read
    integer(size_t)                 :: offset               ! Data offset from start of row to member
    integer                         :: num_members          ! Number of members (columns) in dataset
    integer(hsize_t), dimension(1)  :: data_dims(1)         ! Dataset dimensions (number of rows)
    integer                         :: hdf_error            ! HDF error flag
    integer                         :: field_index          ! Index of field (column) to read
    logical                         :: is_ok                ! Did the function run without error?
    !
    success = .false.
    !
    ! Get data type ID
    call h5dget_type_f(dataset_id, dtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member index
    call h5tget_member_index_f(dtype_id, trim(member_name), field_index, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member type
    call h5tget_member_type_f(dtype_id, field_index, field_type_id, hdf_error) 
    if (hdf_error < 0) return
    !
    ! Get number of members
    call h5tget_nmembers_f(dtype_id, num_members, hdf_error) 
    if (hdf_error < 0) return
    !
    !call h5tget_size_f(H5T_NATIVE_INTEGER, field_type_size, hdf_error)
    call h5tget_size_f(field_type_id, field_type_size, hdf_error)
    if (hdf_error < 0) return
    !
    call h5tcreate_f(H5T_COMPOUND_F, field_type_size, mtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    offset = 0
    call h5tinsert_f(mtype_id, trim(member_name), offset, field_type_id, hdf_error)
    if (hdf_error < 0) return
    !
    data_dims(1) = nrows
    allocate(data_arr(nrows))
    call h5dread_f(dataset_id, mtype_id, data_arr, data_dims, hdf_error)
    if (hdf_error < 0) return
    !
    success = .true.
    !
  end function h5_read_compound_double
  !
  ! .....................................................................................................
  !
  ! Read string field (column) from compound dataset
  !
  ! .....................................................................................................
  ! This function reads an string column from an HDF5 compound dataset
  ! .....................................................................................................
  logical function h5_read_compound_string(dataset_id, member_name, data_arr, nrows) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_compound_string" :: h5_read_compound_string
    integer(hid_t), intent(in)      :: dataset_id           ! HDF dataset identifier
    character(len=*), intent(in)    :: member_name          ! Name of field in dataset
    integer, intent(in)             :: nrows                ! Number of rows in dataset
    character(len=:), allocatable, intent(out), &
      dimension(:)                  :: data_arr             ! Data array from dataset
    integer(hsize_t)                :: str_len              ! Length of strings in the dataset
    integer(hid_t)                  :: field_type_id        ! Identifier of type to read
    integer(hsize_t)                :: field_type_size      ! Size of the type of the field
    integer(hid_t)                  :: mtype_id             ! Memory datatype identifier
    integer(hid_t)                  :: dtype_id             ! Dataset type identifier
    integer(size_t)                 :: offset               ! Data offset from start of row to member
    integer                         :: num_members          ! Number of members (columns) in dataset
    integer(hsize_t), dimension(1)  :: data_dims(1)         ! Dataset dimensions (number of rows)
    integer                         :: hdf_error            ! HDF error flag
    integer                         :: field_index          ! Index of field (column) to read
    logical                         :: is_ok                ! Did the function run without error?
    !
    success = .false.
    !
    ! Get data type ID
    call h5dget_type_f(dataset_id, dtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member index
    call h5tget_member_index_f(dtype_id, trim(member_name), field_index, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get member type
    call h5tget_member_type_f(dtype_id, field_index, field_type_id, hdf_error) 
    if (hdf_error < 0) return
    !
    ! From Steve's code -- not needed anymore?
    !call h5tcopy_f(H5T_NATIVE_CHARACTER, atype_id, hdf_error)
    !call h5tset_size_f(atype_id, str_len, hdf_error)
    !call h5tget_size_f(field_type_id, str_len, hdf_error) ! The size = 1 byte * str_len
    !
    ! Get number of members
    call h5tget_nmembers_f(dtype_id, num_members, hdf_error) 
    if (hdf_error < 0) return
    !
    call h5tget_size_f(field_type_id, field_type_size, hdf_error)
    if (hdf_error < 0) return
    !
    call h5tcreate_f(H5T_COMPOUND_F, field_type_size, mtype_id, hdf_error)
    str_len = field_type_size ! field_type_size = 1 byte * str_len
    if (hdf_error < 0) return
    !
    offset = 0
    call h5tinsert_f(mtype_id, trim(member_name), offset, field_type_id, hdf_error)
    if (hdf_error < 0) return
    !
    data_dims(1) = nrows
    allocate(character(str_len) :: data_arr(nrows)) ! Note special allocation for array of strings
    call h5dread_f(dataset_id, mtype_id, data_arr, data_dims, hdf_error)
    if (hdf_error < 0) return
    !
    success = .true.
    !
  end function h5_read_compound_string
  !
  ! .....................................................................................................
  !
  ! Read compound dataset
  !
  ! .....................................................................................................
  ! This function reads a entire compound dataset from an HDF5 file
  !
  ! .....................................................................................................
  logical function h5_get_compound_dataset(loc_id, dataset_name, h5compound_arr, nrows) result(success)
    integer(hid_t),                intent(in)  :: loc_id
    character(len=*),              intent(in)  :: dataset_name
    type(h5compound), allocatable, intent(out) :: h5compound_arr(:)    
    integer,                       intent(out) :: nrows 
    integer(hid_t)   :: dataset_id
    logical          :: exists 
    success = h5_dataset_exists_in_group_id(loc_id, dataset_name, exists)
    if (success .and. exists) then
      success = h5_open_dataset(loc_id, dataset_name, dataset_id)    
      success = h5_read_compound_dataset(dataset_id, h5compound_arr, nrows) 
      success = h5_close_dataset(dataset_id)
    end if    
  end function
  !
  ! write the contents of a compound dataset to the screen for debugging
  subroutine debug_write_compound(h5compound_arr, nrows)
    type(h5compound), allocatable, intent(in) :: h5compound_arr(:)    
    integer,                       intent(in) :: nrows 
    integer field, row    
    do field = 1, size(h5compound_arr)
      write(*,*) "Field Name = " // h5compound_arr(field)%name
      if (h5compound_arr(field)%is_character) then
          write(*,*) "Character Field: ", (trim(h5compound_arr(field)%char_arr(row)),  row=1, nrows)
      elseif (h5compound_arr(field)%is_integer) then
          write(*,*) "Integer Field: ",   (h5compound_arr(field)%int_arr(row),    row=1, nrows)
      elseif (h5compound_arr(field)%is_real) then
          write(*,*) "Real Field: ",      (h5compound_arr(field)%real_arr(row),   row=1, nrows)   
      elseif (h5compound_arr(field)%is_double) then
          write(*,*) "Double Field: ",    (h5compound_arr(field)%double_arr(row), row=1, nrows)   
      end if
    end do
  end subroutine
  !
  ! .....................................................................................................
  ! This function reads a opened compound dataset from an HDF5 file
  !
  ! .....................................................................................................
  logical function h5_read_compound_dataset(dataset_id, h5compound_arr, nrows) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_read_compound_dataset" :: h5_read_compound_dataset
    !
    integer(hid_t), intent(in)      :: dataset_id           ! HDF dataset identifier
    type(h5compound), intent(out), &
      allocatable, dimension(:)     :: h5compound_arr       ! Array of compound data types
    integer, intent(out)            :: nrows                ! Number of rows in dataset
    integer(hsize_t)                :: str_len              ! Length of strings in the dataset
    character(:), allocatable       :: member_name          ! Name of field in dataset
    integer                         :: member_name_len      ! Length of the member name
    integer(hid_t)                  :: field_type_id        ! Identifier of type to read
    integer(hsize_t)                :: field_type_size      ! Size of the type of the field
    integer(hid_t)                  :: mtype_id             ! Memory datatype identifier
    integer(hid_t)                  :: dtype_id             ! Dataset type identifier
    integer(size_t)                 :: offset               ! Data offset from start of row to member
    integer                         :: num_members          ! Number of members (columns) in dataset
    integer(hsize_t), dimension(1)  :: data_dims(1)         ! Dataset dimensions (number of rows)
    integer                         :: hdf_error            ! HDF error flag
    integer                         :: field_index          ! Index of field (column) to read
    integer                         :: column_index         ! Index of column in output array
    logical                         :: is_ok                ! Did the function run without error?
    integer, allocatable, &
      dimension(:)                  :: integer_out          ! Integer array (output buffer)
    real(kind=4), allocatable, &
      dimension(:)                  :: real_out             ! Real array (output buffer)
    real(kind=8), allocatable, &
      dimension(:)                  :: double_out           ! Double array (output buffer)
    character(len=:), allocatable, &
      dimension(:)                  :: string_out           ! Character/string array (output buffer)
    character(len=512) :: mname
    
    integer(hid_t)  space_id    
    integer(hsize_t), dimension(1)  :: max_dims(1)    
    !
    success = .false.
    !
    ! get the number of rows
    is_ok = h5_get_dataset_dimensions(dataset_id, data_dims, max_dims, 1)
    nrows = data_dims(1)
    !
    ! Get data type ID
    call h5dget_type_f(dataset_id, dtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Get number of members in the dataset
    call h5tget_nmembers_f(dtype_id, num_members, hdf_error) 
    if (hdf_error < 0) return
    !
    if (allocated(h5compound_arr)) deallocate(h5compound_arr)
    allocate(h5compound_arr(num_members))
    !
    do field_index = 0, num_members - 1
      column_index = field_index + 1
      !
      ! Get member name
      call h5tget_member_name_f(dtype_id, field_index, mname, member_name_len, hdf_error)
      member_name = mname(1:member_name_len)
      if (hdf_error < 0) return
      h5compound_arr(column_index)%name = member_name
      !
      ! Get member type
      call h5tget_member_type_f(dtype_id, field_index, field_type_id, hdf_error) 
      if (hdf_error < 0) return
      !
      ! Get member type
      call h5tget_member_type_f(dtype_id, field_index, field_type_id, hdf_error) 
      if (hdf_error < 0) return
      !
      if (is_character(field_type_id) .or. is_string(field_type_id)) then
        ! Read string column
        is_ok = h5_read_compound_string(dataset_id, trim(member_name), string_out, nrows)
        h5compound_arr(column_index)%char_arr = string_out
        h5compound_arr(column_index)%is_character = .true.
      else if (is_integer(field_type_id)) then
        ! Read integer column
        is_ok = h5_read_compound_integer(dataset_id, trim(member_name), integer_out, nrows)
        h5compound_arr(column_index)%int_arr = integer_out
        h5compound_arr(column_index)%is_integer = .true.
      else if (is_double(field_type_id)) then
        is_ok = h5_read_compound_double(dataset_id, trim(member_name), double_out, nrows)
        h5compound_arr(column_index)%double_arr = double_out
        h5compound_arr(column_index)%is_double = .true.
      else if (is_real(field_type_id)) then
        ! Read real column
        is_ok = h5_read_compound_real(dataset_id, trim(member_name), real_out, nrows)
        h5compound_arr(column_index)%real_arr = real_out
        h5compound_arr(column_index)%is_real = .true.
      else
        ! Read string column (default)
        is_ok = h5_read_compound_string(dataset_id, trim(member_name), string_out, nrows)
        h5compound_arr(column_index)%char_arr = string_out
        h5compound_arr(column_index)%is_character = .true.
      end if
    end do
    !
    success = .true.
  end function h5_read_compound_dataset
  !
  !
  ! .....................................................................................................
  !
  ! Write compound dataset
  !
  ! .....................................................................................................
  ! This function write a compound dataset from an HDF5 file
  !
  ! .....................................................................................................
  logical function h5_write_compound_dataset(file_id, dataset_name, h5compound_arr, nrows, member_names) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_write_compound_dataset" :: h5_write_compound_dataset
    !
    integer(hid_t), intent(in)          :: file_id              ! HDF file identifier
    character(len=*), intent(in)        :: dataset_name         ! HDF file identifier
    type(h5compound), intent(in), &
      allocatable, dimension(:)         :: h5compound_arr       ! Array of compound data types
    character(len=:), intent(in), &
      allocatable, dimension(:)         :: member_names         ! Names of the member fields
    integer, intent(in)                 :: nrows                ! Number of rows in dataset
    integer(hid_t)                      :: dataset_id           ! HDF dataset identifier
    integer(hsize_t)                    :: str_len              ! Length of strings in the dataset
    character(:), allocatable           :: member_name          ! Name of field in dataset
    integer(hsize_t), dimension(1)      :: data_dims(1)         ! Dataset dimensions (number of rows)
    integer                             :: member_name_len      ! Length of the member name
    integer(hid_t)                      :: field_type_id        ! Identifier of type to read
    integer(hsize_t)                    :: field_type_size      ! Size of the type of the field
    integer(hid_t)                      :: mtype_id             ! Memory datatype identifier
    integer(hid_t)                      :: dtype_id             ! Dataset type identifier
    integer(size_t)                     :: offset               ! Data offset from start of row to member
    integer                             :: num_members          ! Number of members (columns) in dataset
    integer                             :: hdf_error            ! HDF error flag
    integer                             :: field_index          ! Index of field (column) to read
    integer                             :: column_index         ! Index of column in output array
    logical                             :: is_ok                ! Did the function run without error?
    integer(hid_t)                      :: plist_id             ! Property list identifier
    integer(hid_t)                      :: dspace_id            ! Data space identifier
    character(len=512)                  :: mname                ! Member name buffer (string)
    character(len=:), allocatable, dimension(:)  :: char_member          ! Character buffer to write to dataset
    integer, dimension(nrows)           :: int_member           ! Integer buffer to write to dataset
    double precision, dimension(nrows)  :: double_member        ! Double buffer to write to dataset
    real, dimension(nrows)              :: real_member          ! Real buffer to write to dataset
    integer, parameter                  :: rank = 1             ! Rank of the dataset (dimension of the arrays)
    integer(hid_t)                      :: member_type          ! Member type identifier
    integer(hid_t), allocatable, &
      dimension(:)                      :: member_types   ! Array of native primitive member types
    integer(hid_t), allocatable, &
      dimension(:)                      :: compound_member_types   ! Array of compound member types for the dataset
    integer(hsize_t)                    :: member_size    ! Member size
    integer(hsize_t), allocatable, &
      dimension(:)                      :: member_sizes   ! Array of member sizes for the dataset
    integer(hsize_t), allocatable, &
      dimension(:)                      :: offsets        ! Array of member sizes for the dataset
    integer(size_t)                     :: type_size_compound ! Size of the compound datatype (bytes)
    !
    success = .false.
    !
    num_members = size(h5compound_arr)
    data_dims(1) = nrows
    allocate(member_types(num_members))
    allocate(member_sizes(num_members))
    allocate(compound_member_types(num_members))
    allocate(offsets(num_members))
    !allocate(character(str_len) :: data_arr(nrows)) ! Note special allocation for array of strings
    
    ! Create properties list for (raw) dataset transfer
    call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Set dataset transfer property to preserve partially initialized fields
    ! during write/read to/from dataset with compound datatype.
    !
    ! Note: This function is deprecated as it no longer has any effect; compound 
    !  datatype field preservation is now core functionality in the HDF5 Library.
    !  Source: https://support.hdfgroup.org/HDF5/doc/RM/RM_H5P.html#Property-Create
    call h5pset_preserve_f(plist_id, .true., hdf_error)
    if (hdf_error < 0) return
    !
    ! Create the dataspace
    ! In this example, rank = 1, data_dims = nrows = 6
    call h5screate_simple_f(rank, data_dims, dspace_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! --- Create compound datatype ---
    !
    ! First calculate total size by calculating sizes of each member
    type_size_compound = 0
    offset = 0
    do column_index = 1, num_members
      if (h5compound_arr(column_index)%is_character) then
        char_member = h5compound_arr(column_index)%char_arr
        str_len = len(char_member(1))
        call h5tcopy_f(H5T_NATIVE_CHARACTER, member_type, hdf_error)
        if (hdf_error < 0) return
        call h5tset_size_f(member_type, str_len, hdf_error)
        if (hdf_error < 0) return
        call h5tget_size_f(member_type, member_size, hdf_error)
        if (hdf_error < 0) return
        member_types(column_index) = member_type
        member_sizes(column_index) = member_size
      else if (h5compound_arr(column_index)%is_integer) then
        member_type = H5T_NATIVE_INTEGER
        call h5tget_size_f(member_type, member_size, hdf_error)
        if (hdf_error < 0) return
        member_types(column_index) = member_type
        member_sizes(column_index) = member_size
      else if (h5compound_arr(column_index)%is_double) then
        member_type = H5T_NATIVE_DOUBLE
        call h5tget_size_f(member_type, member_size, hdf_error)
        if (hdf_error < 0) return
        member_types(column_index) = member_type
        member_sizes(column_index) = member_size
      else if (h5compound_arr(column_index)%is_real) then
        member_type = H5T_NATIVE_REAL
        call h5tget_size_f(member_type, member_size, hdf_error)
        if (hdf_error < 0) return
        member_types(column_index) = member_type
        member_sizes(column_index) = member_size
      end if
      !
      offsets(column_index) = offset
      offset = offset + member_size
      type_size_compound = type_size_compound + member_size
      !
    end do
    !
    ! Create compound datatype
    call h5tcreate_f(H5T_COMPOUND_F, type_size_compound, dtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Insert members (all insets beyond the first are non-zero values)
    do column_index = 1, num_members
      call h5tinsert_f(dtype_id, trim(member_names(column_index)), offsets(column_index), member_types(column_index), hdf_error)
      if (hdf_error < 0) return
    end do
    !
    ! Create the dataset with compound datatype.
    call h5dcreate_f(file_id, trim(dataset_name), dtype_id, dspace_id, dataset_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Create memory types. We have to create a compound datatype for each member we want to write.
    ! The order of insertion will determine the order of the fields. They may then be written in any order.
    offset = 0 ! Note: this will be zero for each inserted type
    do column_index = 1, num_members
      call h5tcreate_f(H5T_COMPOUND_F, member_sizes(column_index), dtype_id, hdf_error)
      if (hdf_error < 0) return
      call h5tinsert_f(dtype_id, trim(member_names(column_index)), offset, member_types(column_index), hdf_error)
      if (hdf_error < 0) return
      compound_member_types(column_index) = dtype_id
    end do
    !
    ! Write data by fields in the datatype. Field order is not important.
    ! Note: the order will be character, integer, double, real, but they are
    ! written as real, character, double, integer below:
    do column_index = 1, num_members
      if (h5compound_arr(column_index)%is_character) then
        char_member = h5compound_arr(column_index)%char_arr
        call h5dwrite_f(dataset_id, compound_member_types(column_index), char_member, data_dims, hdf_error, xfer_prp = plist_id)
        if (hdf_error < 0) return
      else if (h5compound_arr(column_index)%is_integer) then
        int_member = h5compound_arr(column_index)%int_arr
        call h5dwrite_f(dataset_id, compound_member_types(column_index), int_member, data_dims, hdf_error, xfer_prp = plist_id)
        if (hdf_error < 0) return
      else if (h5compound_arr(column_index)%is_double) then
        double_member = h5compound_arr(column_index)%double_arr
        call h5dwrite_f(dataset_id, compound_member_types(column_index), double_member, data_dims, hdf_error, xfer_prp = plist_id)
        if (hdf_error < 0) return
      else if (h5compound_arr(column_index)%is_real) then
        real_member = h5compound_arr(column_index)%real_arr
        call h5dwrite_f(dataset_id, compound_member_types(column_index), real_member, data_dims, hdf_error, xfer_prp = plist_id)
        if (hdf_error < 0) return
      end if
    end do
    !
    ! End access to the dataset and release resources used by it
    call h5dclose_f(dataset_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Terminate access to the data space
    call h5sclose_f(dspace_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Terminate access to the datatype
    call h5tclose_f(dtype_id, hdf_error)
    if (hdf_error < 0) return
    !
    ! Close data types
    do column_index = 1, num_members
      call h5tclose_f(compound_member_types(column_index), hdf_error)
      if (hdf_error < 0) return
    end do
    !
    success = .true.
    !
  end function h5_write_compound_dataset
  !
  end module
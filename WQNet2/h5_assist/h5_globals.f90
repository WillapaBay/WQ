!DEC$ FREEFORM
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! HDF5 Interface Module: Globals
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Written by Todd Steissberg, 2017
!
module h5_globals  
  use, non_intrinsic  :: hdf5
  use, intrinsic      :: iso_c_binding ! provides: c_float, c_int, c_ptr
  !
  implicit none
  !
  ! HDF5 Data Types:
  ! 
  !==============================================================================================
  ! Fortran Native Types    Description                   Value
  !----------------------------------------------------------------------------------------------
  ! H5T_NATIVE_INTEGER      Native integer type           50331741
  ! H5T_NATIVE_REAL         Single precision real type    50331742
  ! H5T_NATIVE_DOUBLE       Double precision real type    50331743
  ! H5T_NATIVE_CHARACTER    Character                     50331744
  ! H5T_FORTRAN_S1          Fortran string type           50331786
  ! 
  !==============================================================================================
  ! HDF Size    Size      C# Equivalent   Description
  !----------------------------------------------------------------------------------------------
  ! hid_t:      4 bytes   uint32 or int   Manage references to nodes (ID), unsigned integer
  ! hsize_t:    8 bytes   long            Native multi-precision integer
  ! size_t:     8 bytes   ulong           C native unsigned integer
  !
  !----------------------------------------------------------------------------------------------
  !
  ! HDF5 file access flags
  integer, parameter  :: READ_ONLY             = 0 ! HDF5 constant: H5F_ACC_RDONLY_F
  !                                                ! Existing file is opened with read-only access. 
  !                                                ! If file does not exist, H5Fopen fails.
  integer, parameter  :: READ_WRITE            = 1 ! HDF5 constant: H5F_ACC_RDWR_F
  !                                                ! Existing file is opened with read-write access. 
  !                                                ! If file does not exist, H5Fopen fails.
  integer, parameter  :: TRUNCATE              = 2 ! HDF5 constant: H5F_ACC_TRUN_F
  !                                                ! File is truncated upon opening, i.e., if file 
  !                                                ! already exists, file is opened with read-write 
  !                                                ! access and new data overwrites existing data, 
  !                                                ! destroying all prior content. If file does not exist, 
  !                                                ! it is created and opened with read-write access.
  integer, parameter  :: CREATE_AND_READ_WRITE = 4 ! HDF5 constant: H5F_ACC_EXCL_F
  !                                                ! If file already exists, H5Fcreate fails. If file 
  !                                                ! does not exist, it is created and opened with 
  !                                                ! read-write access.
  !
  ! Set standard string lengths for this module
  integer, parameter  :: SHORT_STR_LEN          = 64     ! Short string
  integer, parameter  :: MEDIUM_STR_LEN         = 128    ! Medium string
  integer, parameter  :: LONG_STR_LEN           = 512    ! Long string
  ! 
  ! Set debug status for printing error and other messages
  integer, parameter  :: DEBUG = .false.
  !
  ! Chunking and compression parameters (using same values as in Steve Piper's mod_hdf_output.for)
  integer, parameter  :: COMPRESSION_LEVEL      = 1   ! Set compression level for ZLIB deflate. Level 1 
  integer, parameter  :: NBYTES_PER_CHUNK       = 1024 * 1024 ! Use 1 MB chunk size
  integer, parameter  :: NUM_CHUNKS_IN_SPACE    = 1   ! Number of chunks along an HDF5 row (spatial dimension, cross-sections)
  integer, parameter  :: NUM_CHUNKS_IN_TIME     = 1   ! Number of chunks along an HDF5 column (time dimension)
  integer, parameter  :: NUM_CHUNKS_IN_ARRAY    = 1   ! Number of chunks along a 1D HDF5 array
  integer, parameter  :: MIN_VALUES_IN_CHUNK = 100 ! Set minimum HDF chunk size (number of values?)
  !
  type h5compound
    character(len=:), allocatable :: name
    logical                 :: is_character = .false.
    logical                 :: is_integer = .false.
    logical                 :: is_real = .false.
    logical                 :: is_double = .false.
    character(len=:), &
      allocatable, &
      dimension(:)          :: char_arr
    integer, &
      allocatable, &
      dimension(:)          :: int_arr
    real(kind=4), &
      allocatable, &
      dimension(:)          :: real_arr
    real(kind=8), &
      allocatable, &
      dimension(:)          :: double_arr
  end type h5compound

  !type h5compound_old(str_len)
  !  integer, len            :: str_len
  !  character(len=str_len)  :: name
  !  logical                 :: is_char
  !  logical                 :: is_int
  !  logical                 :: is_real
  !  logical                 :: is_double
  !  character(len=str_len), &
  !    allocatable, &
  !    dimension(:)          :: char_arr
  !  integer, &
  !    allocatable, &
  !    dimension(:)          :: int_arr
  !  real(kind=4), &
  !    allocatable, &
  !    dimension(:)          :: real_arr
  !  real(kind=8), &
  !    allocatable, &
  !    dimension(:)          :: double_arr
  !end type h5compound_old
  !
  !type h5compound_non_allocatable(n, str_len)
  !  integer, len :: n
  !  integer, len :: str_len
  !  character(len=str_len) :: name
  !  logical :: is_char, is_int, is_real, is_double
  !  character(len=str_len), dimension(n) :: char_arr
  !  integer(kind=4), dimension(n) :: int_arr
  !  real(kind=4), dimension(n) :: real_arr
  !  real(kind=8), dimension(n) :: double_arr
  !end type h5compound_non_allocatable
  !
end module
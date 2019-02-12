!DEC$ FREEFORM
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
! HDF5 Interface Module: Groups
!* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
!
! Written by Todd Steissberg, 2017
!
module h5_groups
  !
  use, non_intrinsic  :: hdf5
  use, intrinsic      :: iso_c_binding ! provides: c_float, c_int, c_ptr
  use, non_intrinsic  :: h5_globals  
  !
  implicit none
  !
  contains
  !
  !
  ! ----- HDF5 Groups -----
  !
  !
  ! .....................................................................................................
  !
  ! Open HDF5 group
  ! .....................................................................................................
  logical function h5_open_group(loc_id, group_name, group_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_open_group" :: h5_open_group
    !
    ! This function should be called after opening an HDF5 file and/or a group.
    ! The location identifier (loc_id) should be either a file identifier or 
    ! a group identifier.
    integer(hid_t),   intent(in)        :: loc_id     ! Location identifier (file or group ID)
    character(len=*), intent(in)        :: group_name ! Name of group to open
    integer(hid_t),   intent(out)       :: group_id   ! Identifier of group to return    
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    ! Open the group in 
    call h5gopen_f(loc_id, trim(group_name), group_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Close HDF5 group
  ! .....................................................................................................
  logical function h5_close_group(group_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_close_group" :: h5_close_group
    !
    integer(hid_t), intent(in)          :: group_id   ! Identifier of group to close
    integer                             :: hdf_error  ! HDF5 error flag
    !
    success = .false.
    !
    call h5gclose_f(group_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Check if HDF5 group exists
  ! .....................................................................................................
  logical function h5_group_exists(loc_id, group_name, link_exists) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_group_exists" :: h5_group_exists
    !
    integer(hid_t), intent(in)          :: loc_id       ! Location ID (file or group ID), 
    !                                                   ! where group_name may be located
    character(len=*), intent(in)        :: group_name   ! Name of group to check
    integer                             :: hdf_error    ! HDF5 error flag
    logical, intent(out)                :: link_exists  ! Are you there?
    !
    success = .false.
    !
    call h5lexists_f(loc_id, trim(group_name), link_exists, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! get the name of a sub-group in a group
  ! .....................................................................................................
  function h5_get_subgroupname(loc_id, group_name, index) result(name)
    integer(hid_t),   intent(in)  :: loc_id       ! Location ID (file or group ID)    
    character(len=*), intent(in)  :: group_name   ! name of base group
    integer,          intent(in)  :: index        ! sub group index
    character(len=:), allocatable :: name     
    integer   nmembers
    integer   hdf_error
    integer   obj_type
    integer   i, n
    allocate(character(len=128)::name)
    !
    ! get the number of members in this group
    call h5gn_members_f(loc_id, group_name, nmembers, hdf_error)
    !
    ! count the members that are group types and stop at index
    n = 0
    do i = 0, nmembers-1
      call h5gget_obj_info_idx_f(loc_id, group_name, i, name, obj_type, hdf_error)
      if (obj_type == H5G_GROUP_F) then
        n = n + 1
        if (n == index) then
          name = trim(name)
          return
        end if
      end if
    end do
    !
    ! must not have found a group at that index
    name = ""
  end function  
  !
  ! .....................................................................................................
  !
  ! Get number of groups in a specified group in an HDF5 file
  ! .....................................................................................................
  logical function h5_num_groups(file_id, group_name, num_groups) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_num_groups" :: h5_num_groups
    !
    integer(hid_t), intent(in)          :: file_id          ! File identifier
    character(len=*), intent(in)        :: group_name       ! Group name (full path, a single group, or "/")
    integer, intent(out)                :: num_groups       ! Number of groups (members) in group
    logical                             :: is_ok            ! Did the function run without error?
    logical                             :: link_exists      ! Does the group link exist?
    integer                             :: hdf_error        ! HDF5 error flag
    integer                             :: num_members = 0  ! Number of members in group
    !
    ! First check if group exists
    is_ok = h5_group_exists(file_id, trim(group_name), link_exists)
    !
    ! Find number of groups in the specified group
    if (link_exists) then
      call h5gn_members_f(file_id, trim(group_name), num_groups, hdf_error)
    end if 
    !
    if (hdf_error == -1) then
      num_groups = -1
      return
    end if
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Delete an HDF group
  ! .....................................................................................................
  logical function h5_delete_group(loc_id, group_name) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_delete_group" :: h5_delete_group
    !
    integer(hid_t), intent(in)          :: loc_id       ! Identifier of an open HDF5 file or group
    character(len=*), intent(in)        :: group_name   ! Name of group to create    
    integer                             :: hdf_error    ! HDF5 error flag
    !
    success = .false.
    !
    ! Delete an HDF5 group
    ! This actually unlinks it. Other steps will have to be taken to reclaim the storage/memory.
    ! TODO Reclaim memory after deleting group
    call h5gunlink_f(loc_id, trim(group_name), hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  !
  ! .....................................................................................................
  !
  ! Create HDF group for output
  ! .....................................................................................................
  logical function h5_create_group(file_id, group_name, group_id) result(success)
    !DEC$ ATTRIBUTES STDCALL, REFERENCE, DLLEXPORT, ALIAS:"h5_create_group" :: h5_create_group
    !
    integer(hid_t), intent(in)          :: file_id      ! Identifier of an open HDF5 file
    character(len=*), intent(in)        :: group_name   ! Name of group to create
    integer(hid_t), intent(out)         :: group_id     ! Group identifier to return    
    integer                             :: hdf_error    ! HDF5 error flag
    !
    success = .false.
    !
    ! Create an HDF5 file for output
    call h5gcreate_f(file_id, trim(group_name), group_id, hdf_error)
    if (hdf_error == -1) return
    !
    success = .true.
  end function
  
end module
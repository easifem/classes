! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: 	HDF5File Methods

SUBMODULE(HDF5File_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 HDF5Open
!----------------------------------------------------------------------------

SUBROUTINE HDF5Open
  INTEGER( I4B ) :: herr
  CHARACTER( LEN = * ), PARAMETER :: myName="HDF5Open"
  IF( .NOT. libh5Open ) THEN
    CALL H5open_f(herr)
    IF(herr .NE. 0) THEN
      CALL e%raiseError( modName // '::' // myName // &
        & ' - There is some error in opening the hdf5lib')
    END IF
    libh5Open=.TRUE.
  END IF
END SUBROUTINE HDF5Open

!----------------------------------------------------------------------------
!                                                                 HDF5Close
!----------------------------------------------------------------------------

SUBROUTINE HDF5Close
  INTEGER( I4B ) :: herr
  herr=-1
  IF(libh5Open) CALL H5close_f(herr)
  IF(herr == 0) libh5Open=.FALSE.
END SUBROUTINE HDF5Close

!----------------------------------------------------------------------------
!                                                                 HDF5Quiet
!----------------------------------------------------------------------------


!> authors: Vikas Sharma, Ph. D.
! date: 	8 May 2021
! summary: Enable/disable HDF5 exception writing

SUBROUTINE HDF5Quiet(quiet)
  LOGICAL, INTENT( IN ) :: quiet
  IF(quiet) THEN
    CALL h5eset_auto_f(0, ierr)
  ELSE
    CALL h5eset_auto_f(1, ierr)
  ENDIF
END SUBROUTINE HDF5Quiet

!----------------------------------------------------------------------------
!                                                              addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE hdf5_addSurrogate

!----------------------------------------------------------------------------
!                                                                 Open
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_open
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_open'
  INTEGER :: acc
  INTEGER( HID_T ) :: plist_id
  !> main program
  IF( obj%isinit ) THEN
    CALL h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, ierr)
    CALL h5pset_fclose_degree_f(plist_id, H5F_CLOSE_SEMI_F, ierr)

    IF (ierr .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Unable to create property list for open operation.')
    END IF
    ! Decide what access type to use
    IF(obj%isNew()) THEN
      acc=H5F_ACC_TRUNC_F
      CALL h5fcreate_f(obj%fullname%chars(), acc, obj%file_id, ierr, &
        & access_prp=plist_id)
      ! If the file is NEW, change the mode to WRITE after
      ! Creating it so we don't keep truncating it repeatedly.
      CALL obj%setNewStat(.FALSE.)
    ELSEIF(obj%isWrite()) THEN
      acc=H5F_ACC_RDWR_F
      CALL h5fopen_f(obj%fullname%chars(), acc, obj%file_id, ierr, &
        & access_prp=plist_id)
    ELSEIF(obj%isRead()) THEN
      acc=H5F_ACC_RDONLY_F
      CALL h5fopen_f(obj%fullname%chars(), acc, obj%file_id, ierr, &
        & access_prp=plist_id)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Unrecognized access mode! The file is not'// &
        & ' set as either new, read, or write!')
    ENDIF
    !>
    IF (ierr .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Unable to open the file.')
    END IF
    !>
    CALL h5pclose_f(plist_id,ierr)
    IF(ierr .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Unable to destroy property list.')
    ELSE
      CALL obj%setOpenStat(.TRUE.)
    ENDIF
  ENDIF
END PROCEDURE hdf5_open

!----------------------------------------------------------------------------
!                                                                 Close
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_close
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_close'
  LOGICAL( LGT ) :: lastStopOnError
  lastStopOnError=e%isStopOnError()
  CALL e%setStopOnError(.FALSE.)
  !Check init status
  IF(.NOT.obj%isinit) THEN
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ELSE
    !Check open status.
    IF(obj%isopen()) THEN
      CALL h5fclose_f(obj%file_id,ierr)
      obj%file_id=0
      IF(ierr /= 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Unable to close HDF5 file.')
      ELSE
        CALL obj%setOpenStat(.FALSE.)
      ENDIF
    ENDIF
  ENDIF
  CALL e%setStopOnError(lastStopOnError)
END PROCEDURE hdf5_close

!----------------------------------------------------------------------------
!                                                                 Delete
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_delete
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_delete'
  CHARACTER(LEN=EXCEPTION_MAX_MESG_LENGTH) :: emesg
  TYPE( String ) :: fileName
  !> main
  IF(obj%isinit) THEN
    !So, HDF5 is special in that the unitno assigned isn't used in the
    !fopen() operation.  So, regardless of the %isOpen() status, it needs
    !to be opened.
    fileName = obj%getFilePath() // "/" // obj%getFileName() // &
      & obj%getFileExt()
    OPEN( UNIT=obj%unitno, FILE=fileName%chars(), IOSTAT=ierr )
    IF(ierr .NE. 0) THEN
      WRITE( emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
        & obj%unitno,' ) IOSTAT=', ierr
      CALL e%raiseError(modName//'::'//myName//' - '//emesg)
    ENDIF
    CLOSE( UNIT=obj%unitno, STATUS='DELETE', IOSTAT=ierr )
    IF(ierr .NE. 0) THEN
      WRITE( emesg,'(a,i4,a,i4)') 'Error deleting file (UNIT=', &
        & obj%unitno,' ) IOSTAT=',ierr
      CALL e%raiseError( modName//'::'//myName//' - '//emesg )
    ELSE
      CALL obj%setOpenStat( .FALSE. )
    ENDIF
  ENDIF
END PROCEDURE hdf5_delete

!----------------------------------------------------------------------------
!                                                                 initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_initiate
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_initiate'
  TYPE( String ) :: fpath, fname, fext, mode_in
  INTEGER( I4B ) :: unitno
  LOGICAL( LGT ) :: ostat,exists
  CHARACTER(LEN=LEN(filename)) :: tempchars
  !> main
  IF(obj%isinit) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' is already initialized!')
    RETURN
  ENDIF
  CALL getPath( chars=filename, path=tempchars )
  fpath=trim(tempchars)
  CALL getFileNameExt( chars=filename, ext=tempchars )
  fext=trim(tempchars)
  CALL getFileName( chars=filename, fname=tempchars )
  fname=trim(tempchars)
  CALL obj%setFilePath( fpath )
  CALL obj%setFileName( fname )
  CALL obj%setFileExt( fext )
  !>
  IF(PRESENT(zlibOpt)) THEN
    IF(zlibOpt .GE. 0) THEN
      obj%hasCompression=.TRUE.
      obj%zlibOpt=zlibOpt
    ENDIF
  ENDIF
  !> Store the access mode
  mode_in=mode
  mode_in = mode_in%upper()
  SELECTCASE(TRIM(mode_in%chars()))
  CASE( 'READ' )
    INQUIRE(FILE=filename,EXIST=exists)
    IF(exists) THEN
      CALL obj%setWriteStat(.FALSE.)
      CALL obj%setReadStat(.TRUE.)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - HDF5 file '//filename//' is being opened with '// &
        & 'mode READ but does not exist.')
    ENDIF
  CASE( 'WRITE' )
    INQUIRE( FILE=filename, EXIST=exists )
    IF( exists ) THEN
      CALL obj%setWriteStat( .TRUE. )
      CALL obj%setReadStat( .FALSE. )
    ELSE
      CALL e%raiseError( modName//'::'//myName// &
        & ' - HDF5 file '//filename//' is being opened with '// &
        & 'mode WRITE but does not exist.')
    ENDIF
  CASE( 'OVERWRITE', 'READWRITE' )
    INQUIRE( FILE=filename, EXIST=exists )
    IF( exists ) THEN
      CALL obj%setWriteStat( .TRUE. )
      CALL obj%setOverwriteStat( .TRUE. )
      CALL obj%setReadStat( .TRUE. )
    ELSE
      CALL e%raiseError( modName // '::' // myName // &
        & ' - HDF5 file ' // filename // ' is being opened with ' // &
        & 'mode OVERWRITE but does not exist.')
    ENDIF
  CASE( 'NEW' )
    CALL obj%setWriteStat( .TRUE. )
    CALL obj%setReadStat( .TRUE. )
    CALL obj%setNewStat( .TRUE. )
    CALL obj%setOverwriteStat( .TRUE. )
  CASE DEFAULT
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Unrecognized access mode.')
  ENDSELECT
  obj%fullname=filename
  ! Initialize the HDF5 interface. This needs be done before any other calls
  ! to the HF5 interface can be made.
  CALL e%raiseDebug(modName//'::'//myName// &
    & ' - CALL HDF5Open(), currently breaks in ubuntu [ISSUE-1]')
  CALL HDF5Open()
  ! Assign arbitrary UNIT number to file.  Used only for deleting file.
  unitno=99
  INQUIRE( UNIT=unitno, OPENED=ostat )
  DO WHILE(obj%unitno == -1)
    IF(ostat) THEN
      unitno=unitno-1_I4B
      INQUIRE( UNIT=unitno, OPENED=ostat )
    ELSE
      obj%unitno=unitno
    ENDIF
  ENDDO
  obj%isinit = .TRUE.
  nhdf5fileinuse = nhdf5fileinuse+1
END PROCEDURE hdf5_initiate

!----------------------------------------------------------------------------
!                                                                 Clear
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_clear
  LOGICAL( LGT ) :: bool
  IF( obj%isinit ) THEN
    !Logical to close or delete the file.
    bool=.FALSE.
    IF( PRESENT(Delete) ) bool=Delete
    IF(bool) THEN
      CALL obj%delete()
    ELSE
      CALL obj%close()
    ENDIF
    ! Close the HDF5 interface. This can only be done once all calls to the
    ! HDF5 library are complete.
    nhdf5fileinuse=nhdf5fileinuse-1
    IF(libh5Open .AND. (nhdf5fileinuse == 0)) CALL HDF5Close()
    obj%isinit=.FALSE.
    obj%newstat=.FALSE.
    obj%hasCompression=.FALSE.
    obj%zlibOpt=-1
    obj%fullname=''
    obj%unitno=-1
    obj%overwriteStat = .FALSE.
    CALL aFile_DeallocateData(obj)
  ENDIF
END PROCEDURE hdf5_clear

!----------------------------------------------------------------------------
!                                                          setOverWriteStat
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_setOverWriteStat
  obj%overwriteStat = bool
END PROCEDURE hdf5_setOverWriteStat

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_getUnitNo
  ans = obj%unitno
END PROCEDURE hdf5_getUnitNo

!----------------------------------------------------------------------------
!                                                                 isNew
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_isNew
  ans = obj%newstat
END PROCEDURE hdf5_isNew

!----------------------------------------------------------------------------
!                                                                 setNewStat
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_setNewStat
  obj%newstat = bool
END PROCEDURE hdf5_setNewStat

!----------------------------------------------------------------------------
!                                                                 ls
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_ls
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_ls'
  CHARACTER( LEN = 1024 ) :: tmpchar
  TYPE( String ) :: path2
  INTEGER( HSIZE_T ) :: i
  INTEGER( HID_T ) :: grp_id
  INTEGER :: store_type, nlinks, max_corder

  ! Make sure the object is initialized
  IF(.NOT. obj%isinit) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ELSEIF(.NOT.obj%isOpen()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' is not opened!')
  ELSE
    IF(ALLOCATED(objs)) THEN
      DEALLOCATE(objs)
    ENDIF
    IF(obj%isGroup(path)) THEN
      path2=TRIM(path)
      CALL h5gopen_f(obj%file_id,TRIM(path),grp_id,ierr)
      IF(ierr .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Unable to open file.')
      END IF

      CALL h5gget_info_f(grp_id,store_type,nlinks,max_corder,ierr)
      IF(ierr .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Unable to get group information.')
      END IF

      ALLOCATE(objs(nlinks))
      DO i=0,nlinks-1
        CALL h5lget_name_by_idx_f(obj%file_id,TRIM(path), &
          & H5_INDEX_NAME_F,H5_ITER_INC_F,i,tmpchar,ierr)
        objs(i+1)=TRIM(tmpchar)
        IF(ierr .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Unable to get object name.')
      END DO

      CALL h5gclose_f(grp_id, ierr)
      IF(ierr .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
          ' - Unable to close group.')
    ENDIF
  ENDIF
END PROCEDURE hdf5_ls

!----------------------------------------------------------------------------
!                                                                 mkdir
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_mkdir
  CHARACTER( LEN = * ), PARAMETER :: myname='hdf5_mkdir'
  TYPE(String) :: path3
  INTEGER(HID_T) :: group_id
  LOGICAL :: dset_exists
  INTEGER(I4B) :: lastslash

  ! Make sure the object is initialized
  IF(.NOT. obj%isinit) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ! Ensure that we have write permissions to the file
  ELSEIF(.NOT.obj%isWrite()) THEN
    CALL e%raiseError(modName &
      & //'::'//myName//' - Can not create group in read-only file.')
  ELSEIF(.NOT.obj%isOpen()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' is already not opened!')
  ELSE
    ! Convert the path to use slashes
    lastslash=INDEX(path, '/',.TRUE.)
    IF(lastslash > 1) THEN
      path3=path(1:lastslash-1)
      IF(.NOT. obj%pathExists(path3%chars())) THEN
        CALL obj%mkdir(path3%chars())
      ENDIF
    ENDIF
    CALL h5lexists_f(obj%file_id,path,dset_exists,ierr)
    IF(ierr .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - invalid group path: '//path)

    IF(obj%overwriteStat .AND. dset_exists) THEN
      ! If group exists, do nothing, but only if overwrites are allowed
      CONTINUE
    ELSE

      ! Create the group
      CALL h5gcreate_f(obj%file_id,path,group_id,ierr)

      IF(ierr == 0) THEN
        ! Close the group
        CALL h5gclose_f(group_id,ierr)
        IF(ierr .NE. 0) CALL e%raiseDebug(modName//'::'// &
          & myName//' - Failed to close HDF group')
      ELSE
        CALL e%raiseDebug(modName//'::'//myName// &
          & ' - Failed to create HDF5 group.')
      ENDIF
    ENDIF
  ENDIF
END PROCEDURE hdf5_mkdir

!----------------------------------------------------------------------------
!                                                                 mkalldir
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_mkalldir
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_mkalldir'
  INTEGER( I4B ) :: i,nslash
  INTEGER( I4B ), ALLOCATABLE :: slashloc(:)
  TYPE( String ) :: path2, tmppath
  INTEGER( HID_T ) :: group_id

  ! Make sure the object is initialized
  IF( .NOT. obj%isinit) THEN
    CALL e%setStopOnError( .FALSE. )
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ! Ensure that we have write permissions to the file
  ELSEIF( .NOT. obj%isWrite()) THEN
    CALL e%raiseError(modName &
      & //'::'//myName//' - Can not create group in read-only file.')
  ELSEIF( .NOT. obj%isOpen()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' is already not opened!')
  ELSE
    ierr=0
    ! Convert the path to use slashes
    path2=TRIM(path)
    CALL path2%strfind( "/", slashloc)
    nslash = SIZE( slashloc )
    DO i = 1, nslash-1
      tmppath = path2%slice( 1, slashloc( i+1 ) - 1 )
      IF( .NOT. obj%pathExists( TRIM( tmppath%chars() ))) THEN
        CALL h5gcreate_f(obj%file_id, TRIM( tmppath%chars()), group_id, ierr)
        IF( ierr .NE. 0 ) THEN
          CALL e%raiseError(modName//'::'// &
          & myName//' - Failed to create a HDF group')
        END IF

        CALL h5gclose_f(group_id, ierr)
        IF( ierr .NE. 0 ) THEN
          CALL e%raiseError(modName//'::'// &
          & myName//' - Failed to close a HDF group')
        END IF
      ENDIF
    END DO
    DEALLOCATE(slashloc)
    ! Create the group
    IF( .NOT. obj%pathExists( TRIM( path2%chars() ))) THEN
      CALL h5gcreate_f(obj%file_id, TRIM( path2%chars()), group_id, ierr)
      IF( ierr .NE. 0 ) THEN
        CALL e%raiseError(modName//'::'// &
          & myName//' - Failed to create a HDF group')
      ELSE
        CALL h5gclose_f(group_id, ierr)
        IF( ierr .NE. 0 ) THEN
          CALL e%raiseError(modName//'::'// &
          & myName//' - Failed to close HDF group')
        END IF
      END IF
    END IF
  ENDIF
END PROCEDURE hdf5_mkalldir

!----------------------------------------------------------------------------
!                                                                 ngrp
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_ngrp
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_ngrp'
  INTEGER( HID_T ) :: grp_id
  INTEGER :: store_type, nlinks, max_corder

  ! Make sure the object is initialized
  IF( .NOT. obj%isinit ) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ! Make sure the object is open
  ELSE IF(.NOT.obj%isOpen()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' is already not opened!')
  ELSE

    CALL h5gopen_f(obj%file_id, TRIM(path), grp_id, ierr)
    IF( ierr .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Could not open group in HDF5 file.')
    END IF

    CALL h5gget_info_f(grp_id, store_type, nlinks, max_corder, ierr)
    IF(ierr .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not get group info in HDF5 file.')
    END IF

    ! Close the group
    CALL h5gclose_f(grp_id, ierr)
    IF(ierr .NE. 0) THEN
      CALL e%raiseDebug(modName//'::'// &
        & myName//' - Failed to close HDF group')
    END IF
    ans = nlinks
  ENDIF
END PROCEDURE hdf5_ngrp

!----------------------------------------------------------------------------
!                                                                 isGroup
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_isgroup
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_isgroup'
  INTEGER( HID_T ) :: obj_id
  INTEGER( I4B ) :: type

  ! Make sure the object is initialized and opened
  ans = .FALSE.
  IF( obj%isinit .AND. obj%isOpen() ) THEN
    ans=obj%pathExists(path)
    IF(ans) THEN
      !Need to get the object ID from the path...
      CALL h5oopen_f(obj%file_id, path, obj_id, ierr)
      IF(ierr == -1) THEN
        ans=.FALSE.
      ELSE
        CALL h5iget_type_f( obj_id, type, ierr )
        ans=( type .EQ. H5I_GROUP_F)
      ENDIF
      ! Close the object
      CALL h5oclose_f(obj_id, ierr)
      IF( ierr .NE. 0) THEN
        CALL e%raiseDebug(modName//'::'// &
          & myName//' - Failed to close HDF object!')
      END IF
    ENDIF
  ENDIF
END PROCEDURE hdf5_isgroup

!----------------------------------------------------------------------------
!                                                                 PathExists
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_pathExists
  INTEGER :: iseg
  TYPE( String ) :: strpath, path2
  TYPE( String ), ALLOCATABLE :: segments( : )

  ! Make sure the object is initialized, and opened
  ans = .FALSE.
  IF( obj%isinit .AND. obj%isOpen() ) THEN
    strpath = TRIM(path)
    CALL strpath%split(segments, '/')
    ans = .TRUE.
    path2 = ''
    DO iseg = 1, SIZE( segments )
      path2 = path2 // '/' // segments(iseg)
      CALL h5lexists_f( obj%file_id, TRIM(path2%chars()), ans, ierr)
      IF( .NOT. ans ) EXIT
    END DO
  ENDIF
END PROCEDURE hdf5_pathExists

!----------------------------------------------------------------------------
!                                                             createHardLink
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_createHardLink
  CHARACTER( LEN = * ), PARAMETER :: myName='hdf5_createHardLink'
  INTEGER( HID_T ) :: src_obj_id

  INTERFACE
    FUNCTION H5Oclose(object_id) RESULT(herr_t) BIND(C,NAME="H5Oclose")
      USE ISO_C_BINDING
      USE HDF5
      INTEGER(HID_T),VALUE :: object_id
      INTEGER :: herr_t
    ENDFUNCTION H5Oclose
  ENDINTERFACE

  IF( obj%pathExists( source_path )) THEN
    IF( .NOT. obj%pathExists( link_path )) THEN
      ! Get the source object ID
      CALL H5Oopen_f(obj%file_id, TRIM(source_path), src_obj_id, ierr)
      !Create the link target object ID
      CALL H5Lcreate_hard_f( src_obj_id, TRIM(source_path), obj%file_id, &
        & TRIM(link_path), ierr )

      !Close the source object
      !CALL H5Oclose_f(src_obj_id,ierr)
      ierr = H5Oclose(src_obj_id)
    ELSE
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Location of new link already exists!')
    ENDIF
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Target of new link must exist in file!')
  ENDIF
END PROCEDURE hdf5_createHardLink

!----------------------------------------------------------------------------
!                                                              getChunkSize
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_getChunkSize
  INTEGER( I4B ) :: ndims, layout
  INTEGER( HID_T ) :: dset_id,dspace_id,dcpl
  INTEGER( HSIZE_T ), ALLOCATABLE :: cdimsH5( : )
  CHARACTER( LEN = * ), PARAMETER :: myName = "hdf5_getChunkSize"

  ! Make sure the object is initialized, and opened
  IF( .NOT. obj%isinit ) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ! Make sure the object is open
  ELSE IF(.NOT.obj%isOpen()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' is already not opened!')
  END IF

  IF( obj%pathExists(path) ) THEN
    !Get the data set ID, associated data space, and rank
    CALL h5dopen_f(obj%file_id,TRIM(path),dset_id,ierr)
    CALL h5dget_space_f(dset_id,dspace_id,ierr)
    CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,ierr)

    !Get the data set creation property list
    CALL h5dget_create_plist_f(dset_id,dcpl,ierr)

    !Get the data space layout and chunk size
    CALL h5pget_layout_f(dcpl,layout,ierr)
    IF(layout == H5D_CHUNKED_F) THEN
      ALLOCATE(cdims(ndims)); cdims=-1
      ALLOCATE(cdimsH5(ndims)); cdimsH5=-1
      CALL h5pget_chunk_f(dcpl,ndims,cdimsH5,ierr)
      cdims=cdimsH5
    ENDIF
  ENDIF
END PROCEDURE hdf5_getChunkSize

!----------------------------------------------------------------------------
!                                                               isCompressed
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_isCompressed
  CHARACTER( LEN = * ), PARAMETER :: myName = "hdf5_isCompressed"
  INTEGER( SIZE_T ), PARAMETER :: namelen=180
  CHARACTER( LEN = namelen ) :: filter_name
  INTEGER( I4B ) :: i, nfilters, filter_id, flags, cd_values(1)
  INTEGER( HID_T ) :: dset_id, dcpl
  INTEGER( SIZE_T ) :: nelmts

  ans = .FALSE.

  ! Make sure the object is initialized, and opened
  ! Make sure the object is initialized, and opened
  IF( .NOT. obj%isinit ) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ! Make sure the object is open
  ELSE IF(.NOT.obj%isOpen()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' is already not opened!')
  END IF

  IF( PRESENT(path) ) THEN
    IF( obj%pathExists(path) ) THEN
      CALL e%setStopOnError(.FALSE.)
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - HDF5file '//obj%getFileName()// &
      & ' path does not exists!')
    END IF

    nelmts=1
    !Get the data set ID
    CALL h5dopen_f( obj%file_id, TRIM(path), dset_id, ierr)
    !Get the data set creation property list
    CALL h5dget_create_plist_f( dset_id, dcpl, ierr )
    !Get the number of filters on the data set, and loop over
    !them to find a compression filter
    CALL h5pget_nfilters_f( dcpl, nfilters, ierr )
    DO i=0,nfilters-1
      CALL h5pget_filter_f( dcpl, i, flags, nelmts, cd_values, &
        & namelen, filter_name, filter_id, ierr )
      ans = ANY(filter_id .EQ. [H5Z_FILTER_DEFLATE_F, &
      & H5Z_FILTER_SZIP_F,H5Z_FILTER_NBIT_F,H5Z_FILTER_NBIT_F] )
      IF( ans ) EXIT
    ENDDO
  ELSE
    ans =  obj%hasCompression
  ENDIF
END PROCEDURE hdf5_isCompressed

!----------------------------------------------------------------------------
!                                                                 preWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE preWrite
  CHARACTER( LEN = * ), PARAMETER :: myName='preWrite'
  INTEGER( HID_T ) :: file_id,oldmem
  INTEGER( HSIZE_T ) :: cdims(rank)
  INTEGER( HSIZE_T ) :: oldsize,newsize
  LOGICAL :: dset_exists
  INTEGER( I4B ) :: lastslash
  TYPE( String ) :: path2

  error=0
  dset_id=-1
  ! Make sure the object is initialized
  IF( .NOT. obj%isinit) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
    error = -1
  ! Check that the file is writable. Best to catch this before HDF5 does.
  ELSEIF(.NOT.obj%isWrite()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - File is readonly!')
    error=-2
  ! Check that the file is Open.
  ELSEIF(.NOT.obj%isOpen()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - File is not Open!')
    error=-3
  ELSE
    file_id=obj%file_id
    !Create an HDF5 parameter list for the dataset creation.
    CALL h5pcreate_f(H5P_DATASET_CREATE_F,plist_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not create parameter list.')
    END IF

    IF(rank .EQ. 0) THEN
      CALL h5screate_f(H5S_SCALAR_F,gspace_id,error)
      IF(error .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Could not create scalar dataspace.')
      END IF
      CALL h5screate_f(H5S_SCALAR_F,dspace_id,error)
      IF(error .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not create scalar dataspace.')
      END IF
    ELSE
      ! Create the dataspace
      ! Global dataspace
      CALL h5screate_simple_f(rank,gdims,gspace_id,error)
      IF(error /= 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not create dataspace.')
      END IF

      ! Local dataspace
      CALL h5screate_simple_f(rank,ldims,dspace_id,error)
      IF(error .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not create dataspace.')
      END IF

      ! Setup the DSpace creation property list to use ZLIB compression
      ! (requires chunking).
      !
      ! Do not compress on scalar data sets.
      IF( obj%hasCompression .AND. &
          & .NOT.(rank .EQ. 1 .AND. &
          & gdims(1) .EQ. 1 )) THEN

        !Compute optimal chunk size and specify in property list.
        CALL compute_chunk_size(mem,gdims,cdims)
        !Logic is equivalent to "compress anything > 1MB"
        IF(.NOT.ALL(gdims == cdims)) THEN
          CALL h5pset_chunk_f(plist_id,rank,cdims,error)
          !Do not presently support user defined compression levels, just level 5
          !5 seems like a good trade-off of speed vs. compression ratio.
          CALL h5pset_deflate_f(plist_id,obj%zlibOpt,error)
        ENDIF
      ENDIF
    ENDIF

    !Create the path if it doesn't exist
    lastslash=INDEX(path,'/',.TRUE.)
    IF(lastslash > 1) THEN
      path2=path(1:lastslash-1)
      IF( .NOT. obj%pathExists( path2%chars() )) THEN
        CALL obj%mkdir( path2%chars( ))
      ENDIF
    ENDIF

    ! Create the dataset, if necessary
    CALL h5lexists_f(file_id,path,dset_exists,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - invalid group path:'//path)
    ENDIF

    IF(obj%overwriteStat .AND. dset_exists) THEN
      ! Open group for overwrite if it already exists and the file has overwrite status
      CALL h5dopen_f(file_id,path,dset_id,error)
      IF(error .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Could not open dataset:'//path)
      END IF

      ! Get the old and new data type sizes
      CALL h5dget_type_f(dset_id,oldmem,error)
      IF(error .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Could not retrieve data type:'//path)
      END IF

      CALL h5tget_size_f(oldmem,oldsize,error)
      IF(error .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Could not retrieve old data type size:'//path)
      END IF

      CALL h5tget_size_f(mem,newsize,error)
      IF(error .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Could not retrieve new data type size:'//path)
      END IF

      ! Check that the size of the data type is equal to or less than the data type size
      ! in the dataset since there is currently no way to resize the dataset
      IF(oldsize < newsize) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Size of new data is greater than size of pre-existing data &
          & type:'//path)
      ENDIF

      ! For non-scalar data, check that the size of the array equal to or less than the
      ! array size in the dataset since there is currently no way to resize the dataset
      CALL h5dget_storage_size_f(dset_id,oldsize,error)
      IF(oldsize < newsize*PRODUCT(gdims)) THEN
        CALL e%raiseError(modName//'::'//myName//" - "// &
          & ' - Storage size of the pre-existing dataset is too small:'//path)
      END IF
    ELSE
      CALL h5dcreate_f(file_id,path,mem,gspace_id,dset_id,error,dcpl_id=plist_id)
      IF(error /= 0) CALL e%raiseError(modName//'::'//myName//" - "// &
          ' - Could not create dataset:'//path)
    ENDIF

    ! Destroy the property list
    CALL h5pclose_f(plist_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not close parameter list.')
    END IF

    ! Select the global dataspace for the dataset
    CALL h5dget_space_f(dset_id,gspace_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not select global dataspace for the dataset.')
    END IF

    ! Create a property list for the write operation
    CALL h5pcreate_f(H5P_DATASET_XFER_F,plist_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not create property list for write operation.')
    END IF
  ENDIF
END PROCEDURE preWrite

!----------------------------------------------------------------------------
!                                                                 ChunkSize
!----------------------------------------------------------------------------

MODULE PROCEDURE compute_chunk_size
  ! Internal variable
  INTEGER( I4B ) :: i, error
  INTEGER( SIZE_T ) :: mb, bsize

  CALL h5tget_size_f(mem,bsize,error)
  mb=1048576/bsize !1MB in terms of the number of elements
  DO i=1,SIZE(cdims)
    cdims(i)=MIN(gdims(i),INT(mb,HSIZE_T))
  ENDDO
END PROCEDURE compute_chunk_size

!----------------------------------------------------------------------------
!                                                                 postwrite
!----------------------------------------------------------------------------

MODULE PROCEDURE postWrite
  CHARACTER(LEN=*),PARAMETER :: myName='postWrite'
  ! Make sure the object is initialized
  IF( .NOT. obj%isinit ) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ! Check that the file is writable. Best to catch this before HDF5 does.
  ELSEIF(.NOT. obj%isWrite()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - File is not Writable!' )
  ! Check that the file is Open.
  ELSEIF(.NOT. obj%isOpen()) THEN
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - File is not Open!')
  ELSE
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Could not write to the dataset.')
    END IF

    ! Close the dataset
    CALL h5dclose_f(dset_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        ' - Could not close the dataset.')
    END IF

    ! Close the dataspace
    CALL h5sclose_f(dspace_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Could not close the dataspace.')
    END IF

    CALL h5sclose_f(gspace_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Could not close the dataspace.')
    END IF

    CALL h5pclose_f(plist_id,error)
    IF(error .NE. 0) THEN
      CALL e%raiseError(modName//'::'//myName//" - "// &
        ' - Could not close the parameter list.')
    END IF
  ENDIF
END PROCEDURE postWrite

!----------------------------------------------------------------------------
!                                                                 PreRead
!----------------------------------------------------------------------------

MODULE PROCEDURE preRead
  CHARACTER( LEN = * ), PARAMETER :: myName='preRead'
  INTEGER( I4B ) :: ndims
  INTEGER( HSIZE_T ) :: maxdims( rank )

  error=0
  ! Make sure the object is initialized
  IF(.NOT. obj%isinit ) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
    error=-1
  ELSEIF(.NOT. obj%isRead() ) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File is not Readable!')
    error=-2
  ELSE
    ! Open the dataset
    CALL h5dopen_f(obj%file_id, path, dset_id, error)
    IF(error .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to open dataset.')

    ! Get dataset dimensions for allocation
    CALL h5dget_space_f(dset_id,dspace_id,error)
    IF(error .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to obtain the dataspace.')

    ! Make sure the rank is right
    IF(rank > 0) THEN
      CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
      IF(error < 0) CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Failed to retrieve number of dataspace dimensions.')
      IF(ndims /= rank) CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Using wrong read function for rank.')
      CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
      IF(error < 0) CALL e%raiseError(modName//'::'//myName//" - "// &
        & ' - Failed to retrieve dataspace dimensions.')
    ELSE
      dims=1
    ENDIF
  ENDIF
END PROCEDURE preRead

!----------------------------------------------------------------------------
!                                                              getDataShape
!----------------------------------------------------------------------------

MODULE PROCEDURE getDataShape
  CHARACTER( LEN = * ), PARAMETER :: myName='preRead'
  CHARACTER( LEN = LEN_TRIM( dsetname ) ) :: path
  INTEGER( I4B ) :: error, ndims
  INTEGER( HID_T ) :: dset_id
  INTEGER( HID_T ) :: dspace_id
  INTEGER( HSIZE_T ), ALLOCATABLE :: dims(:), maxdims(:)

  error=0
  ! Make sure the object is initialized
  IF(.NOT. obj%isinit) THEN
    CALL e%setStopOnError( .FALSE. )
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
    error=-1
  ELSEIF(.NOT.obj%isRead()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File is not Readable!')
    error=-2
  ELSE
    IF(.NOT.obj%isOpen()) THEN
      CALL obj%open()
    ENDIF
    ! Open the dataset
    CALL h5dopen_f(obj%file_id, TRIM(path), dset_id, error)
    IF(error .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to open dataset.')
    CALL h5dget_space_f(dset_id,dspace_id,error)
    IF(error .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to obtain the dataspace.')

    ! Get the number of dimensions
    CALL h5sget_simple_extent_ndims_f(dspace_id,ndims,error)
    IF(error < 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to retrieve number of dataspace dimensions.')

    ! Get the dimensions
    ALLOCATE(dims(ndims))
    ALLOCATE(maxdims(ndims))
    CALL h5sget_simple_extent_dims_f(dspace_id,dims,maxdims,error)
    IF(error < 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to retrieve dataspace dimensions.')

    ! Copy to the Futility integer type
    ALLOCATE(dataShape(SIZE(dims)))
    dataShape(:)=dims(:)
  ENDIF
END PROCEDURE getDataShape

!----------------------------------------------------------------------------
!                                                                getDataType
!----------------------------------------------------------------------------

MODULE PROCEDURE getDataType
  CHARACTER( LEN = * ), PARAMETER :: myName='getDataType'
  CHARACTER( LEN = LEN_TRIM( dsetname ) ) :: path
  INTEGER( I4B ) :: error, class_type
  INTEGER( HID_T ) :: dset_id, dtype
  INTEGER( HSIZE_T ) :: dtype_prec

  error=0
  ! Make sure the object is initialized
  IF( .NOT. obj%isinit ) THEN
    CALL e%setStopOnError( .FALSE. )
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
    error=-1
  ELSEIF( .NOT. obj%isRead() ) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File is not Readable!')
    error=-2
  ELSE
    IF( .NOT. obj%isOpen() ) THEN
      CALL obj%open()
    ENDIF

    ! Open the dataset
    CALL h5dopen_f( obj%file_id, TRIM(path), dset_id, error )
    IF( error .NE. 0 ) CALL e%raiseError( modName//'::'//myName// &
      & ' - Failed to open dataset.' )

    ! Get the dataset type
    CALL h5dget_type_f( dset_id, dtype, error )
    IF( error .NE. 0 ) CALL e%raiseError( modName//'::'//myName// &
      & ' - Failed to retrive dataset type identifier.' )
    CALL h5tget_class_f( dtype, class_type, error )
    IF( error .NE. 0 ) CALL e%raiseError( modName//'::'//myName// &
      & ' - Failed to retrive dataset class.' )
    CALL h5tget_precision_f( dtype, dtype_prec, error )
    IF( error .NE. 0 ) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to retrive dataset precision.')
    dataType='N/A'
    IF( class_type == H5T_FLOAT_F ) THEN
      IF( dtype_prec == 64 ) THEN
        dataType = 'Real64'
      ELSEIF( dtype_prec == 32 ) THEN
        dataType = 'Real32'
      ENDIF
    ELSEIF( class_type == H5T_INTEGER_F ) THEN
      IF( dtype_prec == 64 ) THEN
        dataType = 'Int64'
      ELSEIF( dtype_prec == 32 ) THEN
        dataType = 'Int32'
      ENDIF
    ELSEIF( class_type == H5T_STRING_F ) THEN
      dataType = 'STR'
    ELSE
      CALL e%raiseError( modName//'::'//myName// &
        & ' - Unsupported data type ')
    ENDIF
  ENDIF
END PROCEDURE getDataType

!----------------------------------------------------------------------------
!                                                             postRead
!----------------------------------------------------------------------------

MODULE PROCEDURE postRead
  CHARACTER( LEN=* ), PARAMETER :: myName='postRead'
  INTEGER( HSIZE_T ), ALLOCATABLE :: cdims( : )

  ! Make sure the object is initialized
  IF( .NOT. obj%isinit) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File object not initialized.')
  ELSEIF(.NOT.obj%isRead()) THEN
    CALL e%setStopOnError(.FALSE.)
    CALL e%raiseError(modName// &
      & '::'//myName//' - File is not Readable!')
  ELSE
    IF(error .NE. 0) THEN
      !See if failed read was due to OOM on decompress
      IF( obj%isCompressed( path ) ) THEN
        CALL obj%getChunkSize(path,cdims)
        IF(MAXVAL(cdims) > 16777216_HSIZE_T) THEN !This is 64/128MB
          CALL e%raiseWarning( &      !depending on dataset type.
              modName//'::'//myName//' - Potentially high memory usage'// &
              'when reading decompressed dataset "'//TRIM(path)//'".'// &
              CHAR(10)//CHAR(10)//'Try decompressing file before rerunning:'// &
              CHAR(10)//'$ h5repack -f NONE "'// &
              TRIM(obj%fullname)//'" "'// &
              TRIM(obj%fullname)//'.uncompressed"')
        ENDIF
      ENDIF
      CALL e%raiseError(modName//'::'//myName//" - "// &
        & '- Failed to read data from dataset"'//TRIM(path)//'".')
    ENDIF
    ! Close the dataset
    CALL h5dclose_f(dset_id,error)
    IF(error .NE. 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to close dataset "'//TRIM(path)//'".')
    ! Close the dataspace
    CALL h5sclose_f(dspace_id,error)
    IF(error /= 0) CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - Failed to close dataspace for "'//TRIM(path)//'".')
  ENDIF
END PROCEDURE postRead

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
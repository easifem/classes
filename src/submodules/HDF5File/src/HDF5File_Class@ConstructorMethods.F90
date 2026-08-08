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

SUBMODULE(HDF5File_Class) ConstructorMethods
USE GlobalData, ONLY: CHAR_LF, CHAR_SLASH
USE System_Method, ONLY: RWX_U, System_Mkdir
USE ExceptionHandler_Class, ONLY: EXCEPTION_MAX_MESG_LENGTH
USE AbstractFile_Class, ONLY: AbstractFileDeallocate
USE HDF5, ONLY: SIZE_T, H5P_FILE_ACCESS_F, H5P_DATASET_CREATE_F, &
                H5P_DATASET_XFER_F, H5PCLOSE_F, H5F_CLOSE_SEMI_F, &
                H5F_ACC_RDONLY_F, H5F_ACC_RDWR_F, H5F_ACC_TRUNC_F, &
                H5FCREATE_F, H5FOPEN_F, H5_INDEX_NAME_F, &
                H5_ITER_INC_F, H5I_GROUP_F, H5D_CHUNKED_F, &
                H5Z_FILTER_DEFLATE_F, H5Z_FILTER_NBIT_F, &
                H5Z_FILTER_SZIP_F, H5S_SCALAR_F, H5T_FLOAT_F, &
                H5T_INTEGER_F, H5T_STRING_F, H5DCREATE_F, H5CLOSE_F, &
                H5DCLOSE_F, H5DGET_CREATE_PLIST_F, H5DGET_SPACE_F, &
                H5DGET_STORAGE_SIZE_F, H5DGET_TYPE_F, H5DOPEN_F, &
                H5GGET_INFO_F, H5GOPEN_F, H5IGET_TYPE_F, H5LCREATE_HARD_F, &
                H5LEXISTS_F, H5LGET_NAME_BY_IDX_F, H5OCLOSE_F, H5OOPEN_F, &
                H5OPEN_F, H5PCREATE_F, H5PGET_CHUNK_F, H5PGET_LAYOUT_F, &
                H5PGET_NFILTERS_F, H5ESET_AUTO_F, H5FCLOSE_F, &
                H5GCLOSE_F, H5GCREATE_F, H5PGET_FILTER_F, H5PSET_CHUNK_F, &
                H5PSET_DEFLATE_F, H5PSET_FCLOSE_DEGREE_F, &
                H5SCLOSE_F, H5SCREATE_F, H5SCREATE_SIMPLE_F, &
                H5SGET_SIMPLE_EXTENT_DIMS_F, H5SGET_SIMPLE_EXTENT_NDIMS_F, &
                H5TGET_CLASS_F, H5TGET_PRECISION_F, H5TGET_SIZE_F

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(LEN=*), PARAMETER :: &
  modName = 'HDF5File_Class@ConstructorMethods.F90'
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                              IsInitiated
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsInitiated
ans = obj%isInit
END PROCEDURE obj_IsInitiated

!----------------------------------------------------------------------------
!                                                                 HDF5Open
!----------------------------------------------------------------------------

SUBROUTINE HDF5Open
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "HDF5Open()"
#endif
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (.NOT. libh5Open) THEN
    CALL H5OPEN_F(ierr)

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, "Error opening file.")
#endif

    libh5Open = math%yes
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE HDF5Open

!----------------------------------------------------------------------------
!                                                                 HDF5Close
!----------------------------------------------------------------------------

SUBROUTINE HDF5Close
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "HDF5Close()"
#endif
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  ierr = -1
  IF (libh5Open) CALL H5CLOSE_F(ierr)
  IF (ierr .EQ. 0) libh5Open = math%no

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE HDF5Close

!----------------------------------------------------------------------------
!                                                                 HDF5Quiet
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2026-08-08
! summary: Enable/disable HDF5 exception writing

SUBROUTINE HDF5Quiet(quiet)
  LOGICAL, INTENT(IN) :: quiet
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "HDF5Quiet()"
#endif
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  IF (quiet) THEN
    CALL H5ESET_AUTO_F(0, ierr)
  ELSE
    CALL H5ESET_AUTO_F(1, ierr)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE HDF5Quiet

!----------------------------------------------------------------------------
!                                                                 Open
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_open
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_open()'
#endif
INTEGER :: acc
INTEGER(HID_T) :: plist_id
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isinit) THEN
  CALL H5PCREATE_F(H5P_FILE_ACCESS_F, plist_id, ierr)
  CALL H5PSET_FCLOSE_DEGREE_F(plist_id, H5F_CLOSE_SEMI_F, ierr)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, "Unable to create property list")
#endif

  ! Decide what access type to use
  IF (obj%IsNew()) THEN
    acc = H5F_ACC_TRUNC_F
    CALL H5FCREATE_F(obj%fullname%Chars(), acc, obj%file_id, ierr, &
                     access_prp=plist_id)
    ! If the file is NEW, change the mode to WRITE after
    ! Creating it so we don't keep truncating it repeatedly.
    CALL obj%SetNewStat(math%no)

  ELSEIF (obj%IsWrite()) THEN
    acc = H5F_ACC_RDWR_F
    CALL H5FOPEN_F(obj%fullname%Chars(), acc, obj%file_id, ierr, &
                   access_prp=plist_id)

  ELSEIF (obj%IsRead()) THEN
    acc = H5F_ACC_RDONLY_F
    CALL H5FOPEN_F(obj%fullname%chars(), acc, obj%file_id, ierr, &
                   access_prp=plist_id)

  ELSE
    CALL e%RaiseError(modName//'::'//myName//" - "// &
                      'Unrecognized access mode! The file is not'// &
                      ' set as either new, read, or write!')
  END IF

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, "Unable to open file.")
#endif

  CALL H5PCLOSE_F(plist_id, ierr)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, "Unable to destroy property list.")
#endif

  CALL obj%SetOpenStat(math%yes)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_open

!----------------------------------------------------------------------------
!                                                                 Close
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_close
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_close()'
#endif
LOGICAL(LGT) :: lastStopOnError
INTEGER(I4B) :: ierr

lastStopOnError = e%isStopOnError()
CALL e%SetStopOnError(math%no)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, "File object not initialized.")
#endif

!Check open status.
IF (obj%Isopen()) THEN
  CALL H5FCLOSE_F(obj%file_id, ierr)
  obj%file_id = 0

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, "Unable to close file.")
#endif

  CALL obj%SetOpenStat(math%no)
END IF

CALL e%SetStopOnError(lastStopOnError)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_close

!----------------------------------------------------------------------------
!                                                                 Delete
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_delete
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_delete()'
#endif
CHARACTER(EXCEPTION_MAX_MESG_LENGTH) :: emesg
TYPE(String) :: fileName
INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isinit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

!So, HDF5 is special in that the unitno assigned isn't used in the
!fopen() operation.  So, regardless of the %isOpen() status, it needs
!to be opened.
fileName = obj%getFilePath()//"/"//obj%getFileName()// &
           obj%getFileExt()

OPEN (UNIT=obj%unitno, FILE=fileName%chars(), IOSTAT=ierr)

#ifdef DEBUG_VER
WRITE (emesg, '(a,i4,a,i4)') 'Error deleting file (UNIT=', &
  obj%unitno, ' ) IOSTAT=', ierr
CALL AssertError1(ierr .EQ. 0, myName, emesg)
#endif

CLOSE (UNIT=obj%unitno, STATUS='DELETE', IOSTAT=ierr)

#ifdef DEBUG_VER
WRITE (emesg, '(a,i4,a,i4)') 'Error deleting file (UNIT=', &
  obj%unitno, ' ) IOSTAT=', ierr
CALL AssertError1(ierr .EQ. 0, myName, emesg)
#endif

CALL obj%SetOpenStat(math%no)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_delete

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_initiate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_initiate()'
#endif
TYPE(String) :: fpath, fname, fext, mode_in, file_
INTEGER(I4B) :: unitno, ierr
LOGICAL(LGT) :: ostat, exists
CHARACTER(LEN(filename)) :: tempchars

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(.NOT. obj%isInit, myName, &
                  ' - HDF5file '//obj%GetFileName()// &
                  ' is already initialized!')
#endif

file_ = TRIM(filename)
IF (file_%SCAN(CHAR_SLASH) .EQ. 0_I4B) THEN
  fpath = "."//CHAR_SLASH
ELSE
  fpath = file_%Basedir(sep=CHAR_SLASH)//CHAR_SLASH
END IF

fext = file_%Extension()
fname = file_%Basename(extension=fext%Chars(), sep=CHAR_SLASH)
! CALL getPath(chars=filename, path=tempchars)
! fpath = TRIM(tempchars)
! CALL getFileNameExt(chars=filename, ext=tempchars)
! fext = TRIM(tempchars)
! CALL getFileName(chars=filename, fname=tempchars)
! fname = TRIM(tempchars)
CALL obj%SetFilePath(fpath)
CALL obj%SetFileName(fname)
CALL obj%SetFileExt(fext)

ierr = System_Mkdir(fpath//'', RWX_U)
#ifdef DEBUG_VER
IF (ierr .NE. 0_I4B .AND. ierr .NE. -1_I4B) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'error occured while creating the directory')
END IF
#endif

IF (PRESENT(zlibOpt)) THEN
  IF (zlibOpt .GE. 0) THEN
    obj%hasCompression = math%yes
    obj%zlibOpt = zlibOpt
  END IF
END IF

! Store the access mode
mode_in = mode
mode_in = mode_in%upper()

SELECT CASE (mode_in%Chars())

CASE ('READ')
  INQUIRE (FILE=filename, EXIST=exists)

#ifdef DEBUG_VER
  CALL AssertError1(exists, myName, &
                    ' - HDF5 file '//filename//' is being opened with '// &
                    'mode READ but does not exist.')
#endif
  CALL obj%setWriteStat(.FALSE.)
  CALL obj%setReadStat(.TRUE.)

CASE ('WRITE')
  INQUIRE (FILE=filename, EXIST=exists)

#ifdef DEBUG_VER
  CALL AssertError1(exists, myName, &
                    ' - HDF5 file '//filename//' is being opened with '// &
                    'mode WRITE but does not exist.')
#endif
  CALL obj%setWriteStat(.TRUE.)
  CALL obj%setReadStat(.FALSE.)

CASE ('OVERWRITE', 'READWRITE')
  INQUIRE (FILE=filename, EXIST=exists)

#ifdef DEBUG_VER
  CALL AssertError1(exists, myName, &
                    ' - HDF5 file '//filename//' is being opened with '// &
                    'mode OVERWRITE but does not exist.')
#endif
  CALL obj%setWriteStat(.TRUE.)
  CALL obj%setOverwriteStat(.TRUE.)
  CALL obj%setReadStat(.TRUE.)

CASE ('NEW')
  CALL obj%setWriteStat(.TRUE.)
  CALL obj%setReadStat(.TRUE.)
  CALL obj%setNewStat(.TRUE.)
  CALL obj%setOverwriteStat(.TRUE.)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for access mode.")
#endif

END SELECT

obj%fullname = filename
! Initialize the HDF5 interface. This needs be done before any other calls
! to the HF5 interface can be made.
CALL HDF5Open()
! Assign arbitrary UNIT number to file.  Used only for deleting file.
unitno = 99
INQUIRE (UNIT=unitno, OPENED=ostat)
DO WHILE (obj%unitno == -1)
  IF (ostat) THEN
    unitno = unitno - 1_I4B
    INQUIRE (UNIT=unitno, OPENED=ostat)
  ELSE
    obj%unitno = unitno
  END IF
END DO
obj%isinit = .TRUE.
nhdf5fileinuse = nhdf5fileinuse + 1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_initiate

!----------------------------------------------------------------------------
!                                                                 Clear
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_clear
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_clear()"
#endif
LOGICAL(LGT) :: bool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

!Logical to close or delete the file.
bool = .FALSE.
IF (PRESENT(delete)) bool = Delete
IF (bool) THEN
  CALL obj%Delete()
ELSE
  CALL obj%CLOSE()
END IF

! Close the HDF5 interface. This can only be done once all calls to the
! HDF5 library are complete.
nhdf5fileinuse = nhdf5fileinuse - 1
IF (libh5Open .AND. (nhdf5fileinuse == 0)) CALL HDF5Close()
obj%isinit = .FALSE.
obj%newstat = .FALSE.
obj%hasCompression = .FALSE.
obj%zlibOpt = -1
obj%fullname = ''
obj%unitno = -1
obj%overwriteStat = .FALSE.

CALL AbstractFileDeallocate(obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
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
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_ls()'
#endif
CHARACTER(1024) :: tmpchar
TYPE(String) :: path2
INTEGER(HSIZE_T) :: i
INTEGER(HID_T) :: grp_id
INTEGER :: store_type, nlinks, max_corder, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "File not Initialized")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  'HDF5file '//obj%getFileName()// &
                  ' is not opened!')
#endif

! Make sure the object is initialized
IF (ALLOCATED(objs)) THEN
  DEALLOCATE (objs)
END IF

IF (.NOT. obj%IsGroup(path)) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

path2 = TRIM(path)
CALL H5GOPEN_F(obj%file_id, TRIM(path), grp_id, ierr)

#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  'Unable to open file.')
#endif

CALL H5GGET_INFO_F(grp_id, store_type, nlinks, max_corder, ierr)

#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  'Unable to get group information.')
#endif

ALLOCATE (objs(nlinks))
DO i = 0, nlinks - 1
  CALL H5LGET_NAME_BY_IDX_F(obj%file_id, TRIM(path), &
                            H5_INDEX_NAME_F, H5_ITER_INC_F, i, &
                            tmpchar, ierr)
  objs(i + 1) = TRIM(tmpchar)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Unable to get object name.')
#endif

END DO

CALL H5GCLOSE_F(grp_id, ierr)

#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  "Unable to close group.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_ls

!----------------------------------------------------------------------------
!                                                                       mkdir
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_mkdir
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myname = 'hdf5_mkdir()'
#endif
TYPE(String) :: path3
INTEGER(HID_T) :: group_id
LOGICAL :: dset_exists
INTEGER(I4B) :: lastslash, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsWrite(), myName, &
                  "file object does not have write access.")
#endif

! Make sure the object is initialized
! Convert the path to use slashes
lastslash = INDEX(path, '/', .TRUE.)
IF (lastslash > 1) THEN
  path3 = path(1:lastslash - 1)
  IF (.NOT. obj%PathExists(path3%chars())) THEN
    CALL obj%Mkdir(path3%chars())
  END IF
END IF

CALL H5LEXISTS_F(obj%file_id, path, dset_exists, ierr)
#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  'invalid group path: '//path)
#endif

! If group exists, do nothing, but only if overwrites are allowed
IF (obj%overwriteStat .AND. dset_exists) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

! Create the group
CALL H5GCREATE_F(obj%file_id, path, group_id, ierr)
#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, "failed to create hdf group.")
#endif

! Close the group
CALL h5gclose_f(group_id, ierr)
#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, "failed to close hdf group.")
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_mkdir

!----------------------------------------------------------------------------
!                                                                 mkalldir
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_mkalldir
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_mkalldir()'
#endif
INTEGER(I4B) :: i, nslash, ierr
INTEGER(I4B), ALLOCATABLE :: slashloc(:)
TYPE(String) :: path2, tmppath
INTEGER(HID_T) :: group_id

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not Initialized.")

CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")

CALL AssertError1(obj%IsWrite(), myName, &
                  "file object does not have write access.")
#endif

! Make sure the object is initialized
ierr = 0
! Convert the path to use slashes
path2 = TRIM(path)
CALL path2%strfind("/", slashloc)
nslash = SIZE(slashloc)

DO i = 1, nslash - 1
  tmppath = path2%slice(1, slashloc(i + 1) - 1)
  IF (.NOT. obj%pathExists(TRIM(tmppath%chars()))) THEN
    CALL H5GCREATE_F(obj%file_id, TRIM(tmppath%chars()), group_id, ierr)

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, "failed to create a hdf group.")
#endif

    CALL H5GCLOSE_F(group_id, ierr)

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Failed to close a HDF group')
#endif
  END IF
END DO

DEALLOCATE (slashloc)
! Create the group
IF (.NOT. obj%pathExists(path2%chars())) THEN
  CALL H5GCREATE_F(obj%file_id, TRIM(path2%chars()), group_id, ierr)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Failed to create a HDF group')
#endif

  CALL H5GCLOSE_F(group_id, ierr)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Failed to close HDF group')
#endif

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_mkalldir

!----------------------------------------------------------------------------
!                                                                       ngrp
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_ngrp
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_ngrp()'
#endif
INTEGER(HID_T) :: grp_id
INTEGER :: store_type, nlinks, max_corder, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Make sure the object is initialized

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

CALL H5GOPEN_F(obj%file_id, TRIM(path), grp_id, ierr)
#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, "could not open group in hdf5 file")
#endif

CALL H5GGET_INFO_F(grp_id, store_type, nlinks, max_corder, ierr)
#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  'Could not get group info in HDF5 file.')
#endif

! Close the group
CALL H5GCLOSE_F(grp_id, ierr)
#ifdef DEBUG_VER
CALL AssertError1(ierr .EQ. 0, myName, &
                  'Failed to close HDF group')
#endif

ans = nlinks

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_ngrp

!----------------------------------------------------------------------------
!                                                                 isGroup
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_isgroup
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_isgroup()'
#endif
INTEGER(HID_T) :: obj_id
INTEGER(I4B) :: TYPE, ierr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

! Make sure the object is initialized and opened
ans = .FALSE.

IF (obj%isinit .AND. obj%IsOpen()) THEN
  ans = obj%pathExists(path)

  IF (ans) THEN
    !Need to get the object ID from the path...
    CALL H5OOPEN_F(obj%file_id, path, obj_id, ierr)
    IF (ierr == -1) THEN
      ans = .FALSE.
    ELSE
      CALL h5iget_type_f(obj_id, TYPE, ierr)
      ans = (TYPE .EQ. H5I_GROUP_F)
    END IF
    ! Close the object
    CALL H5OCLOSE_F(obj_id, ierr)

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Failed to close HDF object.')
#endif

  END IF
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_isgroup

!----------------------------------------------------------------------------
!                                                                 PathExists
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_pathExists
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_pathExists()"
#endif
INTEGER :: iseg, ierr
TYPE(String) :: strpath, path2
TYPE(String), ALLOCATABLE :: segments(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = .FALSE.
IF (obj%isinit .AND. obj%isOpen()) THEN
  strpath = TRIM(path)
  CALL strpath%split(segments, '/')
  ans = .TRUE.
  path2 = ''
  DO iseg = 1, SIZE(segments)
    path2 = path2//'/'//segments(iseg)
    CALL H5LEXISTS_F(obj%file_id, TRIM(path2%chars()), ans, ierr)
    IF (.NOT. ans) EXIT
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_pathExists

!----------------------------------------------------------------------------
!                                                             createHardLink
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_createHardLink
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'hdf5_createHardLink()'
#endif
INTEGER(HID_T) :: src_obj_id
INTEGER(I4B) :: ierr

INTERFACE
  FUNCTION H5Oclose(object_id) RESULT(herr_t) BIND(C, NAME="H5Oclose")
    USE HDF5, ONLY: HID_T
    INTEGER(HID_T), VALUE :: object_id
    INTEGER :: herr_t
  END FUNCTION H5Oclose
END INTERFACE

#ifdef DEBUG_VER
CALL AssertError1(obj%pathExists(source_path), myName, &
                  'Target of new link must exist in file!')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%pathExists(link_path), myName, &
                  'Location of new link already exists!')
#endif

! Get the source object ID
CALL H5OOPEN_F(obj%file_id, TRIM(source_path), src_obj_id, ierr)
!Create the link target object ID
CALL H5LCREATE_HARD_F(src_obj_id, TRIM(source_path), obj%file_id, &
                      TRIM(link_path), ierr)

!Close the source object
!CALL H5Oclose_f(src_obj_id,ierr)
ierr = H5OCLOSE(src_obj_id)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_createHardLink

!----------------------------------------------------------------------------
!                                                              getChunkSize
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_getChunkSize
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_getChunkSize()"
#endif
INTEGER(I4B) :: ndims, layout, ierr
INTEGER(HID_T) :: dset_id, dspace_id, dcpl
INTEGER(HSIZE_T), ALLOCATABLE :: cdimsH5(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

IF (obj%pathExists(path)) THEN
  !Get the data set ID, associated data space, and rank
  CALL H5DOPEN_F(obj%file_id, TRIM(path), dset_id, ierr)
  CALL H5DGET_SPACE_F(dset_id, dspace_id, ierr)
  CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, ierr)

  !Get the data set creation property list
  CALL H5DGET_CREATE_PLIST_F(dset_id, dcpl, ierr)

  !Get the data space layout and chunk size
  CALL H5PGET_LAYOUT_F(dcpl, layout, ierr)
  IF (layout == H5D_CHUNKED_F) THEN
    ALLOCATE (cdims(ndims))
    cdims = -1
    ALLOCATE (cdimsH5(ndims))
    cdimsH5 = -1
    CALL H5PGET_CHUNK_F(dcpl, ndims, cdimsH5, ierr)
    cdims = cdimsH5
  END IF
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_getChunkSize

!----------------------------------------------------------------------------
!                                                               isCompressed
!----------------------------------------------------------------------------

MODULE PROCEDURE hdf5_isCompressed
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "hdf5_isCompressed()"
#endif
INTEGER(SIZE_T), PARAMETER :: namelen = 180
CHARACTER(namelen) :: filter_name
INTEGER(I4B) :: i, nfilters, filter_id, flags, cd_values(1), ierr
INTEGER(HID_T) :: dset_id, dcpl
INTEGER(SIZE_T) :: nelmts

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = .FALSE.

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

IF (.NOT. PRESENT(path)) THEN

  ans = obj%hasCompression

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
CALL AssertError1(.NOT. obj%pathExists(path), myName, &
                  ' HDF5file '//obj%getFileName()// &
                  ' path does not exists!')
#endif

nelmts = 1
!Get the data set ID
CALL H5DOPEN_F(obj%file_id, TRIM(path), dset_id, ierr)
!Get the data set creation property list
CALL H5DGET_CREATE_PLIST_F(dset_id, dcpl, ierr)
!Get the number of filters on the data set, and loop over
!them to find a compression filter
CALL H5PGET_NFILTERS_F(dcpl, nfilters, ierr)

DO i = 0, nfilters - 1
  CALL H5PGET_FILTER_F(dcpl, i, flags, nelmts, cd_values, &
                       namelen, filter_name, filter_id, ierr)
  ans = ANY(filter_id .EQ. [H5Z_FILTER_DEFLATE_F, &
                            H5Z_FILTER_SZIP_F, &
                            H5Z_FILTER_NBIT_F, &
                            H5Z_FILTER_NBIT_F])
  IF (ans) EXIT
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE hdf5_isCompressed

!----------------------------------------------------------------------------
!                                                                 preWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE preWrite
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'preWrite()'
#endif
INTEGER(HID_T) :: file_id, oldmem
INTEGER(HSIZE_T) :: cdims(rank)
INTEGER(HSIZE_T) :: oldsize, newsize
LOGICAL :: dset_exists
INTEGER(I4B) :: lastslash
TYPE(String) :: path2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

error = 0
dset_id = -1

#ifdef DEBUG_VER
! error = -1
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
! error = -3
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

#ifdef DEBUG_VER
! error = -2
CALL AssertError1(obj%IsWrite(), myName, &
                  "file object does not have write access.")
#endif

! Make sure the object is initialized
file_id = obj%file_id
!Create an HDF5 parameter list for the dataset creation.
CALL H5PCREATE_F(H5P_DATASET_CREATE_F, plist_id, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not create parameter list.')
#endif

IF (rank .EQ. 0) THEN
  CALL H5SCREATE_F(H5S_SCALAR_F, gspace_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not create scalar dataspace.')
#endif

  CALL H5SCREATE_F(H5S_SCALAR_F, dspace_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not create scalar dataspace.')
#endif

ELSE

  ! Create the dataspace
  ! Global dataspace
  CALL H5SCREATE_SIMPLE_F(rank, gdims, gspace_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not create dataspace.')
#endif

  ! Local dataspace
  CALL H5SCREATE_SIMPLE_F(rank, ldims, dspace_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    ' Could not create dataspace.')
#endif

  ! Setup the DSpace creation property list to use ZLIB compression
  ! (requires chunking).
  ! Do not compress on scalar data sets.
  IF (obj%hasCompression .AND. &
      .NOT. (rank .EQ. 1 .AND. gdims(1) .EQ. 1)) THEN

    !Compute optimal chunk size and specify in property list.
    CALL COMPUTE_CHUNK_SIZE(mem, gdims, cdims)
    !Logic is equivalent to "compress anything > 1MB"
    IF (.NOT. ALL(gdims == cdims)) THEN
      CALL H5PSET_CHUNK_F(plist_id, rank, cdims, error)
      !Do not presently support user defined compression levels, just level 5
      !5 seems like a good trade-off of speed vs. compression ratio.
      CALL H5PSET_DEFLATE_F(plist_id, obj%zlibOpt, error)
    END IF

  END IF
END IF

!Create the path if it doesn't exist
lastslash = INDEX(path, '/', .TRUE.)
IF (lastslash > 1) THEN
  path2 = path(1:lastslash - 1)
  IF (.NOT. obj%pathExists(path2%chars())) THEN
    CALL obj%mkdir(path2%chars())
  END IF
END IF

! Create the dataset, if necessary
CALL H5LEXISTS_F(file_id, path, dset_exists, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'invalid group path:'//path)
#endif

IF (obj%overwriteStat .AND. dset_exists) THEN
  ! Open group for overwrite if it already exists and the file has overwrite status
  CALL H5DOPEN_F(file_id, path, dset_id, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    ' Could not open dataset:'//path)
#endif

  ! Get the old and new data type sizes
  CALL H5DGET_TYPE_F(dset_id, oldmem, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not retrieve data type:'//path)
#endif

  CALL H5TGET_SIZE_F(oldmem, oldsize, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not retrieve old data type size:'//path)
#endif

  CALL H5TGET_SIZE_F(mem, newsize, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not retrieve new data type size:'//path)
#endif

  ! Check that the size of the data type is equal to or less than the
  ! data type size
  ! in the dataset since there is currently no way to resize the dataset

#ifdef DEBUG_VER
  CALL AssertError1(oldsize .GE. newsize, myName, &
      'Size of new data is greater than size of pre-existing data &
      & type:'//path)
#endif

  ! For non-scalar data, check that the size of the array equal to
  ! or less than the
  ! array size in the dataset since there is currently no way to
  ! resize the dataset
  CALL H5DGET_STORAGE_SIZE_F(dset_id, oldsize, error)

#ifdef DEBUG_VER
  CALL AssertError1(oldsize .GE. (newsize * PRODUCT(gdims)), myName, &
                    'Storage size of the pre-existing dataset is too small:' &
                    //path)
#endif
ELSE
  CALL H5DCREATE_F(file_id,path,mem,gspace_id,dset_id,error,dcpl_id=plist_id)

#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, &
                    'Could not create dataset:'//path)
#endif

END IF

! Destroy the property list
CALL H5PCLOSE_F(plist_id, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not close parameter list.')
#endif

! Select the global dataspace for the dataset
CALL H5DGET_SPACE_F(dset_id, gspace_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not select global dataspace for the dataset.')
#endif

! Create a property list for the write operation
CALL H5PCREATE_F(H5P_DATASET_XFER_F, plist_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not create property list for write operation.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE preWrite

!----------------------------------------------------------------------------
!                                                                 ChunkSize
!----------------------------------------------------------------------------

MODULE PROCEDURE compute_chunk_size
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "compute_chunk_size()"
#endif
INTEGER(I4B) :: i, error
INTEGER(SIZE_T) :: mb, bsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL H5TGET_SIZE_F(mem, bsize, error)
mb = 1048576 / bsize
!1MB in terms of the number of elements
DO i = 1, SIZE(cdims)
  cdims(i) = MIN(gdims(i), INT(mb, HSIZE_T))
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE compute_chunk_size

!----------------------------------------------------------------------------
!                                                                 postwrite
!----------------------------------------------------------------------------

MODULE PROCEDURE postWrite
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'postWrite()'
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsWrite(), myName, &
                  "file object does not have write access.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not write to the dataset.')
#endif

! Close the dataset
CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not close the dataset.')
#endif

! Close the dataspace
CALL H5SCLOSE_F(dspace_id, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not close the dataspace.')
#endif

CALL H5SCLOSE_F(gspace_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not close the dataspace.')
#endif

CALL H5PCLOSE_F(plist_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Could not close the parameter list.')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE postWrite

!----------------------------------------------------------------------------
!                                                                 PreRead
!----------------------------------------------------------------------------

MODULE PROCEDURE preRead
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'preRead()'
#endif
INTEGER(I4B) :: ndims
INTEGER(HSIZE_T) :: maxdims(rank)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

error = 0
! Make sure the object is initialized

#ifdef DEBUG_VER
! error = -1
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

#ifdef DEBUG_VER
! error = -2
CALL AssertError1(obj%IsRead(), myName, &
                  "file object does not have read access.")
#endif

! Open the dataset
CALL H5DOPEN_F(obj%file_id, path, dset_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to open dataset.')
#endif

! Get dataset dimensions for allocation
CALL H5DGET_SPACE_F(dset_id, dspace_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to obtain the dataspace.')
#endif

! Make sure the rank is right
IF (rank > 0) THEN
  CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .GE. 0, myName, &
                    'Failed to retrieve number of dataspace dimensions.')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(ndims .EQ. rank, myName, &
                    'Using wrong read function for rank.')
#endif

  CALL H5SGET_SIMPLE_EXTENT_DIMS_F(dspace_id, dims, maxdims, error)

#ifdef DEBUG_VER
  CALL AssertError1(error .GE. 0, myName, &
                    'Failed to retrieve dataspace dimensions.')
#endif

ELSE
  dims = 1
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE preRead

!----------------------------------------------------------------------------
!                                                              getDataShape
!----------------------------------------------------------------------------

MODULE PROCEDURE getDataShape
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'getDataShape()'
#endif
CHARACTER(LEN_TRIM(dsetname)) :: path
INTEGER(I4B) :: error, ndims
INTEGER(HID_T) :: dset_id
INTEGER(HID_T) :: dspace_id
INTEGER(HSIZE_T), ALLOCATABLE :: dims(:), maxdims(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

error = 0

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsRead(), myName, &
                  "file object does not have read access.")
#endif

! Open the dataset
CALL H5DOPEN_F(obj%file_id, TRIM(path), dset_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to open dataset.')
#endif

CALL H5DGET_SPACE_F(dset_id, dspace_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to obtain the dataspace.')
#endif

! Get the number of dimensions
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, error)
#ifdef DEBUG_VER
CALL AssertError1(error .GE. 0, myName, &
                  'Failed to retrieve number of dataspace dimensions.')
#endif

! Get the dimensions
ALLOCATE (dims(ndims))
ALLOCATE (maxdims(ndims))
CALL H5SGET_SIMPLE_EXTENT_DIMS_F(dspace_id, dims, maxdims, error)
#ifdef DEBUG_VER
CALL AssertError1(error .GE. 0, myName, &
                  'Failed to retrieve dataspace dimensions.')
#endif

! Copy to the Futility integer type
ALLOCATE (dataShape(SIZE(dims)))
dataShape = INT(dims, kind=I4B)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE getDataShape

!----------------------------------------------------------------------------
!                                                                getDataType
!----------------------------------------------------------------------------

MODULE PROCEDURE getDataType
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'getDataType()'
#endif
CHARACTER(LEN_TRIM(dsetname)) :: path
INTEGER(I4B) :: error, class_type
INTEGER(HID_T) :: dset_id, dtype
INTEGER(HSIZE_T) :: dtype_prec

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

error = 0

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsRead(), myName, &
                  "file object does not have read access.")
#endif

! Open the dataset
CALL H5DOPEN_F(obj%file_id, TRIM(path), dset_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to open dataset.')
#endif

! Get the dataset type
CALL H5DGET_TYPE_F(dset_id, dtype, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  ' Failed to retrive dataset type identifier.')
#endif

CALL H5TGET_CLASS_F(dtype, class_type, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to retrive dataset class.')
#endif

CALL H5TGET_PRECISION_F(dtype, dtype_prec, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to retrive dataset precision.')
#endif

dataType = 'N/A'
IF (class_type == H5T_FLOAT_F) THEN
  IF (dtype_prec == 64) THEN
    dataType = 'Rea'
  ELSEIF (dtype_prec == 32) THEN
    dataType = 'Rea'
  END IF
ELSEIF (class_type == H5T_INTEGER_F) THEN
  IF (dtype_prec == 64) THEN
    dataType = 'Int'
  ELSEIF (dtype_prec == 32) THEN
    dataType = 'Int'
  END IF
ELSEIF (class_type == H5T_STRING_F) THEN
  dataType = 'STR'
ELSE
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'Unsupported data type ')
#endif
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE getDataType

!----------------------------------------------------------------------------
!                                                             postRead
!----------------------------------------------------------------------------

MODULE PROCEDURE postRead
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = 'postRead()'
#endif
INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInit, myName, &
                  "file object is not initialized.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsOpen(), myName, &
                  "file object is not open.")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsRead(), myName, &
                  "file object does not have read access.")
#endif

! Make sure the object is initialized
#ifdef DEBUG_VER
IF (error .NE. 0) THEN
  !See if failed read was due to OOM on decompress
  IF (obj%IsCompressed(path)) THEN

    CALL obj%GetChunkSize(path, cdims)

    IF (MAXVAL(cdims) > 16777216_HSIZE_T) THEN

      !This is 64/128MB
      CALL e%RaiseWarning( &
        modName//'::'//myName//' - Potentially high memory usage'// &
        'when reading decompressed dataset "'//TRIM(path)//'".'// &
        CHAR(10)//CHAR(10)//'Try decompressing file before rerunning:'// &
        CHAR(10)//'$ h5repack -f NONE "'// &
        obj%fullname%Chars()//'" "'// &
        obj%fullname%Chars()//'.uncompressed"')
    END IF
  END IF

  CALL e%RaiseError(modName//'::'//myName//" - "// &
                    'Failed to read data from dataset"'//TRIM(path)//'".')
END IF
#endif

! Close the dataset
CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to close dataset "'//TRIM(path))
#endif

! Close the dataspace
CALL H5SCLOSE_F(dspace_id, error)
#ifdef DEBUG_VER
CALL AssertError1(error .EQ. 0, myName, &
                  'Failed to close dataspace for "'//TRIM(path))
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE postRead

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INCLUDE "../../include/errors.F90"

END SUBMODULE ConstructorMethods

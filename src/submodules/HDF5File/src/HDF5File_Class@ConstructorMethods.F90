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
  H5_ITER_NATIVE_F, H5I_GROUP_F, H5D_CHUNKED_F, &
  H5Z_FILTER_DEFLATE_F, H5Z_FILTER_NBIT_F, &
  H5Z_FILTER_SZIP_F, H5S_SCALAR_F, H5T_FLOAT_F, &
  H5T_INTEGER_F, H5T_STRING_F, H5DCREATE_F, H5CLOSE_F, H5T_ENUM_F, &
  H5DCLOSE_F, H5DGET_CREATE_PLIST_F, H5DGET_SPACE_F, &
  H5DGET_TYPE_F, H5DOPEN_F, &
  H5GGET_INFO_F, H5GOPEN_F, H5IGET_TYPE_F, H5LCREATE_HARD_F, &
  H5LEXISTS_F, H5LGET_NAME_BY_IDX_F, H5OCLOSE_F, H5OOPEN_F, &
  H5OPEN_F, H5PCREATE_F, H5PGET_CHUNK_F, H5PGET_LAYOUT_F, &
  H5PGET_NFILTERS_F, H5ESET_AUTO_F, H5FCLOSE_F, &
  H5GCLOSE_F, H5GCREATE_F, H5PGET_FILTER_F, H5PSET_CHUNK_F, &
  H5PSET_DEFLATE_F, H5PSET_FCLOSE_DEGREE_F, H5PSET_SHUFFLE_F, &
  H5SCLOSE_F, H5SCREATE_F, H5SCREATE_SIMPLE_F, &
  H5SGET_SIMPLE_EXTENT_DIMS_F, H5SGET_SIMPLE_EXTENT_NDIMS_F, &
  H5PSET_LAYOUT_F, H5D_COMPACT_F, &
  H5PSET_LIBVER_BOUNDS_F, H5F_LIBVER_V18_F, H5F_LIBVER_V110_F, &
  H5F_LIBVER_V112_F, H5F_LIBVER_V114_F, H5F_LIBVER_V200_F, &
  H5F_LIBVER_LATEST_F, &
  H5TGET_CLASS_F, H5TGET_PRECISION_F, H5TGET_SIZE_F, H5TCLOSE_F

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
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_IsInitiated()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%isInit

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ierr = -1
  IF (libh5Open) CALL H5CLOSE_F(ierr)
  IF (ierr .EQ. 0) libh5Open = math%no

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (quiet) THEN
    CALL H5ESET_AUTO_F(0, ierr)
  ELSE
    CALL H5ESET_AUTO_F(1, ierr)
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END SUBROUTINE HDF5Quiet

!----------------------------------------------------------------------------
!                                                                 Open
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Open
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_Open()'
#endif
  INTEGER :: acc, libver
  INTEGER(HID_T) :: plist_id
  INTEGER(I4B) :: ierr, ierr2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (obj%isinit) THEN
    ! ierr is the status of creating or opening the file; the open
    ! status (and the new status) change only on success
    CALL H5PCREATE_F(H5P_FILE_ACCESS_F, plist_id, ierr)
    CALL H5PSET_FCLOSE_DEGREE_F(plist_id, H5F_CLOSE_SEMI_F, ierr)

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, "Unable to create property list")
#endif

    ! Decide what access type to use
    IF (obj%IsNew()) THEN
      ! Format version of the new file: (libverLow, latest); 0 keeps the
      ! default bounds of the HDF5 library
      SELECT CASE (obj%libverLow)
      CASE (18)
        libver = H5F_LIBVER_V18_F
      CASE (110)
        libver = H5F_LIBVER_V110_F
      CASE (112)
        libver = H5F_LIBVER_V112_F
      CASE (114)
        libver = H5F_LIBVER_V114_F
      CASE (200)
        libver = H5F_LIBVER_V200_F
      CASE DEFAULT
        libver = -1
      END SELECT

      IF (libver .GE. 0) THEN
        CALL H5PSET_LIBVER_BOUNDS_F(plist_id, libver, H5F_LIBVER_LATEST_F, &
                                    ierr2)
#ifdef DEBUG_VER
        CALL AssertError1(ierr2 .EQ. 0, myName, &
                          "Unable to set the file format version.")
#endif
      END IF

      acc = H5F_ACC_TRUNC_F
      CALL H5FCREATE_F(obj%fullname%Chars(), acc, obj%file_id, ierr, &
                       access_prp=plist_id)
      ! If the file is NEW, change the mode to WRITE after
      ! Creating it so we don't keep truncating it repeatedly.
      IF (ierr .EQ. 0) CALL obj%SetNewStat(math%no)

    ELSE IF (obj%IsWrite()) THEN
      acc = H5F_ACC_RDWR_F
      CALL H5FOPEN_F(obj%fullname%Chars(), acc, obj%file_id, ierr, &
                     access_prp=plist_id)

    ELSE IF (obj%IsRead()) THEN
      acc = H5F_ACC_RDONLY_F
      CALL H5FOPEN_F(obj%fullname%chars(), acc, obj%file_id, ierr, &
                     access_prp=plist_id)

    ELSE
      ierr = -1
#ifdef DEBUG_VER
      CALL e%RaiseError(modName // '::' // myName // " - " // &
                        'Unrecognized access mode! The file is not' // &
                        ' set as either new, read, or write!')
#endif
    END IF

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, "Unable to open file.")
#endif

    CALL H5PCLOSE_F(plist_id, ierr2)

#ifdef DEBUG_VER
    CALL AssertError1(ierr2 .EQ. 0, myName, &
                      "Unable to destroy property list.")
#endif

    IF (ierr .EQ. 0) THEN
      CALL obj%SetOpenStat(math%yes)
    ELSE
      obj%file_id = 0
    END IF
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Open

!----------------------------------------------------------------------------
!                                                                 Close
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Close
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_Close()'
#endif
  LOGICAL(LGT) :: lastStopOnError
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  lastStopOnError = e%isStopOnError()
  CALL e%SetStopOnError(math%no)

#ifdef DEBUG_VER
  CALL AssertError1(obj%isInit, myName, "File object not initialized.")
#endif

!Check open status.
  IF (obj%IsOpen()) THEN
    CALL H5FCLOSE_F(obj%file_id, ierr)

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, "Unable to close file.")
#endif

    ! On failure the file is still open (for example, an object of the
    ! file is still open), so keep the id and the open status
    IF (ierr .EQ. 0) THEN
      obj%file_id = 0
      CALL obj%SetOpenStat(math%no)
    END IF
  END IF

  CALL e%SetStopOnError(lastStopOnError)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Close

!----------------------------------------------------------------------------
!                                                                 Delete
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Delete
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_Delete()'
#endif
  CHARACTER(EXCEPTION_MAX_MESG_LENGTH) :: emesg, iomsg
  TYPE(String) :: fileName
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. obj%isinit) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif
    RETURN
  END IF

  !So, HDF5 is special in that the unitno assigned isn't used in the
  !fopen() operation.  So, regardless of the %IsOpen() status, it needs
  !to be opened.
  fileName = obj%GetFilePath() // "/" // obj%GetFileName() // &
    obj%GetFileExt()

  iomsg = ''
  OPEN(UNIT=obj%unitno, FILE=fileName%chars(), ACTION='READ', &
       IOSTAT=ierr, iomsg=iomsg)

#ifdef DEBUG_VER
  WRITE(emesg, '(a,i4,a,i4,a)') 'Error opening file (UNIT=', &
    obj%unitno, ' ) IOSTAT=', ierr, ' IOMSG='
  CALL AssertError1(ierr .EQ. 0, myName, TRIM(emesg) // TRIM(iomsg))
#endif

  CLOSE(UNIT=obj%unitno, STATUS='DELETE', IOSTAT=ierr, iomsg=iomsg)

#ifdef DEBUG_VER
  WRITE(emesg, '(a,i4,a,i4,a)') 'Error deleting file (UNIT=', &
    obj%unitno, ' ) IOSTAT=', ierr, ' IOMSG='
  CALL AssertError1(ierr .EQ. 0, myName, TRIM(emesg) // TRIM(iomsg))
#endif

  CALL obj%SetOpenStat(math%no)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Delete

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_Initiate()'
#endif
  TYPE(String) :: fpath, fname, fext, mode_in, file_
  INTEGER(I4B) :: unitno, ierr
  LOGICAL(LGT) :: ostat, exists

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. obj%isInit, myName, &
                    ' - HDF5file ' // obj%GetFileName() // &
                    ' is already initialized!')
#endif

  file_ = TRIM(filename)
  IF (file_%SCAN(CHAR_SLASH) .EQ. 0_I4B) THEN
    fpath = "." // CHAR_SLASH
  ELSE
    fpath = file_%Basedir(sep=CHAR_SLASH) // CHAR_SLASH
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

  ! Layout and compression settings: default values unless chunkPolicy
  ! is present; zlibOpt replaces the deflate level
  obj%chunkPolicy = HDF5ChunkPolicy_()
  IF (PRESENT(chunkPolicy)) obj%chunkPolicy = chunkPolicy
  IF (PRESENT(zlibOpt)) obj%chunkPolicy%deflateLevel = zlibOpt

#ifdef DEBUG_VER
  CALL AssertError1(obj%chunkPolicy%deflateLevel .GE. -1 .AND. &
                    obj%chunkPolicy%deflateLevel .LE. 9, myName, &
                    'deflateLevel must be -1 (no compression) or 0 to 9.')
  CALL AssertError1(obj%chunkPolicy%targetBytes .GT. 0, myName, &
                    'targetBytes must be positive.')
  CALL AssertError1(obj%chunkPolicy%minBytes .GE. 0, myName, &
                    'minBytes must not be negative.')
  CALL AssertError1(obj%chunkPolicy%chunkMethod .EQ. 1 .OR. &
                    obj%chunkPolicy%chunkMethod .EQ. 2, myName, &
                    'chunkMethod must be 1 (uniform) or 2 (max).')
  CALL AssertError1(obj%chunkPolicy%stepsPerChunk .GE. 1, myName, &
                    'stepsPerChunk must be at least 1.')
#endif

  obj%hasCompression = obj%chunkPolicy%deflateLevel .GE. 0

  ! File format version of a new file and limit of the compact layout
  obj%libverLow = 110
  IF (PRESENT(libverLow)) obj%libverLow = libverLow
  obj%compactBytes = 4096_INT64
  IF (PRESENT(compactBytes)) obj%compactBytes = compactBytes

#ifdef DEBUG_VER
  CALL AssertError1(ANY(obj%libverLow .EQ. [0, 18, 110, 112, 114, 200]), &
                    myName, &
                    'libverLow must be 0, 18, 110, 112, 114 or 200.')
  CALL AssertError1(obj%compactBytes .GE. 0 .AND. &
                    obj%compactBytes .LE. 64000, myName, &
                    'compactBytes must be between 0 and 64000.')
#endif

! Store the access mode
  mode_in = mode
  mode_in = mode_in%upper()

  SELECT CASE (mode_in%Chars())

  CASE ('READ')
    INQUIRE(FILE=filename, EXIST=exists)

#ifdef DEBUG_VER
    CALL AssertError1(exists, myName, &
                      ' - HDF5 file ' // filename // &
                      ' is being opened with ' // &
                      'mode READ but does not exist.')
#endif
    CALL obj%SetWriteStat(math%no)
    CALL obj%SetReadStat(math%yes)

  CASE ('WRITE')
    INQUIRE(FILE=filename, EXIST=exists)

#ifdef DEBUG_VER
    CALL AssertError1(exists, myName, &
                      ' - HDF5 file ' // filename // &
                      ' is being opened with ' // &
                      'mode WRITE but does not exist.')
#endif
    CALL obj%SetWriteStat(math%yes)
    CALL obj%SetReadStat(math%no)

  CASE ('OVERWRITE', 'READWRITE')
    INQUIRE(FILE=filename, EXIST=exists)

#ifdef DEBUG_VER
    CALL AssertError1(exists, myName, &
                      ' - HDF5 file ' // filename // &
                      ' is being opened with ' // &
                      'mode OVERWRITE but does not exist.')
#endif
    CALL obj%SetWriteStat(math%yes)
    CALL obj%SetOverwriteStat(math%yes)
    CALL obj%SetReadStat(math%yes)

  CASE ('NEW')
    ! Only a new file needs its directory; create it if it is missing
    ierr = System_Mkdir(fpath // '', RWX_U)
#ifdef DEBUG_VER
    IF (ierr .NE. 0_I4B .AND. ierr .NE. -1_I4B) THEN
      CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                        'error occured while creating the directory')
    END IF
#endif

    CALL obj%SetWriteStat(math%yes)
    CALL obj%SetReadStat(math%yes)
    CALL obj%SetNewStat(math%yes)
    CALL obj%SetOverwriteStat(math%yes)

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
  INQUIRE(UNIT=unitno, OPENED=ostat)
  DO WHILE (obj%unitno == -1)
    IF (ostat) THEN
      unitno = unitno - 1_I4B
      INQUIRE(UNIT=unitno, OPENED=ostat)
    ELSE
      obj%unitno = unitno
    END IF
  END DO
  obj%isinit = math%yes
  nhdf5fileinuse = nhdf5fileinuse + 1

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Initiate

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif
  LOGICAL(LGT) :: bool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  IF (.NOT. obj%isInit) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif
    RETURN
  END IF

  ! Close the file; then delete it if delete is true (the HDF5 file id
  ! must not stay open while the file is removed)
  bool = math%no
  IF (PRESENT(delete)) bool = delete
  CALL obj%CLOSE()
  IF (bool) CALL obj%Delete()

  ! Close the HDF5 interface. This can only be done once all calls to the
  ! HDF5 library are complete.
  nhdf5fileinuse = nhdf5fileinuse - 1
  IF (libh5Open .AND. (nhdf5fileinuse == 0)) CALL HDF5Close()
  obj%isinit = math%no
  obj%newstat = math%no
  obj%hasCompression = math%no
  obj%chunkPolicy = HDF5ChunkPolicy_()
  obj%libverLow = 110
  obj%compactBytes = 4096_INT64
  obj%fullname = ''
  obj%unitno = -1
  obj%overwriteStat = math%no

  CALL AbstractFileDeallocate(obj)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                          setOverWriteStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOverwriteStat
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetOverwriteStat()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  obj%overwriteStat = bool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetOverwriteStat

!----------------------------------------------------------------------------
!                                                                 getUnitNo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetUnitNo
  ans = obj%unitno
END PROCEDURE obj_GetUnitNo

!----------------------------------------------------------------------------
!                                                                 isNew
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsNew
  ans = obj%newstat
END PROCEDURE obj_IsNew

!----------------------------------------------------------------------------
!                                                                 setNewStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNewStat
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetNewStat()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  obj%newstat = bool

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetNewStat

!----------------------------------------------------------------------------
!                                                                 ls
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Ls
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_Ls()'
#endif
  CHARACTER(1024) :: tmpchar
  TYPE(String), ALLOCATABLE :: names(:)
  INTEGER(HSIZE_T) :: i
  INTEGER(HID_T) :: grp_id
  INTEGER :: store_type, nlinks, max_corder, ierr
  INTEGER(I4B) :: k
  INTEGER(I4B), ALLOCATABLE :: idx(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%isInit, myName, &
                    "File not Initialized")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsOpen(), myName, &
                    'HDF5file ' // obj%GetFileName() // &
                    ' is not opened!')
#endif

! Make sure the object is initialized
  IF (ALLOCATED(objs)) THEN
    DEALLOCATE(objs)
  END IF

  IF (.NOT. obj%IsGroup(path)) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif
    RETURN
  END IF

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

  ! The names are read in the native order of the group, which HDF5 gives
  ! without sorting (in increasing order it sorts all the names for every
  ! call, which is O(n^2 log n) for n names), and then sorted once
  ALLOCATE(names(nlinks))
  DO i = 0, nlinks - 1
    CALL H5LGET_NAME_BY_IDX_F(grp_id, ".", H5_INDEX_NAME_F, &
                              H5_ITER_NATIVE_F, i, tmpchar, ierr)
    names(i + 1) = TRIM(tmpchar)

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Unable to get object name.')
#endif

  END DO

  ALLOCATE(idx(nlinks))
  DO k = 1, nlinks
    idx(k) = k
  END DO
  CALL ArgSortNames(names, idx)
  ALLOCATE(objs(nlinks))
  DO k = 1, nlinks
    objs(k) = names(idx(k))
  END DO

  CALL H5GCLOSE_F(grp_id, ierr)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    "Unable to close group.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Ls

!----------------------------------------------------------------------------
!                                                               ArgSortNames
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-26
! summary: Sort the indices of names in increasing (byte) order
!
! Merge sort of idx, so that names(idx(:)) is in increasing order of the
! character codes (as the name index of HDF5); O(n log n).

SUBROUTINE ArgSortNames(names, idx)
  TYPE(String), INTENT(IN) :: names(:)
  !! names to sort
  INTEGER(I4B), INTENT(INOUT) :: idx(:)
  !! on entry 1, ..., n; on exit the sorted order
  INTEGER(I4B), ALLOCATABLE :: tmp(:)
  INTEGER(I4B) :: n, width, lo, mid, hi, i, j, k

  n = SIZE(idx)
  IF (n .LT. 2) RETURN
  ALLOCATE(tmp(n))

  width = 1
  DO WHILE (width .LT. n)
    lo = 1
    DO WHILE (lo .LE. n)
      mid = MIN(lo + width - 1, n)
      hi = MIN(lo + 2 * width - 1, n)
      i = lo
      j = mid + 1
      DO k = lo, hi
        IF (i .LE. mid .AND. j .LE. hi) THEN
          IF (LLE(names(idx(i))%chars(), names(idx(j))%chars())) THEN
            tmp(k) = idx(i)
            i = i + 1
          ELSE
            tmp(k) = idx(j)
            j = j + 1
          END IF
        ELSE IF (i .LE. mid) THEN
          tmp(k) = idx(i)
          i = i + 1
        ELSE
          tmp(k) = idx(j)
          j = j + 1
        END IF
      END DO
      lo = lo + 2 * width
    END DO
    idx = tmp
    width = 2 * width
  END DO
END SUBROUTINE ArgSortNames

!----------------------------------------------------------------------------
!                                                                       mkdir
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Mkdir
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myname = 'obj_Mkdir()'
#endif
  TYPE(String) :: path3
  INTEGER(HID_T) :: group_id
  LOGICAL :: dset_exists
  INTEGER(I4B) :: lastslash, ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myname // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%isInit, myname, &
                    "file object is not initialized.")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsOpen(), myname, &
                    "file object is not open.")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsWrite(), myname, &
                    "file object does not have write access.")
#endif

! Make sure the object is initialized
! Convert the path to use slashes
  lastslash = INDEX(path, '/', math%yes)
  IF (lastslash > 1) THEN
    path3 = path(1:lastslash - 1)
    IF (.NOT. obj%PathExists(path3%chars())) THEN
      CALL obj%Mkdir(path3%chars())
    END IF
  END IF

  CALL H5LEXISTS_F(obj%file_id, path, dset_exists, ierr)
#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myname, &
                    'invalid group path: ' // path)
#endif

! If the group exists, there is nothing to do (in every mode: creating
! an existing group does not overwrite anything)
  IF (dset_exists) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myname // ' - ' // &
                            '[END] ')
#endif
    RETURN
  END IF

! Create the group
  CALL H5GCREATE_F(obj%file_id, path, group_id, ierr)
#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myname, "failed to create hdf group.")
#endif

! Close the group
  CALL h5gclose_f(group_id, ierr)
#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myname, "failed to close hdf group.")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myname // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Mkdir

!----------------------------------------------------------------------------
!                                                                 mkalldir
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Mkalldir
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_Mkalldir()'
#endif
  INTEGER(I4B) :: i, nslash, ierr
  INTEGER(I4B), ALLOCATABLE :: slashloc(:)
  TYPE(String) :: path2, tmppath
  INTEGER(HID_T) :: group_id

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
    IF (.NOT. obj%PathExists(TRIM(tmppath%chars()))) THEN
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

  DEALLOCATE(slashloc)
! Create the group
  IF (.NOT. obj%PathExists(path2%chars())) THEN
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Mkalldir

!----------------------------------------------------------------------------
!                                                                       ngrp
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Ngrp
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_Ngrp()'
#endif
  INTEGER(HID_T) :: grp_id
  INTEGER :: store_type, nlinks, max_corder, ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Ngrp

!----------------------------------------------------------------------------
!                                                                 isGroup
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsGroup
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_IsGroup()'
#endif
  INTEGER(HID_T) :: obj_id
  INTEGER(I4B) :: TYPE, ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

! Make sure the object is initialized and opened
  ans = math%no

  IF (obj%isinit .AND. obj%IsOpen()) THEN
    ans = obj%PathExists(path)

    IF (ans) THEN
      !Need to get the object ID from the path...
      CALL H5OOPEN_F(obj%file_id, path, obj_id, ierr)
      IF (ierr == -1) THEN
        ans = math%no
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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_IsGroup

!----------------------------------------------------------------------------
!                                                                 PathExists
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_PathExists
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_PathExists()"
#endif
  INTEGER :: iseg, ierr
  TYPE(String) :: strpath, path2
  TYPE(String), ALLOCATABLE :: segments(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = math%no
  IF (obj%isinit .AND. obj%IsOpen()) THEN
    ! The root group always exists. Segments of slashes only (leading or
    ! double slashes) are skipped, so that each checked prefix is a valid
    ! link name: "/a", "/a/b", ...
    ans = math%yes

    ! A path of slashes only is the root group
    IF (VERIFY(TRIM(path), '/') .EQ. 0) THEN
#ifdef DEBUG_VER
      CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                              '[END] ')
#endif
      RETURN
    END IF

    strpath = TRIM(path)
    CALL strpath%split(segments, '/')
    path2 = ''
    aloop: DO iseg = 1, SIZE(segments)
      IF (VERIFY(segments(iseg)%chars(), '/') .EQ. 0) CYCLE aloop
      path2 = path2 // '/' // segments(iseg)
      CALL H5LEXISTS_F(obj%file_id, TRIM(path2%chars()), ans, ierr)
      IF (ierr .NE. 0) ans = math%no
      IF (.NOT. ans) EXIT aloop
    END DO aloop
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_PathExists

!----------------------------------------------------------------------------
!                                                             createHardLink
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_CreateHardLink
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_CreateHardLink()'
  LOGICAL(LGT) :: isok
#endif
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  isok = obj%PathExists(source_path)
  CALL AssertError1(isok, myName, 'Target of new link must exist in file!')
#endif

#ifdef DEBUG_VER
  isok = .NOT. obj%PathExists(link_path)
  CALL AssertError1(isok, myName, 'Location of new link already exists!')
#endif

! Both paths are absolute, so the file is the location of both; the
! source object does not need to be opened
  CALL H5LCREATE_HARD_F(obj%file_id, TRIM(source_path), obj%file_id, &
                        TRIM(link_path), ierr)

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, 'Could not create the hard link.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_CreateHardLink

!----------------------------------------------------------------------------
!                                                              getChunkSize
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetChunkSize
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetChunkSize()"
#endif
  INTEGER(I4B) :: ndims, layout, ierr
  INTEGER(HID_T) :: dset_id, dspace_id, dcpl
  INTEGER(HSIZE_T), ALLOCATABLE :: cdimsH5(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  CALL AssertError1(obj%PathExists(path), myName, &
                    "path: " // path // " does not exit.")
#endif

  !Get the data set ID, associated data space, and rank
  CALL H5DOPEN_F(obj%file_id, TRIM(path), dset_id, ierr)
  CALL H5DGET_SPACE_F(dset_id, dspace_id, ierr)
  CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, ierr)

  !Get the data set creation property list
  CALL H5DGET_CREATE_PLIST_F(dset_id, dcpl, ierr)

  !Get the data space layout and chunk size
  CALL H5PGET_LAYOUT_F(dcpl, layout, ierr)
  IF (layout == H5D_CHUNKED_F) THEN
    ALLOCATE(cdims(ndims))
    cdims = -1
    ALLOCATE(cdimsH5(ndims))
    cdimsH5 = -1
    CALL H5PGET_CHUNK_F(dcpl, ndims, cdimsH5, ierr)
    cdims = cdimsH5
  END IF

  CALL H5PCLOSE_F(dcpl, ierr)
#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Failed to close the creation property list.')
#endif

  CALL H5SCLOSE_F(dspace_id, ierr)
#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Failed to close the dataspace.')
#endif

  CALL H5DCLOSE_F(dset_id, ierr)
#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Failed to close the dataset.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetChunkSize

!----------------------------------------------------------------------------
!                                                               isCompressed
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsCompressed
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_IsCompressed()"
  LOGICAL(LGT) :: isok
#endif
  INTEGER(SIZE_T), PARAMETER :: namelen = 180
  CHARACTER(namelen) :: filter_name
  INTEGER(I4B) :: i, nfilters, filter_id, flags, cd_values(1), ierr
  INTEGER(HID_T) :: dset_id, dcpl
  INTEGER(SIZE_T) :: nelmts

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = math%no

#ifdef DEBUG_VER
  CALL AssertError1(obj%isInit, myName, "file object is not initialized.")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsOpen(), myName, "file object is not open.")
#endif

  IF (.NOT. PRESENT(path)) THEN

    ans = obj%hasCompression

#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif

    RETURN
  END IF

  ! The following code is executed when path is present

#ifdef DEBUG_VER
  isok = obj%PathExists(path)
  CALL AssertError1(isok, myName, 'path=' // path // ' does not exist!')
#endif

  nelmts = 1
  !Get the data set ID
  CALL H5DOPEN_F(obj%file_id, TRIM(path), dset_id, ierr)
  !Get the data set creation property list
  CALL H5DGET_CREATE_PLIST_F(dset_id, dcpl, ierr)
  !Get the number of filters on the data set, and loop over
  !them to find a compression filter
  CALL H5PGET_NFILTERS_F(dcpl, nfilters, ierr)

  mainloop: DO i = 0, nfilters - 1
    CALL H5PGET_FILTER_F(dcpl, i, flags, nelmts, cd_values, &
                         namelen, filter_name, filter_id, ierr)
    ans = ANY(filter_id .EQ. [H5Z_FILTER_DEFLATE_F, &
                              H5Z_FILTER_SZIP_F, &
                              H5Z_FILTER_NBIT_F])
    IF (ans) EXIT mainloop
  END DO mainloop

  CALL H5PCLOSE_F(dcpl, ierr)
  CALL H5DCLOSE_F(dset_id, ierr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_IsCompressed

!----------------------------------------------------------------------------
!                                                                 preWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE preWrite
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'preWrite()'
  INTEGER(HID_T) :: oldmem
  INTEGER(HSIZE_T) :: oldsize, newsize
#endif
  INTEGER(HID_T) :: oldspace
  INTEGER(HSIZE_T) :: olddims(MAX(RANK, 1)), oldmaxdims(MAX(RANK, 1))
  INTEGER(I4B) :: oldrank
  LOGICAL(LGT) :: sameShape
  INTEGER(HID_T) :: file_id
  INTEGER(HSIZE_T) :: cdims(RANK)
  INTEGER(SIZE_T) :: bsize
  INTEGER(INT64) :: nbytes
  LOGICAL :: dset_exists
  INTEGER(I4B) :: lastslash, ierr
  TYPE(String) :: path2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! ERROR keeps the first failure; ierr is the status of each call.
  ! The ids which are not created are set to -1, so postWrite does not
  ! close them.
  ERROR = 0
  dset_id = -1
  dspace_id = -1
  gspace_id = -1
  plist_id = -1

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

  file_id = obj%file_id

  ! Create an HDF5 parameter list for the dataset creation.
  CALL H5PCREATE_F(H5P_DATASET_CREATE_F, plist_id, ierr)
  IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Could not create parameter list.')
#endif

  IF (RANK .EQ. 0) THEN
    CALL H5SCREATE_F(H5S_SCALAR_F, gspace_id, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not create scalar dataspace.')
#endif

    CALL H5SCREATE_F(H5S_SCALAR_F, dspace_id, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not create scalar dataspace.')
#endif

  ELSE

    ! Global dataspace
    CALL H5SCREATE_SIMPLE_F(RANK, gdims, gspace_id, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not create dataspace.')
#endif

    ! Local dataspace
    CALL H5SCREATE_SIMPLE_F(RANK, ldims, dspace_id, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not create dataspace.')
#endif

  END IF

  ! Size of the data in bytes (0 for an empty dataset)
  nbytes = 0
  bsize = 0
  IF (ERROR .EQ. 0 .AND. ALL(gdims .GT. 0)) THEN
    CALL H5TGET_SIZE_F(mem, bsize, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not get the size of the data type.')
#endif

    IF (ierr .EQ. 0) nbytes = INT(bsize, INT64) * PRODUCT(INT(gdims, INT64))
  END IF

  ! Layout of a new dataset:
  ! - chunked and compressed: rank > 0, compression on, at least
  !   chunkPolicy%minBytes of data (compression needs chunking)
  ! - compact (data in the object header): less than compactBytes
  ! - contiguous: otherwise
  IF (RANK .GT. 0 .AND. obj%hasCompression .AND. nbytes .GT. 0 .AND. &
      nbytes .GE. obj%chunkPolicy%minBytes) THEN
    CALL compute_chunk_size(mem, gdims, obj%chunkPolicy, cdims)
    CALL H5PSET_CHUNK_F(plist_id, RANK, cdims, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not set the chunk size.')
#endif

    ! Shuffle the bytes before deflate; useless for 1-byte elements
    IF (obj%chunkPolicy%shuffle .AND. bsize .GT. 1) THEN
      CALL H5PSET_SHUFFLE_F(plist_id, ierr)
      IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
      CALL AssertError1(ierr .EQ. 0, myName, &
                        'Could not set the shuffle filter.')
#endif
    END IF

    CALL H5PSET_DEFLATE_F(plist_id, obj%chunkPolicy%deflateLevel, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not set the deflate filter.')
#endif

  ELSE IF (nbytes .GT. 0 .AND. nbytes .LT. obj%compactBytes) THEN
    CALL H5PSET_LAYOUT_F(plist_id, H5D_COMPACT_F, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not set the compact layout.')
#endif
  END IF

  ! Create the path if it doesn't exist
  lastslash = INDEX(path, '/', math%yes)
  IF (ERROR .EQ. 0 .AND. lastslash > 1) THEN
    path2 = path(1:lastslash - 1)
    IF (.NOT. obj%PathExists(path2%chars())) THEN
      CALL obj%Mkdir(path2%chars())
    END IF
  END IF

  ! Create the dataset, if necessary
  dset_exists = .FALSE.
  IF (ERROR .EQ. 0) THEN
    CALL H5LEXISTS_F(file_id, path, dset_exists, ierr)
    ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'invalid group path:' // path)
#endif
  END IF

  IF (ERROR .NE. 0) THEN
    ! Nothing to do; the dataset is not opened or created

  ELSE IF (obj%overwriteStat .AND. dset_exists) THEN
    ! Open group for overwrite if it already exists and
    ! the file has overwrite status
    CALL H5DOPEN_F(file_id, path, dset_id, ierr)
    ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      ' Could not open dataset:' // path)
#endif

    ! The data is written in place, so the new data must have the shape of
    ! the existing dataset (use Append for data that grows); otherwise
    ! nothing is written and ERROR is not 0
    IF (ERROR .EQ. 0) THEN
      CALL H5DGET_SPACE_F(dset_id, oldspace, ierr)
      CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(oldspace, oldrank, ierr)
      sameShape = oldrank .EQ. RANK
      IF (sameShape .AND. RANK .GT. 0) THEN
        CALL H5SGET_SIMPLE_EXTENT_DIMS_F(oldspace, olddims, oldmaxdims, ierr)
        sameShape = ALL(olddims(1:RANK) .EQ. gdims(1:RANK))
      END IF
      CALL H5SCLOSE_F(oldspace, ierr)

#ifdef DEBUG_VER
      CALL AssertError1(sameShape, myName, &
                        'The dataset ' // path // ' exists with another ' // &
                        'shape. Overwrite writes in place, so the new ' // &
                        'data must have the same shape; use Append for ' // &
                        'data that grows, or write a new dataset.')
#endif

      IF (.NOT. sameShape) THEN
        CALL H5DCLOSE_F(dset_id, ierr)
        ERROR = -1
      END IF
    END IF

#ifdef DEBUG_VER
    ! The element of the dataset must not be smaller than the new one
    IF (ERROR .EQ. 0) THEN
      CALL H5DGET_TYPE_F(dset_id, oldmem, ierr)
      CALL AssertError1(ierr .EQ. 0, myName, &
                        'Could not retrieve data type:' // path)

      CALL H5TGET_SIZE_F(oldmem, oldsize, ierr)
      CALL AssertError1(ierr .EQ. 0, myName, &
                        'Could not retrieve old data type size:' // path)

      CALL H5TGET_SIZE_F(mem, newsize, ierr)
      CALL AssertError1(ierr .EQ. 0, myName, &
                        'Could not retrieve new data type size:' // path)

      CALL AssertError1(oldsize .GE. newsize, myName, &
                        'Size of new data is greater than size of ' // &
                        'pre-existing data  type:' // path)

      CALL H5TCLOSE_F(oldmem, ierr)
      CALL AssertError1(ierr .EQ. 0, myName, &
                        'Could not close old data type:' // path)
    END IF
#endif

  ELSE
    CALL H5DCREATE_F(file_id, path, mem, gspace_id, dset_id, ierr, &
                     dcpl_id=plist_id)
    ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not create dataset:' // path)
#endif

  END IF

  IF (ERROR .NE. 0) dset_id = -1

  ! Destroy the property list
  IF (plist_id .GT. 0) THEN
    CALL H5PCLOSE_F(plist_id, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr
    plist_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not close parameter list.')
#endif
  END IF

  ! Close the dataspace created above; H5DGET_SPACE_F returns a new one
  IF (gspace_id .GT. 0) THEN
    CALL H5SCLOSE_F(gspace_id, ierr)
    IF (ERROR .EQ. 0) ERROR = ierr
    gspace_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not close the created dataspace.')
#endif
  END IF

  IF (ERROR .NE. 0) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                            '[END] ')
#endif
    RETURN
  END IF

  ! Select the global dataspace for the dataset
  CALL H5DGET_SPACE_F(dset_id, gspace_id, ierr)
  ERROR = ierr
  IF (ierr .NE. 0) gspace_id = -1

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Could not select global dataspace for the dataset.')
#endif

  ! Create a property list for the write operation
  CALL H5PCREATE_F(H5P_DATASET_XFER_F, plist_id, ierr)
  IF (ERROR .EQ. 0) ERROR = ierr
  IF (ierr .NE. 0) plist_id = -1

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Could not create property list for write operation.')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  INTEGER(SIZE_T) :: bsize
  INTEGER(HSIZE_T) :: ntarget, nother, c, nch

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  CALL H5TGET_SIZE_F(mem, bsize, error)
  bsize = MAX(bsize, 1_SIZE_T)

  ! targetBytes in terms of the number of elements
  ntarget = MAX(INT(policy%targetBytes / INT(bsize, INT64), HSIZE_T), &
                1_HSIZE_T)

  ! Start from the whole dataset and reduce the last dimensions first
  cdims = MAX(gdims, 1_HSIZE_T)
  dimLoop: DO i = SIZE(cdims), 1, -1
    IF (PRODUCT(cdims) .LE. ntarget) EXIT dimLoop
    nother = PRODUCT(cdims) / cdims(i)
    IF (nother .GE. ntarget) THEN
      cdims(i) = 1
    ELSE
      ! largest size that fits (chunkMethod 2)
      c = MAX(ntarget / nother, 1_HSIZE_T)

      ! chunkMethod 1: same number of chunks along dimension i, but equal
      ! parts, so the last chunk is (almost) full
      IF (policy%chunkMethod .NE. 2) THEN
        nch = (cdims(i) + c - 1) / c
        c = (cdims(i) + nch - 1) / nch
      END IF

      cdims(i) = c
    END IF
  END DO dimLoop

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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

#ifdef DEBUG_VER
  CALL AssertError1(ERROR .EQ. 0, myName, &
                    'Could not write to the dataset.')
#endif

! Close the ids which were created; preWrite sets the others to -1.
! ERROR keeps the status of the write.
  IF (dset_id .GT. 0) THEN
    CALL H5DCLOSE_F(dset_id, ierr)
    dset_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not close the dataset.')
#endif
  END IF

  IF (dspace_id .GT. 0) THEN
    CALL H5SCLOSE_F(dspace_id, ierr)
    dspace_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not close the dataspace.')
#endif
  END IF

  IF (gspace_id .GT. 0) THEN
    CALL H5SCLOSE_F(gspace_id, ierr)
    gspace_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not close the dataspace.')
#endif
  END IF

  IF (plist_id .GT. 0) THEN
    CALL H5PCLOSE_F(plist_id, ierr)
    plist_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Could not close the parameter list.')
#endif
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  INTEGER(I4B) :: ndims, ierr
  INTEGER(HSIZE_T) :: maxdims(RANK)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ! ERROR is 0 on success and negative on the first failure. The ids
  ! which are not opened are set to -1, so postRead does not close them.
  ERROR = 0
  dset_id = -1
  dspace_id = -1
  dims = 1

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
  CALL H5DOPEN_F(obj%file_id, path, dset_id, ierr)
  ERROR = ierr
  IF (ierr .NE. 0) dset_id = -1

#ifdef DEBUG_VER
  CALL AssertError1(ierr .EQ. 0, myName, &
                    'Failed to open dataset.')
#endif

! Get dataset dimensions for allocation
  IF (ERROR .EQ. 0) THEN
    CALL H5DGET_SPACE_F(dset_id, dspace_id, ierr)
    ERROR = ierr
    IF (ierr .NE. 0) dspace_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Failed to obtain the dataspace.')
#endif
  END IF

! Make sure the rank is right
  IF (RANK > 0 .AND. ERROR .EQ. 0) THEN
    CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id, ndims, ierr)
    IF (ierr .LT. 0) ERROR = ierr

#ifdef DEBUG_VER
    CALL AssertError1(ierr .GE. 0, myName, &
                      'Failed to retrieve number of dataspace dimensions.')
#endif

#ifdef DEBUG_VER
    CALL AssertError1(ndims .EQ. RANK, myName, &
                      'Using wrong read function for rank.')
#endif

    ! On success hdferr is the rank of the dataspace, not 0
    IF (ERROR .EQ. 0) THEN
      CALL H5SGET_SIMPLE_EXTENT_DIMS_F(dspace_id, dims, maxdims, ierr)
      IF (ierr .LT. 0) ERROR = ierr

#ifdef DEBUG_VER
      CALL AssertError1(ierr .GE. 0, myName, &
                        'Failed to retrieve dataspace dimensions.')
#endif
    END IF
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE preRead

!----------------------------------------------------------------------------
!                                                              GetDataShape
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDataShape
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_GetDataShape()'
#endif
  INTEGER(I4B) :: error, ndims
  INTEGER(HID_T) :: dset_id
  INTEGER(HID_T) :: dspace_id
  INTEGER(HSIZE_T), ALLOCATABLE :: dims(:), maxdims(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0

#ifdef DEBUG_VER
  CALL AssertError1(obj%isInit, myName, "file object is not initialized.")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsOpen(), myName, "file object is not open.")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsRead(), myName, &
                    "file object does not have read access.")
#endif

  ! Open the dataset
  CALL H5DOPEN_F(obj%file_id, dsetname, dset_id, error)
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
  ALLOCATE(dims(ndims))
  ALLOCATE(maxdims(ndims))
  CALL H5SGET_SIMPLE_EXTENT_DIMS_F(dspace_id, dims, maxdims, error)
#ifdef DEBUG_VER
  CALL AssertError1(error .GE. 0, myName, &
                    'Failed to retrieve dataspace dimensions.')
#endif

  ! Copy to the Futility integer type
  ALLOCATE(dataShape(SIZE(dims)))
  dataShape = INT(dims, KIND=I4B)

  CALL H5SCLOSE_F(dspace_id, error)
  CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetDataShape

!----------------------------------------------------------------------------
!                                                                GetDataType
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDataType
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_GetDataType()'
#endif
  INTEGER(I4B) :: error, class_type
  INTEGER(HID_T) :: dset_id, dtype
  INTEGER(HSIZE_T) :: dtype_prec

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  error = 0

#ifdef DEBUG_VER
  CALL AssertError1(obj%isInit, myName, "file object is not initialized.")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsOpen(), myName, "file object is not open.")
#endif

#ifdef DEBUG_VER
  CALL AssertError1(obj%IsRead(), myName, &
                    "file object does not have read access.")
#endif

! Open the dataset
  CALL H5DOPEN_F(obj%file_id, dsetname, dset_id, error)
#ifdef DEBUG_VER
  CALL AssertError1(error .EQ. 0, myName, 'Failed to open dataset.')
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

  ! Class and precision (bits): R32, R64, I08, I16, I32, I64, STR
  dataType = 'N/A'
  IF (class_type == H5T_FLOAT_F) THEN
    SELECT CASE (dtype_prec)
    CASE (32)
      dataType = 'R32'
    CASE (64)
      dataType = 'R64'
    CASE DEFAULT
      dataType = 'N/A'
    END SELECT
  ELSE IF (class_type == H5T_INTEGER_F) THEN
    SELECT CASE (dtype_prec)
    CASE (8)
      dataType = 'I08'
    CASE (16)
      dataType = 'I16'
    CASE (32)
      dataType = 'I32'
    CASE (64)
      dataType = 'I64'
    CASE DEFAULT
      dataType = 'N/A'
    END SELECT
  ELSE IF (class_type == H5T_ENUM_F) THEN
    ! logical data: the enum {FALSE, TRUE} on an 8-bit integer
    dataType = 'BOL'
  ELSE IF (class_type == H5T_STRING_F) THEN
    dataType = 'STR'
  ELSE
#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      'Unsupported data type ')
#endif
  END IF

  CALL H5TCLOSE_F(dtype, error)
  CALL H5DCLOSE_F(dset_id, error)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetDataType

!----------------------------------------------------------------------------
!                                                             postRead
!----------------------------------------------------------------------------

MODULE PROCEDURE postRead
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'postRead()'
  INTEGER(HSIZE_T), ALLOCATABLE :: cdims(:)
#endif
  INTEGER(I4B) :: ierr

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
  IF (ERROR .NE. 0) THEN
    !See if failed read was due to OOM on decompress
    IF (obj%IsCompressed(path)) THEN

      CALL obj%GetChunkSize(path, cdims)

      IF (MAXVAL(cdims) > 16777216_HSIZE_T) THEN

        !This is 64/128MB
        CALL e%RaiseWarning(modName // '::' // myName // &
                            ' - Potentially high memory usage' // &
                            'when reading decompressed dataset "' // &
                            TRIM(path) // '".' // CHAR(10) // CHAR(10) // &
                            'Try decompressing file before rerunning:' // &
                            CHAR(10) // '$ h5repack -f NONE "' // &
                            obj%fullname%Chars() // '" "' // &
                            obj%fullname%Chars() // '.uncompressed"')
      END IF
    END IF

    CALL e%RaiseError(modName // '::' // myName // " - " // &
                      'Failed to read data from dataset"' // TRIM(path) // &
                      '".')
  END IF
#endif

! Close the ids which were opened; preRead sets the others to -1.
! ERROR keeps the status of the read.
  IF (dset_id .GT. 0) THEN
    CALL H5DCLOSE_F(dset_id, ierr)
    dset_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Failed to close dataset "' // TRIM(path))
#endif
  END IF

  IF (dspace_id .GT. 0) THEN
    CALL H5SCLOSE_F(dspace_id, ierr)
    dspace_id = -1

#ifdef DEBUG_VER
    CALL AssertError1(ierr .EQ. 0, myName, &
                      'Failed to close dataspace for "' // TRIM(path))
#endif
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE postRead

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INCLUDE "../../include/errors.F90"

END SUBMODULE ConstructorMethods

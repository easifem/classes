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

SUBMODULE(XMLFile_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Initiate
CHARACTER(*), PARAMETER :: myName = 'xmlFile_Initiate'
TYPE(String) :: fpath, fname, fext, mode_in, file_
CHARACTER(1024) :: tempchars
LOGICAL(LGT) :: exists
!
! check
!
IF (obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' - xmlFile '//obj%getFileName()// &
    & ' is already initialized!')
  RETURN
END IF
!

file_ = TRIM(filename)
IF (file_%SCAN(CHAR_SLASH) .EQ. 0_I4B) THEN
  fpath = "."//CHAR_SLASH
ELSE
  fpath = file_%basedir(sep=CHAR_SLASH)//CHAR_SLASH
END IF
fext = file_%extension()
fname = file_%basename(extension=fext%chars(), sep=CHAR_SLASH)

#ifdef _OLD_VERSION_

CALL getPath(chars=filename, path=tempchars)
fpath = TRIM(tempchars)
CALL getFileNameExt(chars=filename, ext=tempchars)
fext = TRIM(tempchars)
CALL getFileName(chars=filename, fname=tempchars)
fname = TRIM(tempchars)

#endif

CALL obj%setFilePath(fpath)
CALL obj%setFileName(fname)
CALL obj%setFileExt(fext)
!
! MODE
!
mode_in = mode
mode_in = mode_in%upper()
SELECT CASE (TRIM(mode_in%chars()))
CASE ("READ")
  INQUIRE (FILE=filename, EXIST=exists)
  IF (exists) THEN
    CALL obj%setWriteStat(.FALSE.)
    CALL obj%setReadStat(.TRUE.)
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - XML file '//filename//' is being opened with '// &
      & 'mode READ but does not exist.')
  END IF
CASE ("WRITE")
  INQUIRE (FILE=filename, EXIST=exists)
  IF (exists) THEN
    CALL obj%setWriteStat(.TRUE.)
    CALL obj%setReadStat(.FALSE.)
  ELSE
    CALL e%raiseError(modName//'::'//myName// &
      & ' - XML file '//filename//' is being opened with '// &
      & 'mode WRITE but does not exist.')
  END IF
CASE ("OVERWRITE")
  INQUIRE (FILE=filename, EXIST=exists)
  IF (exists) THEN
    CALL obj%setWriteStat(.TRUE.)
    CALL obj%setOverwriteStat(.TRUE.)
    CALL obj%setReadStat(.TRUE.)
  ELSE
    CALL e%raiseError(modName//'::'//myName// &
      & ' - XML file '//filename//' is being opened with '// &
      & 'mode OVERWRITE but does not exist.')
  END IF
CASE ("NEW")
  CALL obj%setWriteStat(.TRUE.)
  CALL obj%setReadStat(.TRUE.)
  CALL obj%setNewStat(.TRUE.)
  CALL obj%setOverwriteStat(.TRUE.)
CASE DEFAULT
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' - Unrecognized access mode.')
END SELECT
obj%fullname = TRIM(filename)
ALLOCATE (obj%root)
obj%isInitiated = .TRUE.
END PROCEDURE xmlFile_Initiate

!----------------------------------------------------------------------------
!                                                           Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Deallocate
LOGICAL(LGT) :: bool
!>
IF (ASSOCIATED(obj%root)) THEN
  CALL obj%root%DEALLOCATE()
  DEALLOCATE (obj%root)
END IF
!>
IF (obj%isInitiated) THEN
  !Logical to close or delete the file.
  IF (PRESENT(delete)) THEN
    bool = delete
  ELSE
    bool = .FALSE.
  END IF
  IF (bool) THEN
    CALL obj%delete()
  ELSE
    CALL obj%CLOSE()
  END IF
  !>
  obj%isInitiated = .FALSE.
  obj%newstat = .FALSE.
  obj%fullname = ''
  obj%unitno = -1
  obj%overwriteStat = .FALSE.
  obj%version = 1.0
  obj%encoding = 'UTF-8'
  CALL AbstractFileDeallocate(obj)
END IF
END PROCEDURE xmlFile_Deallocate

!----------------------------------------------------------------------------
!                                                                     Final
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Final
CALL obj%DEALLOCATE()
END PROCEDURE xmlFile_Final

!----------------------------------------------------------------------------
!                                                                       Open
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Open
CHARACTER(*), PARAMETER :: myName = 'xmlFile_Open'
CHARACTER(7) :: statusvar
CHARACTER(10) :: accessvar
CHARACTER(11) :: formvar
CHARACTER(9) :: actionvar
INTEGER(I4B) :: ierr
TYPE(String) :: fpath
!
! check
!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' - The xmlFile is not initiated.')
END IF
!
! isOpen
!
IF (obj%isOpen()) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
                    ' - File is already open!')
END IF
!
IF (.NOT. obj%newstat) THEN
  statusvar = 'OLD'
ELSE
  IF (obj%overwriteStat) THEN
    statusvar = 'REPLACE'
  ELSE
    statusvar = 'NEW'
  END IF
END IF
!
IF (TRIM(statusvar) .NE. 'OLD') THEN
  fpath = obj%getFilePath()
  ierr = system_mkdir(fpath//'', RWX_U)
  IF (ierr .NE. 0_I4B .AND. ierr .NE. -1_I4B) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'error occured while creating the directory')
  END IF
END IF
!
!
! FORM clause value
!
IF (obj%isFormatted()) THEN
  formvar = 'FORMATTED'
ELSE
  formvar = 'UNFORMATTED'
END IF
!ACCESS clause value
accessvar = 'SEQUENTIAL'
!ACTION clause value
IF (obj%isRead() .AND. .NOT. obj%isWrite()) THEN
  actionvar = 'READ'
ELSE IF (.NOT. obj%isRead() .AND. obj%isWrite()) THEN
  actionvar = 'WRITE'
ELSE IF (obj%isRead() .AND. obj%isWrite()) THEN
  actionvar = 'READWRITE'
END IF
OPEN ( &
  & NEWUNIT=obj%unitNo, FILE=TRIM(obj%fullname%chars()), &
  & STATUS=statusvar, ACCESS=accessvar, &
  & FORM=formvar, ACTION=actionvar, ENCODING=obj%encoding, &
  & IOSTAT=ierr)
!
IF (ierr == 0) THEN
  CALL obj%setOpenStat(.TRUE.)
ELSE
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' - Trouble opening file!')
  CALL obj%setOpenStat(.FALSE.)
END IF
END PROCEDURE xmlFile_Open

!----------------------------------------------------------------------------
!                                                                      Close
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Close
CHARACTER(*), PARAMETER :: myName = 'xmlFile_Close'
INTEGER(I4B) :: ierr
!>
IF (obj%isOpen()) THEN
  CLOSE (obj%unitNo, IOSTAT=ierr)
  IF (ierr .EQ. 0) THEN
    CALL obj%setOpenStat(.FALSE.)
  ELSE
    CALL e%raiseError(modName//'::'//myName//" - "// &
      & ' - trouble closing file!')
  END IF
END IF
END PROCEDURE xmlFile_Close

!----------------------------------------------------------------------------
!                                                                    Delete
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlFile_Delete
CHARACTER(*), PARAMETER :: myName = 'xmlFile_Delete'
INTEGER(I4B) :: ierr

IF (obj%isOpen()) THEN
  CLOSE (obj%unitNo, STATUS='DELETE', IOSTAT=ierr)
ELSE
  CALL obj%OPEN()
  CLOSE (obj%unitNo, STATUS='DELETE', IOSTAT=ierr)
END IF
IF (ierr == 0) THEN
  CALL obj%setOpenStat(.FALSE.)
ELSE
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & ' - trouble closing file!')
END IF
END PROCEDURE xmlFile_Delete

END SUBMODULE ConstructorMethods

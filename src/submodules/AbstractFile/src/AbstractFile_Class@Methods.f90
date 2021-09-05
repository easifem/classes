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

SUBMODULE( AbstractFile_Class ) Methods
USE StringiFor
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE setFilePath_file
  CHARACTER(LEN=*),PARAMETER :: myName='setFilePath_file'
  IF(obj%openstat) THEN
    CALL obj%e%raiseError(modName//'::'//myName//' - '// &
        'Cannot change path of file while it is open!')
  ELSE
    obj%path=TRIM(ADJUSTL(path))
    obj%pathlen=LEN_TRIM(obj%path)
  ENDIF
END PROCEDURE setFilePath_file

!----------------------------------------------------------------------------
!                                                               setFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE setFileName_file
  CHARACTER(LEN=*),PARAMETER :: myName='setFileName_file'
  IF(obj%openstat) THEN
    CALL obj%e%raiseError(modName//'::'//myName//' - '// &
        'Cannot change Filename of file while it is open!')
  ELSE
    obj%fileName=TRIM(ADJUSTL(fileName))
    obj%fnamelen=LEN_TRIM(obj%fileName)
  ENDIF
END PROCEDURE setFileName_file

!----------------------------------------------------------------------------
!                                                                 setFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE setFileExt_file
  CHARACTER(LEN=*),PARAMETER :: myName='setFileExt_file'

  IF(obj%openstat) THEN
    CALL obj%e%raiseError(modName//'::'//myName//' - '// &
      & 'Cannot change extension of file while it is open!')
  ELSE
    obj%ext=TRIM(ADJUSTL(ext))
    obj%extlen=LEN_TRIM(obj%ext)
  ENDIF
END PROCEDURE setFileExt_file

!----------------------------------------------------------------------------
!                                                              getFileParts
!----------------------------------------------------------------------------

MODULE PROCEDURE getFileParts_file
  path=obj%path
  fileName=obj%fileName
  ext=obj%ext
END PROCEDURE getFileParts_file

!----------------------------------------------------------------------------
!                                                                getFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE getFilePath_file
  path = obj%path
END PROCEDURE getFilePath_file

!----------------------------------------------------------------------------
!                                                                getFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE getFileName_file
  fileName = obj%fileName
END PROCEDURE getFileName_file

!----------------------------------------------------------------------------
!                                                                getFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE getFileExt_file
  ext = obj%ext
END PROCEDURE getFileExt_file

!----------------------------------------------------------------------------
!                                                                 isOpen
!----------------------------------------------------------------------------

MODULE PROCEDURE isOpen_file
  ans = obj%openStat
END PROCEDURE isOpen_file

!----------------------------------------------------------------------------
!                                                                 isEOF
!----------------------------------------------------------------------------

MODULE PROCEDURE isEOF_file
  ans = obj%eofStat
END PROCEDURE isEOF_file

!----------------------------------------------------------------------------
!                                                                 isWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE isWrite_file
  ans = obj%writeStat
END PROCEDURE isWrite_file

!----------------------------------------------------------------------------
!                                                                 isRead
!----------------------------------------------------------------------------

MODULE PROCEDURE isRead_file
  ans = obj%readStat
END PROCEDURE isRead_file

!----------------------------------------------------------------------------
!                                                                 setEOF
!----------------------------------------------------------------------------

MODULE PROCEDURE setEOFstat_file
  CHARACTER(LEN=*),PARAMETER :: myName='setEOFstat_file'
  IF(obj%openstat) THEN
    obj%EOFstat=stat
  ELSE
    CALL obj%e%raiseDebug(modName//'::'//myName// &
        ' - EOF status cannot be changed on a file that is not open!')
  ENDIF
END PROCEDURE setEOFstat_file

!----------------------------------------------------------------------------
!                                                                 setOpenStat
!----------------------------------------------------------------------------

MODULE PROCEDURE setOpenStat_file
  obj%openStat = stat
END PROCEDURE setOpenStat_file

!----------------------------------------------------------------------------
!                                                                setReadStat
!----------------------------------------------------------------------------

MODULE PROCEDURE setReadStat_file
  CHARACTER(LEN=*),PARAMETER :: myName='setReadStat_file'
  IF(obj%openstat) THEN
    CALL obj%e%raiseDebug(modName//'::'//myName// &
        ' - Cannot change read status of a file if it is open!')
  ELSE
    obj%readstat=stat
  ENDIF
END PROCEDURE setReadStat_file

!----------------------------------------------------------------------------
!                                                               setWriteStat
!----------------------------------------------------------------------------

MODULE PROCEDURE setWriteStat_file
  CHARACTER(LEN=*),PARAMETER :: myName='setWriteStat_file'
  IF(obj%openstat) THEN
    CALL obj%e%raiseDebug(modName//'::'//myName// &
        ' - Cannot change write status of a file if it is open!')
  ELSE
    obj%writestat=stat
  ENDIF
END PROCEDURE setWriteStat_file

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE deallocateData_file
  obj%path=''
  obj%fileName=''
  obj%ext=''
  obj%pathlen=0
  obj%fnamelen=0
  obj%extlen=0
  obj%openstat=.FALSE.
  obj%EOFstat=.FALSE.
  obj%readstat=.FALSE.
  obj%writestat=.FALSE.
  CALL obj%e%reset()
END PROCEDURE deallocateData_file

END SUBMODULE Methods
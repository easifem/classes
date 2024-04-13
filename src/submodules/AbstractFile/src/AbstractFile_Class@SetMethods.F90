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

SUBMODULE(AbstractFile_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_setFilePath
  CHARACTER(LEN=*),PARAMETER :: myName='aFile_setFilePath'
  IF(obj%openstat) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Cannot change path of file while it is open!')
  ELSE
    obj%path=path
    obj%pathlen=path%len_trim()
  ENDIF
END PROCEDURE aFile_setFilePath

!----------------------------------------------------------------------------
!                                                               setFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_setFileName
  CHARACTER(LEN=*),PARAMETER :: myName='aFile_setFileName'
  IF(obj%openstat) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Cannot change Filename of file while it is open!')
  ELSE
    obj%fileName=fileName
    obj%fnamelen=filename%len_trim()
  ENDIF
END PROCEDURE aFile_setFileName

!----------------------------------------------------------------------------
!                                                                 setFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_setFileExt
  CHARACTER(LEN=*),PARAMETER :: myName='aFile_setFileExt'

  IF(obj%openstat) THEN
    CALL e%raiseError(modName//'::'//myName//' - '// &
      & 'Cannot change extension of file while it is open!')
  ELSE
    obj%ext=ext
    obj%extlen=ext%len_trim()
  ENDIF
END PROCEDURE aFile_setFileExt

!----------------------------------------------------------------------------
!                                                                 setEOF
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_setEOFstat
  CHARACTER(LEN=*),PARAMETER :: myName='aFile_setEOFstat'
  IF(obj%openstat) THEN
    obj%EOFstat=stat
  ELSE
    CALL e%raiseDebug(modName//'::'//myName// &
      & ' - EOF status cannot be changed on a file that is not open!')
  ENDIF
END PROCEDURE aFile_setEOFstat

!----------------------------------------------------------------------------
!                                                                 setOpenStat
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_setOpenStat
  obj%openStat = stat
END PROCEDURE aFile_setOpenStat

!----------------------------------------------------------------------------
!                                                                setReadStat
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_setReadStat
  CHARACTER(LEN=*),PARAMETER :: myName='aFile_setReadStat'
  IF(obj%openstat) THEN
    CALL e%raiseDebug(modName//'::'//myName// &
        ' - Cannot change read status of a file if it is open!')
  ELSE
    obj%readstat=stat
  ENDIF
END PROCEDURE aFile_setReadStat

!----------------------------------------------------------------------------
!                                                               setWriteStat
!----------------------------------------------------------------------------

MODULE PROCEDURE aFile_setWriteStat
  CHARACTER(LEN=*),PARAMETER :: myName='aFile_setWriteStat'
  IF(obj%openstat) THEN
    CALL e%raiseDebug(modName//'::'//myName// &
        ' - Cannot change write status of a file if it is open!')
  ELSE
    obj%writestat=stat
  ENDIF
END PROCEDURE aFile_setWriteStat

END SUBMODULE SetMethods
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
!                                                                SetFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFilePath
  CHARACTER(*), PARAMETER :: myName = 'obj_SetFilePath()'

  IF (obj%openstat) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      'Cannot change path of file while it is open!')
  ELSE
    obj%path = path
    obj%pathlen = path%len_trim()
  END IF
END PROCEDURE obj_SetFilePath

!----------------------------------------------------------------------------
!                                                                SetFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFileName
  CHARACTER(*), PARAMETER :: myName = 'obj_SetFileName()'

  IF (obj%openstat) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      'Cannot change Filename of file while it is open!')
  ELSE
    obj%fileName = fileName
    obj%fnamelen = fileName%len_trim()
  END IF
END PROCEDURE obj_SetFileName

!----------------------------------------------------------------------------
!                                                                 SetFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFileExt
  CHARACTER(*), PARAMETER :: myName = 'obj_SetFileExt()'

  IF (obj%openstat) THEN
    CALL e%RaiseError(modName // '::' // myName // ' - ' // &
                      'Cannot change extension of file while it is open!')
  ELSE
    obj%ext = ext
    obj%extlen = ext%len_trim()
  END IF
END PROCEDURE obj_SetFileExt

!----------------------------------------------------------------------------
!                                                                 SetEOFStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEOFStat
  CHARACTER(*), PARAMETER :: myName = 'obj_SetEOFStat()'

  IF (obj%openstat) THEN
    obj%EOFstat = STAT
  ELSE
    CALL e%RaiseDebug(modName // '::' // myName // ' - ' // &
                      'EOF status cannot be changed on a file that is ' // &
                      'not open!')
  END IF
END PROCEDURE obj_SetEOFStat

!----------------------------------------------------------------------------
!                                                                SetOpenStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOpenStat
  obj%openStat = STAT
END PROCEDURE obj_SetOpenStat

!----------------------------------------------------------------------------
!                                                                SetReadStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetReadStat
  CHARACTER(*), PARAMETER :: myName = 'obj_SetReadStat()'

  IF (obj%openstat) THEN
    CALL e%RaiseDebug(modName // '::' // myName // ' - ' // &
                      'Cannot change read status of a file if it is open!')
  ELSE
    obj%readstat = STAT
  END IF
END PROCEDURE obj_SetReadStat

!----------------------------------------------------------------------------
!                                                               SetWriteStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetWriteStat
  CHARACTER(*), PARAMETER :: myName = 'obj_SetWriteStat()'

  IF (obj%openstat) THEN
    CALL e%RaiseDebug(modName // '::' // myName // ' - ' // &
                      'Cannot change write status of a file if it is open!')
  ELSE
    obj%writestat = STAT
  END IF
END PROCEDURE obj_SetWriteStat

END SUBMODULE SetMethods

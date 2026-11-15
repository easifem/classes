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
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "AbstractFile_Class@SetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                SetFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFilePath
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_SetFilePath()'
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. obj%openstat, myName, &
                    "cannot change path of file while it is open.")
#endif

  obj%path = path
  obj%pathlen = path%len_trim()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetFilePath

!----------------------------------------------------------------------------
!                                                                SetFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFileName
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_SetFileName()'
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. obj%openstat, myName, &
                    "cannot change filename while it is open.")
#endif

  obj%fileName = fileName
  obj%fnamelen = fileName%len_trim()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetFileName

!----------------------------------------------------------------------------
!                                                                 SetFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFileExt
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_SetFileExt()'
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. obj%openstat, myName, &
                    "cannot change extension while it is open.")
#endif

  obj%ext = ext
  obj%extlen = ext%len_trim()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetFileExt

!----------------------------------------------------------------------------
!                                                                 SetEOFStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetEOFStat
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_SetEOFStat()'
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  obj%EOFstat = STAT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetEOFStat

!----------------------------------------------------------------------------
!                                                                SetOpenStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOpenStat
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_SetOpenStat()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  obj%openStat = STAT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetOpenStat

!----------------------------------------------------------------------------
!                                                                SetReadStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetReadStat
  CHARACTER(*), PARAMETER :: myName = 'obj_SetReadStat()'

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. obj%openstat, myName, &
                    'Cannot change read status of a file if it is open!')
#endif

  obj%readstat = STAT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetReadStat

!----------------------------------------------------------------------------
!                                                               SetWriteStat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetWriteStat
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = 'obj_SetWriteStat()'
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL AssertError1(.NOT. obj%openstat, myName, &
                    'Cannot change write status of a file if it is open!')
#endif

  obj%writestat = STAT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_SetWriteStat

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

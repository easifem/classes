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

SUBMODULE(AbstractFile_Class) GetMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
  "AbstractFile_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                               GetFileParts
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFileParts
  path = obj%path
  fileName = obj%fileName
  ext = obj%ext
END PROCEDURE obj_GetFileParts

!----------------------------------------------------------------------------
!                                                                GetFilePath
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFilePath
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetFilePath()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  path = obj%path

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetFilePath

!----------------------------------------------------------------------------
!                                                                GetFileName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFileName
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetFileName()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fileName = obj%fileName

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetFileName

!----------------------------------------------------------------------------
!                                                                 GetFileExt
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFileExt
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetFileExt()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ext = obj%ext

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetFileExt

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

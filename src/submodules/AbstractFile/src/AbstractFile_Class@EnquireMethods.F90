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

SUBMODULE(AbstractFile_Class) EnquireMethods
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
  "AbstractFile_Class@EnquireMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                     IsOpen
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsOpen
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_IsOpen()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%openStat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_IsOpen

!----------------------------------------------------------------------------
!                                                                      IsEOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsEOF
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_IsEOF()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%eofStat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_IsEOF

!----------------------------------------------------------------------------
!                                                                    IsWrite
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsWrite
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_IsWrite()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%writeStat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_IsWrite

!----------------------------------------------------------------------------
!                                                                     IsRead
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsRead
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_IsRead()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  ans = obj%readStat

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_IsRead

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE EnquireMethods

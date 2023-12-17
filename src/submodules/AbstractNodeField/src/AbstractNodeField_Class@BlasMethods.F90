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

SUBMODULE(AbstractNodeField_Class) BlasMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                     AXPY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AXPY
CHARACTER(*), PARAMETER :: myName = "obj_AXPY()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
problem = obj%engine .NE. "NATIVE_SERIAL"
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: engine of obj should be NATIVE_SERIAL.')
  RETURN
END IF

problem = x%engine .NE. "NATIVE_SERIAL"
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: engine of x should be NATIVE_SERIAL.')
  RETURN
END IF
#endif

CALL AXPY(X=x%realVec, Y=obj%realVec, A=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_AXPY

!----------------------------------------------------------------------------
!                                                                     SCAL
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SCAL
CHARACTER(*), PARAMETER :: myName = "obj_SCAL()"
LOGICAL(LGT) :: problem

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

#ifdef DEBUG_VER
problem = obj%engine .NE. "NATIVE_SERIAL"
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: engine of obj should be NATIVE_SERIAL.')
  RETURN
END IF
#endif

CALL SCAL(x=obj%realVec, A=scale)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_SCAL

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BlasMethods

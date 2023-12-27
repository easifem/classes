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
!                                                                      COPY
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_COPY
CHARACTER(*), PARAMETER :: myName = "obj_COPY()"
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

problem = obj2%engine .NE. "NATIVE_SERIAL"
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: engine of obj2 should be NATIVE_SERIAL.')
  RETURN
END IF
#endif

CALL COPY(y=obj%realVec, x=obj2%realVec)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif
END PROCEDURE obj_COPY

!----------------------------------------------------------------------------
!                                                                    NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm2
CHARACTER(*), PARAMETER :: myName = "obj_Norm2()"
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  ans = NORM2(obj=obj%realvec)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
    & 'child classes')
  ans = 0.0_DFP
END IF
END PROCEDURE obj_Norm2

!----------------------------------------------------------------------------
!                                                                    NORM1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Norm1
CHARACTER(*), PARAMETER :: myName = "obj_Norm1()"
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  ans = NORM1(obj=obj%realvec)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
    & 'child classes')
  ans = 0.0_DFP
END IF
END PROCEDURE obj_Norm1

!----------------------------------------------------------------------------
!                                                                    NORM2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Normi
CHARACTER(*), PARAMETER :: myName = "obj_Normi()"
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  ans = NORMi(obj=obj%realvec)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
    & 'child classes')
  ans = 0.0_DFP
END IF
END PROCEDURE obj_Normi

!----------------------------------------------------------------------------
!                                                               DOT_PRODUCT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DOT_PRODUCT
CHARACTER(*), PARAMETER :: myName = "obj_DOT_PRODUCT()"
IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  ans = DOT_PRODUCT(obj1=obj%realvec, obj2=obj2%realvec)
ELSE
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[IMPLEMENTATION ERROR] :: This routine should be implemented by '//&
    & 'child classes')
  ans = 0.0_DFP
END IF
END PROCEDURE obj_DOT_PRODUCT

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE BlasMethods

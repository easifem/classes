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

SUBMODULE(AbstractNodeField_Class) SetMethods
USE InputUtility, ONLY: Input
USE RealVector_Method, ONLY: Set, Add
USE BaseType, ONLY: math => TypeMathOpt

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             SetParam
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

INTEGER(I4B) :: ii, tsize1

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(dof_tPhysicalVars)) obj%dof_tPhysicalVars = dof_tPhysicalVars
IF (PRESENT(dof_storageFMT)) obj%dof_storageFMT = dof_storageFMT
IF (PRESENT(dof_spaceCompo)) obj%dof_spaceCompo = dof_spaceCompo
IF (PRESENT(dof_timeCompo)) obj%dof_timeCompo = dof_timeCompo
IF (PRESENT(dof_tNodes)) obj%dof_tNodes = dof_tNodes
IF (PRESENT(tSize)) obj%tSize = tSize

IF (PRESENT(dof_names_char)) THEN
  IF (ALLOCATED(obj%dof_names_char)) DEALLOCATE (obj%dof_names_char)
  tsize1 = SIZE(dof_names_char)
  ALLOCATE (obj%dof_names_char(tsize1))

  DO ii = 1, tsize1
    obj%dof_names_char(ii) (1:1) = dof_names_char(ii) (1:1)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSingle
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSingle()"
#endif

REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)
areal = Input(option=scale, default=math%one)

IF (abool) THEN
  CALL Add(obj%realVec, nodenum=indx, VALUE=VALUE, scale=areal)
ELSE
  areal = areal * VALUE
  CALL Set(obj%realVec, nodenum=indx, VALUE=areal)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetSingle

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple1()"
#endif
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add(obj%realVec, VALUE=VALUE, scale=areal, nodenum=indx)
ELSE
  CALL Set(obj%realVec, VALUE=VALUE, nodenum=indx)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple1

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple2()"
#endif
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=.FALSE.)

IF (abool) THEN
  areal = Input(option=scale, default=1.0_DFP)
  CALL Add( &
    obj%realVec, VALUE=VALUE, scale=areal, istart=istart, iend=iend, &
    stride=stride)
ELSE
  CALL Set( &
    obj%realVec, VALUE=VALUE, istart=istart, iend=iend, stride=stride)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple2

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple3()"
#endif
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)

IF (abool) THEN
  areal = Input(option=scale, default=math%one)
  CALL Add( &
    obj=obj%realVec, VALUE=VALUE, scale=areal, istart=istart, &
    iend=iend, stride=stride, istart_value=istart_value, &
    iend_value=iend_value, stride_value=stride_value)
ELSE
  CALL Set( &
    obj=obj%realVec, VALUE=VALUE, istart=istart, &
    iend=iend, stride=stride, istart_value=istart_value, &
    iend_value=iend_value, stride_value=stride_value)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple3

!----------------------------------------------------------------------------
!                                                                 SetSingle
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMultiple4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetMultiple4()"
#endif
LOGICAL(LGT) :: abool
REAL(DFP) :: areal

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=addContribution, default=math%no)

IF (abool) THEN
  areal = Input(option=scale, default=math%one)
  CALL Add( &
    obj%realVec, VALUE=VALUE, scale=areal, istart=istart, iend=iend, &
    stride=stride)
ELSE
  CALL Set( &
    obj%realVec, VALUE=VALUE, istart=istart, iend=iend, stride=stride)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetMultiple4

!----------------------------------------------------------------------------
!                                                                 SetAll
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetAll
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetAll()"
#endif
REAL(DFP) :: areal
LOGICAL(LGT) :: abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

abool = Input(option=AddContribution, default=math%no)
areal = Input(option=scale, default=math%one)

IF (abool) THEN
  CALL Add(obj%realVec, VALUE=VALUE, scale=areal)
ELSE
  areal = areal * VALUE
  CALL Set(obj%realVec, VALUE=areal)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetAll

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

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

SUBMODULE(BlockNodeField_Class) GetMethods
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: GetNodeLoc_
USE DOF_Method, ONLY: OPERATOR(.SpaceComponents.)
USE DOF_Method, ONLY: OPERATOR(.tdof.)
USE DOF_Method, ONLY: OPERATOR(.TimeComponents.)
USE FEVariable_Method, ONLY: NodalVariable
USE InputUtility, ONLY: Input
USE SwapUtility, ONLY: Swap_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "BlockNodeField_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: isok
#endif
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, ivar=ivar, idof=idof)
CALL obj%GetSingle(VALUE=VALUE, indx=indx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
#endif
INTEGER(I4B) :: iend

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

iend = obj%SIZE()
CALL obj%GetMultiple(istart=1, iend=iend, stride=1, VALUE=VALUE, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#endif

INTEGER(I4B) :: indx(SIZE(globalNode))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=SIZE(VALUE) b = SIZE(globalNode)")
#endif

#include "./localNodeError.F90"

CALL GetNodeLoc_(obj=obj%dof, ivar=ivar, idof=idof, nodenum=globalNode, &
                 ans=indx, tsize=tsize)
CALL obj%GetMultiple(indx=indx, tsize=tsize, VALUE=VALUE)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
#endif
REAL(DFP) :: value0(SIZE(globalNode))
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Get(VALUE=value0, globalNode=globalNode, ivar=ivar, idof=idof, &
             islocal=islocal, tsize=tsize)

VALUE = NodalVariable(value0, TypeFEVariableScalar, TypeFEVariableSpace)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
#endif
INTEGER(I4B) :: timeCompo, spaceCompo, case_id, tdof, ii, &
                tsize
REAL(DFP), ALLOCATABLE :: m3a(:, :, :)
REAL(DFP), ALLOCATABLE :: m3b(:, :, :)
REAL(DFP), ALLOCATABLE :: value0(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tdof = obj%dof .tdof. ivar
case_id = SIZE(globalNode)

ALLOCATE (m3a(case_id, tdof, 1))

DO ii = 1, tdof
  CALL obj%Get(VALUE=m3a(:, ii, 1), globalNode=globalNode, &
               ivar=ivar, idof=ii, islocal=islocal, tsize=tsize)
END DO

ALLOCATE (value0(case_id * tdof))
value0 = RESHAPE(m3a, [case_id * tdof])

DEALLOCATE (m3a)

case_id = 0_I4B
timeCompo = obj%dof .TimeComponents. ivar
spaceCompo = obj%dof .SpaceComponents. ivar

IF ((spaceCompo .GT. 1)) THEN
  IF (timeCompo .GT. 1) THEN
    case_id = 1
  ELSE
    case_id = 2
  END IF
ELSE
  IF (timeCompo .GT. 1) THEN
    case_id = 3
  ELSE
    case_id = 4
  END IF
END IF

SELECT CASE (case_id)
CASE (1)
  ! vector space-time
  m3b = RESHAPE(value0, [SIZE(globalNode), spaceCompo, timeCompo])
  ALLOCATE (m3a(spaceCompo, SIZE(globalNode), timeCompo))
  ! Here m3b is in (J, i, a) format, but we need (i,J,a) format
  CALL SWAP_(a=m3a, b=m3b, i1=2, i2=1, i3=3)
  VALUE = NodalVariable(m3a, TypeFEVariableVector, TypeFEVariableSpaceTime)

CASE (2)
  ! vector space
  VALUE = NodalVariable(TRANSPOSE(RESHAPE(value0, &
                                          [SIZE(globalNode), spaceCompo])), &
                        TypeFEVariableVector, TypeFEVariableSpace)

CASE (3)
  ! scalar space-time
  VALUE = NodalVariable(RESHAPE(value0, [SIZE(globalNode), timeCompo]), &
                        TypeFEVariableScalar, TypeFEVariableSpaceTime)

CASE (4)
  ! scalar space
  VALUE = NodalVariable(value0, TypeFEVariableScalar, TypeFEVariableSpace)

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for case_id, case_id="//ToString(case_id))
#endif

END SELECT

IF (ALLOCATED(value0)) DEALLOCATE (value0)
IF (ALLOCATED(m3a)) DEALLOCATE (m3a)
IF (ALLOCATED(m3b)) DEALLOCATE (m3b)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
#endif

INTEGER(I4B) :: indx(SIZE(globalNode)), idof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%IsInitiated(), myName, &
                  'BlockNodeField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(globalNode), SIZE(VALUE), myName, &
                  "a=size(globalNode), b=Size(value)")
#endif

#include "./localNodeError.F90"

idof = GetIDOF(obj=obj%dof, ivar=ivar, spaceCompo=spaceCompo, &
               timeCompo=timeCompo)

CALL GetNodeLoc_(obj=obj%dof, idof=idof, nodenum=globalNode, &
                 ans=indx, tsize=tsize)

CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get7()"
#endif
REAL(DFP) :: value0(SIZE(globalNode))
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Get(VALUE=value0, globalNode=globalNode, ivar=ivar, &
             spaceCompo=spaceCompo, timeCompo=timeCompo, islocal=islocal, &
             tsize=tsize)
VALUE = NodalVariable(value0, TypeFEVariableScalar, TypeFEVariableSpace)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEVariable()"
#endif

INTEGER(I4B) :: ivar0

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ivar0 = Input(option=ivar, default=1_I4B)
CALL obj%Get(globalNode=globalNode, VALUE=VALUE, ivar=ivar0, &
             islocal=islocal)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

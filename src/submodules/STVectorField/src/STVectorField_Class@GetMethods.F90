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

SUBMODULE(STVectorField_Class) GetMethods
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: TypeFEVariableSpaceTime
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: math => TypeMathOpt
USE Display_Method, ONLY: ToString
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: GetNodeLoc_
USE DOF_Method, ONLY: OPERATOR(.tnodes.)
USE FEVariable_Method, ONLY: NodalVariable
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE RealVector_Method, ONLY: GetValue_
USE ScalarField_Class, ONLY: ScalarField_
USE STScalarField_Class, ONLY: STScalarField_
USE SwapUtility, ONLY: Swap_
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "STVectorField_Class@GetMethods.F90"
#endif
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif
LOGICAL(LGT) :: isnode, isspace, istime
INTEGER(I4B), PARAMETER :: indx_size = 128
INTEGER(I4B) :: indx(indx_size), ii, s(3)
CHARACTER(3) :: mycase

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isnode = PRESENT(globalNode)
isspace = PRESENT(spacecompo)
istime = PRESENT(timecompo)

mycase = "NNN"

IF (isnode) mycase(1:1) = "Y"
IF (isspace) mycase(2:2) = "Y"
IF (istime) mycase(3:3) = "Y"

SELECT CASE (mycase)

CASE ("YYY")
  ! node | space | time

  indx(1) = GetNodeLoc(obj=obj%dof, nodenum=globalNode, &
                       ivar=1, spaceCompo=spaceCompo, timeCompo=timeCompo)
  CALL obj%GetSingle(VALUE=VALUE(1), indx=indx(1))
  tsize = 1

CASE ("YNN")
  ! node | no space | no time

  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, ivar=1, idof=obj%idofs, &
                   ans=indx, tsize=tsize)
  CALL obj%GetMultiple(VALUE=VALUE, indx=indx(1:tsize), tsize=tsize)

CASE ("YYN")
  ! node | space | no time

  CALL GetNodeLoc_( &
    obj=obj%dof, nodenum=globalNode, ivar=1, &
    spaceCompo=spaceCompo, timeCompo=obj%time_idofs, ans=indx, tsize=tsize)
  CALL obj%GetMultiple(VALUE=VALUE, indx=indx(1:tsize), tsize=tsize)

CASE ("YNY")
  ! node | no space | time

  CALL GetNodeLoc_( &
    obj=obj%dof, nodenum=globalNode, ivar=1, &
    spaceCompo=obj%space_idofs, timeCompo=obj%timeCompo, ans=indx, &
    tsize=tsize)
  CALL obj%GetMultiple(VALUE=VALUE, indx=indx(1:tsize), tsize=tsize)

CASE ("NYY")
  ! no node | space | time

  indx(1) = GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
                    tspaceCompo=obj%spaceCompo)
  s = GetNodeLoc(obj=obj%dof, idof=indx(1))
  CALL obj%GetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize)

CASE ("NYN")
  ! no node | space | no time

  tsize = 0
  DO ii = 1, obj%timeCompo
    indx(1) = GetIDOF(spaceCompo=spaceCompo, timeCompo=ii, &
                      tspaceCompo=obj%spaceCompo)
    s = GetNodeLoc(obj=obj%dof, idof=indx(1))
    CALL obj%GetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                         tsize=indx(2))
    tsize = tsize + indx(2)
  END DO

CASE ("NNY")
  ! no node | no space | time

  tsize = 0
  DO ii = 1, obj%spaceCompo
    indx(1) = GetIDOF(spaceCompo=ii, timeCompo=timeCompo, &
                      tspaceCompo=obj%spaceCompo)
    s = GetNodeLoc(obj=obj%dof, idof=indx(1))
    CALL obj%GetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                         tsize=indx(2))
    tsize = tsize + indx(2)
  END DO

CASE DEFAULT
#ifdef DEBUG_VER
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found.')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
#endif
INTEGER(I4B) :: ii, jj, idof, s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (storageFMT .EQ. NODES_FMT) THEN
  dim1 = obj%spaceCompo
  dim2 = obj%timeCompo
  dim3 = obj%dof.tNodes.1

  DO jj = 1, dim3
    CALL obj%Get(VALUE=VALUE(:, :, jj), globalNode=jj, islocal=.TRUE., &
                 nrow=s(1), ncol=s(2))
  END DO

ELSE
  dim1 = obj%dof.tNodes.1
  dim2 = obj%spaceCompo
  dim3 = obj%timeCompo

  idof = 0
  DO jj = 1, dim3
    DO ii = 1, dim2

      idof = idof + 1
      s = GetNodeLoc(obj=obj%dof, idof=idof)
      CALL obj%GetMultiple(VALUE=VALUE(:, ii, jj), &
                           istart=s(1), iend=s(2), stride=s(3), tsize=dim1)
    END DO
  END DO

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#endif
INTEGER(I4B) :: ii, jj, idof, s(3), indx(SIZE(globalNode))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (storageFMT .EQ. NODES_FMT) THEN
  dim1 = obj%spaceCompo
  dim2 = obj%timeCompo
  dim3 = SIZE(globalNode)

  DO jj = 1, dim3
    CALL obj%Get(VALUE=VALUE(:, :, jj), globalNode=globalNode(jj), &
                 islocal=islocal, nrow=s(1), ncol=s(2))
  END DO

ELSE
  dim1 = SIZE(globalNode)
  dim2 = obj%spaceCompo
  dim3 = obj%timeCompo

  idof = 0
  DO jj = 1, dim3
    DO ii = 1, dim2

      CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, ivar=1, &
                       spaceCompo=ii, timeCompo=jj, ans=indx, tsize=dim1)

      CALL obj%GetMultiple(VALUE=VALUE(:, ii, jj), indx=indx, tsize=dim1)

    END DO
  END DO

END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
#endif
INTEGER(I4B) :: indx(SIZE(globalNode))

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetNodeLoc_( &
  obj=obj%dof, nodenum=globalNode, ivar=1, spaceCompo=spaceCompo, &
  timeCompo=timeCompo, ans=indx, tsize=tsize)
CALL obj%GetMultiple(VALUE=VALUE, indx=indx, tsize=tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
#endif
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

indx = GetNodeLoc( &
       obj=obj%dof, nodenum=globalNode, ivar=1, spaceCompo=spaceCompo, &
       timeCompo=timeCompo)
CALL obj%GetSingle(VALUE=VALUE, indx=indx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
#endif
INTEGER(I4B) :: indx(obj%spaceCompo), ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ncol = obj%timeCompo

DO ii = 1, ncol
  CALL GetNodeLoc_( &
    obj=obj%dof, nodenum=globalNode, ivar=1, spaceCompo=obj%space_idofs, &
    timeCompo=ii, ans=indx, tsize=nrow)

  CALL obj%GetMultiple(VALUE=VALUE(:, ii), indx=indx, tsize=nrow)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get7()"
#endif
REAL(DFP) :: m3b(obj%spaceCompo, obj%timeCompo, SIZE(globalNode)), &
             m3a(obj%spaceCompo, SIZE(globalNode), obj%timeCompo)

INTEGER(I4B) :: dim1, dim2, dim3

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Get(VALUE=m3b, globalNode=globalNode, islocal=islocal, &
             dim1=dim1, dim2=dim2, dim3=dim3, storageFMT=NODES_FMT)

! Here m3b is in (i, a, J) format,
! so we have to swap the dimensions to (i,J,a)
! We will call swap method from Utility.
CALL SWAP_(a=m3a, b=m3b, i1=1, i2=3, i3=2)

VALUE = NodalVariable(m3a, TypeFEVariableVector, TypeFEVariableSpacetime)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                              GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetFEVariable()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%Get(VALUE=VALUE, globalNode=globalNode, islocal=islocal)

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

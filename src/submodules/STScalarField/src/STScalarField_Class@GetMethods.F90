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

SUBMODULE(STScalarField_Class) GetMethods
USE AbstractField_Class, ONLY: TypeField

USE RealVector_Method, ONLY: GetValue_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableScalar, TypeFEVariableSpaceTime

USE FEVariable_Method, ONLY: NodalVariable

USE DOF_Method, ONLY: GetIDOF, &
                      OPERATOR(.tnodes.)

! USE ScalarField_Class, ONLY: ScalarField_
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: bool1, bool2

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

#endif

bool1 = PRESENT(globalNode)
bool2 = PRESENT(timeCompo)

#ifdef DEBUG_VER

IF (bool1 .AND. bool2) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
        '[INTERNAL ERROR] :: Both globalNode and timeCompo cannot be present')
  RETURN
END IF

#endif

IF (bool2) THEN
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, ivar=1, idof=timeCompo, &
                 VALUE=VALUE, tsize=tsize)
  RETURN
END IF

! IF (bool1) if globalNode present

! note that we overwriting bool1
bool1 = obj%fieldType .EQ. TypeField%constant
IF (bool1) THEN
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, &
                 ivar=1_I4B, VALUE=VALUE, nodenum=[1], &
                 tsize=tsize)
  RETURN
END IF

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, &
               ivar=1_I4B, VALUE=VALUE, &
               nodenum=[globalnode], tsize=tsize)

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"

#ifdef DEBUG_VER

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

#endif

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idof=obj%idofs, &
               VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=storageFMT)

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"

#include "./localNodeError.inc"

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, &
               idof=obj%idofs, VALUE=VALUE, nodenum=globalNode, &
               nrow=nrow, ncol=ncol, storageFMT=storageFMT)
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"

#include "./localNodeError.inc"

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, ivar=1, &
               idof=timeCompo, VALUE=VALUE, tsize=tsize, nodenum=globalNode)

END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"

#include "./localNodeError.inc"

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, ivar=1_I4B, idof=timeCompo, &
               VALUE=VALUE, nodenum=globalnode)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
REAL(DFP), ALLOCATABLE :: v(:, :)
INTEGER(I4B) :: nrow, ncol

#include "./localNodeError.inc"

nrow = obj%timeCompo
ncol = SIZE(globalNode)

ALLOCATE (v(nrow, ncol))

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, VALUE=v, &
               idof=obj%idofs, nodenum=globalNode, nrow=nrow, ncol=ncol, &
               storageFMT=NODES_FMT)

VALUE = NodalVariable(v, TypeFEVariableScalar, TypeFEVariableSpaceTime)

DEALLOCATE (v)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
CHARACTER(*), PARAMETER :: myName = "obj_Get7()"

SELECT TYPE (VALUE)
TYPE IS (ScalarField_)
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, VALUE=VALUE%realVec, &
                 idof=timeCompo)

  ! TYPE IS(ScalarFieldLis_)

TYPE IS (STScalarField_)
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idofobj=timeCompo, &
                 VALUE=VALUE%realVec, dofvalue=VALUE%dof, idofvalue=timeCompo)

  ! TYPE IS(STScalarFieldLis_)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for type of value')
  RETURN

END SELECT
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                     Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
INTEGER(I4B) :: idofobj
INTEGER(I4B) :: idofvalue

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get8()"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: tsize_value

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
                   '[INTERNAL ERROR] :: STScalarField_::obj is not initiated')
  RETURN
END IF

IF (.NOT. VALUE%isInitiated) THEN
  CALL e%RaiseError(modName//'::'//myName//" - "// &
            '[INTERNAL ERROR] :: AbstractNodeField_ ::value is not initiated')
  RETURN
END IF

tsize = obj%dof.tNodes. [ivar, idof]
tsize_value = VALUE%dof.tNodes. [ivar_value, idof_value]
IF (tsize .NE. tsize_value) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
           '[INTERNAL ERROR] :: tSize of obj(ivar, idof) is NOT equal to '// &
                    'value(ivar_value, idof_value)')
  RETURN
END IF

#endif

idofobj = GetIDOF(obj=obj%dof, ivar=ivar, idof=idof)
idofvalue = GetIDOF(obj=VALUE%dof, ivar=ivar_value, idof=idof_value)

CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idofobj=idofobj, &
               VALUE=VALUE%realVec, dofvalue=VALUE%dof, idofvalue=idofvalue)

END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                              GetTimeCompo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTimeCompo
ans = obj%timeCompo
END PROCEDURE obj_GetTimeCompo

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CALL obj%Get(globalNode=globalNode, VALUE=VALUE, islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

END SUBMODULE GetMethods

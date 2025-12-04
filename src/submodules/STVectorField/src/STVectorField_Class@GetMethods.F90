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
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE ScalarField_Class, ONLY: ScalarField_
! USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
! USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_
! USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE RealVector_Method, ONLY: GetValue_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableVector, &
                    TypeFEVariableSpaceTime

USE FEVariable_Method, ONLY: NodalVariable

USE DOF_Method, ONLY: GetIDOF, &
                      OPERATOR(.tnodes.), &
                      GetNodeLoc, &
                      GetNodeLoc_

USE SwapUtility, ONLY: Swap_

USE Display_Method, ONLY: ToString

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: isnode, isspace, istime
INTEGER(I4B) :: indx(128), ii, idof, s(3)
CHARACTER(3) :: mycase

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

  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, ivar=1, &
       spaceCompo=spaceCompo, timeCompo=obj%time_idofs, ans=indx, tsize=tsize)
  CALL obj%GetMultiple(VALUE=VALUE, indx=indx(1:tsize), tsize=tsize)

CASE ("YNY")
  ! node | no space | time

  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, ivar=1, &
   spaceCompo=obj%space_idofs, timeCompo=obj%timeCompo, ans=indx, tsize=tsize)
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
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found.')
  RETURN
END SELECT

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: ii, jj, idof, s(3)

IF (storageFMT .EQ. NODES_FMT) THEN
  dim1 = obj%spaceCompo
  dim2 = obj%timeCompo
  dim3 = obj%dof.tNodes.1

  !$OMP PARALLEL DO PRIVATE(jj, s)
  DO jj = 1, dim3
    CALL obj%Get(VALUE=VALUE(:, :, jj), globalNode=jj, islocal=.TRUE., &
                 nrow=s(1), ncol=s(2))
  END DO
  !$OMP END PARALLEL DO

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
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
INTEGER(I4B) :: ii, jj, idof, s(3), indx(SIZE(globalNode))

IF (storageFMT .EQ. NODES_FMT) THEN
  dim1 = obj%spaceCompo
  dim2 = obj%timeCompo
  dim3 = SIZE(globalNode)

  !$OMP PARALLEL DO PRIVATE(jj, s)
  DO jj = 1, dim3
    CALL obj%Get(VALUE=VALUE(:, :, jj), globalNode=globalNode(jj), &
                 islocal=islocal, nrow=s(1), ncol=s(2))
  END DO
  !$OMP END PARALLEL DO

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
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
INTEGER(I4B) :: indx(SIZE(globalNode))
CALL GetNodeLoc_( &
  obj=obj%dof, nodenum=globalNode, ivar=1, spaceCompo=spaceCompo, &
  timeCompo=timeCompo, ans=indx, tsize=tsize)
CALL obj%GetMultiple(VALUE=VALUE, indx=indx, tsize=tsize)
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
INTEGER(I4B) :: indx
indx = GetNodeLoc( &
       obj=obj%dof, nodenum=globalNode, ivar=1, spaceCompo=spaceCompo, &
       timeCompo=timeCompo)
CALL obj%GetSingle(VALUE=VALUE, indx=indx)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
INTEGER(I4B) :: indx(obj%spaceCompo), ii

ncol = obj%timeCompo

DO ii = 1, ncol
  CALL GetNodeLoc_( &
    obj=obj%dof, nodenum=globalNode, ivar=1, spaceCompo=obj%space_idofs, &
    timeCompo=ii, ans=indx, tsize=nrow)

  CALL obj%GetMultiple(VALUE=VALUE(:, ii), indx=indx, tsize=nrow)
END DO
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                      Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
REAL(DFP) :: m3b(obj%spaceCompo, obj%timeCompo, SIZE(globalNode)), &
             m3a(obj%spaceCompo, SIZE(globalNode), obj%timeCompo)

INTEGER(I4B) :: dim1, dim2, dim3

CALL obj%Get(VALUE=m3b, globalNode=globalNode, islocal=islocal, &
             dim1=dim1, dim2=dim2, dim3=dim3, storageFMT=NODES_FMT)

! Here m3b is in (i, a, J) format,
! so we have to swap the dimensions to (i,J,a)
! We will call swap method from Utility.
CALL SWAP_(a=m3a, b=m3b, i1=1, i2=3, i3=2)

VALUE = NodalVariable(m3a, TypeFEVariableVector, TypeFEVariableSpacetime)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                 Get
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Get8
! #ifdef DEBUG_VER
! LOGICAL(LGT) :: isok
! #endif
! CHARACTER(*), PARAMETER :: myName = "obj_Get8()"
! INTEGER(I4B) :: idof, idof_value, ii
! CHARACTER(2) :: mycase
!
! #ifdef DEBUG_VER
! isok = obj%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   "STVectorField_:: obj is not initiated")
!
! isok = VALUE%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   "AbstractNodeField_:: value is not initiated")
!
! CALL AssertError2(obj%dof.tNodes.1, VALUE%dof.tNodes.1, myName, &
!                   "a=tNodes in obj, b= tNodes in value")
!
! IF (PRESENT(spaceCompo)) THEN
!   isok = spaceCompo .LE. obj%spaceCompo
!   CALL AssertError1(isok, myName, "spaceCompo is greater than obj%spacecompo")
! END IF
!
! IF (PRESENT(timeCompo)) THEN
!   isok = timeCompo .LE. obj%timeCompo
!   CALL AssertError1(isok, myName, "timeCompo is greater than obj%timeCompo")
! END IF
! #endif
!
! mycase = "NN"
!
! IF (PRESENT(spaceCompo)) mycase(1:1) = "Y"
! IF (PRESENT(timeCompo)) mycase(2:2) = "Y"
!
! SELECT CASE (mycase)
! ! spacecompo and timecompo are present
! CASE ("YY")
!   idof = GetIDOF(spaceCompo=spacecompo, timeCompo=timecompo, &
!                  tspaceCompo=obj%spaceCompo)
!
!   SELECT TYPE (VALUE)
!   CLASS IS (ScalarField_)
!     idof_value = 1
!
!   CLASS IS (STScalarField_)
!     idof_value = timeCompo
!
!   CLASS IS (VectorField_)
!     idof_value = spaceCompo
!
!   CLASS IS (STVectorField_)
!     idof_value = GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
!                          tspaceCompo=VALUE%spaceCompo)
!
!   CLASS DEFAULT
!     CALL valueTypeError
!     RETURN
!   END SELECT
!
!   CALL obj%Get(ivar=1, idof=idof, VALUE=VALUE, ivar_value=1, &
!                idof_value=idof_value)
!
! ! spaceCompo is present
! ! timeCompo is not present
!
! CASE ("YN")
!   SELECT TYPE (VALUE)
!
!   CLASS IS (STScalarField_)
!     DO idof_value = 1, obj%timeCompo
!       idof = GetIDOF(spaceCompo=spacecompo, timeCompo=idof_value, &
!                      tspaceCompo=obj%spaceCompo)
!       CALL obj%Get(ivar=1, idof=idof, VALUE=VALUE, ivar_value=1, &
!                    idof_value=idof_value)
!     END DO
!
!   CLASS IS (STVectorField_)
!
!     DO ii = 1, obj%timeCompo
!       idof = GetIDOF(spaceCompo=spaceCompo, timeCompo=ii, &
!                      tspaceCompo=obj%spaceCompo)
!       idof_value = GetIDOF(spaceCompo=spaceCompo, timeCompo=ii, &
!                            tspaceCompo=VALUE%spaceCompo)
!
!       CALL obj%Get(ivar=1, idof=idof, VALUE=VALUE, ivar_value=1, &
!                    idof_value=idof_value)
!     END DO
!
!   CLASS DEFAULT
!     CALL ValueTypeError
!
!   END SELECT
!
! ! timecompo is present
! ! spaceCompo is not present
!
! CASE ("NY")
!   SELECT TYPE (VALUE)
!
!   CLASS IS (VectorField_)
!     DO idof_value = 1, obj%spaceCompo
!       idof = GetIDOF(spaceCompo=idof_value, timeCompo=timeCompo, &
!                      tspaceCompo=obj%spaceCompo)
!       CALL obj%Get(ivar=1, idof=idof, VALUE=VALUE, ivar_value=1, &
!                    idof_value=idof_value)
!     END DO
!
!   CLASS IS (STVectorField_)
!
!     DO ii = 1, obj%spaceCompo
!       idof = GetIDOF(spaceCompo=ii, timeCompo=timeCompo, &
!                      tspaceCompo=obj%spaceCompo)
!       idof_value = GetIDOF(spaceCompo=ii, timeCompo=timeCompo, &
!                            tspaceCompo=VALUE%spaceCompo)
!
!       CALL obj%Get(ivar=1, idof=idof, VALUE=VALUE, ivar_value=1, &
!                    idof_value=idof_value)
!     END DO
!
!   CLASS DEFAULT
!     CALL ValueTypeError
!
!   END SELECT
!
! ! spacecompo is not present
! ! timecompo is not present
! CASE ("NN")
!   SELECT TYPE (VALUE)
!
!   CLASS IS (STVectorField_)
!     CALL VALUE%Copy(obj)
!
!   CLASS DEFAULT
!     CALL ValueTypeError
!
!   END SELECT
!
! CASE default
!   CALL ValueTypeError
! END SELECT
!
! CONTAINS
! SUBROUTINE ValueTypeError
!   CALL e%raiseError(modName//'::'//myName//' - '// &
!                     '[INTERNAL ERROR] :: No case found for type of value')
! END SUBROUTINE ValueTypeError
!
! END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Get9
! #ifdef DEBUG_VER
! LOGICAL(LGT) :: isok
! CHARACTER(*), PARAMETER :: myName = "obj_Get9()"
! #endif
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[START] ')
! #endif
!
! #ifdef DEBUG_VER
! isok = obj%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   "STVectorField_:: obj is not initiated")
!
! isok = VALUE%IsInitiated()
! CALL AssertError1(isok, myName, &
!                   "STVectorField_:: value is not initiated")
! #endif
!
! SELECT TYPE (VALUE)
!
! ! CLASS IS (ScalarField_)
! !   CALL VALUE%Set(ivar=1, idof=1, VALUE=obj, ivar_value=ivar, idof_value=idof)
!
! CLASS IS (STScalarField_)
!   CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
!                  idof_value=idof)
!
! CLASS IS (VectorField_)
!   CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
!                  idof_value=idof)
!
! CLASS IS (STVectorField_)
!   CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
!                  idof_value=idof)
!
! #ifdef DEBUG_VER
! CLASS DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTENRAL ERROR] :: No case found for the type of value')
! #endif
!
! END SELECT
!
! #ifdef DEBUG_VER
! CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!                         '[END] ')
! #endif
! END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                              GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CALL obj%Get(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

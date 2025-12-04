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

SUBMODULE(STVectorField_Class) SetMethods
USE InputUtility, ONLY: Input

USE AbstractMesh_Class, ONLY: AbstractMesh_

USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt

USE AbstractNodeField_Class, ONLY: AbstractNodeFieldGetPointer

USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE STVectorFieldLis_Class, ONLY: STVectorFieldLis_

USE RealVector_Method, ONLY: Set, Add, GetPointer

USE Display_Method, ONLY: tostring

USE GlobalData, ONLY: SpaceTime

USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF, &
                      GetNodeLoc_, &
                      GetIndex_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableVector, &
                    TypeFEVariableSpaceTime, &
                    TypeFEVariableConstant

USE FEVariable_Method, ONLY: Get

USE ReallocateUtility, ONLY: Reallocate

USE SafeSizeUtility, ONLY: SafeSize

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: EXPAND_FACTOR = 2

INTEGER(I4B), PARAMETER :: TEMP_INTVEC_LEN = 128
INTEGER(I4B) :: TEMP_INTVEC(TEMP_INTVEC_LEN)
!$OMP THREADPRIVATE(TEMP_INTVEC)

INTEGER(I4B), ALLOCATABLE :: TEMP_DYNA_INTVEC(:)
!$OMP THREADPRIVATE(TEMP_DYNA_INTVEC)

CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")

CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")
#endif

#include "./localNodeError.F90"

DO ii = 1, obj%timeCompo
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
                tsize=tsize, timeCompo=ii, spaceCompo=obj%space_idofs, ivar=1)
  CALL obj%SetMultiple(VALUE=VALUE(:, ii), indx=TEMP_INTVEC(1:tsize), &
                       scale=scale, addContribution=addContribution)
END DO

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif

INTEGER(I4B) :: ii, jj, s(3)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")

CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")
#endif

DO jj = 1, obj%timeCompo
  DO ii = 1, obj%spaceCompo
    s = GetNodeLoc(obj=obj%dof, idof=GetIDOF(spaceCompo=ii, timeCompo=jj, &
                                             tspaceCompo=obj%spaceCompo))

    CALL obj%SetMultiple(VALUE=VALUE(ii, jj), istart=s(1), iend=s(2), &
                    stride=s(3), scale=scale, addContribution=addContribution)

  END DO
END DO

END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

#endif

s = GetNodeLoc(obj=obj%dof, &
               idof=GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
                            tspaceCompo=obj%spaceCompo))

CALL obj%SetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), &
                    stride=s(3), scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")

CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")

ii = obj%dof.tNodes.1

CALL AssertError2(SIZE(VALUE, 3), ii, myName, &
                  "a=size(value, 2) b=obj%dof.tNodes.1")

#endif

tsize = obj%dof.tNodes.1

!$OMP PARALLEL DO PRIVATE(ii)
DO ii = 1, tsize
  CALL obj%Set(VALUE=VALUE(:, :, ii), scale=scale, globalNode=ii, &
               islocal=.TRUE., addContribution=addContribution)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

#endif

s = GetNodeLoc(obj=obj%dof, &
               idof=GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
                            tspaceCompo=obj%spaceCompo))

CALL obj%SetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), &
                    stride=s(3), scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")

CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")

#endif

!$OMP PARALLEL DO PRIVATE(ii)
DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE, globalNode=globalNode(ii), scale=scale, &
               addContribution=addContribution, islocal=islocal)
END DO
!$OMP END PARALLEL DO
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
#endif

INTEGER(I4B) :: ii

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

CALL AssertError2(SIZE(VALUE, 1), obj%spaceCompo, myName, &
                  "a=size(value, 1) b=obj%spaceCompo")

CALL AssertError2(SIZE(VALUE, 2), obj%timeCompo, myName, &
                  "a=size(value, 2) b=obj%timeCompo")

CALL AssertError2(SIZE(VALUE, 3), SIZE(globalNode), myName, &
                  "a=size(value, 2) b=size(globalNode)")

#endif

!$OMP PARALLEL DO PRIVATE(ii)
DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE(:, :, ii), globalNode=globalNode(ii), &
               scale=scale, addContribution=addContribution, &
               islocal=islocal)
END DO
!$OMP END PARALLEL DO
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
INTEGER(I4B) :: tsize

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompo out of bound")

isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo out of bound")

CALL AssertError2(SIZE(VALUE), SIZE(globalNode), myName, &
                  "a=size(value), b= size(globalNode)")

#endif

#include "./localNodeError.F90"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
              tsize=tsize, timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)
  RETURN
END IF

IF (tsize .GT. SafeSize(TEMP_DYNA_INTVEC)) THEN
  CALL Reallocate(TEMP_DYNA_INTVEC, EXPAND_FACTOR * tsize)
END IF

CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, ans=TEMP_DYNA_INTVEC, &
              tsize=tsize, timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompo out of bound")

isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo out of bound")

#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, &
                  ivar=1, spaceCompo=spaceCompo, timeCompo=timeCompo)

CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"

SELECT CASE (VALUE%vartype); CASE (SpaceTime)

CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableVector,TypeFEVariableSpaceTime), &
               globalNode=globalNode, islocal=islocal, scale=scale, &
               addContribution=addContribution)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for Value%vartype '// &
                    ' only SpaceTime allowed')
  RETURN

END SELECT

END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
INTEGER(I4B) :: nrow, ncol
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set12()"
INTEGER(I4B) :: indx, ii, s(3), tsize

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

isok = timeCompo .LE. obj%timeCompo
CALL AssertError1(isok, myName, "timeCompo out of bound")

DO ii = 1, SIZE(spaceCompo)
  isok = spaceCompo(ii) .LE. obj%spaceCompo
  CALL AssertError1(isok, myName, &
                    "spaceCompo "//tostring(ii)//" out of bound")
END DO

IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = SIZE(spaceCompo)
  ncol = obj%dof.tNodes.1
ELSE
  nrow = obj%dof.tNodes.1
  ncol = SIZE(spaceCompo)
END IF

CALL AssertError2(SIZE(VALUE, 1), nrow, myName, &
                  "a=size(value, 1) b=size(spaceCompo)")

CALL AssertError2(SIZE(VALUE, 2), ncol, myName, &
                  "a=size(value, 2) b=obj%dof.tNodes.1")

#endif

IF (storageFMT .EQ. DOF_FMT) THEN

  DO ii = 1, SIZE(spaceCompo)

    indx = GetIDOF(spaceCompo=spaceCompo(ii), timeCompo=timeCompo, &
                   tspaceCompo=obj%spaceCompo)

    s = GetNodeLoc(obj=obj%dof, idof=indx)

    CALL obj%SetMultiple(VALUE=VALUE(:, ii), istart=s(1), iend=s(2), &
                    stride=s(3), scale=scale, addContribution=addContribution)

  END DO

  RETURN

END IF

indx = obj%dof.tNodes.1

!$OMP PARALLEL DO PRIVATE(ii)
DO ii = 1, indx

  CALL GetNodeLoc_(obj=obj%dof, nodenum=ii, ans=TEMP_INTVEC, tsize=tsize, &
                   timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

  CALL obj%SetMultiple(VALUE=VALUE(:, ii), indx=TEMP_INTVEC(1:tsize), &
                       scale=scale, addContribution=addContribution)

END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
INTEGER(I4B) :: nrow, ncol
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
INTEGER(I4B) :: indx, ii, s(3), tsize

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  'STVectorField_::obj is not initiated')

isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompo out of bound")

DO ii = 1, SIZE(timeCompo)
  isok = timeCompo(ii) .LE. obj%timeCompo
  CALL AssertError1(isok, myName, &
                    "timeCompo "//tostring(ii)//" out of bound")
END DO

IF (storageFMT .EQ. NODES_FMT) THEN
  nrow = SIZE(timeCompo)
  ncol = obj%dof.tNodes.1
ELSE
  nrow = obj%dof.tNodes.1
  ncol = SIZE(timeCompo)
END IF

CALL AssertError2(SIZE(VALUE, 1), nrow, myName, &
                  "a=size(value, 1) b=size( timeCompo)")

CALL AssertError2(SIZE(VALUE, 2), ncol, myName, &
                  "a=size(value, 2) b=obj%dof.tNodes.1")

#endif

IF (storageFMT .EQ. DOF_FMT) THEN

  DO ii = 1, SIZE(timeCompo)

    indx = GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo(ii), &
                   tspaceCompo=obj%spaceCompo)

    s = GetNodeLoc(obj=obj%dof, idof=indx)

    CALL obj%SetMultiple(VALUE=VALUE(:, ii), istart=s(1), iend=s(2), &
                    stride=s(3), scale=scale, addContribution=addContribution)

  END DO

  RETURN

END IF

indx = obj%dof.tNodes.1

!$OMP PARALLEL DO PRIVATE(ii)
DO ii = 1, indx

  CALL GetNodeLoc_(obj=obj%dof, nodenum=ii, ans=TEMP_INTVEC, tsize=tsize, &
                   timeCompo=timeCompo, spaceCompo=spaceCompo, ivar=1)

  CALL obj%SetMultiple(VALUE=VALUE(:, ii), indx=TEMP_INTVEC(1:tsize), &
                       scale=scale, addContribution=addContribution)

END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set14
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set14()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

SELECT TYPE (VALUE); TYPE IS (STVectorField_)

  CALL obj%Copy(VALUE)

#ifdef DEBUG_VER
CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for the type of VALUE')
  RETURN
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Set14
! CHARACTER(*), PARAMETER :: myName = "obj_Set14()"
! INTEGER(I4B) :: ivar, idof, ivar_value, idof_value
!
! ivar = 1
! ivar_value = 1
! idof = GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
!                tspaceCompo=obj%spaceCompo)
!
! SELECT TYPE (VALUE)
! CLASS IS (ScalarField_)
!   idof_value = 1
!
! CLASS IS (STScalarField_)
!   idof_value = timeCompo
!
! CLASS IS (VectorField_)
!   idof_value = spaceCompo
!
! TYPE IS (STVectorField_)
!   idof_value = GetIDOF(spaceCompo=spaceCompo, timeCompo=timeCompo, &
!                        tspaceCompo=VALUE%spaceCompo)
!
! CLASS DEFAULT
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTERNAL ERROR] :: No case found for the type of VALUE')
!   RETURN
! END SELECT
!
! CALL obj%Set(ivar=ivar, idof=idof, VALUE=VALUE, ivar_value=ivar_value, &
!           idof_value=idof_value, scale=scale, addContribution=addContribution)
! END PROCEDURE obj_Set14

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

! MODULE PROCEDURE obj_Set16
! CHARACTER(*), PARAMETER :: myName = "obj_Set16()"
! INTEGER(I4B) :: s(3), p(3), tsize
! REAL(DFP), POINTER :: realvec(:)
!
! #ifdef DEBUG_VER
! CALL AssertError1(obj%isInitiated(), myName, &
!                   'STVectorField_::obj is not initiated')
!
! CALL AssertError1(VALUE%isInitiated(), myName, &
!                   'AbstractNodeField_::value is not initiated')
! #endif
!
! s = GetNodeLoc(obj=obj%dof, idof=idof)
!
! SELECT TYPE (VALUE)
!
! TYPE IS (ScalarField_)
!
!   realvec => VALUE%GetPointer()
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!          addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3))
!   realvec => NULL()
!
! TYPE IS (STScalarField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!
!   realvec => VALUE%GetPointer()
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!        addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
!                        istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! TYPE IS (VectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!
!   realvec => VALUE%GetPointer()
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!        addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
!                        istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! TYPE IS (STVectorField_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!
!   realvec => VALUE%GetPointer()
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!        addContribution=addContribution, istart=s(1), iend=s(2), stride=s(3), &
!                        istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! TYPE IS (ScalarFieldLis_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=1)
!   realvec => AbstractNodeFieldGetPointer(VALUE)
!   CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
!                          tsize=tsize, istart_value=p(1), iend_value=p(2), &
!                          stride_value=p(3))
!
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!                     addContribution=addContribution, istart=s(1), iend=s(2), &
!            stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! TYPE IS (STScalarFieldLis_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!   realvec => AbstractNodeFieldGetPointer(VALUE)
!   CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
!                          tsize=tsize, istart_value=p(1), iend_value=p(2), &
!                          stride_value=p(3))
!
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!                     addContribution=addContribution, istart=s(1), iend=s(2), &
!            stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! ! TYPE IS (STScalarFieldLis_)
!
! TYPE IS (VectorFieldLis_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!   realvec => AbstractNodeFieldGetPointer(VALUE)
!   CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
!                          tsize=tsize, istart_value=p(1), iend_value=p(2), &
!                          stride_value=p(3))
!
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!                     addContribution=addContribution, istart=s(1), iend=s(2), &
!            stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! TYPE IS (STVectorFieldLis_)
!
!   p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
!   realvec => AbstractNodeFieldGetPointer(VALUE)
!   CALL VALUE%GetMultiple(VALUE=realvec, istart=p(1), iend=p(2), stride=p(3), &
!                          tsize=tsize, istart_value=p(1), iend_value=p(2), &
!                          stride_value=p(3))
!
!   CALL obj%SetMultiple(VALUE=realvec, scale=scale, &
!                     addContribution=addContribution, istart=s(1), iend=s(2), &
!            stride=s(3), istart_value=p(1), iend_value=p(2), stride_value=p(3))
!   realvec => NULL()
!
! CLASS DEFAULT
!
!   CALL e%RaiseError(modName//'::'//myName//' - '// &
!                     '[INTERNAL ERROR] :: No case found for the type of VALUE')
!   RETURN
!
! END SELECT
! END PROCEDURE obj_Set16

!----------------------------------------------------------------------------
!                                                         SetFromVectorField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetFromVectorField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetFromVectorField()"
#endif
! INTEGER(I4B) :: ii, jj

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! DO ii = 1, obj%spaceCompo
!   jj = GetIDOF(spaceCompo=ii, timeCompo=1, tspaceCompo=obj%spaceCompo)
!   CALL obj%Set(idof=jj, ivar=1, VALUE=VALUE, idof_value=ii, &
!                ivar_value=1, scale=scale, addContribution=addContribution)
! END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetFromVectorField

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: istimes, problem
INTEGER(I4B) :: ttime, nsd, tnodes, ii, globalNode(1), itime, ispace, nrow, &
                ncol
REAL(DFP) :: args(4), xij(3, 1)
REAL(DFP), ALLOCATABLE :: VALUE(:)
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

istimes = PRESENT(times)
problem = .FALSE.

args = 0.0_DFP
ttime = 1
IF (istimes) THEN
  ttime = SIZE(times)
  problem = ttime .NE. obj%timeCompo
END IF

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
              '[INTERNAL ERROR] :: size of times should be obj%timeCompo='// &
                    tostring(obj%timeCompo))
  RETURN
END IF

meshptr => NULL()
meshptr => obj%fedof%GetMeshPointer()
problem = .NOT. ASSOCIATED(meshptr)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: domain is not ASSOCIATED.')
  RETURN
END IF

nsd = meshptr%GetNSD()
tnodes = meshptr%GetTotalNodes()

IF (istimes) THEN
  DO ii = 1, tnodes
    globalNode = ii
    CALL meshptr%GetNodeCoord(globalNode=globalNode, nodeCoord=xij, &
                              islocal=.TRUE., nrow=nrow, ncol=ncol)
    args(1:nsd) = xij(1:nsd, 1)

    DO itime = 1, obj%timeCompo
      args(4) = times(itime)
      CALL func%Get(val=VALUE, args=args)
      DO ispace = 1, obj%spaceCompo
        CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE(ispace), &
                     timeCompo=itime, spaceCompo=ispace, islocal=.TRUE.)
      END DO
    END DO
  END DO
END IF

IF (.NOT. istimes) THEN

  DO ii = 1, tnodes
    globalNode = ii
    CALL meshptr%GetNodeCoord(globalNode=globalNode, nodeCoord=xij, &
                              islocal=.TRUE., nrow=nrow, ncol=ncol)
    args(1:nsd) = xij(1:nsd, 1)
    CALL func%Get(val=VALUE, args=args)

    DO itime = 1, obj%timeCompo
      DO ispace = 1, obj%spaceCompo
        CALL obj%Set(globalNode=globalNode(1), VALUE=VALUE(ispace), &
                     timeCompo=itime, spaceCompo=ispace, islocal=.TRUE.)
      END DO
    END DO
  END DO

END IF

IF (ALLOCATED(VALUE)) DEALLOCATE (VALUE)
NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

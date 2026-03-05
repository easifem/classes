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

SUBMODULE(VectorField_Class) SetMethods
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ArangeUtility, ONLY: Arange
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE BlockNodeField_Class, ONLY: Blocknodefield_
USE Display_Method, ONLY: tostring
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetIndex_
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: GetNodeLoc_
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE ElemShapeData_Method, ONLY: ElemShapeData_Deallocate => DEALLOCATE
USE FEVariable_Method, ONLY: Get
USE GlobalData, ONLY: Constant
USE GlobalData, ONLY: NODES_FMT
USE GlobalData, ONLY: NONE
USE GlobalData, ONLY: Scalar
USE GlobalData, ONLY: Space
USE GlobalData, ONLY: Vector
USE InputUtility, ONLY: Input
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Deallocate => DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
USE RealVector_Method, ONLY: Set, Add, GetPointer
USE SafeSizeUtility, ONLY: SafeSize
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE StringUtility, ONLY: UpperCase
USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE STVectorField_Class, ONLY: STVectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "VectorField_Class@SetMethods.F90"
#endif

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
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_::obj not initiated")
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%fieldType .NE. TypeFieldOpt%constant, myName, &
                  "Not callable for constant STScalar field")
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE), obj%spaceCompo, myName, &
                  "a=SIZE(VALUE), b=obj%spaceCompo")
#endif

#ifdef DEBUG_VER
isok = obj%spaceCompo .LE. TEMP_INTVEC_LEN
CALL AssertError1(isok, myName, "size of TEMP_INTVEC is not enough")
#endif

#include "./localNodeError.F90"

CALL GetIndex_(obj=obj%dof, nodenum=globalNode, ans=TEMP_INTVEC, &
               tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set2()"
#endif
INTEGER(I4B) :: idof, s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%fieldType .EQ. TypeFieldOpt%constant, myName, &
                  'Not callable for constant STScalar field')
#endif

DO idof = 1, obj%spaceCompo
  s = GetNodeLoc(obj=obj%dof, idof=idof)
  CALL obj%SetMultiple( &
    VALUE=VALUE(idof), istart=s(1), iend=s(2), &
    stride=s(3), scale=scale, addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
                  'spaceComposhould be less or equal to obj%spaceCompo')
#endif

#ifdef DEBUG_VER
isok = obj%fieldType .NE. TypeFieldOpt%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant field')
#endif

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)
CALL obj%SetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                     scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
INTEGER(I4B) :: nrow
#endif

INTEGER(I4B) :: jj, ncol

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%fieldType .NE. TypeFieldOpt%constant, myName, &
                  'Not callable for constant STScalar field')
#endif

#ifdef DEBUG_VER
IF (storageFMT .EQ. TypeFieldOpt%storageFormatNodes) THEN
  nrow = obj%spaceCompo
  ncol = obj%dof.tNodes.1
ELSE
  nrow = obj%dof.tNodes.1
  ncol = obj%spaceCompo
END IF
#endif

#ifdef DEBUG_VER
CALL AssertError2(SIZE(VALUE, 1), nrow, myName, 'a=SIZE(VALUE, 1), b=nrow')
CALL AssertError2(SIZE(VALUE, 2), ncol, myName, 'a=SIZE(VALUE, 2), b=ncol')
#endif

IF (storageFMT .EQ. obj%GetStorageFMT()) THEN
  DO jj = 1, obj%spaceCompo
    CALL obj%Set(VALUE=VALUE(:, jj), spaceCompo=jj, scale=scale, &
                 addContribution=addContribution)
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

ncol = obj%dof.tNodes.1
DO jj = 1, ncol
  CALL obj%Set(VALUE=VALUE(:, jj), scale=scale, &
               addContribution=addContribution, globalNode=jj, &
               islocal=math%yes)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set5()"
INTEGER(I4B) :: tsize
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'STScalarField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompoout of bound")
#endif

#ifdef DEBUG_VER
isok = obj%fieldType .NE. TypeFieldOpt%constant
CALL AssertError1(isok, myName, "Not callable for constant field")
#endif

#ifdef DEBUG_VER
tsize = obj%dof.tNodes.spaceCompo
CALL AssertError2(SIZE(VALUE), tsize, myName, &
                  "a=SIZE(VALUE), b=obj%dof.tNodes.spaceCompo")
#endif

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)
CALL obj%SetMultiple(VALUE=VALUE, scale=scale, istart=s(1), &
                     iend=s(2), stride=s(3), addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

! CALL obj%Set(ivar=1, idof=spaceCompo, VALUE=VALUE, ivar_value=1, &
!           idof_value=spaceCompo, scale=scale, &
!           addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set7()"
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

DO ii = 1, SIZE(globalNode)
  CALL obj%Set(VALUE=VALUE, globalNode=globalNode(ii), islocal=islocal, &
               scale=scale, addContribution=addContribution)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set8()"
LOGICAL(LGT) :: isok, abool
#endif
INTEGER(I4B) :: ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = obj%IsInitiated()
CALL AssertError1(isok, myName, &
                  'VectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
isok = obj%fieldType .NE. TypeFieldOpt%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant vector field')
#endif

#ifdef DEBUG_VER
abool = storageFMT .EQ. TypeFieldOpt%storageFormatNodes

IF (abool) THEN
  isok = SIZE(VALUE, 1) .EQ. obj%spaceCompo
  CALL AssertError1(isok, myName, &
                    'SIZE(VALUE, 1) not same as obj%spaceCompo')

  isok = SIZE(VALUE, 2) .EQ. SIZE(globalNode)
  CALL AssertError1(isok, myName, &
                    'SIZE(VALUE, 2) not same as size(globalNode)')

ELSE

  isok = SIZE(VALUE, 2) .EQ. obj%spaceCompo
  CALL AssertError1(isok, myName, &
                    'SIZE(VALUE, 2) not same as obj%spaceCompo')

  isok = SIZE(VALUE, 1) .EQ. SIZE(globalNode)
  CALL AssertError1(isok, myName, &
                    'SIZE(VALUE, 1) not same as size(globalNode)')
END IF
#endif

SELECT CASE (storageFMT)

CASE (TypeFieldOpt%storageFormatNodes)

  DO ii = 1, SIZE(VALUE, 2)
    CALL obj%Set(globalNode=globalNode(ii), islocal=islocal, &
             VALUE=VALUE(:, ii), addContribution=addContribution, scale=scale)
  END DO

CASE (TypeFieldOpt%storageFormatDOF)

  DO ii = 1, SIZE(VALUE, 2)
    CALL obj%Set(globalNode=globalNode, islocal=islocal, &
                 spaceCompo=ii, VALUE=VALUE(:, ii), &
                 addContribution=addContribution, scale=scale)
  END DO

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for storageFMT")
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_::obj not initiated")

isok = spaceCompo .LE. obj%spaceCompo
CALL AssertError1(isok, myName, "spaceCompois out of bound")

isok = obj%fieldType .NE. TypeFieldOpt%constant
CALL AssertError1(isok, myName, &
                  'Not callable for constant STScalar field')

CALL AssertError2(SIZE(VALUE), SIZE(globalNode), &
                  myName, 'a=SIZE(VALUE), b=size(globalNode)')

#endif

#include "./localNodeError.F90"

tsize = SIZE(globalNode)

IF (tsize .LE. TEMP_INTVEC_LEN) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=spaceCompo, &
                   nodenum=globalNode, ans=TEMP_INTVEC, tsize=tsize)

  CALL obj%SetMultiple(indx=TEMP_INTVEC(1:tsize), VALUE=VALUE, scale=scale, &
                       addContribution=addContribution)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN

END IF

IF (tsize .GT. SafeSize(TEMP_DYNA_INTVEC)) THEN
  CALL Reallocate(TEMP_DYNA_INTVEC, EXPAND_FACTOR * tsize)
END IF

CALL GetNodeLoc_(obj=obj%dof, idof=spaceCompo, nodenum=globalNode, &
                 ans=TEMP_DYNA_INTVEC, tsize=tsize)

CALL obj%SetMultiple(indx=TEMP_DYNA_INTVEC(1:tsize), VALUE=VALUE, &
                     scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set10
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set10()"
#endif
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            'given spaceCompo should be less than or equal to obj%spaceCompo')

CALL AssertError1(obj%fieldType .NE. TypeFieldOpt%constant, myName, &
                  'Not callable for constant vector field')
#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, idof=spaceCompo, nodenum=globalNode)

CALL obj%SetSingle(VALUE=VALUE, indx=indx, scale=scale, &
                   addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set10

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set11
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set11()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'VectorField_::obj is not initiated')
CALL AssertError1(obj%fieldType .NE. TypeFieldOpt%constant, myName, &
                  'Not callable for constant vector field')
#endif

SELECT CASE (VALUE%vartype)

CASE (Constant)
  CALL obj%Set( &
    VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableConstant), &
    globalNode=globalNode, scale=scale, addContribution=addContribution, &
    islocal=islocal)

CASE (Space)
  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableSpace), &
               globalNode=globalNode, scale=scale, islocal=islocal, &
               addContribution=addContribution, &
               storageFMT=TypeFieldOpt%storageFormatNodes)

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'No case found for the type of value.')
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set11

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set12
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set12()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set12

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set13
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set13()"
#endif

INTEGER(I4B) :: tsize
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarFieldLis_::obj is not initiated")
#endif

SELECT TYPE (VALUE)
TYPE IS (VectorField_)

  realvec => VALUE%GetPointer()
  tsize = SIZE(realvec)
  CALL obj%SetMultiple( &
    VALUE=realvec, istart=1_I4B, iend=tsize, stride=1_I4B)
  realvec => NULL()

#ifdef DEBUG_VER
CLASS DEFAULT
  CALL AssertError1(math%no, myName, &
                    'Unknown type of ScalarField_::value')
#endif
END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Set13

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: returnType
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

returnType = func%GetReturnType()

SELECT CASE (returnType)
CASE (TypeFEVariableOpt%scalar)

#ifdef DEBUG_VER
  isok = PRESENT(spaceCompo)
  CALL AssertError1(isok, myName, &
                    "WIP: spaceCompo must be present for scalar function")
#endif

  CALL Help_SetByScalarFunction(obj, func, times, spaceCompo)

CASE (TypeFEVariableOpt%vector)

  CALL Help_SetByVectorFunction(obj, func, times)

CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    "No case found for returnType.")
#endif

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetByFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_SetByScalarFunction(obj, func, times, spaceCompo)
  CLASS(VectorField_), INTENT(INOUT) :: obj
  CLASS(UserFunction_), INTENT(INOUT) :: func
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  INTEGER(I4B), INTENT(IN) :: spaceCompo

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Help_SetByScalarFunction()"
  LOGICAL(LGT) :: isok
#endif

  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  INTEGER(I4B) :: telements, iel, maxNNS, maxGeoNNS, maxNips, tans, &
                  xij_i, xij_j, tcon, ii
  TYPE(QuadraturePoint_) :: quad(8), facetQuad(8), cellQuad
  TYPE(ElemShapeData_) :: cellElemsd, geoCellElemsd, geoElemsd(8), &
                          geoFacetElemsd(8), elemsd(8), facetElemsd(8)
  REAL(DFP) :: times0
  REAL(DFP), ALLOCATABLE :: xij(:, :), ans(:), massMat(:, :), &
                            funcValue(:), temp(:)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:), con(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  meshptr => obj%fedof%GetMeshPointer()

#ifdef DEBUG_VER
  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myName, &
                    "mesh pointer obtained from fedof is not associated...")
#endif

  times0 = 0.0_DFP
  IF (PRESENT(times)) times0 = times(1)

  maxNNS = obj%fedof%GetMaxTotalConnectivity()
  maxGeoNNS = obj%geofedof%GetMaxTotalConnectivity()
  maxNips = obj%fedof%GetMaxTotalQuadraturePoints()

  CALL Reallocate(massMat, maxNNS, maxNNS)
  CALL Reallocate(ipiv, maxNNS)
  CALL Reallocate(xij, 3, maxGeoNNS)
  CALL Reallocate(ans, maxNNS)
  CALL Reallocate(temp, maxNNS)
  CALL Reallocate(con, maxNNS)
  CALL Reallocate(funcValue, maxNips)

  telements = meshptr%GetTotalElements()

  DO iel = 1, telements
    CALL obj%fedof%SetFE(globalElement=iel, islocal=.TRUE.)
    feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)

    CALL obj%geofedof%SetFE(globalElement=iel, islocal=.TRUE.)
    geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)

    CALL meshptr%GetNodeCoord(nodeCoord=xij, nrow=xij_i, &
                              ncol=xij_j, globalElement=iel, islocal=.TRUE.)

    CALL feptr%GetDOFValue( &
      geofeptr=geofeptr, elemsd=elemsd, geoElemsd=geoElemsd, &
      facetElemsd=facetElemsd, geoFacetElemsd=geoFacetElemsd, &
      cellElemsd=cellElemsd, geoCellElemsd=geoCellElemsd, &
      quad=quad, facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, &
      times=times0, func=func, ans=ans, tsize=tans, massMat=massMat, &
      ipiv=ipiv, funcValue=funcValue, temp=temp, icompo=spaceCompo)

    CALL obj%fedof%GetConnectivity_(ans=con, tsize=tcon, opt="A", &
                                    globalElement=iel, islocal=.TRUE.)

    ! (obj, ans, tsize, opt, globalElement, islocal)
    CALL obj%Set(VALUE=ans(1:tans), globalNode=con(1:tcon), &
                 islocal=.TRUE., spaceCompo=spaceCompo)
  END DO

  DEALLOCATE (massMat, ipiv, xij, ans, temp, con, funcValue)

  DO ii = 1, SIZE(quad)
    CALL QuadraturePoint_Deallocate(quad(ii))
    CALL QuadraturePoint_Deallocate(facetQuad(ii))
    CALL ElemShapeData_Deallocate(elemsd(ii))
    CALL ElemShapeData_Deallocate(facetElemsd(ii))
    CALL ElemShapeData_Deallocate(geoElemsd(ii))
    CALL ElemShapeData_Deallocate(geoFacetElemsd(ii))
  END DO

  CALL QuadraturePoint_Deallocate(cellQuad)
  CALL ElemShapeData_Deallocate(cellElemsd)
  CALL ElemShapeData_Deallocate(geoCellElemsd)

  NULLIFY (meshptr, feptr, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Help_SetByScalarFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_SetByVectorFunction(obj, func, times)
  CLASS(VectorField_), INTENT(INOUT) :: obj
  CLASS(UserFunction_), INTENT(INOUT) :: func
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "Help_SetByVectorFunction()"
  LOGICAL(LGT) :: isok
#endif

  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  INTEGER(I4B) :: telements, iel, maxNNS, maxGeoNNS, maxNips, tans, &
                  xij_i, xij_j, tcon, ii, spaceCompo(1), &
                  icompo
  TYPE(QuadraturePoint_) :: quad(8), facetQuad(8), cellQuad
  TYPE(ElemShapeData_) :: cellElemsd, geoCellElemsd, geoElemsd(8), &
                          geoFacetElemsd(8), elemsd(8), facetElemsd(8)
  REAL(DFP) :: times0
  REAL(DFP), ALLOCATABLE :: xij(:, :), ans(:, :), massMat(:, :), &
                            funcValue(:), temp(:)
  INTEGER(I4B), ALLOCATABLE :: ipiv(:), con(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  meshptr => obj%fedof%GetMeshPointer()

#ifdef DEBUG_VER
  isok = ASSOCIATED(meshptr)
  CALL AssertError1(isok, myName, &
                    "mesh pointer obtained from fedof is not associated...")
#endif

  times0 = 0.0_DFP
  IF (PRESENT(times)) times0 = times(1)

  spaceCompo = obj%GetSpaceCompo(1)
  maxNNS = obj%fedof%GetMaxTotalConnectivity()
  maxGeoNNS = obj%geofedof%GetMaxTotalConnectivity()
  maxNips = obj%fedof%GetMaxTotalQuadraturePoints()

  CALL Reallocate(massMat, maxNNS, maxNNS)
  CALL Reallocate(ipiv, maxNNS)
  CALL Reallocate(xij, 3, maxGeoNNS)
  CALL Reallocate(ans, spaceCompo(1), maxNNS)
  CALL Reallocate(temp, maxNNS)
  CALL Reallocate(con, maxNNS)
  CALL Reallocate(funcValue, maxNips)

  telements = meshptr%GetTotalElements()

  DO iel = 1, telements
    CALL obj%fedof%SetFE(globalElement=iel, islocal=.TRUE.)
    feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)

    CALL obj%geofedof%SetFE(globalElement=iel, islocal=.TRUE.)
    geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)

    CALL meshptr%GetNodeCoord(nodeCoord=xij, nrow=xij_i, &
                              ncol=xij_j, globalElement=iel, islocal=.TRUE.)

    DO icompo = 1, spaceCompo(1)
      CALL feptr%GetDOFValue( &
        geofeptr=geofeptr, elemsd=elemsd, geoElemsd=geoElemsd, &
        facetElemsd=facetElemsd, geoFacetElemsd=geoFacetElemsd, &
        cellElemsd=cellElemsd, geoCellElemsd=geoCellElemsd, &
        quad=quad, facetQuad=facetQuad, cellQuad=cellQuad, xij=xij, &
        times=times0, func=func, ans=ans(icompo, 1:maxNNS), &
        tsize=tans, massMat=massMat, &
        ipiv=ipiv, funcValue=funcValue, temp=temp, icompo=icompo)
    END DO

    CALL obj%fedof%GetConnectivity_(ans=con, tsize=tcon, opt="A", &
                                    globalElement=iel, islocal=.TRUE.)

    ! (obj, ans, tsize, opt, globalElement, islocal)
    CALL obj%Set(VALUE=ans(1:spaceCompo(1), 1:tans), globalNode=con(1:tcon), &
                 islocal=.TRUE., storageFMT=NODES_FMT)
  END DO

  DEALLOCATE (massMat, ipiv, xij, ans, temp, con, funcValue)

  DO ii = 1, SIZE(quad)
    CALL QuadraturePoint_Deallocate(quad(ii))
    CALL QuadraturePoint_Deallocate(facetQuad(ii))
    CALL ElemShapeData_Deallocate(elemsd(ii))
    CALL ElemShapeData_Deallocate(facetElemsd(ii))
    CALL ElemShapeData_Deallocate(geoElemsd(ii))
    CALL ElemShapeData_Deallocate(geoFacetElemsd(ii))
  END DO

  CALL QuadraturePoint_Deallocate(cellQuad)
  CALL ElemShapeData_Deallocate(cellElemsd)
  CALL ElemShapeData_Deallocate(geoCellElemsd)

  NULLIFY (meshptr, feptr, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Help_SetByVectorFunction

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

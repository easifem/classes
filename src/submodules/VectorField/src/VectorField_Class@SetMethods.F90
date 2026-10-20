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
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableConstant
USE BaseType, ONLY: TypeFEVariableOpt
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeFEVariableVector
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE BaseType, ONLY: math => TypeMathOpt
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetIndex_
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: GetNodeLoc_
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE ElemShapeData_Method, ONLY: ElemShapeDataDeallocate => DEALLOCATE
USE ElemShapeData_Method, ONLY: GetInterpolation_
USE FEVariable_Method, ONLY: Get
USE InputUtility, ONLY: Input
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => DEALLOCATE
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
USE ReallocateUtility, ONLY: Reallocate
USE SafeSizeUtility, ONLY: SafeSize
USE StringUtility, ONLY: UpperCase
USE Display_Method, ONLY: Display
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
                  "VectorField_::obj not initiated")
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
                  'VectorField_::obj is not initiated')
#endif

#ifdef DEBUG_VER
CALL AssertError1(obj%fieldType .EQ. TypeFieldOpt%constant, myName, &
                  'Not callable for constant VectorField_')
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
                  'VectorField_::obj is not initiated')
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
                  'VectorField_::obj is not initiated')
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
                  'VectorField_::obj is not initiated')
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
                  "VectorField_::obj not initiated")

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

CASE (TypeFieldOpt%constant)
  CALL obj%Set( &
    VALUE=GET(VALUE, TypeFEVariableVector, TypeFEVariableConstant), &
    globalNode=globalNode, scale=scale, addContribution=addContribution, &
    islocal=islocal)

CASE (TypeFieldOpt%space)
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
                  "VectorField_::obj is not initiated")
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
                    'Unknown type of VectorField_::value')
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

  INTEGER(I4B), PARAMETER :: MAX_TOPO_SIZE = 8
  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  INTEGER(I4B) :: telements, iel, maxNNS, maxGeoNNS, maxNips, tans, &
                  xij_i, xij_j, tcon, ii
  TYPE(QuadraturePoint_) :: quad(MAX_TOPO_SIZE), facetQuad(MAX_TOPO_SIZE)
  TYPE(QuadraturePoint_) :: cellQuad
  TYPE(ElemShapeData_) :: cellElemsd, geoCellElemsd
  TYPE(ElemShapeData_) :: geoElemsd(MAX_TOPO_SIZE), &
                          geoFacetElemsd(MAX_TOPO_SIZE), &
                          elemsd(MAX_TOPO_SIZE), &
                          facetElemsd(MAX_TOPO_SIZE)
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
    CALL QuadraturePointDeallocate(quad(ii))
    CALL QuadraturePointDeallocate(facetQuad(ii))
    CALL ElemShapeDataDeallocate(elemsd(ii))
    CALL ElemShapeDataDeallocate(facetElemsd(ii))
    CALL ElemShapeDataDeallocate(geoElemsd(ii))
    CALL ElemShapeDataDeallocate(geoFacetElemsd(ii))
  END DO

  CALL QuadraturePointDeallocate(cellQuad)
  CALL ElemShapeDataDeallocate(cellElemsd)
  CALL ElemShapeDataDeallocate(geoCellElemsd)

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

  INTEGER(I4B), PARAMETER :: MAX_TOPO_SIZE = 8
  CLASS(AbstractMesh_), POINTER :: meshptr
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  INTEGER(I4B) :: telements, iel, maxNNS, maxGeoNNS, maxNips, tans, &
                  xij_i, xij_j, tcon, ii, icompo
  INTEGER(I4B) :: spaceCompo(1)
  TYPE(QuadraturePoint_) :: cellQuad
  TYPE(QuadraturePoint_) :: quad(MAX_TOPO_SIZE), facetQuad(MAX_TOPO_SIZE)
  TYPE(ElemShapeData_) :: cellElemsd, geoCellElemsd
  TYPE(ElemShapeData_) :: geoElemsd(MAX_TOPO_SIZE), &
                          geoFacetElemsd(MAX_TOPO_SIZE), &
                          elemsd(MAX_TOPO_SIZE), &
                          facetElemsd(MAX_TOPO_SIZE)
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
                 islocal=.TRUE., storageFMT=TypeFieldOpt%storageFormatNodes)
  END DO

  DEALLOCATE (massMat, ipiv, xij, ans, temp, con, funcValue)

  DO ii = 1, SIZE(quad)
    CALL QuadraturePointDeallocate(quad(ii))
    CALL QuadraturePointDeallocate(facetQuad(ii))
    CALL ElemShapeDataDeallocate(elemsd(ii))
    CALL ElemShapeDataDeallocate(facetElemsd(ii))
    CALL ElemShapeDataDeallocate(geoElemsd(ii))
    CALL ElemShapeDataDeallocate(geoFacetElemsd(ii))
  END DO

  CALL QuadraturePointDeallocate(cellQuad)
  CALL ElemShapeDataDeallocate(cellElemsd)
  CALL ElemShapeDataDeallocate(geoCellElemsd)

  NULLIFY (meshptr, feptr, geofeptr)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE Help_SetByVectorFunction

!----------------------------------------------------------------------------
!                                                     SetHigerOrderNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetHigherOrderNodeCoord
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetHigherOrderNodeCoord()"
#endif

INTEGER(I4B) :: iel, tElements, ii, maxGeoNNS, maxCon, &
                elemCoord_i, elemCoord_j, xij_i, xij_j, tfecon
INTEGER(I4B) :: cellOrder(3), ipType(3)
INTEGER(I4B), ALLOCATABLE :: fecon(:)
CLASS(AbstractFE_), POINTER :: feptr
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(FEDOF_), POINTER :: fedof
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

fedof => obj%fedof
mesh => fedof%GetMeshPointer()
tElements = mesh%GetTotalElements()
maxGeoNNS = mesh%GetMaxNNE()
maxCon = fedof%GetMaxTotalConnectivity()

CALL Reallocate(elemCoord, math%three_i, maxGeoNNS)
CALL Reallocate(xij, math%three_i, maxCon)
CALL Reallocate(fecon, maxCon)

cell_loop: DO iel = 1, tElements
  isok = mesh%isElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE cell_loop

  CALL fedof%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                              ans=fecon, tsize=tfecon, opt="A")

  CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL fedof%GetCellOrder(ans=cellOrder, tsize=ii, globalElement=iel, &
                          islocal=math%yes)
  cellOrder(2:3) = cellOrder(1)

  ipType(1) = feptr%GetIpType()
  ipType(2:3) = ipType(1)

  CALL mesh%GetNodeCoord( &
    globalElement=iel, nodeCoord=elemCoord, nrow=elemCoord_i, &
    ncol=elemCoord_j, islocal=math%yes)

  CALL feptr%GetInterpolationPoints( &
    xij=elemCoord(1:elemCoord_i, 1:elemCoord_j), ans=xij, nrow=xij_i, &
    ncol=xij_j, order=cellOrder, ipType=ipType)

  !! tsize in the following is readonly, so no problem
  CALL feptr%FixOrientOfNptrs(nptrs=fecon, tsize=tfecon)

  CALL obj%Set(VALUE=xij(1:obj%spaceCompo, 1:tfecon), &
               globalNode=fecon(1:tfecon), &
               islocal=math%yes, scale=math%one, &
               addContribution=math%no, &
               storageFMT=TypeFieldOpt%storageFormatNodes)
END DO cell_loop

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(elemCoord)) DEALLOCATE (elemCoord)
IF (ALLOCATED(fecon)) DEALLOCATE (fecon)
NULLIFY (fedof, mesh, feptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetHigherOrderNodeCoord

!----------------------------------------------------------------------------
!                                                             SetLagrangeDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLagrangeDOF
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetLagrangeDOF()"
#endif

INTEGER(I4B) :: iel, tElements, ii, maxCon, &
                tfecon, tElements2, tfecon2, maxCon2, maxGeoNNS2, &
                refElemCoord2_i, refElemCoord2_j, &
                xij_i, xij_j, elemCoord2_i, elemCoord2_j, &
                nodalValue2_i, nodalValue2_j, &
                nodalValue1_i, nodalValue1_j
INTEGER(I4B) :: cellOrder(3), ipType(3)
LOGICAL(LGT) :: isok
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: geoelemsd2, elemsd2
INTEGER(I4B), ALLOCATABLE :: fecon(:), fecon2(:)
REAL(DFP), ALLOCATABLE :: elemCoord2(:, :), &
                          refElemCoord2(:, :), &
                          xij(:, :), &
                          nodalValue1(:, :), &
                          nodalValue2(:, :)
CLASS(AbstractFE_), POINTER :: feptr, feptr2, geofeptr2
CLASS(AbstractMesh_), POINTER :: mesh, mesh2
CLASS(FEDOF_), POINTER :: fedof, fedof2, geofedof2

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

fedof => obj%fedof
mesh => fedof%GetMeshPointer()
tElements = mesh%GetTotalElements()
maxCon = fedof%GetMaxTotalConnectivity()

fedof2 => obj2%fedof
geofedof2 => obj2%geofedof
mesh2 => fedof2%GetMeshPointer()
tElements2 = mesh2%GetTotalElements()
maxGeoNNS2 = geofedof2%GetMaxTotalConnectivity()
maxCon2 = fedof2%GetMaxTotalConnectivity()

!check
#ifdef DEBUG_VER
CALL AssertError2(tElements, tElements2, myName, &
                  "a=tElements in obj, b=tElements in obj2")
#endif

!check
#ifdef DEBUG_VER
CALL AssertError2(obj%spaceCompo, obj2%spaceCompo, myName, &
                  "a=spaceCompo in obj, b=spaceCompo in obj2")
#endif

CALL Reallocate(fecon, maxCon)
CALL Reallocate(nodalValue1, obj%spaceCompo, maxCon)
CALL Reallocate(xij, obj%spaceCompo + 1, maxCon)

CALL Reallocate(refElemCoord2, math%three_i, maxGeoNNS2)
CALL Reallocate(fecon2, maxCon2)
CALL Reallocate(elemCoord2, math%three_i, maxGeoNNS2)
CALL Reallocate(nodalValue2, obj2%spaceCompo, maxCon2)

cell_loop: DO iel = 1, tElements
  isok = mesh2%isElementActive(globalElement=iel, islocal=math%yes)
  IF (.NOT. isok) CYCLE cell_loop

  CALL fedof2%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                               ans=fecon2, tsize=tfecon2, opt="A")

  CALL fedof2%SetFE(globalElement=iel, islocal=math%yes)
  feptr2 => fedof2%GetFEPointer(globalElement=iel, islocal=math%yes)

  CALL feptr2%GetRefElemCoord(ans=refElemCoord2, nrow=refElemCoord2_i, &
                              ncol=refElemCoord2_j)

  ! Get cellorder and ipType from obj
  CALL fedof%GetCellOrder(ans=cellOrder, tsize=ii, globalElement=iel, &
                          islocal=math%yes)
  cellOrder(2:3) = cellOrder(1)
  CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
  ipType(1) = feptr%GetIpType()
  ipType(2:3) = ipType(1)

  ! Get interpolation points from feptr2
  CALL feptr2%GetInterpolationPoints( &
    xij=refElemCoord2(1:refElemCoord2_i, 1:refElemCoord2_j), &
    ans=xij, nrow=xij_i, ncol=xij_j, order=cellOrder, ipType=ipType)

  ! Create quadrature points
  CALL QuadraturePointInitiate(obj=quad, points=xij(:, 1:xij_j))

  ! Get elemCoord2 from mesh2
  CALL mesh2%GetNodeCoord(nodeCoord=elemCoord2, nrow=elemCoord2_i, &
                          ncol=elemCoord2_j, globalElement=iel, &
                          islocal=math%yes)

  CALL geofedof2%SetFE(globalElement=iel, islocal=math%yes)
  geofeptr2 => geofedof2%GetFEPointer(globalElement=iel, &
                                      islocal=math%yes)

  ! Get elemsd2 from feptr2
  CALL feptr2%GetGlobalElemShapeData2( &
    geofeptr=geofeptr2, elemsd=elemsd2, geoelemsd=geoelemsd2, &
    xij=elemCoord2, quad=quad, doNotInitQuad=math%yes)

  ! Get nodal value from obj2
  CALL obj2%Get(VALUE=nodalValue2, globalNode=fecon2(1:tfecon2), &
                islocal=math%yes, nrow=nodalValue2_i, ncol=nodalValue2_j, &
                storageFMT=TypeFieldOpt%storageFormatNodes)

  ! use elemsd2 to get the interpolation value
  CALL GetInterpolation_(obj=elemsd2, ans=nodalValue1, &
                         val=nodalValue2(1:nodalValue2_i, 1:nodalValue2_j), &
                         nrow=nodalValue1_i, ncol=nodalValue1_j, &
                         scale=math%one, addContribution=math%no)

  CALL fedof%GetConnectivity_(globalElement=iel, islocal=math%yes, &
                              ans=fecon, tsize=tfecon, opt="A")

  ! Set the values in obj
  CALL obj%Set(VALUE=nodalValue1(1:nodalValue1_i, 1:nodalValue1_j), &
               globalNode=fecon(1:tfecon), &
               islocal=math%yes, scale=math%one, &
               addContribution=math%no, &
               storageFMT=TypeFieldOpt%storageFormatNodes)
END DO cell_loop

CALL ElemShapeDataDeallocate(elemsd2)
CALL ElemShapeDataDeallocate(geoelemsd2)
CALL QuadraturePointDeallocate(quad)
IF (ALLOCATED(fecon)) DEALLOCATE (fecon)
IF (ALLOCATED(fecon2)) DEALLOCATE (fecon2)
IF (ALLOCATED(elemCoord2)) DEALLOCATE (elemCoord2)
IF (ALLOCATED(refElemCoord2)) DEALLOCATE (refElemCoord2)
IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(nodalValue1)) DEALLOCATE (nodalValue1)
IF (ALLOCATED(nodalValue2)) DEALLOCATE (nodalValue2)
NULLIFY (feptr, feptr2, geofeptr2, mesh, mesh2, fedof, fedof2, geofedof2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetLagrangeDOF

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE SetMethods

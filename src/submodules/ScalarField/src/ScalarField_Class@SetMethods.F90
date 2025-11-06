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

SUBMODULE(ScalarField_Class) SetMethods
USE GlobalData, ONLY: Constant, Space, Scalar
USE InputUtility, ONLY: Input
USE AbstractFE_Class, ONLY: AbstractFE_
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE RealVector_Method, ONLY: Set, Add
USE Display_Method, ONLY: ToString
USE ArangeUtility, ONLY: Arange
USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF
USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableConstant, &
                    TypeFEVariableSpace, &
                    QuadraturePoint_, &
                    ElemShapeData_
USE FEVariable_Method, ONLY: GET
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE ReallocateUtility, ONLY: Reallocate
USE StringUtility, ONLY: UpperCase
USE ReferenceElement_Method, ONLY: ReferenceElementInfo

IMPLICIT NONE

#ifdef USE_LIS
#include "lisf.h"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set1
CHARACTER(*), PARAMETER :: myName = "obj_Set1()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "ScalarField_::obj not initiated")
#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=1_I4B)
CALL obj%SetSingle(indx=indx, VALUE=VALUE, scale=scale, &
                   addContribution=addContribution)

END PROCEDURE obj_Set1

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set2
CALL obj%SetAll(VALUE=VALUE, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set2

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set3()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, "ScalarField_::obj not initiated")

isok = obj%fieldType .NE. TypeFieldOpt%constant
CALL AssertError1(isok, myName, "Not callable for Constant field")

isok = SIZE(VALUE) .GE. (obj%dof.tNodes.1_I4B)
CALL AssertError1(isok, myName, "Size of value is not enought")
#endif

s = GetNodeLoc(obj=obj%dof, idof=1_I4B)

CALL obj%SetMultiple( &
  VALUE=VALUE, scale=scale, addContribution=addContribution, &
  istart=s(1), iend=s(2), stride=s(3))

END PROCEDURE obj_Set3

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set4
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set4()"
REAL(DFP) :: value0(SIZE(globalNode))

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, "ScalarField_::obj not initiated")
isok = obj%fieldType .NE. TypeFieldOpt%constant
CALL AssertError1(isok, myName, "Not callable for Constant field")
#endif

#include "./localNodeError.F90"

value0 = VALUE

CALL obj%SetMultiple(indx=globalNode, VALUE=value0, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set4

!----------------------------------------------------------------------------
!                                                                        Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set5
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Set5()"

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, "ScalarField_::obj not initiated")

isok = obj%fieldType .NE. TypeFieldOpt%constant
CALL AssertError1(isok, myName, "Not callable for Constant field")

isok = SIZE(VALUE) .GE. SIZE(globalNode)
CALL AssertError1(isok, myName, "Size of value is not enought")

#endif

#include "./localNodeError.F90"

CALL obj%SetMultiple(indx=globalNode, VALUE=VALUE, scale=scale, &
                     addContribution=addContribution)

END PROCEDURE obj_Set5

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set6
CHARACTER(*), PARAMETER :: myName = "obj_Set6()"

SELECT CASE (VALUE%vartype)

CASE (Constant)

CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableConstant), &
        globalNode=globalNode, scale=scale, addContribution=addContribution, &
               islocal=islocal)

CASE (Space)

  CALL obj%Set(VALUE=GET(VALUE, TypeFEVariableScalar, TypeFEVariableSpace), &
        globalNode=globalNode, scale=scale, addContribution=addContribution, &
               islocal=islocal)

CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')
  RETURN
END SELECT

END PROCEDURE obj_Set6

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set7
CALL obj%Set(ivar=1_I4B, idof=1_I4B, VALUE=VALUE, ivar_value=1_I4B, &
             idof_value=1_I4B)
END PROCEDURE obj_Set7

!----------------------------------------------------------------------------
!                                                                       Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set8
CALL obj%Set(ivar=1_I4B, idof=1_I4B, VALUE=VALUE, ivar_value=1_I4B, &
             idof_value=1_I4B, scale=scale, addContribution=addContribution)
END PROCEDURE obj_Set8

!----------------------------------------------------------------------------
!                                                                      Set
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Set9
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Set9()"
#endif

INTEGER(I4B) :: s(3), p(3), ierr, tsize
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, "ScalarField_::obj not initiated")
CALL AssertError1(VALUE%isInitiated(), myName, &
                  "AbstractNodeField_::value not initiated")
#endif

s = GetNodeLoc(obj=obj%dof, idof=1_I4B)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

TYPE is (BlockNodeField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=ivar_value, &
                                             idof=idof_value))
  realvec => VALUE%GetPointer()

  CALL obj%SetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                  VALUE=realvec, scale=scale, addContribution=addContribution)

  realvec => NULL()

#if USE_LIS

TYPE IS (ScalarFieldLis_)

  p = GetNodeLoc(obj=VALUE%dof, idof=1)
  tsize = obj%dof.tNodes.1

  realvec => obj%GetPointer()
  CALL lis_vector_get_values_from_range(VALUE%lis_ptr, p(1), p(3), &
                                        tsize, realvec, ierr)
  realvec => NULL()

TYPE IS (STScalarFieldLis_)

  tsize = obj%dof.tNodes.1
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))

  realvec => obj%GetPointer()
  CALL lis_vector_get_values_from_range(VALUE%lis_ptr, p(1), p(3), &
                                        tsize, realvec, ierr)
  realvec => NULL()

TYPE IS (VectorFieldLis_)

  tsize = obj%dof.tNodes.1
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  realvec => obj%GetPointer()
  CALL lis_vector_get_values_from_range(VALUE%lis_ptr, p(1), p(3), &
                                        tsize, realvec, ierr)
  realvec => NULL()

#endif

CLASS DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found')
  RETURN

END SELECT

END PROCEDURE obj_Set9

!----------------------------------------------------------------------------
!                                                             SetByFunction
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetByFunction
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetByFunction()"
LOGICAL(LGT) :: isok
#endif

CLASS(AbstractMesh_), POINTER :: meshptr
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
INTEGER(I4B) :: telements, iel, maxNNS, maxGeoNNS, maxNips, tans, &
                xij_i, xij_j, tcon
TYPE(QuadraturePoint_) :: quad(8), facetQuad(8), cellQuad
TYPE(ElemShapeData_) :: cellElemsd, geoCellElemsd, geoElemsd(8), &
                        geoFacetElemsd(8), elemsd(8), facetElemsd(8)
REAL(DFP) :: args(4), times0
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
    ipiv=ipiv, funcValue=funcValue, temp=temp)

  ! obj, geofeptr, elemsd, geoelemsd, facetElemsd, geoFacetElemsd, &
  ! cellElemsd, geoCellElemsd, quad, facetQuad, cellQuad, xij, times, &
  ! func, ans, tsize, massMat, ipiv, funcValue, temp)
  CALL obj%fedof%GetConnectivity_(ans=con, tsize=tcon, opt="A", &
                                  globalElement=iel, islocal=.TRUE.)

  ! (obj, ans, tsize, opt, globalElement, islocal)
  CALL obj%Set(VALUE=ans(1:tans), globalNode=con(1:tcon), &
               islocal=.TRUE.)
END DO

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

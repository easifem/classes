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

SUBMODULE(ScalarField_Class) GetMethods
USE RealVector_Method, ONLY: GetValue_, Get, GetValue
USE ScalarField_Class, ONLY: ScalarField_
USE STScalarField_Class, ONLY: STScalarField_
USE VectorField_Class, ONLY: VectorField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_
USE ArangeUtility, ONLY: Arange
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE ReallocateUtility, ONLY: Reallocate
USE Display_Method, ONLY: ToString, Display
USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractFE_Class, ONLY: AbstractFE_

USE FEVariable_Method, ONLY: NodalVariable, &
                             QuadratureVariable, &
                             FEVariable_Set => Set
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Initiate => Initiate, &
                                  QuadraturePoint_Set => Set
USE BaseType, ONLY: TypeFEVariableScalar, &
                    TypeFEVariableSpace, &
                    QuadraturePoint_, ElemShapeData_
USE DOF_Method, ONLY: GetNodeLoc, &
                      OPERATOR(.tNodes.), &
                      GetIDOF
USE ElemshapeData_Method, ONLY: ElemshapeData_GetInterpolation => &
                                GetInterpolation

#ifdef DEBUG_VER
USE FEVariable_Method, ONLY: FEVariable_Display => Display
#endif

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#include "./localNodeError.F90"
CALL obj%GetSingle(VALUE=VALUE, indx=globalNode)
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
INTEGER(I4B) :: s(3)
s = GetNodeLoc(obj=obj%dof, idof=1)
CALL obj%GetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                     tsize=tsize)
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#include "./localNodeError.F90"
CALL obj%GetMultiple(VALUE=VALUE, indx=globalNode, tsize=tsize)
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
#include "./localNodeError.F90"

VALUE = NodalVariable( &
        Get(obj=obj%realVec, nodenum=globalNode, dataType=1.0_DFP), &
        TypeFEVariableScalar, TypeFEVariableSpace)
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CALL obj%Get(ivar=1, idof=1, VALUE=VALUE, ivar_value=1, idof_value=1)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CHARACTER(*), PARAMETER :: myName = "obj_Get6()"
INTEGER(I4B) :: tsize
INTEGER(I4B) :: s(3), p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated, myName, &
                  'ScalarFieldLis_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated, myName, &
                  'AbstractNodeField::value is not initiated')
#endif

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  s = GetNodeLoc(obj=obj%dof, idof=1)
  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (STScalarField_)

  s = GetNodeLoc(obj=obj%dof, idof=1)
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize, istart_value=p(1), iend_value=p(2), &
                       stride_value=p(3))
  realvec => NULL()

TYPE IS (VectorField_)

  s = GetNodeLoc(obj=obj%dof, idof=1)
  p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
                                             idof=idof_value))
  tsize = obj%dof.tNodes.1

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize, istart_value=p(1), iend_value=p(2), &
                       stride_value=p(3))
  realvec => NULL()

! TYPE IS (STVectorField_)
!
!   s = GetNodeLoc(obj=obj%dof, idof=1)
!   p = GetNodeLoc(obj=VALUE%dof, idof=GetIDOF(obj=VALUE%dof, ivar=1_I4B, &
!                                              idof=idof_value))
!   tsize = obj%dof.tNodes.1
!
!   realvec => VALUE%GetPointer()
!   CALL obj%GetMultiple(VALUE=realvec, istart=s(1), iend=s(2), stride=s(3), &
!                        tsize=tsize, istart_value=p(1), iend_value=p(2), &
!                        stride_value=p(3))
!   realvec => NULL()

TYPE IS (ScalarFieldLis_)

  CALL VALUE%Set(ivar=1_I4B, idof=1_I4B, VALUE=obj, idof_value=1_I4B, &
                 ivar_value=1_I4B)

TYPE IS (STScalarFieldLis_)

  CALL VALUE%Set(ivar=1_I4B, idof=idof_value, VALUE=obj, idof_value=1_I4B, &
                 ivar_value=1_I4B)

TYPE IS (VectorFieldLis_)

  CALL VALUE%Set(ivar=1_I4B, idof=idof_value, VALUE=obj, idof_value=1_I4B, &
                 ivar_value=1_I4B)

! TYPE IS (STVectorFieldLis_)
!
!   CALL VALUE%Set(ivar=1_I4B, idof=idof_value, VALUE=obj, idof_value=1_I4B, &
!                  ivar_value=1_I4B)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for type value')
  RETURN
END SELECT

END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                              GetFeVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
CALL obj%Get(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetPrefix
ans = myprefix
END PROCEDURE obj_GetPrefix

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
ans = obj%local_n
END PROCEDURE obj_Size

!----------------------------------------------------------------------------
!                                                              GetStorageFMT
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetStorageFMT
ans = MYSTORAGEFORMAT
END PROCEDURE obj_GetStorageFMT

!----------------------------------------------------------------------------
!                                                                  MeshField
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetMeshField
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetMeshField()"
#endif

LOGICAL(LGT) :: isMeshFieldInit
CHARACTER(:), ALLOCATABLE :: name, engine
INTEGER(I4B) :: spaceCompo, maxCon, tElements, iel, ii, maxNNE, &
                xij_i, xij_j, elemCoord_i, elemCoord_j, tsol, tSolCon, &
                refElemCoord_i, refElemCoord_j, maxFedofCon
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
TYPE(FEVariable_) :: fevar, sol_fevar
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: elemsd, geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :), sol(:), &
                          refElemCoord(:, :)
INTEGER(I4B), ALLOCATABLE :: solCon(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isMeshFieldInit = meshField%IsInitiated()
name = obj%GetName()
engine = obj%GetEngineName()
spaceCompo = 1

mesh => obj%fedof%GetMeshPointer()
maxNNE = mesh%GetMaxNNE()
! maxNNE is the maximum number of nodes in an element of a mesh
! It will be used for allocating geocon, refElemCoord, and elemCoord
tElements = mesh%GetTotalElements()
! tElements is the total number of elements in the mesh
maxFedofCon = obj%fedof%GetMaxTotalConnectivity()
! maxFedofCon is the maximum number of connectivity in the fedof
! It will be used to allocate sol and solCon

maxCon = 0
DO iel = 1, tElements
  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)
  ii = feptr%GetTotalInterpolationPoints(order=order, ipType=ipType)
  maxCon = MAX(maxCon, ii)
END DO
! maxCon is the maximum number of interpolation points in
! in the fedof, it will be used to allocate xij, quad, fevar

IF (.NOT. isMeshFieldInit) THEN
  CALL ScalarMeshFieldInitiate( &
    obj=meshField, name=name, fieldType=TypeFieldOpt%normal, &
    varType=TypeFieldOpt%space, engine=engine, defineOn=TypeFieldOpt%nodal, &
    nns=maxCon, mesh=mesh)
END IF

CALL Reallocate(elemCoord, 3, maxNNE)
CALL Reallocate(refElemCoord, 3, maxNNE)
CALL Reallocate(sol, maxFedofCon)
CALL Reallocate(solCon, maxFedofCon)
CALL Reallocate(xij, 4, maxCon)
CALL QuadraturePoint_Initiate(obj=quad, txi=3, tpoints=maxCon)

fevar = QuadratureVariable( &
        tsize=maxCon, rank=TypeFEVariableScalar, varType=TypeFEVariableSpace)

sol_fevar = NodalVariable(tsize=maxFedofCon, rank=TypeFEVariableScalar, &
                          varType=TypeFEVariableSpace)

DO iel = 1, tElements

  CALL obj%geofedof%SetFE(globalElement=iel, islocal=.TRUE.)
  CALL obj%fedof%SetFE(globalElement=iel, islocal=.TRUE.)

  feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)
  geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=.TRUE.)

  CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=refElemCoord_i, &
                             ncol=refElemCoord_j)

  CALL feptr%GetInterpolationPoints( &
    xij=refElemCoord, ans=xij, nrow=xij_i, ncol=xij_j, order=order, &
    ipType=ipType)

  CALL QuadraturePoint_Initiate(obj=quad, points=xij(1:4, 1:xij_j))

  CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

  CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

  CALL mesh%GetNodeCoord(globalElement=iel, nodeCoord=elemCoord, &
                         nrow=elemCoord_i, ncol=elemCoord_j, islocal=.TRUE.)

  ! This step is necessary to fix the orientation in Lagrange elements
  CALL feptr%GetGlobalElemShapeData( &
    elemsd=elemsd, geoelemsd=geoelemsd, &
    xij=elemCoord(1:elemCoord_i, 1:elemCoord_j))

  CALL obj%fedof%GetConnectivity_( &
    ans=solCon, tsize=tSolCon, globalElement=iel, islocal=.TRUE., opt='A')

  CALL obj%Get(VALUE=sol, tsize=tSol, globalNode=solCon(1:tSolCon), &
               islocal=.TRUE.)

  CALL FEVariable_Set( &
    obj=sol_fevar, val=sol(1:tSol), rank=TypeFEVariableScalar, &
    varType=TypeFEVariableSpace, scale=1.0_DFP, addContribution=.FALSE.)

  CALL ElemshapeData_GetInterpolation(obj=elemsd, ans=fevar, val=sol_fevar)

  CALL meshField%Insert(globalElement=iel, islocal=.TRUE., fevar=fevar)

END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetMeshField

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

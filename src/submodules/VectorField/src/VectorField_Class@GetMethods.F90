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

SUBMODULE(VectorField_Class) GetMethods
USE GlobalData, ONLY: NODES_FMT
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractFE_Class, ONLY: AbstractFE_
USE ElemshapeData_Method, ONLY: ElemshapeData_GetInterpolation => &
                                GetInterpolation
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Initiate => Initiate, &
                                  QuadraturePoint_Set => Set
USE MeshField_Class, ONLY: VectorMeshFieldInitiate
USE ScalarField_Class, ONLY: ScalarField_
USE ScalarFieldLis_Class, ONLY: ScalarFieldLis_

USE STScalarField_Class, ONLY: STScalarField_
USE STScalarFieldLis_Class, ONLY: STScalarFieldLis_

USE VectorField_Class, ONLY: VectorField_
USE VectorFieldLis_Class, ONLY: VectorFieldLis_

USE RealVector_Method, ONLY: GetValue_

USE ArangeUtility, ONLY: Arange

USE BaseType, ONLY: TypeFEVariableVector, TypeFEVariableSpace, &
                    QuadraturePoint_, ElemShapeData_

USE FEVariable_Method, ONLY: NodalVariable, &
                             QuadratureVariable, &
                             FEVariable_Set => Set

USE DOF_Method, ONLY: GetIDOF, &
                      OPERATOR(.tnodes.), &
                      GetNodeLoc, &
                      GetNodeLoc_

USE Display_Method, ONLY: ToString, Display

USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
LOGICAL(LGT) :: isok
#endif

LOGICAL(LGT) :: bool1, bool2
INTEGER(I4B) :: s(3), indx(obj%spaceCompo)

bool1 = PRESENT(globalNode)
bool2 = PRESENT(spaceCompo)

#ifdef DEBUG_VER

isok = .NOT. (bool1 .AND. bool2)

CALL AssertError1(isok, myName, &
                  "Both globalNode and spaceCompocannot be present")

isok = bool1 .OR. bool2
CALL AssertError1(isok, myName, &
                  "Either globalNode or spaceComposhould be present")

IF (bool1) THEN
  isok = SIZE(VALUE) .GE. obj%spaceCompo
  CALL AssertError1(isok, myName, "Size of value is not enough")
END IF

IF (bool2) THEN
  isok = SIZE(VALUE) .GE. (obj%dof.tNodes.1_I4B)
  CALL AssertError1(isok, myName, "Size of value is not enough")
END IF

#endif

! globalnode present
IF (bool1) THEN
  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=globalNode, &
                   ans=indx, tsize=tsize)

  CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)

  RETURN

END IF

!> Get all values ofspaceCompo
! IF (bool2) THEN

tsize = obj%dof.tNodes.1_I4B

s = GetNodeLoc(obj=obj%dof, idof=spaceCompo)

CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=VALUE, &
                     tsize=tsize)

! END IF

END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: s(3), jj, mynrow
INTEGER(I4B) :: indx(obj%spaceCompo)

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_:: obj is not initiated")

IF (storageFMT .EQ. MYSTORAGEFORMAT) THEN
  nrow = obj%dof.tNodes.1
  ncol = obj%spaceCompo

ELSE
  ncol = obj%dof.tNodes.1
  nrow = obj%spaceCompo
END IF

isok = SIZE(VALUE, 1) .GE. nrow
CALL AssertError1(isok, myName, "Number of rows in value is not enough")

isok = SIZE(VALUE, 2) .GE. ncol
CALL AssertError1(isok, myName, "Number of cols in not enough")
#endif

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idof=obj%idofs, &
                 VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=storageFMT)

  RETURN
END IF

IF (storageFMT .EQ. MYSTORAGEFORMAT) THEN
  nrow = obj%dof.tNodes.1
  ncol = obj%spaceCompo

  !$OMP PARALLEL DO PRIVATE(jj, mynrow, s)
  DO jj = 1, ncol
    s = GetNodeLoc(obj=obj%dof, idof=jj)

    CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), &
                         VALUE=VALUE(:, jj), tsize=mynrow)

  END DO
  !$OMP END PARALLEL DO

  RETURN
END IF

nrow = obj%spaceCompo
ncol = obj%dof.tNodes.1

!$OMP PARALLEL DO PRIVATE(jj, indx, mynrow)
DO jj = 1, ncol
  CALL GetNodeLoc_(obj=obj%dof, idof=obj%idofs, nodenum=jj, ans=indx, &
                   tsize=mynrow)
  CALL obj%GetMultiple(indx=indx, VALUE=VALUE(:, jj), tsize=nrow)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
LOGICAL(LGT) :: isok
#endif

CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
INTEGER(I4B) :: jj, mynrow

#include "./localNodeError.F90"

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_:: obj is not initiated")

IF (storageFMT .EQ. MYSTORAGEFORMAT) THEN
  nrow = SIZE(globalNode)
  ncol = obj%spaceCompo

ELSE
  ncol = SIZE(globalNode)
  nrow = obj%spaceCompo
END IF

isok = SIZE(VALUE, 1) .GE. nrow
CALL AssertError1(isok, myName, "Number of rows in value is not enough")

isok = SIZE(VALUE, 2) .GE. ncol
CALL AssertError1(isok, myName, "Number of cols in not enough")
#endif

IF (obj%engine%chars() .EQ. "NATIVE_SERIAL") THEN
  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, idof=obj%idofs, &
                 VALUE=VALUE, nrow=nrow, ncol=ncol, storageFMT=storageFMT, &
                 nodenum=globalNode)

  RETURN
END IF

IF (storageFMT .EQ. MYSTORAGEFORMAT) THEN
  nrow = SIZE(globalNode)
  ncol = obj%spaceCompo

  !$OMP PARALLEL DO PRIVATE(jj, mynrow)
  DO jj = 1, ncol
    CALL obj%Get(globalNode=globalNode, islocal=islocal, VALUE=VALUE(:, jj), &
                 tsize=mynrow, spaceCompo=jj)
  END DO
  !$OMP END PARALLEL DO

  RETURN
END IF

nrow = obj%spaceCompo
ncol = SIZE(globalNode)

!$OMP PARALLEL DO PRIVATE(jj, mynrow)
DO jj = 1, ncol
  CALL obj%Get(globalNode=globalNode(jj), VALUE=VALUE(:, jj), &
               tsize=mynrow)
END DO
!$OMP END PARALLEL DO

END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
CHARACTER(*), PARAMETER :: myName = "obj_Get4()"

#include "./localNodeError.F90"

IF (obj%engine%chars() .NE. "NATIVE_SERIAL") THEN

  CALL GetValue_(obj=obj%realVec, dofobj=obj%dof, ivar=1, &
                 idof=spaceCompo, VALUE=VALUE, tsize=tsize, &
                 nodenum=globalNode)

ELSE

  CALL me_if_not_native

END IF

CONTAINS

SUBROUTINE me_if_not_native
  INTEGER(I4B) :: indx(SIZE(globalNode))
  CALL GetNodeLoc_(obj=obj%dof, nodenum=globalNode, idof=spaceCompo, &
                   ans=indx, tsize=tsize)
  CALL obj%GetMultiple(indx=indx, VALUE=VALUE, tsize=tsize)
END SUBROUTINE

END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get5
CHARACTER(*), PARAMETER :: myName = "obj_Get5()"
INTEGER(I4B) :: indx

#ifdef DEBUG_VER
CALL AssertError1(obj%isInitiated(), myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(spaceCompo .LE. obj%spaceCompo, myName, &
            'given spaceCompo should be less than or equal to obj%spaceCompo')

#endif

#include "./localNodeError.F90"

indx = GetNodeLoc(obj=obj%dof, nodenum=globalNode, idof=spaceCompo)
CALL obj%GetSingle(VALUE=VALUE, indx=indx)
END PROCEDURE obj_Get5

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get6
CALL obj%GetFEVariable(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_Get6

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get7
CALL obj%Get(idof=spaceCompo, ivar=1_I4B, VALUE=VALUE, &
             idof_value=spaceCompo, ivar_value=1_I4B)
END PROCEDURE obj_Get7

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get8
#ifdef DEBUG_VER

CHARACTER(*), PARAMETER :: myName = "obj_Set9()"

CALL AssertError1(obj%isInitiated(), myName, &
                  'VectorField_::obj is not initiated')

CALL AssertError1(VALUE%isInitiated(), myName, &
                  'VectorField_::value is not initiated')
#endif

CALL VALUE%Copy(obj)
END PROCEDURE obj_Get8

!----------------------------------------------------------------------------
!                                                                        Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get9
CHARACTER(*), PARAMETER :: myName = "obj_Get9()"

INTEGER(I4B) :: s(3), tsize, p(3)
REAL(DFP), POINTER :: realvec(:)

#ifdef DEBUG_VER

CALL AssertError1(obj%isInitiated(), myName, &
                  "STScalarField_:: obj is not initiated")

CALL AssertError1(VALUE%isInitiated(), myName, &
                  "STScalarField_:: value is not initiated")

#endif

s = GetNodeLoc(obj=obj%dof, idof=idof)

SELECT TYPE (VALUE)

TYPE IS (ScalarField_)

  realvec => VALUE%GetPointer()
  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (STScalarField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => VALUE%GetPointer()

  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                       tsize=tsize)
  realvec => NULL()

TYPE IS (VectorField_)

  p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  realvec => VALUE%GetPointer()

  CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
                      istart_value=p(1), iend_value=p(2), stride_value=p(3), &
                       tsize=tsize)
  realvec => NULL()

! TYPE IS (STVectorField_)

  ! p = GetNodeLoc(obj=VALUE%dof, idof=idof_value)
  ! realvec => VALUE%GetPointer()
  !
  ! CALL obj%GetMultiple(istart=s(1), iend=s(2), stride=s(3), VALUE=realvec, &
  !                     istart_value=p(1), iend_value=p(2), stride_value=p(3), &
  !                      tsize=tsize)
  ! realvec => NULL()

TYPE IS (ScalarFieldLis_)
  CALL VALUE%Set(ivar=1, idof=1, VALUE=obj, ivar_value=ivar, idof_value=idof)

TYPE IS (STScalarFieldLis_)
  CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
                 idof_value=idof)

TYPE IS (VectorFieldLis_)
  CALL VALUE%Set(ivar=1, idof=idof_value, VALUE=obj, ivar_value=ivar, &
                 idof_value=idof)

CLASS DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTENRAL ERROR] :: No case found for the type of value')
  RETURN
END SELECT

END PROCEDURE obj_Get9

!----------------------------------------------------------------------------
!                                                           GetFEVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
REAL(DFP) :: v(obj%spaceCompo, SIZE(globalNode))
INTEGER(I4B) :: nrow, jj

!$OMP PARALLEL DO PRIVATE(jj)
DO jj = 1, SIZE(v, 2)
  CALL obj%Get(VALUE=v(:, jj), globalNode=globalNode(jj), tsize=nrow)
END DO
!$OMP END PARALLEL DO

VALUE = NodalVariable(v, TypeFEVariableVector, TypeFEVariableSpace)
END PROCEDURE obj_GetFEVariable

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
INTEGER(I4B) :: spaceCompo(1), maxCon, tElements, iel, ii, maxNNE, &
                xij_i, xij_j, elemCoord_i, elemCoord_j, tsol, tSolCon, &
                refElemCoord_i, refElemCoord_j, maxFedofCon, nrow, &
                ncol
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
TYPE(FEVariable_) :: fevar, sol_fevar
TYPE(QuadraturePoint_) :: quad
TYPE(ElemShapeData_) :: elemsd, geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :), sol(:, :), &
                          refElemCoord(:, :)
INTEGER(I4B), ALLOCATABLE :: solCon(:)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isMeshFieldInit = meshField%IsInitiated()
name = obj%GetName()
engine = obj%GetEngineName()

spaceCompo = obj%GetSpaceCompo(1)
IF (spaceCompo(1) .LT. 3) spaceCompo = 3

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
  CALL VectorMeshFieldInitiate( &
    obj=meshField, name=name, fieldType=TypeFieldOpt%normal, &
    varType=TypeFieldOpt%space, engine=engine, defineOn=TypeFieldOpt%nodal, &
    nns=maxCon, mesh=mesh, spaceCompo=spaceCompo(1))
END IF

CALL Reallocate(elemCoord, 3, maxNNE)
CALL Reallocate(refElemCoord, 3, maxNNE)
CALL Reallocate(sol, spaceCompo(1), maxFedofCon)
CALL Reallocate(solCon, maxFedofCon)
CALL Reallocate(xij, 4, maxCon)
CALL QuadraturePoint_Initiate(obj=quad, txi=3, tpoints=maxCon)

fevar = QuadratureVariable(nrow=spaceCompo(1), ncol=maxCon, &
                       rank=TypeFEVariableVector, varType=TypeFEVariableSpace)

sol_fevar = NodalVariable(nrow=spaceCompo(1), ncol=maxFedofCon, &
                          rank=TypeFEVariableVector, &
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

  ! WARN: even when force3D is true, nrow can be smaller than 3
  CALL obj%Get(VALUE=sol, nrow=nrow, ncol=ncol, &
               globalNode=solCon(1:tSolCon), storageFMT=NODES_FMT, &
               islocal=.TRUE., force3D=.TRUE.)

  ! Due to the above WARN, spaceCompo is used instead of nrow here
  CALL FEVariable_Set(obj=sol_fevar, val=sol(1:spaceCompo(1), 1:ncol), &
                     rank=TypeFEVariableVector, varType=TypeFEVariableSpace, &
                      scale=1.0_DFP, addContribution=.FALSE.)

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

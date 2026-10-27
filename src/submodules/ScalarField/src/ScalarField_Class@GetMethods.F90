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
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: ElemShapeData_
USE BaseType, ONLY: math => TypeMathOpt
USE BaseType, ONLY: QuadraturePoint_
USE BaseType, ONLY: TypeFEVariableScalar
USE BaseType, ONLY: TypeFEVariableSpace
USE BaseType, ONLY: TypeInterpolationOpt
USE Display_Method, ONLY: Display
USE DOF_Method, ONLY: GetIDOF
USE DOF_Method, ONLY: GetNodeLoc
USE DOF_Method, ONLY: OPERATOR(.tNodes.)
USE ElemshapeData_Method, ONLY: ElemshapeDataGetInterpolation => &
  GetInterpolation
USE ElemshapeData_Method, ONLY: ElemshapeDataDeallocate => &
  DEALLOCATE
USE FEVariable_Method, ONLY: FEVariableSet => Set
USE FEVariable_Method, ONLY: NodalVariable
USE FEVariable_Method, ONLY: QuadratureVariable
USE GlobalData, ONLY: INT8
USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
USE QuadraturePoint_Method, ONLY: QuadraturePointInitiate => Initiate
USE QuadraturePoint_Method, ONLY: QuadraturePointDeallocate => &
  DEALLOCATE
USE ReallocateUtility, ONLY: Reallocate
USE RealVector_Method, ONLY: GetValue_
USE RealVector_Method, ONLY: Get
USE RealVector_Method, ONLY: GetValue
USE ReferenceElement_Method, ONLY: GetVTKElementTypeFromTopology_
USE ReferenceElement_Method, ONLY: RefElementGetTotalNodes => GetTotalNodes

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
  "ScalarField_Class@GetMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Get1()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#include "./include/localNodeError.F90"
  CALL obj%GetSingle(VALUE=VALUE, indx=globalNode)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Get1

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Get2()"
#endif
  INTEGER(I4B) :: s(3)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  s = GetNodeLoc(obj=obj%dof, idof=1)
  CALL obj%GetMultiple(VALUE=VALUE, istart=s(1), iend=s(2), stride=s(3), &
                       tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Get2

!----------------------------------------------------------------------------
!                                                                   Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get3
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Get3()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#include "./include/localNodeError.F90"
  CALL obj%GetMultiple(VALUE=VALUE, indx=globalNode, tsize=tsize)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Get3

!----------------------------------------------------------------------------
!                                                                       Get
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Get4
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_Get4()"
#endif

  INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

#include "./include/localNodeError.F90"

#ifdef DEBUG_VER
  CALL AssertError1(VALUE%isInit, myName, &
                    "value is not initiated")
#endif

  CALL obj%Get(VALUE=VALUE%val, globalNode=globalNode, islocal=islocal, &
               tsize=tsize)
  VALUE%tshape = 1
  VALUE%s(1) = tsize
  VALUE%defineOn = TypeFieldOpt%nodal
  VALUE%varType = TypeFieldOpt%space
  VALUE%RANK = TypeFieldOpt%scalar
  VALUE%len = tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_Get4

!----------------------------------------------------------------------------
!                                                              GetFeVariable
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFEVariable
  CALL obj%Get(VALUE=VALUE, globalNode=globalNode, islocal=islocal)
END PROCEDURE obj_GetFEVariable

!----------------------------------------------------------------------------
!                                                                 GetPrefix
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Size
  ans = obj%local_n
END PROCEDURE obj_Size

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
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
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
    feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
    ii = feptr%GetTotalInterpolationPoints(order=order, ipType=ipType)
    maxCon = MAX(maxCon, ii)
  END DO
! maxCon is the maximum number of interpolation points in
! in the fedof, it will be used to allocate xij, quad, fevar

  IF (.NOT. isMeshFieldInit) THEN
    CALL ScalarMeshFieldInitiate(obj=meshField, name=name, &
                                 fieldType=TypeFieldOpt%normal, &
                                 varType=TypeFieldOpt%space, engine=engine, &
                                 defineOn=TypeFieldOpt%nodal, nns=maxCon, &
                                 mesh=mesh)
  END IF

  CALL Reallocate(elemCoord, 3, maxNNE)
  CALL Reallocate(refElemCoord, 3, maxNNE)
  CALL Reallocate(sol, maxFedofCon)
  CALL Reallocate(solCon, maxFedofCon)
  CALL Reallocate(xij, 4, maxCon)
  CALL QuadraturePointInitiate(obj=quad, txi=3, tpoints=maxCon)

  fevar = QuadratureVariable( &
                              tsize=maxCon, RANK=TypeFEVariableScalar, &
                              varType=TypeFEVariableSpace)

  sol_fevar = NodalVariable(tsize=maxFedofCon, RANK=TypeFEVariableScalar, &
                            varType=TypeFEVariableSpace)

  DO iel = 1, tElements

    CALL obj%geofedof%SetFE(globalElement=iel, islocal=math%yes)
    CALL obj%fedof%SetFE(globalElement=iel, islocal=math%yes)

    feptr => obj%fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
    geofeptr => obj%geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=refElemCoord_i, &
                               ncol=refElemCoord_j)

    CALL feptr%GetInterpolationPoints(xij=refElemCoord, ans=xij, nrow=xij_i, &
                                      ncol=xij_j, order=order, ipType=ipType)

    CALL QuadraturePointInitiate(obj=quad, points=xij(1:4, 1:xij_j))

    CALL feptr%GetLocalElemShapeData(elemsd=elemsd, quad=quad)

    CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

    CALL mesh%GetNodeCoord(globalElement=iel, nodeCoord=elemCoord, &
                           nrow=elemCoord_i, ncol=elemCoord_j, &
                           islocal=math%yes)

    ! This step is necessary to fix the orientation in Lagrange elements
    CALL feptr%GetGlobalElemShapeData(elemsd=elemsd, geoelemsd=geoelemsd, &
                                      xij=elemCoord(1:elemCoord_i, &
                                                    1:elemCoord_j))

    CALL obj%fedof%GetConnectivity_(ans=solCon, tsize=tSolCon, &
                                    globalElement=iel, islocal=math%yes, &
                                    opt='A')

    CALL obj%Get(VALUE=sol, tsize=tsol, globalNode=solCon(1:tSolCon), &
                 islocal=math%yes)

    CALL FEVariableSet(obj=sol_fevar, val=sol(1:tsol), &
                       RANK=TypeFEVariableScalar, &
                       varType=TypeFEVariableSpace, scale=math%one, &
                       addContribution=math%no)

    CALL ElemshapeDataGetInterpolation(obj=elemsd, ans=fevar, val=sol_fevar)

    CALL meshField%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)

  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif
END PROCEDURE obj_GetMeshField

!----------------------------------------------------------------------------
!                                                   GetVtkUnstructuredValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredValue1
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredValue1()"
#endif
  REAL(DFP), ALLOCATABLE :: refElemCoord(:, :), xij(:, :), &
    elemCoord(:, :), nodalValue(:)
  INTEGER(I4B), ALLOCATABLE :: vtkindx(:), fecon(:)
  INTEGER(I4B) :: tElements, maxNNE, maxCon, tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  tElements = obj%fedof%GetTotalCells()
  maxNNE = obj%geofedof%GetMaxTotalConnectivity()
  maxCon = obj%fedof%GetMaxTotalConnectivity()
  tsize = obj%fedof%GetSumElemTotalDOF()

  CALL Reallocate(VALUE, tsize)
  CALL Reallocate(nodalValue, maxCon)
  CALL Reallocate(fecon, maxCon)
  CALL Reallocate(vtkindx, maxCon)
  CALL Reallocate(xij, 4, maxCon)
  CALL Reallocate(refElemCoord, 3, maxNNE)
  CALL Reallocate(elemCoord, 3, maxNNE)

  CALL obj%GetVtkUnstructuredValue(VALUE=VALUE, &
                                   tsize=tsize, &
                                   refElemCoord=refElemCoord, xij=xij, &
                                   elemCoord=elemCoord, &
                                   nodalValue=nodalValue, &
                                   fecon=fecon, &
                                   vtkindx=vtkindx)

  IF (ALLOCATED(nodalValue)) DEALLOCATE(nodalValue)
  IF (ALLOCATED(fecon)) DEALLOCATE(fecon)
  IF (ALLOCATED(xij)) DEALLOCATE(xij)
  IF (ALLOCATED(refElemCoord)) DEALLOCATE(refElemCoord)
  IF (ALLOCATED(elemCoord)) DEALLOCATE(elemCoord)
  IF (ALLOCATED(vtkindx)) DEALLOCATE(vtkindx)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetVtkUnstructuredValue1

!----------------------------------------------------------------------------
!                                                   GetVtkUnstructuredValue
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetVtkUnstructuredValue2
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "obj_GetVtkUnstructuredValue2()"
#endif
  CLASS(FEDOF_), POINTER :: fedof
  CLASS(FEDOF_), POINTER :: geofedof
  CLASS(AbstractMesh_), POINTER :: mesh
  CLASS(AbstractFE_), POINTER :: feptr, geofeptr
  TYPE(QuadraturePoint_) :: quad
  TYPE(ElemShapeData_) :: elemsd, geoelemsd
  INTEGER(I4B) :: tElements, iel, ii, nn, &
    ip0, ic0, maxNNE, maxCon, cellOrder(3), &
    ipType(3), topo, nrow, ncol, xrow, xcol, crow, ccol, &
    nodalValue_size, tfecon
  INTEGER(INT8) :: vtktype
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[START] ')
#endif

  fedof => obj%fedof
  geofedof => obj%geofedof
  mesh => fedof%GetMeshPointer()
  tElements = mesh%GetTotalElements()
  maxNNE = geofedof%GetMaxTotalConnectivity()
  maxCon = fedof%GetMaxTotalConnectivity()
  ipType = TypeInterpolationOpt%equidistance

#ifdef DEBUG_VER
  CALL AssertError3(maxCon, SIZE(vtkindx), myName, &
                    "a=maxCon, b=size(vtkindx)")

  CALL AssertError3(maxCon, SIZE(nodalValue), myName, &
                    "a=maxCon, b=size(nodalValue)")

  CALL AssertError3(maxCon, SIZE(fecon), myName, &
                    "a=maxCon, b=size(fecon)")

  CALL AssertError3(math%four_i, SIZE(xij, 1), myName, &
                    "a=3, b=size(xij,1)")

  CALL AssertError3(maxCon, SIZE(xij, 2), myName, &
                    "a=maxCon, b=size(xij,2)")

  CALL AssertError3(math%three_i, SIZE(refElemCoord, 1), myName, &
                    "a=3, b=size(refElemCoord,1)")

  CALL AssertError3(maxNNE, SIZE(refElemCoord, 2), myName, &
                    "a=maxNNE, b=size(refElemCoord,2)")

  CALL AssertError3(math%three_i, SIZE(elemCoord, 1), myName, &
                    "a=3, b=size(elemCoord,1)")

  CALL AssertError3(maxNNE, SIZE(elemCoord, 2), myName, &
                    "a=maxNNE, b=size(elemCoord,2)")
#endif

  xij = math%zero
  refElemCoord = math%zero
  elemCoord = math%zero

! lazy compute and cached
  tsize = obj%fedof%GetSumElemTotalDOF()

!check size of value
#ifdef DEBUG_VER
  CALL AssertError3(tsize, SIZE(VALUE), myName, &
                    "a=tsize, b=size(value)")
#endif

! Second pass: points, cells, and values
  ip0 = 0
  ic0 = 0

  loop2: DO iel = 1, tElements

    ! Check if the element is active or not
    isok = mesh%isElementActive(globalElement=iel, islocal=math%yes)
    IF (.NOT. isok) CYCLE loop2

    ! get vtk element info
    CALL fedof%GetCellOrder(ans=cellOrder, tsize=ii, &
                            globalElement=iel, islocal=math%yes)
    cellOrder(2:3) = cellOrder(1)
    topo = mesh%GetElemTopology(globalElement=iel, islocal=math%yes)

    CALL GetVTKElementTypeFromTopology_(topoType=topo, &
                                        order=cellOrder(1), &
                                        vtk_type=vtktype, &
                                        nptrs=vtkindx, tsize=nn)

    ! get finite element from  fedof of obj
    CALL fedof%SetFE(globalElement=iel, islocal=math%yes)
    feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    ! get finite element from geofedof of obj
    CALL geofedof%SetFE(globalElement=iel, islocal=math%yes)
    geofeptr => geofedof%GetFEPointer(globalElement=iel, islocal=math%yes)

    ! get nodal coordinates of reference element
    CALL feptr%GetRefElemCoord(ans=refElemCoord, nrow=nrow, ncol=ncol)

    ! xij = 0.0_DFP
    CALL feptr%GetInterpolationPoints(xij=refElemCoord(1:nrow, 1:ncol), &
                                      ans=xij, nrow=xrow, ncol=xcol, &
                                      order=cellOrder, ipType=ipType)

#ifdef DEBUG_VER
    CALL AssertError2(xcol, nn, myName, &
                      'a=xcol (ncol from GetInterpolationPoints)' // &
                      'b=tpoints returned GetCellType.')
#endif

    CALL QuadraturePointInitiate(obj=quad, points=xij(1:4, 1:nn))

    CALL geofeptr%GetLocalElemShapeData(elemsd=geoelemsd, quad=quad)

    CALL mesh%GetNodeCoord(globalElement=iel, nodeCoord=elemCoord, &
                           nrow=crow, ncol=ccol, islocal=math%yes)

    ! Get element shape data with orientation fixed shape data
    ! This step is necessary to fix the orientation of shape functions
    ! TODO: We do not need global data, we just need to fix the orientation
    CALL feptr%GetGlobalElemShapeData2(elemsd=elemsd, &
                                       geoelemsd=geoelemsd, &
                                       xij=elemCoord(1:crow, 1:ccol), &
                                       quad=quad, geofeptr=geofeptr, &
                                       doNotInitQuad=math%yes)

    ! get the connectivity of obj
    CALL fedof%GetConnectivity_(ans=fecon, tsize=tfecon, opt="A", &
                                globalElement=iel, islocal=math%yes)

    ! TODO: Instead of calling  GetGlobalElemShapeData2 above
    !       we can call GetLocalElemShapeData
    !       then we will fix orientation of fecon
    !
    CALL obj%Get(VALUE=nodalValue, globalNode=fecon(1:tfecon), &
                 islocal=math%yes, tsize=nodalValue_size)

    ! check tfecon and nodalValue_size should be same

    VALUE(ip0 + 1:ip0 + nn) = MATMUL(nodalValue(1:nodalValue_size), &
                                     elemsd%N(1:nodalValue_size, 1:nn))

    ip0 = ip0 + nn
    ic0 = ic0 + nn
  END DO loop2

  NULLIFY(fedof, geofedof, mesh, feptr, geofeptr)
  CALL QuadraturePointDeallocate(quad)
  CALL ElemShapeDataDeallocate(elemsd)
  CALL ElemShapeDataDeallocate(geoelemsd)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName // '::' // myName // ' - ' // &
                          '[END] ')
#endif

END PROCEDURE obj_GetVtkUnstructuredValue2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE GetMethods

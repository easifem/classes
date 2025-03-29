! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(ScalarWave2DSTFEM_Class) AssembleMethods

USE BaseType, ONLY: TypeFEVariableSpace, &
                    TypeFEVariableVector, &
                    TypeFEVariableScalar

USE ProductUtility, ONLY: OuterProd_, OTimesTilda

USE GlobalData, ONLY: DOF_FMT, &
                      NODES_FMT, &
                      NONE

USE ReallocateUtility, ONLY: Reallocate

USE MassMatrix_Method
USE DiffusionMatrix_Method
USE BaseType, ONLY: FEVariableScalar_
USE NeumannBC_Class, ONLY: NeumannBC_
USE FEVariable_Method, ONLY: NodalVariable, &
                             FEVar_Display => Display, &
                             DEALLOCATE
USE ForceVector_Method, ONLY: ForceVector
USE Display_Method
USE ElemshapeData_InterpolMethods

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             AssembleTanmat
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleTanmat
CHARACTER(*), PARAMETER :: myName = "obj_AssembleTanmat()"
INTEGER(I4B) :: nrow, ncol, tsize, iel, nnt, nns
REAL(DFP) :: dt, dts, dts_by_2, scale
REAL(DFP) :: xij(3, obj%maxNNE)
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

dt = obj%algoParam%elemLength(obj%algoParam%currentTimeStep)
dts = dt * dt
dts_by_2 = dt * dt * half
nnt = obj%algoParam%nnt

CALL obj%massMat%set(VALUE=zero)
CALL obj%diffMat%set(VALUE=zero)
CALL obj%tanmat%set(VALUE=zero)
! CALL obj%dampMat%set(VALUE=zero)

DO iel = 1, obj%totalSpaceElements

  CALL obj%fedof%GetQuadraturePoints1(quad=obj%quadForSpace, &
                                      globalElement=iel, &
                                      quadratureType=obj%quadTypeForSpace, &
                                      order=2 * obj%spaceOrder, &
                                      islocal=.TRUE.)

  CALL obj%fedof%GetLocalElemShapeData(globalElement=iel, &
                                       elemsd=obj%elemsdForSpace, &
                                       quad=obj%quadForSpace, &
                                       islocal=.TRUE.)

  CALL obj%cellmesh%GetNodeCoord(nodecoord=xij, nrow=nrow, ncol=ncol, &
                                 globalElement=iel, islocal=.TRUE.)

  CALL obj%fedof%GetGlobalElemShapeData(globalElement=iel, &
                                        elemsd=obj%elemsdForSpace, &
                                        xij=xij, islocal=.TRUE.)

  CALL obj%fedof%GetConnectivity_(globalElement=iel, islocal=.TRUE., &
                                  ans=obj%cellcon, tsize=tsize, opt="A")

  obj%ke = zero
  obj%ms = zero
  CALL MassMatrix_(test=obj%elemsdForSpace, trial=obj%elemsdForSpace, &
                   ans=obj%ms, nrow=nrow, ncol=ncol, opt=spaceCompo)
  nns = nrow

  CALL obj%massMat%Set(globalNode=obj%cellcon(1:tsize), islocal=.TRUE., &
                       VALUE=obj%ms(1:nrow, 1:ncol), storageFMT=DOF_FMT, &
                       scale=one, addContribution=.TRUE.)

  CALL OTimesTilda(a=obj%algoParam%ct(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=zero, scale=one)

  CALL OTimesTilda(a=obj%algoParam%mtplus(1:nnt, 1:nnt), &
                   b=obj%ms(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=one)

  obj%ks = zero
  CALL DiffusionMatrix_(test=obj%elemsdForSpace, trial=obj%elemsdForSpace, &
                        k=obj%squareWaveSpeed, krank=TypeFEVariableScalar, &
                        ans=obj%ks, nrow=nrow, ncol=ncol, opt=spaceCompo)
  nns = nrow

  CALL obj%diffMat%Set(globalNode=obj%cellcon(1:tsize), islocal=.TRUE., &
                       VALUE=obj%ks(1:nrow, 1:ncol), storageFMT=DOF_FMT, &
                       scale=one, addContribution=.TRUE.)

  CALL OTimesTilda(a=obj%algoParam%kt_tilda(1:nnt, 1:nnt), &
                   b=obj%ks(1:nns, 1:nns), &
                   ans=obj%ke, nrow=nrow, ncol=ncol, &
                   anscoeff=one, scale=dts_by_2)

  CALL obj%tanmat%Set(globalNode=obj%cellcon(1:tsize), islocal=.TRUE., &
                      VALUE=obj%ke(1:nrow, 1:ncol), storageFMT=DOF_FMT, &
                      scale=one, addContribution=.TRUE.)

END DO

isok = ALLOCATED(obj%dbcIndx)
CALL obj%tanmat%ApplyDBC(dbcPtrs=obj%dbcIndx)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_AssembleTanmat

!----------------------------------------------------------------------------
!                                                                AssembleRHS
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleRHS

CHARACTER(*), PARAMETER :: myName = "obj_AssembleRHS()"
REAL(DFP) :: scale, dt, dt_by_2, minus_dt_by_2
INTEGER(I4B) :: nnt, ii, fid

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

dt = obj%algoParam%elemLength(obj%algoParam%currentTimeStep)
dt_by_2 = dt * half
minus_dt_by_2 = minus_one * dt_by_2

nnt = obj%algoParam%nnt

CALL obj%rhs%Set(VALUE=zero)

DO fid = 1, nnt
  DO ii = 1, nnt
    scale = obj%algoParam%mt(ii, fid) * dt_by_2
    CALL obj%rhs%Set(VALUE=obj%forceVecs(fid)%ptr, timeCompo=ii, &
                     addContribution=.TRUE., scale=scale)
  END DO
END DO

CALL obj%massMat%matVec(y=obj%tmpVecs(1)%ptr, x=obj%v0)
CALL obj%diffMat%matVec(y=obj%tmpVecs(2)%ptr, x=obj%u0)

DO ii = 1, nnt
  scale = obj%algoParam%timeShapeFuncBndy(ii, 1)
  CALL obj%rhs%Set(VALUE=obj%tmpVecs(1)%ptr, timeCompo=ii, &
                   addContribution=.TRUE., scale=scale)
  scale = obj%algoParam%tat(ii) * minus_dt_by_2
  CALL obj%rhs%Set(VALUE=obj%tmpVecs(2)%ptr, timeCompo=ii, &
                   addContribution=.TRUE., scale=scale)
END DO

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[END] ')

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleSurfaceSource
CHARACTER(*), PARAMETER :: myName = "obj_AssembleSurfaceSource()"
REAL(DFP) :: times(1)
REAL(DFP), ALLOCATABLE :: xij(:, :), fevec(:), forceVec(:)
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tnbc, ibc, tmesh, imesh, id, dim, nns, iel, &
                nrow, ncol, tsize, tel, con(obj%maxNNE), itime, nnt
INTEGER(I4B), ALLOCATABLE :: meshID(:)
CLASS(neumannBC_), POINTER :: nbc => NULL()
CLASS(AbstractMesh_), POINTER :: meshptr
TYPE(FEVariable_) :: forceVar

isok = ALLOCATED(obj%nbc)

IF (.NOT. isok) RETURN

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')

tnbc = SIZE(obj%nbc)
dim = nsd - 1_I4B
nnt = obj%algoParam%nnt

DO itime = 1, nnt
  times = obj%algoParam%tij(1, itime)
  DO ibc = 1, tnbc
    nbc => obj%nbc(ibc)%ptr
    isok = ASSOCIATED(nbc)
    IF (.NOT. isok) CYCLE

    CALL obj%tmpVecs(itime)%ptr%ApplyDirichletBC(dbc=nbc, &
                                                 times=times)
    nbc => NULL()
  END DO
END DO

DO ibc = 1, tnbc
  nbc => obj%nbc(ibc)%ptr
  isok = ASSOCIATED(nbc)
  CALL AssertError1(isok, myname, "NeumannBC is not associated")

  CALL nbc%GetParam(isSelectionByMeshID=isok)
  call AssertError1(isok, myname, "currently only SelectionByMeshID is supported")
  CALL nbc%GetParam(isNormal=isok)
  CALL AssertError1(.NOT. isok, myname, "Normal is not supported now")
  CALL nbc%GetParam(isTangent=isok)
  CALL AssertError1(.NOT. isok, myname, "Tangent is not supported now")

  meshID = nbc%GetMeshID(dim=dim)
  tmesh = SIZE(meshID)

  DO imesh = 1, tmesh
    id = meshID(imesh)
    meshptr => obj%boundaries(id)%ptr

    tel = meshptr%GetTotalElements()

    isok = meshptr%isEmpty()
    IF (isok) CYCLE

    CALL obj%fedofNBC%DEALLOCATE()
    CALL obj%fedofNBC%Initiate(baseContinuity=obj%baseContinuityForSpace, &
                            baseInterpolation=obj%baseInterpolationForSpace, &
                               order=obj%spaceOrder, &
                               mesh=meshptr, &
                               ipType=obj%ipTypeForSpace)

    nns = meshptr%GetMaxNNE()
    CALL Reallocate(xij, 3, nns)
    CALL Reallocate(fevec, nns)
    CALL Reallocate(forceVec, nns)

    DO iel = 1, tel

      CALL obj%fedofNBC%GetQuadraturePoints1(quad=obj%quadForSpaceBnd, &
                                             globalElement=iel, &
                                        quadratureType=obj%quadTypeForSpace, &
                                             order=2 * obj%spaceOrder, &
                                             islocal=.TRUE.)

      CALL obj%fedofNBC%GetLocalElemShapeData(globalElement=iel, &
                                              elemsd=obj%elemsdForSpaceBnd, &
                                              quad=obj%quadForSpaceBnd, &
                                              islocal=.TRUE.)

      CALL meshptr%GetNodeCoord(nodecoord=xij, nrow=nrow, ncol=ncol, &
                                globalElement=iel, islocal=.TRUE.)

      CALL obj%fedofNBC%GetGlobalElemShapeData(globalElement=iel, &
                                               elemsd=obj%elemsdForSpaceBnd, &
                                               xij=xij, islocal=.TRUE.)

      CALL obj%fedofNBC%GetConnectivity_(globalElement=iel, islocal=.TRUE., &
                                        ans=obj%cellcon, tsize=tsize, opt="A")
      con(1:tsize) = meshptr%GetGlobalNodeNumber(obj%cellcon(1:tsize))

      DO itime = 1, nnt
        CALL obj%tmpVecs(itime)%ptr%Get(VALUE=forceVec, &
                                        globalNode=con(1:tsize), &
                                        tsize=tsize, islocal=.TRUE.)

        forceVar = NodalVariable(val=forceVec, rank=TypeFEVariableScalar, &
                                 vartype=TypeFEVariableSpace)

        fevec = ForceVector(test=obj%elemsdforSpaceBnd, c=forceVar, &
                            crank=TypeFEVariableScalar)

        CALL obj%forceVecs(itime)%ptr%Set(globalNode=con(1:tsize), &
                                          VALUE=fevec, &
                                          scale=one, addContribution=.TRUE., &
                                          islocal=.TRUE.)
      END DO

    END DO
  END DO
END DO

NULLIFY (meshptr, nbc)

IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
IF (ALLOCATED(xij)) DEALLOCATE (xij)

CALL DEALLOCATE (forceVar)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
& '[END] ')

END PROCEDURE obj_AssembleSurfaceSource

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssemblePointSource
CHARACTER(*), PARAMETER :: myName = "obj_AssemblePointSource()"
CLASS(NeumannBC_), POINTER :: nbc
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tnbc, nbcNo, ii, jj, itime, nnt
INTEGER(I4B) :: nodeNum(obj%maxNodeNum_pointSource)
REAL(DFP) :: nodalValue(obj%maxNodeNum_pointSource, 1)
REAL(DFP) :: times(1)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')

isok = ALLOCATED(obj%nbc_point)

IF (.NOT. isok) RETURN

tnbc = SIZE(obj%nbc_point)
NULLIFY (nbc)

nnt = obj%algoParam%nnt

DO nbcNo = 1, tnbc
  nbc => NULL()
  nbc => obj%nbc_point(nbcNo)%ptr

  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE
  CALL nbc%GetParam(isSelectionByNodeNum=isok)
  IF (.NOT. isok) CYCLE

  DO itime = 1, nnt
    times = obj%algoParam%tij(1, itime)
    CALL nbc%Get(fedof=obj%fedof, nodeNum=nodeNum, nodalValue=nodalValue, &
                 times=times, nrow=ii, ncol=jj)

    CALL obj%forceVecs(itime)%ptr%Set(globalNode=nodeNum(1:ii), &
                                      VALUE=nodalValue(1:ii, 1), &
                                      scale=one, addContribution=.TRUE., &
                                      islocal=.TRUE.)
  END DO

END DO

NULLIFY (nbc)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')

END PROCEDURE obj_AssemblePointSource

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE AssembleMethods

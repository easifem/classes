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

SUBMODULE(ElastoDynamics2DFEM_Class) AssembleMethods

USE BaseType, ONLY: TypeFEVariableSpace, &
                    TypeFEVariableVector, &
                    TypeFEVariableScalar

USE ProductUtility, ONLY: OuterProd_, OTimesTilda

USE GlobalData, ONLY: DOF_FMT, &
                      NODES_FMT, &
                      NONE

USE ReallocateUtility, ONLY: Reallocate

USE MassMatrix_Method
USE StiffnessMatrix_Method
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
INTEGER(I4B) :: nrow, ncol, tsize, iel
REAL(DFP) :: dt, dts, scale
REAL(DFP) :: xij(3, obj%maxNNE)
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                                   '[START] ')

dt = obj%timeStepSize(obj%currentTimeStep)
dts = dt * dt

CALL obj%massMat%set(VALUE=zero)
CALL obj%stiffMat%set(VALUE=zero)
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

  obj%ms = zero
  CALL MassMatrix_(test=obj%elemsdForSpace, trial=obj%elemsdForSpace, &
                   ans=obj%ms, nrow=nrow, ncol=ncol, opt=spaceCompo)

  CALL obj%massMat%Set(globalNode=obj%cellcon(1:tsize), islocal=.TRUE., &
                       VALUE=obj%ms(1:nrow, 1:ncol), storageFMT=DOF_FMT, &
                       scale=one, addContribution=.TRUE.)

  obj%ks = zero
  CALL StiffnessMatrix_(test=obj%elemsdForSpace, trial=obj%elemsdForSpace, &
                        Cijkl=obj%Cijkl, ans=obj%ks, nrow=nrow, ncol=ncol)

  CALL obj%stiffMat%Set(globalNode=obj%cellcon(1:tsize), islocal=.TRUE., &
                        VALUE=obj%ks(1:nrow, 1:ncol), storageFMT=DOF_FMT, &
                        scale=one, addContribution=.TRUE.)

END DO

CALL obj%tanmat%set(VALUE=zero)

scale = obj%algoParam%tanmat(1)
CALL obj%tanmat%Set(VALUE=obj%massMat, scale=scale, &
                    addContribution=.TRUE.)

scale = obj%algoParam%tanmat(3) * dts
CALL obj%tanmat%Set(VALUE=obj%stiffMat, scale=scale, &
                    addContribution=.TRUE.)

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
REAL(DFP) :: scale, dt, dt2
LOGICAL(LGT) :: isok

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')

dt = obj%timeStepSize(obj%currentTimeStep)
dt2 = dt * dt

CALL obj%rhs%Set(VALUE=zero)

isok = .NOT. obj%algoParam%rhs_f1_zero
IF (isok) THEN
  scale = obj%algoParam%rhs_f1 * dt2
  CALL obj%rhs%AXPY(x=obj%force1, scale=scale)
END IF

isok = .NOT. obj%algoParam%rhs_f2_zero
IF (isok) THEN
  scale = obj%algoParam%rhs_f2 * dt2
  CALL obj%rhs%AXPY(x=obj%force2, scale=scale)
END IF

!----------------------------------------------------------------------------
! make M(a*U+b*V*dt+c*A*dt*dt)
!----------------------------------------------------------------------------

CALL obj%tmp1%Set(VALUE=zero)
scale = obj%algoParam%rhs_u1(1)
isok = .NOT. obj%algoParam%rhs_u1_zero(1)
IF (isok)  &
  & CALL obj%tmp1%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%rhs_v1(1) * dt
isok = .NOT. obj%algoParam%rhs_v1_zero(1)
IF (isok)  &
  & CALL obj%tmp1%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%rhs_a1(1) * dt2
isok = .NOT. obj%algoParam%rhs_a1_zero(1)
IF (isok)  &
  & CALL obj%tmp1%AXPY(x=obj%a0, scale=scale)

scale = one
CALL obj%massMat%matVec(y=obj%rhs, x=obj%tmp1, &
                        addContribution=.TRUE., scale=scale)

!----------------------------------------------------------------------------
! make C*dt*(a*U+b*V*dt+c*A*dt*dt)
!----------------------------------------------------------------------------

! CALL obj%tmp1%Set(VALUE=0.0_DFP)
!
! scale = obj%algoParam%rhs_u1(2)
! isok = .NOT. obj%algoParam%rhs_u1_zero(2)
! IF (isok) &
!   & CALL obj%tmp1%AXPY(x=obj%u0, scale=scale)
!
! scale = obj%algoParam%rhs_v1(2) * dt
! isok = .NOT. obj%algoParam%rhs_v1_zero(2)
! IF (isok) &
!  & CALL obj%tmp1%AXPY(x=obj%v0, scale=scale)
!
! scale = obj%algoParam%rhs_a1(2) * dt2
! isok = .NOT. obj%algoParam%rhs_a1_zero(2)
! IF (isok) &
!   & CALL obj%tmp1%AXPY(x=obj%a0, scale=scale)
!
! scale = dt
! CALL obj%dampingMat%matVec(y=obj%rhs, x=obj%tmp1,  &
!   & addContribution=.TRUE., scale=scale)

!----------------------------------------------------------------------------
! make K*dt+dt*(a*U+b*V*dt+c*A*dt*dt)
!----------------------------------------------------------------------------
CALL obj%tmp1%Set(VALUE=zero)

scale = obj%algoParam%rhs_u1(3)
isok = .NOT. obj%algoParam%rhs_u1_zero(3)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%u0, scale=scale)

scale = obj%algoParam%rhs_v1(3) * dt
isok = .NOT. obj%algoParam%rhs_v1_zero(3)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%v0, scale=scale)

scale = obj%algoParam%rhs_a1(3) * dt2
isok = .NOT. obj%algoParam%rhs_a1_zero(3)
IF (isok) &
  CALL obj%tmp1%AXPY(x=obj%a0, scale=scale)

scale = dt2
CALL obj%stiffMat%matVec(y=obj%rhs, x=obj%tmp1,  &
  & addContribution=.TRUE., scale=scale)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
& '[END] ')

END PROCEDURE obj_AssembleRHS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_AssembleSurfaceSource
CHARACTER(*), PARAMETER :: myName = "obj_AssembleSurfaceSource()"
REAL(DFP) :: times(1)
REAL(DFP), ALLOCATABLE :: xij(:, :), fevec(:, :), forceVec(:, :)
LOGICAL(LGT) :: isok
INTEGER(I4B) :: tnbc, ibc, idof, tmesh, imesh, id, dim, nns, iel, &
                nrow, ncol, tsize, tel, con(obj%maxNNE)
INTEGER(I4B), ALLOCATABLE :: meshID(:)
CLASS(neumannBC_), POINTER :: nbc => NULL()
CLASS(AbstractMesh_), POINTER :: meshptr
TYPE(FEVariable_) :: forceVar

isok = ALLOCATED(obj%nbc)

IF (.NOT. isok) RETURN

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')

CALL obj%force2%set(VALUE=zero)

tnbc = SIZE(obj%nbc)
dim = nsd - 1_I4B
times = obj%currentTime + obj%timeStepSize(obj%currentTimeStep)

DO ibc = 1, tnbc
  nbc => obj%nbc(ibc)%ptr
  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE

  ! store force in tmp1
  CALL obj%tmp1%ApplyDirichletBC(dbc=nbc, times=times)
  nbc => NULL()
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
  idof = nbc%GetDOFNo()
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
    CALL Reallocate(fevec, nsd, nns)
    CALL Reallocate(forceVec, nsd, nns)

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

      CALL obj%tmp1%Get(VALUE=forceVec, globalNode=con(1:tsize), &
                   nrow=nrow, ncol=ncol, islocal=.TRUE., storageFMT=NODES_FMT)

      forceVar = NodalVariable(val=forceVec, rank=TypeFEVariableVector, &
                               vartype=TypeFEVariableSpace)

      fevec = ForceVector(test=obj%elemsdforSpaceBnd, c=forceVar, &
                          crank=TypeFEVariableVector)

      CALL obj%force2%Set(globalNode=con(1:tsize), VALUE=fevec, &
                          scale=one, addContribution=.TRUE., &
                          islocal=.TRUE., storageFMT=NODES_FMT)

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
INTEGER(I4B) :: tnbc, nbcNo, ii, jj, idof
INTEGER(I4B) :: nodeNum(obj%maxNodeNum_pointSource)
REAL(DFP) :: nodalValue(obj%maxNodeNum_pointSource, 1)
REAL(DFP) :: times(1)

IF (debug) CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')

isok = ALLOCATED(obj%nbc_point)

IF (.NOT. isok) RETURN

tnbc = SIZE(obj%nbc_point)
NULLIFY (nbc)

times = obj%currentTime + obj%timeStepSize(obj%currentTimeStep)

DO nbcNo = 1, tnbc
  nbc => NULL()
  nbc => obj%nbc_point(nbcNo)%ptr
  ! inclined point force is not supported
  idof = nbc%GetDOFNo()

  isok = ASSOCIATED(nbc)
  IF (.NOT. isok) CYCLE
  CALL nbc%GetParam(isSelectionByNodeNum=isok)
  IF (.NOT. isok) CYCLE
  ! call nbc%boundary%GetNodeNum(ans=nodeNum, tsize=tsize)

  CALL nbc%Get(fedof=obj%fedof, nodeNum=nodeNum, nodalValue=nodalValue, &
               times=times, nrow=ii, ncol=jj)

  CALL obj%force2%Set(globalNode=nodeNum(1:ii), VALUE=nodalValue(1:ii, 1), &
                      spaceCompo=idof, &
                      scale=one, addContribution=.TRUE., &
                      islocal=.TRUE.)
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

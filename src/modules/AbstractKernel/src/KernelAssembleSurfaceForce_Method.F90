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

MODULE KernelAssembleSurfaceForce_Method
USE GlobalData
USE Field
USE BaseMethod
USE BaseType
USE FieldFactory
USE Domain_Class
USE Mesh_Class
USE FiniteElement_Class
USE ExceptionHandler_Class, ONLY: e
USE AbstractKernelParam, ONLY: KernelProblemType
USE UserFunction_Class
USE NeumannBC_Class
IMPLICIT NONE

PRIVATE
PUBLIC :: KernelAssembleSurfaceForce

CHARACTER(*), PARAMETER :: modName = "KernelAssembleSurfaceForce_Method"

INTERFACE KernelAssembleSurfaceForce
  MODULE PROCEDURE KernelAssembleSurfaceForce1
  ! MODULE PROCEDURE KernelAssembleSurfaceForce2
END INTERFACE KernelAssembleSurfaceForce

CONTAINS

!----------------------------------------------------------------------------
!                                                KernelAssembleSurfaceForce
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleSurfaceForce1(rhs, dom, nbcPtrs, func, fe,  &
  & linFE, spaceElemSD, linSpaceElemSD, reset, scale, times)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbcPtrs(:)
  CLASS(UserFunction_), INTENT(INOUT) :: func
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: fe(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleSurfaceForce1()"
  TYPE(FEVariable_) :: forceVar
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(NeumannBC_), POINTER :: nbc
  LOGICAL(LGT) :: problem
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel, tnbc, nbcNo, idof, jd, returnType
  INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshID(:)
  REAL(DFP), ALLOCATABLE :: fevec(:), xij(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  returnType = func%GetReturnType()
  problem = returnType .NE. Scalar

  IF (problem) THEN
    CALL e%RaiseError(modName//'::'//myName//' - '// &
      & '[INTERNAL ERROR] :: The return type of func should be a vector.')
  END IF

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  tnbc = SIZE(nbcPtrs)
  nsd = dom%GetNSD()
  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE, nbc)

  DO nbcNo = 1, tnbc
    nbc => nbcPtrs(nbcNo)%ptr
    problem = .NOT. ASSOCIATED(nbc)
    IF (problem) CYCLE

    CALL nbc%boundary%GetParam(isSelectionByMeshID=problem)

    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: Currently, found isSelectionByMeshID false.')
      CYCLE
    END IF

    meshID = nbc%GetMeshID(dim=nsd - 1_I4B)
    idof = nbc%GetDOFNo()
    tmesh = SIZE(meshID)

    DO id = 1, tmesh
      jd = meshID(id)
      meshptr => dom%GetMeshPointer(dim=nsd - 1_I4B, entityNum=jd)

      problem = meshptr%isEmpty()
      IF (problem) CYCLE

      spaceFE => fe(jd)%ptr
      linSpaceFE => linFE(jd)%ptr

      elemsd = spaceElemSD(jd)
      linElemSD = linSpaceElemSD(jd)

      refelem => meshptr%GetRefElemPointer()
      nns = (.nne.refelem)

      CALL Reallocate(xij, nsd, nns)
      CALL Reallocate(fevec, nns)

      DO iel = meshptr%minElemNum, meshptr%maxElemNum

        problem = .NOT. meshptr%IsElementPresent(iel)
        IF (problem) CYCLE

        nptrs = meshptr%GetConnectivity(iel)
        CALL dom%GetNodeCoord(nodeCoord=xij, globalNode=nptrs)

        CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
          & geoElemSD=linElemSD)

        CALL func%Get(fevar=forceVar, xij=xij, times=times)

        fevec = ForceVector(test=elemsd, c=forceVar,  &
          & crank=TypeFEVariableScalar)

        CALL rhs%Set(globalNode=nptrs, VALUE=fevec, spacecompo=idof, &
          & scale=scale, addContribution=.TRUE.)
      END DO
    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE, nbc)

  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)

  CALL DEALLOCATE (forceVar)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleSurfaceForce1

!----------------------------------------------------------------------------
!                                                 KernelAssembleSurfaceForce
!----------------------------------------------------------------------------

! SUBROUTINE KernelAssembleSurfaceForce2(rhs, dom, bodyVec, fe,  &
!   & linFE, spaceElemSD, linSpaceElemSD, reset, scale)
!   CLASS(VectorField_), INTENT(INOUT) :: rhs
!   CLASS(Domain_), INTENT(INOUT) :: dom
!   CLASS(VectorField_), INTENT(INOUT) :: bodyVec
!   TYPE(FiniteElementPointer_), INTENT(INOUT) :: fe(:)
!   TYPE(FiniteElementPointer_), INTENT(INOUT) :: linFE(:)
!   TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
!   TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
!   LOGICAL(LGT), INTENT(IN) :: reset
!   REAL(DFP), INTENT(IN) :: scale
!
!   ! internal variables
!   CHARACTER(*), PARAMETER :: myName = "KernelAssembleSurfaceForce2()"
!   LOGICAL(LGT) :: problem
!   INTEGER(I4B) :: tmesh, nsd, id, nns, iel
!   INTEGER(I4B), ALLOCATABLE :: nptrs(:)
!   REAL(DFP), ALLOCATABLE :: fevec(:, :), xij(:, :), aSurfaceVec(:, :)
!   TYPE(FEVariable_) :: bodyvar
!   TYPE(ElemShapeData_) :: elemsd, linElemSD
!   CLASS(Mesh_), POINTER :: meshptr
!   CLASS(ReferenceElement_), POINTER :: refelem
!   CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!     & '[START] ')
! #endif DEBUG_VER
!
!   IF (reset) CALL rhs%Set(VALUE=0.0_DFP)
!
!   nsd = dom%GetNSD()
!   tmesh = dom%GetTotalMesh(dim=nsd)
!   NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)
!
!   DO id = 1, tmesh
!     meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
!     problem = meshptr%isEmpty()
!     IF (problem) CYCLE
!
!     spaceFE => fe(id)%ptr
!     linSpaceFE => linFE(id)%ptr
!
!     elemsd = spaceElemSD(id)
!     linElemSD = linSpaceElemSD(id)
!
!     refelem => meshptr%GetRefElemPointer()
!     nns = (.nne.refelem)
!
!     CALL Reallocate(xij, nsd, nns)
!     CALL Reallocate(fevec, nsd, nns)
!
!     DO iel = meshptr%minElemNum, meshptr%maxElemNum
!
!       problem = meshptr%isElementPresent(iel)
!       IF (problem) CYCLE
!
!       nptrs = meshptr%GetConnectivity(iel)
!
!       CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)
!
!       CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
!         & geoElemSD=linElemSD)
!
!       CALL bodyVec%Get(VALUE=aSurfaceVec, globalNode=nptrs)
!       bodyvar = NodalVariable(val=aSurfaceVec, rank=TypeFEVariableVector,  &
!         & vartype=TypeFEVariableSpace)
!
!       fevec = ForceVector(test=elemsd, c=bodyvar,  &
!         & crank=TypeFEVariableVector)
!
!       CALL rhs%Set(globalNode=nptrs, scale=scale, addContribution=.TRUE.,  &
!         & VALUE=fevec)
!
!     END DO
!
!   END DO
!
!   NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)
!   IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
!   IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
!   IF (ALLOCATED(aSurfaceVec)) DEALLOCATE (aSurfaceVec)
!   CALL DEALLOCATE (bodyvar)
!   CALL DEALLOCATE (elemsd)
!   CALL DEALLOCATE (linElemSD)
!
! #ifdef DEBUG_VER
!   CALL e%RaiseInformation(modName//'::'//myName//' - '// &
!     & '[END] ')
! #endif DEBUG_VER
!
! END SUBROUTINE KernelAssembleSurfaceForce2

END MODULE KernelAssembleSurfaceForce_Method

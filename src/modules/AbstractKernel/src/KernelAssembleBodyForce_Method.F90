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

MODULE KernelAssembleBodyForce_Method
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
IMPLICIT NONE

PRIVATE
PUBLIC :: KernelAssembleBodyForce

CHARACTER(*), PARAMETER :: modName = "KernelAssembleBodyForce_Method"

INTERFACE KernelAssembleBodyForce
  MODULE PROCEDURE KernelAssembleBodyForce1
  MODULE PROCEDURE KernelAssembleBodyForce2
END INTERFACE KernelAssembleBodyForce

CONTAINS

!----------------------------------------------------------------------------
!                                                   KernelAssembleBodyForce
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleBodyForce1(rhs, dom, func, cellFE,  &
  & linCellFE, spaceElemSD, linSpaceElemSD, reset, scale, times)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  CLASS(Domain_), INTENT(INOUT) :: dom
  CLASS(UserFunction_), INTENT(INOUT) :: func
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale
  REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleBodyForce1()"
  LOGICAL(LGT) :: problem, isok
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  REAL(DFP), ALLOCATABLE :: fevec(:, :), xij(:, :)
  TYPE(FEVariable_) :: forceVar
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.nne.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(fevec, nsd, nns)

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=linElemSD)

      CALL func%Get(fevar=forceVar, xij=xij, times=times)

      fevec = ForceVector(test=elemsd, c=forceVar,  &
        & crank=TypeFEVariableVector)

      CALL rhs%Set(globalNode=nptrs, scale=scale,  &
        & addContribution=.TRUE., VALUE=fevec)

    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  CALL DEALLOCATE (forceVar)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleBodyForce1

!----------------------------------------------------------------------------
!                                                   KernelAssembleBodyForce
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleBodyForce2(rhs, dom, bodyVec, cellFE,  &
  & linCellFE, spaceElemSD, linSpaceElemSD, reset, scale)
  CLASS(VectorField_), INTENT(INOUT) :: rhs
  CLASS(Domain_), INTENT(INOUT) :: dom
  CLASS(VectorField_), INTENT(INOUT) :: bodyVec
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset
  REAL(DFP), INTENT(IN) :: scale

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleBodyForce2()"
  LOGICAL(LGT) :: problem, isok
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  REAL(DFP), ALLOCATABLE :: fevec(:, :), xij(:, :), forceVec(:, :)
  TYPE(FEVariable_) :: forceVar
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL rhs%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.nne.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(fevec, nsd, nns)

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=linElemSD)

      CALL bodyVec%Get(VALUE=forceVec, globalNode=nptrs)
      forceVar = NodalVariable(val=forceVec, rank=TypeFEVariableVector,  &
        & vartype=TypeFEVariableSpace)

      fevec = ForceVector(test=elemsd, c=forceVar,  &
        & crank=TypeFEVariableVector)

      CALL rhs%Set(globalNode=nptrs, scale=scale,  &
        & addContribution=.TRUE., VALUE=fevec)

    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, linSpaceFE)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  IF (ALLOCATED(forceVec)) DEALLOCATE (forceVec)
  CALL DEALLOCATE (forceVar)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleBodyForce2

END MODULE KernelAssembleBodyForce_Method

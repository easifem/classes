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

MODULE KernelAssembleMassMatrix_Method
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
IMPLICIT NONE
PRIVATE

PUBLIC :: KernelAssembleMassMatrix

INTERFACE KernelAssembleMassMatrix
  MODULE PROCEDURE KernelAssembleMassMatrix1
END INTERFACE KernelAssembleMassMatrix

CHARACTER(*), PARAMETER :: modName = "KernelAssembleMassMatrix_Method"

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleMassMatrix1(mat, massDensity, dom, cellFE,  &
  & linCellFE, spaceElemSD, linSpaceElemSD, problemType, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  INTEGER(I4B), INTENT(IN) :: problemType
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleMassMatrix1()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  TYPE(FEVariable_) :: fevar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: rhoField
  CLASS(ReferenceElement_), POINTER :: refelem
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  tdof = 1
  IF (problemType .EQ. KernelProblemType%vector) tdof = nsd

  NULLIFY (meshptr, spaceFE, rhoField, refelem, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    telems = meshptr%GetTotalElements()
    IF (telems .EQ. 0_I4B) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    rhoField => massDensity(id)%ptr

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      IF (.NOT. meshptr%IsElementPresent(iel)) CYCLE

      CALL rhoField%Get(fevar=fevar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=linElemSD)

      Mmat = MassMatrix(test=elemsd, trial=elemsd, opt=tdof,  &
        & rho=fevar, rhorank=TypeFEVariableScalar)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)
  CALL DEALLOCATE (fevar)

  NULLIFY (meshptr, spaceFE, rhoField, refelem, linSpaceFE)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleMassMatrix1

END MODULE KernelAssembleMassMatrix_Method

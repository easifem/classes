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

MODULE KernelAssembleDiffusionMatrix_Method
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

PUBLIC :: KernelAssembleDiffusionMatrix

CHARACTER(*), PARAMETER :: modName = "KernelAssembleDiffusionMatrix_Method"

INTERFACE KernelAssembleDiffusionMatrix
  MODULE PROCEDURE KernelAssembleIsoDiffMat
END INTERFACE KernelAssembleDiffusionMatrix

! TODO:  add some other assemble method e.g. anisotropic

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                   KernelAssembleIsotropicDiffusionMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleIsoDiffMat(mat, coefficient, dom, cellFE, &
  & linCellFE, spaceElemSD, linSpaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: coefficient(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: linCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: linSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleIsoDiffMat()"
  INTEGER(I4B) :: id, tmesh, nsd, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, linElemSD
  TYPE(FEVariable_) :: coefVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(AbstractScalarMeshField_), POINTER :: coefficientField
  REAL(DFP), ALLOCATABLE :: Mmat(:, :), xij(:, :)
  INTEGER(I4B), ALLOCATABLE :: nptrs(:)
  LOGICAL(LGT) :: isok, problem

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)
  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)
  tdof = 1_I4B

  NULLIFY (meshptr, spaceFE, linSpaceFE, coefficientField, refelem)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)

    isok = ASSOCIATED(meshptr)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: Null mesh found at id = '//tostring(id))
      RETURN
    END IF

    problem = meshptr%isEmpty()
    IF (problem) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => linCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    linElemSD = linSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    coefficientField => coefficient(id)%ptr

    isok = ASSOCIATED(coefficientField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: coefficient('//tostring(id)//') is NULL.')
      RETURN
    END IF

    DO iel = meshptr%minElemNum, meshptr%maxElemNum
      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      CALL coefficientField%Get(fevar=coefVar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      ! CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
      !   & geoElemSD=linElemSD)
      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij)

      Mmat = DiffusionMatrix(test=elemsd, trial=elemsd, k=coefVar,  &
             & krank=TypeFEVariableScalar)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  NULLIFY (meshptr, spaceFE, coefficientField, refelem, linSpaceFE)

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (linElemSD)
  CALL DEALLOCATE (coefVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleIsoDiffMat

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE KernelAssembleDiffusionMatrix_Method

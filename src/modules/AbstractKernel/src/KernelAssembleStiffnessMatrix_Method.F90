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

MODULE KernelAssembleStiffnessMatrix_Method
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

PUBLIC :: KernelAssembleStiffnessMatrix

CHARACTER(*), PARAMETER :: modName = "KernelAssembleStiffnessMatrix_Method"

INTERFACE KernelAssembleStiffnessMatrix
  MODULE PROCEDURE KernelAssembleIsoStiffMat
  MODULE PROCEDURE KernelAssembleCijklStiffMat
END INTERFACE KernelAssembleStiffnessMatrix

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

!----------------------------------------------------------------------------
!                                   KernelAssembleIsotropicStiffnessMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleIsoStiffMat(mat, youngsModulus, shearModulus,  &
  & dom, cellFE, geoCellFE, spaceElemSD, geoSpaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: youngsModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: shearModulus(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: geoCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: geoSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleIsoStiffMat()"
  INTEGER(I4B) :: id, tmesh, nsd, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, geoElemSD
  TYPE(FEVariable_) :: muVar, youngsVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractScalarMeshField_), POINTER :: youngsModulusField,  &
    & shearModulusField
  CLASS(ReferenceElement_), POINTER :: refelem
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
  tdof = nsd

  NULLIFY (meshptr, spaceFE, linSpaceFE, youngsModulusField,  &
    & shearModulusField, refelem)

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
    linSpaceFE => geoCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    geoElemSD = geoSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    youngsModulusField => youngsModulus(id)%ptr
    shearModulusField => shearModulus(id)%ptr

#ifdef DEBUG_VER
    isok = ASSOCIATED(youngsModulusField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: youngsModulus('//tostring(id)//') is NULL.')
      RETURN
    END IF

    isok = ASSOCIATED(shearModulusField)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: shearModulus('//tostring(id)//') is NULL.')
      RETURN
    END IF
#endif

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      isok = meshptr%isElementPresent(iel)
      IF (.NOT. isok) CYCLE

      CALL youngsModulusField%Get(fevar=youngsVar, globalElement=iel)

      CALL shearModulusField%Get(fevar=muVar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=geoElemSD)

      Mmat = StiffnessMatrix(test=elemsd, trial=elemsd, &
        & lambda=youngsVar, mu=muVar, isLambdaYoungsModulus=.TRUE.)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  NULLIFY (meshptr, spaceFE, youngsModulusField, shearModulusField, refelem, linSpaceFE)

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (geoElemSD)
  CALL DEALLOCATE (youngsVar)
  CALL DEALLOCATE (muVar)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleIsoStiffMat

!----------------------------------------------------------------------------
!                                             KernelAssembleStiffnessMatrix
!----------------------------------------------------------------------------

SUBROUTINE KernelAssembleCijklStiffMat(mat, Cijkl, dom, cellFE,  &
  & geoCellFE, spaceElemSD, geoSpaceElemSD, reset)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
  CLASS(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: Cijkl(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: cellFE(:)
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: geoCellFE(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  TYPE(ElemShapeData_), INTENT(INOUT) :: geoSpaceElemSD(:)
  LOGICAL(LGT), INTENT(IN) :: reset

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleCijklStiffMat()"
  INTEGER(I4B) :: id, tmesh, nsd, telems, nns, tdof, iel
  TYPE(ElemShapeData_) :: elemsd, geoElemSD
  TYPE(FEVariable_) :: CijklVar
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(FiniteElement_), POINTER :: spaceFE, linSpaceFE
  CLASS(AbstractTensorMeshField_), POINTER :: CijklField
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
  tdof = nsd

  NULLIFY (meshptr, spaceFE, CijklField, refelem, linSpaceFE)

  DO id = 1, tmesh
    meshptr => dom%GetMeshPointer(dim=nsd, entityNum=id)
    telems = meshptr%GetTotalElements()
    IF (telems .EQ. 0_I4B) CYCLE

    spaceFE => cellFE(id)%ptr
    linSpaceFE => geoCellFE(id)%ptr

    elemsd = spaceElemSD(id)
    geoElemSD = geoSpaceElemSD(id)

    refelem => meshptr%GetRefElemPointer()
    nns = (.NNE.refelem)

    CALL Reallocate(xij, nsd, nns)
    CALL Reallocate(Mmat, tdof * nns, tdof * nns)

    CijklField => Cijkl(id)%ptr

    DO iel = meshptr%minElemNum, meshptr%maxElemNum

      IF (.NOT. meshptr%IsElementPresent(iel)) CYCLE

      CALL CijklField%Get(fevar=CijklVar, globalElement=iel)

      nptrs = meshptr%GetConnectivity(iel)

      CALL dom%GetNodeCoord(globalNode=nptrs, nodeCoord=xij)

      CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij,  &
        & geoElemSD=geoElemSD)

      Mmat = StiffnessMatrix(test=elemsd, trial=elemsd, Cijkl=CijklVar)

      CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
        & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

    END DO
  END DO

  IF (ALLOCATED(Mmat)) DEALLOCATE (Mmat)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)
  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (geoElemSD)
  CALL DEALLOCATE (CijklVar)
  NULLIFY (meshptr, spaceFE, CijklField, refelem, linSpaceFE)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelAssembleCijklStiffMat

END MODULE KernelAssembleStiffnessMatrix_Method

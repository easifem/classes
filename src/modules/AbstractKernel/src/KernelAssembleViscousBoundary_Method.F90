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

MODULE KernelAssembleViscousBoundary_Method
USE GlobalData
USE Field
USE BaseMethod
USE BaseType
USE FieldFactory
USE Domain_Class
USE Mesh_Class
USE FiniteElement_Class
USE ExceptionHandler_Class, ONLY: e
USE UserFunction_Class
USE NeumannBC_Class
IMPLICIT NONE

PRIVATE
PUBLIC :: KernelAssembleViscousBoundary

CHARACTER(*), PARAMETER :: modName = "KernelAssembleViscousBoundary_Method"

INTERFACE KernelAssembleViscousBoundary
  MODULE PROCEDURE KernelAssembleViscousBoundary1
END INTERFACE KernelAssembleViscousBoundary

CONTAINS

!----------------------------------------------------------------------------
!                                              KernelAssembleViscousBoundary
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-01-15
! summary:  KernelAssembleViscousBoundary1

SUBROUTINE KernelAssembleViscousBoundary1(mat, youngsModulus, shearModulus, &
  & massDensity, dom, nbcPtrs, fe, geoFE, spaceElemSD, geoSpaceElemSD,  &
  & reset, scale)
  CLASS(MatrixField_), INTENT(INOUT) :: mat
    !! rhs to assemble
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: youngsModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: shearModulus(:)
  CLASS(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: massDensity(:)
  CLASS(Domain_), INTENT(INOUT) :: dom
    !! Domain
  TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbcPtrs(:)
    !! Absorbing boundary conditions
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: fe(:)
  !! Finite elements on boundary
  TYPE(FiniteElementPointer_), INTENT(INOUT) :: geoFE(:)
  !! finite elements for geometry
  TYPE(ElemShapeData_), INTENT(INOUT) :: spaceElemSD(:)
  !! space-element data
  TYPE(ElemShapeData_), INTENT(INOUT) :: geoSpaceElemSD(:)
  !! geo space element data
  LOGICAL(LGT), INTENT(IN) :: reset
  !! reset
  REAL(DFP), INTENT(IN) :: scale
  !! scale

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelAssembleViscousBoundary1()"
  TYPE(FEVariable_) :: forceVar
  TYPE(ElemShapeData_) :: elemsd, geoElemSD
  CLASS(Mesh_), POINTER :: meshptr
  CLASS(ReferenceElement_), POINTER :: refelem
  CLASS(FiniteElement_), POINTER :: spaceFE, geoSpaceFE
  CLASS(NeumannBC_), POINTER :: nbc
  LOGICAL(LGT) :: problem, isSelectionByMeshID
  INTEGER(I4B) :: tmesh, nsd, id, nns, iel, tnbc, nbcNo, idof, jd
  INTEGER(I4B), ALLOCATABLE :: nptrs(:), meshID(:)
  REAL(DFP), ALLOCATABLE :: fevec(:, :), xij(:, :), forceVec(:, :)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  IF (reset) CALL mat%Set(VALUE=0.0_DFP)
  nsd = dom%GetNSD()
  tdof = nsd
  tnbc = SIZE(nbcPtrs)
  NULLIFY (meshptr, refelem, spaceFE, geoSpaceFE, nbc)

  DO nbcNo = 1, tnbc
    nbc => nbcPtrs(nbcNo)%ptr
    problem = .NOT. ASSOCIATED(nbc)
    IF (problem) CYCLE

    CALL nbc%GetParam(isSelectionByMeshID=isSelectionByMeshID)

    problem = .NOT. isSelectionByMeshID
    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: Currently, found isSelectionByMeshID false.')
      CYCLE
    END IF

    meshID = nbc%GetMeshID(dim=nsd - 1_I4B)
    tmesh = SIZE(meshID)

    DO id = 1, tmesh
      jd = meshID(id)
      meshptr => dom%GetMeshPointer(dim=nsd - 1_I4B, entityNum=jd)

      problem = meshptr%isEmpty()
      IF (problem) CYCLE

      spaceFE => fe(jd)%ptr
      geoSpaceFE => geoFE(jd)%ptr

      elemsd = spaceElemSD(jd)
      geoElemSD = geoSpaceElemSD(jd)

      refelem => meshptr%GetRefElemPointer()
      nns = (.nne.refelem)

      CALL Reallocate(xij, nsd, nns)
      CALL Reallocate(Mmat, tdof * nsd, tdof * nns)

      youngsModulusField => youngsModulus(jd)%ptr
      shearModulusField => shearModulus(jd)%ptr
      massDensity => massDensity(jd)%ptr

#ifdef DEBUG_VER
      isok = ASSOCIATED(youngsModulusField)
      IF (.NOT. isok) THEN
        CALL e%RaiseError(modName//'::'//myName//' - '// &
          & '[INTERNAL ERROR] :: youngsModulus('//tostring(jd)//') is NULL.')
        RETURN
      END IF

      isok = ASSOCIATED(shearModulusField)
      IF (.NOT. isok) THEN
        CALL e%RaiseError(modName//'::'//myName//' - '// &
          & '[INTERNAL ERROR] :: shearModulus('//tostring(jd)//') is NULL.')
        RETURN
      END IF

      isok = ASSOCIATED(massDensityField)
      IF (.NOT. isok) THEN
        CALL e%RaiseError(modName//'::'//myName//' - '// &
          & '[INTERNAL ERROR] :: massDensity('//tostring(jd)//') is NULL.')
        RETURN
      END IF
#endif

      DO iel = meshptr%minElemNum, meshptr%maxElemNum

        problem = .NOT. meshptr%IsElementPresent(iel)
        IF (problem) CYCLE

        nptrs = meshptr%GetConnectivity(iel)
        CALL dom%GetNodeCoord(nodeCoord=xij, globalNode=nptrs)

        CALL spaceFE%GetGlobalElemShapeData(elemsd=elemsd, xij=xij, &
          & geoElemSD=geoElemSD)

        ! INTERFACE ViscousBoundaryMassMatrix
        ! MODULE PURE FUNCTION MassMatrix_5(test, trial, lambda, mu, rho) &

        Mmat = ViscousBoundaryMassMatrix(test=elemsd, trial=elemsd, &
          & lambda=youngsVar, mu=muVar, rho=rhoVar,  &
          & isLambdaYoungsModulus=.TRUE.)

        CALL mat%Set(globalNode=nptrs, VALUE=Mmat,  &
          & storageFMT=DOF_FMT, scale=1.0_DFP, addContribution=.TRUE.)

      END DO
    END DO

  END DO

  NULLIFY (meshptr, refelem, spaceFE, geoSpaceFE, nbc)

  IF (ALLOCATED(nptrs)) DEALLOCATE (nptrs)
  IF (ALLOCATED(meshID)) DEALLOCATE (meshID)
  IF (ALLOCATED(fevec)) DEALLOCATE (fevec)
  IF (ALLOCATED(xij)) DEALLOCATE (xij)

  CALL DEALLOCATE (elemsd)
  CALL DEALLOCATE (geoElemSD)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelAssembleViscousBoundary1

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END MODULE KernelAssembleViscousBoundary_Method

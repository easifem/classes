! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

SUBMODULE(AssembleDiffusionMatrixUtility) ScalarFieldMethods
USE Display_Method, ONLY: ToString
USE ExceptionHandler_Class, ONLY: e
USE ReallocateUtility, ONLY: Reallocate
USE MassMatrix_Method, ONLY: MassMatrix_
USE DiffusionMatrix_Method, ONLY: DiffusionMatrix_
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE FEDOF_Class, ONLY: FEDOF_
USE BaseType, ONLY: QuadraturePoint_, ElemshapeData_, FEVariable_, &
                    TypeFEVariableScalar

#ifdef DEBUG_VER
USE QuadraturePoint_Method, ONLY: QuadraturePoint_Display => Display
USE ElemshapeData_Method, ONLY: ElemshapeData_Display => Display
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

TYPE(DefaultOpt_), PARAMETER :: defaultOpt = DefaultOpt_()

CONTAINS

!----------------------------------------------------------------------------
!                                         ScalarFieldAssembleDiffusionMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleDiffusionMatrix1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleDiffusionMatrix1()"
#endif

INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, ks_i, ks_j, xij_i, xij_j
TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), ks(:, :)
INTEGER(I4B), ALLOCATABLE :: cellCon(:)
TYPE(FEVariable_) :: diffCoeffVar
CLASS(AbstractFE_), POINTER :: feptr, geofeptr
CLASS(AbstractMesh_), POINTER :: mesh
CLASS(FEDOF_), POINTER :: fedof, geofedof

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

fedof => nodeField%fedof
mesh => fedof%GetMeshPointer()
geofedof => nodeField%geofedof
tElements = mesh%GetTotalElements()

IF (reset) CALL tanmat%set(VALUE=defaultOpt%zero)

maxNNEGeo = geofedof%GetMaxTotalConnectivity()
maxNNE = fedof%GetMaxTotalConnectivity()

CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(cellCon, maxNNE)
CALL Reallocate(ks, maxNNE, maxNNE)

DO iel = 1, tElements

  CALL fedof%SetFE(globalElement=iel, islocal=defaultOpt%yes)
  feptr => fedof%GetFEPointer(globalElement=iel, islocal=defaultOpt%yes)

  CALL geofedof%SetFE(globalElement=iel, islocal=defaultOpt%yes)
  geofeptr => geofedof%GetFEPointer(globalElement=iel, &
                                    islocal=defaultOpt%yes)

  CALL mesh%GetNodeCoord( &
    nodeCoord=xij, nrow=xij_i, ncol=xij_j, islocal=defaultOpt%yes, &
    globalElement=iel)

  CALL fedof%GetConnectivity_(globalElement=iel, islocal=defaultOpt%yes, &
                              ans=cellcon, tsize=tcellCon, opt="A")

  CALL feptr%GetGlobalElemShapeData2( &
    geofeptr=geofeptr, elemsd=elemsd, geoelemsd=geoelemsd, xij=xij, &
    quad=quad)

  ! TODO: No allocatation in diffCoeffVar
  CALL diffCoeffField%Get(globalElement=iel, islocal=defaultOpt%yes, &
                          fevar=diffCoeffVar)

  ks = defaultOpt%zero
  CALL DiffusionMatrix_( &
    test=elemsd, trial=elemsd, k=diffCoeffVar, krank=TypeFEVariableScalar, &
    ans=ks, nrow=ks_i, ncol=ks_j)

  CALL tanmat%Set( &
    globalNode=cellcon(1:tcellCon), islocal=defaultOpt%yes, &
    VALUE=ks(1:ks_i, 1:ks_j), storageFMT=defaultOpt%storageFormatDOF, &
    scale=scale, addContribution=defaultOpt%yes)
END DO

IF (ALLOCATED(xij)) DEALLOCATE (xij)
IF (ALLOCATED(ks)) DEALLOCATE (ks)
IF (ALLOCATED(cellCon)) DEALLOCATE (cellCon)
NULLIFY (feptr, geofeptr, mesh, fedof, geofedof)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE ScalarFieldAssembleDiffusionMatrix1

!----------------------------------------------------------------------------
!                                                          Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ScalarFieldMethods

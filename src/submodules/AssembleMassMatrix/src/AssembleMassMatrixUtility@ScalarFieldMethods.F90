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

SUBMODULE(AssembleMassMatrixUtility) ScalarFieldMethods
USE ReallocateUtility, ONLY: Reallocate
USE MassMatrix_Method, ONLY: MassMatrix_
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE FEDOF_Class, ONLY: FEDOF_
USE FEVariable_Method, ONLY: QuadratureVariable
USE BaseType, ONLY: QuadraturePoint_, ElemshapeData_, FEVariable_, &
                    TypeFEVariableScalar, TypeFEVariableSpace

IMPLICIT NONE

TYPE(DefaultOpt_), PARAMETER :: defaultOpt = DefaultOpt_()

CONTAINS

!----------------------------------------------------------------------------
!                                         ScalarFieldAssembleMassMatrix
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarFieldAssembleMassMatrix1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "ScalarFieldAssembleMassMatrix1()"
#endif

INTEGER(I4B) :: iel, tElements, maxNNE, maxNNEGeo, &
                tcellCon, ks_i, ks_j, xij_i, xij_j, maxQuadPoints
TYPE(QuadraturePoint_) :: quad
TYPE(ElemshapeData_) :: elemsd, geoelemsd
REAL(DFP), ALLOCATABLE :: xij(:, :), ks(:, :)
INTEGER(I4B), ALLOCATABLE :: cellCon(:)
TYPE(FEVariable_) :: rhoVar
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
maxQuadPoints = fedof%GetMaxTotalQuadraturePoints()

CALL Reallocate(xij, 3, maxNNEGeo)
CALL Reallocate(cellCon, maxNNE)
CALL Reallocate(ks, maxNNE, maxNNE)

rhoVar = QuadratureVariable(tsize=maxQuadPoints, &
                            varType=TypeFEVariableSpace, &
                            rank=TypeFEVariableScalar)

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

  CALL massDensityField%Get(globalElement=iel, islocal=defaultOpt%yes, &
                            fevar=rhoVar)

  ks = defaultOpt%zero
  CALL MassMatrix_( &
    test=elemsd, trial=elemsd, rho=rhoVar, rhoRank=TypeFEVariableScalar, &
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
END PROCEDURE ScalarFieldAssembleMassMatrix1

!----------------------------------------------------------------------------
!                                                          Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE ScalarFieldMethods

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
!

MODULE AssembleSurfaceSourceUtility
USE GlobalData, ONLY: DFP, LGT, I4B
USE ExceptionHandler_Class, ONLY: e
USE NeumannBC_Class, ONLY: NeumannBC_, NeumannBCPointer_
USE ScalarField_Class, ONLY: ScalarField_
USE VectorField_Class, ONLY: VectorField_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE FEDOF_Class, ONLY: FEDOF_
USE UserFunction_Class, ONLY: UserFunction_
USE BaseType, ONLY: FEVariable_, QuadraturePoint_, ElemShapeData_
USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE
PRIVATE

PUBLIC :: ScalarFieldAssembleSurfaceSource

CHARACTER(*), PARAMETER :: modName = "AssembleSurfaceSourceUtility"

!----------------------------------------------------------------------------
!                                                                 DefaultOpt_
!----------------------------------------------------------------------------

TYPE :: DefaultOpt_
  REAL(DFP) :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
               half = 0.5_DFP
  LOGICAL(LGT) :: yes = .TRUE.
  LOGICAL(LGT) :: no = .FALSE.
  INTEGER(I4B) :: storageFormatDOF = TypeFieldOpt%storageFormatDOF
  INTEGER(I4B) :: storageFormatNodes = TypeFieldOpt%storageFormatNodes
END TYPE DefaultOpt_

!----------------------------------------------------------------------------
!                        ScalarFieldAssembleSurfaceSource@ScalarFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Assemble surface source into RHS
!
!# Introduction
!
! This is an internal routine that assemble surface source (Neumann BC)
! into the right-hand side (RHS) vector of a scalar field.

INTERFACE ScalarFieldAssembleSurfaceSource
  MODULE SUBROUTINE ScalarFieldAssembleSurfaceSource1( &
    obj, nbc, fedof, geofedof, mesh, nbcField, scale, geoCellCon, &
    geoFacetCon, cellCon, xij, facetXij, forceVec, nbcValue, forceVar, &
    quad, facetQuad, elemsd, facetElemsd, geoElemsd, geoFacetElemsd)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(NeumannBC_), INTENT(INOUT) :: nbc
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    CLASS(ScalarField_), INTENT(INOUT) :: nbcField
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(INOUT) :: geoCellCon(:), geoFacetCon(:), &
                                   cellCon(:)
    !! Working array
    REAL(DFP), INTENT(INOUT) :: xij(:, :), facetXij(:, :), forceVec(:), &
                                nbcValue(:)
    !! Working variable
    TYPE(FEVariable_), INTENT(INOUT) :: forceVar
    !! Working variables
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! Working variables
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, facetElemsd, geoElemsd, &
                                           geoFacetElemsd
    !! Working variables
  END SUBROUTINE ScalarFieldAssembleSurfaceSource1
END INTERFACE ScalarFieldAssembleSurfaceSource

!----------------------------------------------------------------------------
!                        ScalarFieldAssembleSurfaceSource@ScalarFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Add contribution of surface traction (NBC) to RHS

INTERFACE ScalarFieldAssembleSurfaceSource
  MODULE SUBROUTINE ScalarFieldAssembleSurfaceSource2( &
    obj, geofedof, mesh, nbcField, scale)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    CLASS(ScalarField_), INTENT(INOUT) :: nbcField
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE ScalarFieldAssembleSurfaceSource2
END INTERFACE ScalarFieldAssembleSurfaceSource

!----------------------------------------------------------------------------
!                        ScalarFieldAssembleSurfaceSource@ScalarFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Assemble surface source into RHS
!
!# Introduction
!
! This is an internal routine that assemble surface source (Neumann BC)
! into the right-hand side (RHS) vector of a scalar field.

INTERFACE ScalarFieldAssembleSurfaceSource
  MODULE SUBROUTINE ScalarFieldAssembleSurfaceSource3( &
    obj, nbc, fedof, geofedof, mesh, func, scale, geoCellCon, &
    geoFacetCon, cellCon, xij, facetXij, forceVec, nbcValue, forceVar, &
    quad, facetQuad, elemsd, facetElemsd, geoElemsd, geoFacetElemsd)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(NeumannBC_), INTENT(INOUT) :: nbc
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    REAL(DFP), INTENT(IN) :: scale
    INTEGER(I4B), INTENT(INOUT) :: geoCellCon(:), geoFacetCon(:), &
                                   cellCon(:)
    !! Working array
    REAL(DFP), INTENT(INOUT) :: xij(:, :), facetXij(:, :), forceVec(:), &
                                nbcValue(:)
    !! Working variable
    TYPE(FEVariable_), INTENT(INOUT) :: forceVar
    !! Working variables
    TYPE(QuadraturePoint_), INTENT(INOUT) :: quad, facetQuad
    !! Working variables
    TYPE(ElemShapeData_), INTENT(INOUT) :: elemsd, facetElemsd, geoElemsd, &
                                           geoFacetElemsd
    !! Working variables
  END SUBROUTINE ScalarFieldAssembleSurfaceSource3
END INTERFACE ScalarFieldAssembleSurfaceSource

!----------------------------------------------------------------------------
!                        ScalarFieldAssembleSurfaceSource@ScalarFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Add contribution of surface traction (NBC) to RHS

INTERFACE ScalarFieldAssembleSurfaceSource
  MODULE SUBROUTINE ScalarFieldAssembleSurfaceSource4( &
    obj, geofedof, mesh, func, scale)
    CLASS(ScalarField_), INTENT(INOUT) :: obj
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE ScalarFieldAssembleSurfaceSource4
END INTERFACE ScalarFieldAssembleSurfaceSource

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AssembleSurfaceSourceUtility

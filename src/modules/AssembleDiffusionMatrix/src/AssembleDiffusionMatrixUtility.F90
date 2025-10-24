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

MODULE AssembleDiffusionMatrixUtility
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE GlobalData, ONLY: DFP, LGT, I4B
USE MatrixField_Class, ONLY: MatrixField_
USE FEDOF_Class, ONLY: FEDOF_
USE MeshField_Class, ONLY: MeshField_
USE FieldOpt_Class, ONLY: TypeFieldOpt
IMPLICIT NONE

PRIVATE
PUBLIC :: ScalarFieldAssembleDiffusionMatrix

CHARACTER(*), PARAMETER :: modName = "AssembleDiffusionMatrixUtility"

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

TYPE(DefaultOpt_), PARAMETER :: defaultOpt = DefaultOpt_()

!----------------------------------------------------------------------------
!                       ScalarFieldAssembleDiffusionMatrix@ScalarFieldMethods
!----------------------------------------------------------------------------

INTERFACE ScalarFieldAssembleDiffusionMatrix
  MODULE SUBROUTINE ScalarFieldAssembleDiffusionMatrix1( &
    tanmat, mesh, geofedof, fedof, diffCoeffField, reset, scale)
    CLASS(MatrixField_), INTENT(INOUT) :: tanmat
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    !! geofedof
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    !! fedof
    CLASS(MeshField_), INTENT(INOUT) :: diffCoeffField
    !! diffusiob coefficient field
    LOGICAL(LGT), INTENT(IN) :: reset
    !! if reset is true, then tanmat is set to zero before assembly
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE ScalarFieldAssembleDiffusionMatrix1
END INTERFACE ScalarFieldAssembleDiffusionMatrix

END MODULE AssembleDiffusionMatrixUtility

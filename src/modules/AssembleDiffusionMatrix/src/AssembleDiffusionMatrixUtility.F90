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
USE GlobalData, ONLY: DFP, LGT, I4B
USE MatrixField_Class, ONLY: MatrixField_
USE MeshField_Class, ONLY: MeshField_
USE ScalarField_Class, ONLY: ScalarField_
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

!----------------------------------------------------------------------------
!                       ScalarFieldAssembleDiffusionMatrix@ScalarFieldMethods
!----------------------------------------------------------------------------

INTERFACE ScalarFieldAssembleDiffusionMatrix
  MODULE SUBROUTINE ScalarFieldAssembleDiffusionMatrix1( &
    tanmat, nodeField, diffCoeffField, reset, scale)
    CLASS(MatrixField_), INTENT(INOUT) :: tanmat
    !! Matrix field to assemble
    CLASS(ScalarField_), INTENT(INOUT) :: nodeField
    !! Scalar field to get the fedof, mesh, and geofedof
    CLASS(MeshField_), INTENT(INOUT) :: diffCoeffField
    !! diffusion coefficient field
    LOGICAL(LGT), INTENT(IN) :: reset
    !! if reset is true, then tanmat is set to zero before assembly
    REAL(DFP), INTENT(IN) :: scale
  END SUBROUTINE ScalarFieldAssembleDiffusionMatrix1
END INTERFACE ScalarFieldAssembleDiffusionMatrix

END MODULE AssembleDiffusionMatrixUtility

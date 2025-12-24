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

MODULE AssembleStiffnessMatrixUtility
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE GlobalData, ONLY: DFP, LGT, I4B
USE MatrixField_Class, ONLY: MatrixField_
USE FEDOF_Class, ONLY: FEDOF_
USE MeshField_Class, ONLY: MeshField_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE VectorField_Class, ONLY: VectorField_
IMPLICIT NONE

PRIVATE
PUBLIC :: VectorFieldAssembleStiffnessMatrix

CHARACTER(*), PARAMETER :: modName = "AssembleStiffnessMatrixUtility"

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
!                       VectorFieldAssembleStiffnessMatrix@VectorFieldMethods
!----------------------------------------------------------------------------

INTERFACE VectorFieldAssembleStiffnessMatrix
  MODULE SUBROUTINE VectorFieldAssembleStiffnessMatrix1( &
    tanmat, nodeField, lambdaField, muField, reset, scale, &
    islambdaYoungsModulus)
    CLASS(MatrixField_), INTENT(INOUT) :: tanmat
    CLASS(VectorField_), INTENT(INOUT) :: nodeField
    CLASS(MeshField_), INTENT(INOUT) :: lambdaField, muField
    !! diffusiob coefficient field
    LOGICAL(LGT), INTENT(IN) :: reset
    !! if reset is true, then tanmat is set to zero before assembly
    REAL(DFP), INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islambdaYoungsModulus
  END SUBROUTINE VectorFieldAssembleStiffnessMatrix1
END INTERFACE VectorFieldAssembleStiffnessMatrix

END MODULE AssembleStiffnessMatrixUtility

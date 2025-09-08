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

MODULE AssembleBodySourceUtility
USE ScalarField_Class, ONLY: ScalarField_
USE VectorField_Class, ONLY: VectorField_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE UserFunction_Class, ONLY: UserFunction_
USE FEDOF_Class, ONLY: FEDOF_
USE GlobalData, ONLY: DFP, LGT, I4B
USE FieldOpt_Class, ONLY: TypeFieldOpt
IMPLICIT NONE

PRIVATE
PUBLIC :: ScalarFieldAssembleBodySource

CHARACTER(*), PARAMETER :: modName = "AssembleBodySourceUtility"

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
!                            ScalarFieldAssembleBodySource@ScalarFieldMethods
!----------------------------------------------------------------------------

INTERFACE ScalarFieldAssembleBodySource
  MODULE SUBROUTINE ScalarFieldAssembleBodySource1(rhs, mesh, bodySource, &
                                                   geofedof, fedof, &
                                                   nodeCoord, scale, &
                                                   times)
    CLASS(ScalarField_), INTENT(INOUT) :: rhs
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    CLASS(VectorField_), INTENT(INOUT) :: nodeCoord
    REAL(DFP), INTENT(IN) :: scale
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE ScalarFieldAssembleBodySource1
END INTERFACE ScalarFieldAssembleBodySource

!----------------------------------------------------------------------------
!                            ScalarFieldAssembleBodySource@ScalarFieldMethods
!----------------------------------------------------------------------------

INTERFACE ScalarFieldAssembleBodySource
  MODULE SUBROUTINE ScalarFieldAssembleBodySource2(rhs, mesh, bodySource, &
                                                   geofedof, fedof, &
                                                   nodeCoord, scale, &
                                                   times)
    CLASS(ScalarField_), INTENT(INOUT) :: rhs
    CLASS(AbstractMesh_), INTENT(INOUT) :: mesh
    CLASS(ScalarField_), INTENT(INOUT) :: bodySource
    CLASS(FEDOF_), INTENT(INOUT) :: geofedof
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    CLASS(VectorField_), INTENT(INOUT) :: nodeCoord
    REAL(DFP), INTENT(IN) :: scale
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE ScalarFieldAssembleBodySource2
END INTERFACE ScalarFieldAssembleBodySource

END MODULE AssembleBodySourceUtility

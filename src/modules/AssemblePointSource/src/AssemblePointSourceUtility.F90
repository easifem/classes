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

MODULE AssemblePointSourceUtility
USE GlobalData, ONLY: DFP, LGT, I4B
USE ExceptionHandler_Class, ONLY: e
USE NeumannBC_Class, ONLY: NeumannBC_, NeumannBCPointer_
USE ScalarField_Class, ONLY: ScalarField_
USE FEDOF_Class, ONLY: FEDOF_

IMPLICIT NONE
PRIVATE

PUBLIC :: ScalarFieldAssemblePointSource

CHARACTER(*), PARAMETER :: modName = "AssemblePointSourceUtility"

!----------------------------------------------------------------------------
!                                                                 DefaultOpt_
!----------------------------------------------------------------------------

TYPE :: DefaultOpt_
  REAL(DFP) :: one = 1.0_DFP, zero = 0.0_DFP, minus_one = -1.0_DFP, &
               half = 0.5_DFP
  LOGICAL(LGT) :: yes = .TRUE.
  LOGICAL(LGT) :: no = .FALSE.
END TYPE DefaultOpt_

TYPE(DefaultOpt_), PARAMETER :: defaultOpt = DefaultOpt_()

!----------------------------------------------------------------------------
!                          ScalarFieldAssemblePointSource@ScalarFieldMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Assemble RHS

INTERFACE ScalarFieldAssemblePointSource
  MODULE SUBROUTINE ScalarFieldAssemblePointSource1(rhs, nbc, fedof, scale, &
                                                    times)
    CLASS(ScalarField_), INTENT(INOUT) :: rhs
    CLASS(NeumannBCPointer_), INTENT(INOUT) :: nbc(:)
    CLASS(FEDOF_), INTENT(INOUT) :: fedof
    REAL(DFP), INTENT(IN) :: scale
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE ScalarFieldAssemblePointSource1
END INTERFACE ScalarFieldAssemblePointSource

END MODULE AssemblePointSourceUtility

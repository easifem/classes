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

MODULE StfemAlgorithm2FactoryUtility
USE tomlf, ONLY: toml_table
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractStfemAlgorithm2_Class, ONLY: AbstractStfemAlgorithm2_
IMPLICIT NONE

PRIVATE
PUBLIC :: StfemAlgorithm2Factory

!----------------------------------------------------------------------------
!                                                                 FEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-20
! summary: This method returns pointer to child of AbstractStfemAlgorithm2_
!
! This method returns pointer to child of AbstractStfemAlgorithm2_.
! This method will read the name from toml table.
! Then it will call StfemAlgorithm2Factory2.

INTERFACE StfemAlgorithm2Factory
  MODULE FUNCTION StfemAlgorithm2Factory1(table) RESULT(ans)
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractStfemAlgorithm2_), POINTER :: ans
  END FUNCTION StfemAlgorithm2Factory1
END INTERFACE StfemAlgorithm2Factory

!----------------------------------------------------------------------------
!                                                                 FEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-20
! summary: This method returns pointer to child of AbstractStfemAlgorithm2_
!
! This method returns pointer to child of AbstractStfemAlgorithm2_.
! Based on name this function will allocate ans as
! UStfemAlgorithm2 or VStfemAlgorithm2, and return the pointer.

INTERFACE StfemAlgorithm2Factory
  MODULE FUNCTION StfemAlgorithm2Factory2(name) RESULT(ans)
    CHARACTER(*), INTENT(IN) :: name
    !! name of the algorithm, it should be atleast three character long.
    !! examples are ustfem, vstfem
    CLASS(AbstractStfemAlgorithm2_), POINTER :: ans
  END FUNCTION StfemAlgorithm2Factory2
END INTERFACE StfemAlgorithm2Factory

END MODULE StfemAlgorithm2FactoryUtility

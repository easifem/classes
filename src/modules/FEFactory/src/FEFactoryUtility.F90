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

MODULE FEFactoryUtility
USE GlobalData, ONLY: I4B, DFP, LGT
USE AbstractFE_Class, ONLY: AbstractFE_
USE tomlf, ONLY: toml_table
USE ExceptionHandler_Class, ONLY: e

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "FEFactoryUtility"

PUBLIC :: FEFactory

!----------------------------------------------------------------------------
!                                                                 FEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-20
! summary: FEFactory

INTERFACE FEFactory
  MODULE FUNCTION FEFactory1(elemType, baseContinuity, baseInterpolation) &
    RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    CLASS(AbstractFE_), POINTER :: ans
  END FUNCTION FEFactory1
END INTERFACE FEFactory

!----------------------------------------------------------------------------
!                                                                 FEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-20
! summary: FEFactory

INTERFACE FEFactory
  MODULE FUNCTION FEFactory2(elemType, table) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractFE_), POINTER :: ans
  END FUNCTION FEFactory2
END INTERFACE FEFactory

END MODULE FEFactoryUtility

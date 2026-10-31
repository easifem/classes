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
USE AbstractFE_Class, ONLY: AbstractFE_
USE AbstractOneDimFE_Class, ONLY: AbstractOneDimFE_
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE tomlf, ONLY: toml_table
IMPLICIT NONE
PRIVATE

PUBLIC :: FEFactory
PUBLIC :: OneDimFEFactory

!----------------------------------------------------------------------------
!                                                                 FEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-20
! summary: FEFactory

INTERFACE FEFactory
  MODULE FUNCTION FEFactory1(elemType, baseContinuity, baseInterpolation, &
                             isBrokenSpace) RESULT(ans)
    INTEGER(I4B), INTENT(IN) :: elemType
    !! element type, also known as element name
    CHARACTER(*), INTENT(IN) :: baseContinuity
    !! base continuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    !! base interpolation
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isBrokenSpace
    !! Are we dealing with broken spaces
    !! Default value is false,
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

!----------------------------------------------------------------------------
!                                                            OneDimFEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Internal routine for creating pointer form baseContinuity
! and baseInterpolation

INTERFACE OneDimFEFactory
  MODULE FUNCTION OneDimFEFactory1(baseContinuity, baseInterpolation, &
                                   isBrokenSpace) &
    RESULT(ans)
    CHARACTER(*), INTENT(IN) :: baseContinuity
    CHARACTER(*), INTENT(IN) :: baseInterpolation
    CLASS(AbstractOneDimFE_), POINTER :: ans
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isBrokenSpace
  END FUNCTION OneDimFEFactory1
END INTERFACE OneDimFEFactory

!----------------------------------------------------------------------------
!                                                            OneDimFEFactory
!----------------------------------------------------------------------------

INTERFACE OneDimFEFactory
  MODULE FUNCTION OneDimFEFactory2(table) RESULT(ans)
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractOneDimFE_), POINTER :: ans
  END FUNCTION OneDimFEFactory2
END INTERFACE OneDimFEFactory

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FEFactoryUtility

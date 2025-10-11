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

SUBMODULE(BasisOpt_Class) LagrangeMethods
USE Display_Method, ONLY: ToString
USE LagrangePolynomialUtility, ONLY: LagrangeDOF
USE BasisOptUtility, ONLY: SetIntegerType

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                           SetLagrangeOrder
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetLagrangeOrder
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetLagrangeOrder()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = PRESENT(errCheck)
IF (isok) THEN
  IF (errCheck) CALL CheckErrorLagrange(obj=obj, &
                                        order=order, &
                                        anisoOrder=anisoOrder)
END IF
#endif

obj%isIsotropicOrder = PRESENT(order)

IF (obj%isIsotropicOrder) THEN
  CALL obj%ResetAnisotropicOrder()
  obj%order = order
  obj%tdof = LagrangeDOF(order=obj%order, elemType=obj%topoType)
END IF

obj%isAnisotropicOrder = PRESENT(anisoOrder)
IF (obj%isAnisotropicOrder) THEN
  CALL obj%ResetIsotropicOrder()
  CALL SetIntegerType(a=obj%anisoOrder, b=anisoOrder, n=obj%xidim)
  obj%tdof = LagrangeDOF(p=obj%anisoOrder(1), &
                         q=obj%anisoOrder(2), &
                         r=obj%anisoOrder(3), &
                         elemType=obj%topoType)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetLagrangeOrder

!----------------------------------------------------------------------------
!                                                           SetLagrangeOrder
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-09
! summary:  Check essential param in parameter list

SUBROUTINE CheckErrorLagrange(obj, order, anisoOrder)
  CLASS(BasisOpt_), INTENT(INOUT) :: obj
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: order
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: anisoOrder(:)

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CheckErrorLagrange()"
  LOGICAL(LGT) :: isok
  INTEGER(I4B) :: tsize

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')

  IF (PRESENT(order)) THEN
    isok = order .NE. 0
    CALL AssertError1(isok, myName, "zero order found.")
  END IF

  IF (PRESENT(anisoOrder)) THEN
    tsize = SIZE(anisoOrder)
    isok = obj%xidim .LE. tsize
    CALL AssertError1(isok, myName, "size of anisoOrder is not enough.")
  END IF

  isok = PRESENT(order) .OR. PRESENT(anisoOrder)
  CALL AssertError1(isok, myName, "no order or anisotropic order found.")

  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END SUBROUTINE CheckErrorLagrange

!----------------------------------------------------------------------------
!                                                                      Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE LagrangeMethods

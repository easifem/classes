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

MODULE CapillaryFactory
USE GlobalData, ONLY: I4B, DFP, LGT
USE StringUtility, ONLY: UpperCase
USE AbstractCapillaryModel_Class, ONLY: AbstractCapillaryModel_
! USE VanGenuchtenModel_Class, ONLY: VanGenuchtenModel_
USE VanGenuchtenMualemModel_Class, ONLY: VanGenuchtenMualemModel_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE
PUBLIC :: CapillaryModelFactory

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "CapillaryFactory.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                      CapillaryModelFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-07-10
! summary: Capillary model factory

FUNCTION CapillaryModelFactory(name) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: name
  CLASS(AbstractCapillaryModel_), POINTER :: ans

  CHARACTER(:), ALLOCATABLE :: astr
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CapillaryModelFactory()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  astr = UpperCase(TRIM(name))

  SELECT CASE (astr)
  CASE ("VANGENUCHTENMUALEM")
    ALLOCATE (VanGenuchtenMualemModel_ :: ans)

  CASE ("VANGENUCHTENBURDINE")
    ! ALLOCATE (VanGenuchtenMualemModel_ :: ans)

#ifdef DEBUG_VER
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'VANGENUCHTENBURDINE model is not implemented yet:')
#endif

  CASE ("BROOKSCOREY")

#ifdef DEBUG_VER
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'Brooks-Corey model is not implemented yet:')
#endif

  CASE ("GARDNER")

#ifdef DEBUG_VER
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                      'Gardner model is not implemented yet:')
#endif

  CASE DEFAULT

#ifdef DEBUG_VER
    CALL AssertError1(math%no, myName, &
                      "no case found for name="//astr)
#endif

  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END FUNCTION CapillaryModelFactory

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END MODULE CapillaryFactory

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

MODULE FEFactory_Method
USE GlobalData, ONLY: I4B, DFP, LGT
USE String_Class, ONLY: String
USE AbstractFE_Class, ONLY: AbstractFE_
USE LagrangeFE_Class, ONLY: LagrangeFE_
USE HierarchicalFE_Class, ONLY: HierarchicalFE_
USE OrthogonalFE_Class, ONLY: OrthogonalFE_
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE StringUtility, ONLY: UpperCase
USE BasisOpt_Class, ONLY: TypeBasisOpt
USE tomlf, ONLY: toml_table
USE TomlUtility, ONLY: GetValue

IMPLICIT NONE

CHARACTER(*), PARAMETER :: modName = "FEFactory"

PRIVATE

PUBLIC :: FEFactory

INTERFACE FEFactory
  MODULE PROCEDURE :: FEFactory1, FEFactory2, FEFactory3
END INTERFACE FEFactory

CONTAINS

!----------------------------------------------------------------------------
!                                                           FEFactory
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Internal routine for creating pointer form baseContinuity
! and baseInterpolation

FUNCTION FEFactory1(baseContinuity, baseInterpolation) RESULT(ans)
  CHARACTER(*), INTENT(IN) :: baseContinuity
  CHARACTER(*), INTENT(IN) :: baseInterpolation
  CLASS(AbstractFE_), POINTER :: ans

  ! internal variables
  CHARACTER(*), PARAMETER :: myName = "FEFactory1()"
  CHARACTER(LEN=4) :: baseInterpolation0

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  baseInterpolation0 = UpperCase(baseInterpolation(1:4))

  ans => NULL()

  SELECT CASE (baseInterpolation0)
  CASE ("LAGR")
    ALLOCATE (LagrangeFE_ :: ans)
  CASE ("HIER", "HEIR")
    ALLOCATE (HierarchicalFE_ :: ans)
  CASE ("ORTHO")
    ALLOCATE (OrthogonalFE_ :: ans)
  CASE DEFAULT
    CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[INTERNAL ERROR] :: No case found for baseInterpolation')
    RETURN
  END SELECT

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION FEFactory1

!----------------------------------------------------------------------------
!                                                          FEFactory
!----------------------------------------------------------------------------

FUNCTION FEFactory2(param) RESULT(ans)
  TYPE(ParameterList_) :: param
  CLASS(AbstractFE_), POINTER :: ans

  ! internal variables

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactory2()"
#endif

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION FEFactory2

!----------------------------------------------------------------------------
!                                                            FEFactory
!----------------------------------------------------------------------------

FUNCTION FEFactory3(table) RESULT(ans)
  TYPE(toml_table), INTENT(INOUT) :: table
  CLASS(AbstractFE_), POINTER :: ans

  ! internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "FEFactory3()"
#endif

  TYPE(String) :: baseInterpolation, baseContinuity
  CHARACTER(4) :: baseInterpolation0
  CHARACTER(2) :: baseContinuity0
  LOGICAL(LGT) :: isFound
  INTEGER(I4B) :: stat, origin

  ! Read baseInterpolation and baseContinuity from table
  baseInterpolation0 = TypeBasisOpt%GetBaseInterpolation()
  baseContinuity0 = TypeBasisOpt%GetBaseContinuity()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading baseInterpolation...')
#endif

  CALL GetValue(table=table, key="baseInterpolation", &
                VALUE=baseInterpolation, &
                default_value=baseInterpolation0, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  IF (.NOT. isFound) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                   'baseInterpolation not found in toml, using default value')
  END IF
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading baseContinuity...')
#endif

  CALL GetValue(table=table, key="baseContinuity", &
                VALUE=baseContinuity, &
                default_value=baseContinuity0, &
                origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  IF (.NOT. isFound) THEN
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                      'baseContinuity not found in toml, using default value')
  END IF
#endif

  ans => FEFactory1(baseContinuity=baseContinuity%chars(), &
                    baseInterpolation=baseInterpolation%chars())

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

END FUNCTION FEFactory3

!----------------------------------------------------------------------------
!                                                            FEFactory
!----------------------------------------------------------------------------

END MODULE FEFactory_Method

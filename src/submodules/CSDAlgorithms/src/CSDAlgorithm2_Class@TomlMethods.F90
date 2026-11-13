! This program is a part of EASIFEM library
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

SUBMODULE(CSDAlgorithm2_Class) TomlMethods
USE Display_Method, ONLY: Display
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: stdout
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_serialize
USE TomlUtility, ONLY: GetValue

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         ReadOptionalReal
!----------------------------------------------------------------------------

! val is allocated only if key is in node. An unallocated actual
! argument is an absent optional argument, so the defaults of the method
! routines apply to the missing keys.

SUBROUTINE ReadOptionalReal(node, key, val)
  TYPE(toml_table), INTENT(INOUT) :: node
  !! table of the method
  CHARACTER(*), INTENT(IN) :: key
  !! key of the parameter
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val
  !! value, allocated only if found

  REAL(DFP) :: areal
  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isFound

  CALL GetValue(table=node, key=key, VALUE=areal, &
                default_value=math%zero, origin=origin, stat=stat, &
                isFound=isFound)
  IF (isFound) val = areal
END SUBROUTINE ReadOptionalReal

!----------------------------------------------------------------------------
!                                                    BatheMethodImportFromToml
!----------------------------------------------------------------------------

! Read the parameters of Bathe method from the table astr. The key gamma
! is read as splittingRatio if splittingRatio is not given. The names
! starting with RHO (BETA) select rho-infinity-Bathe (beta1/beta2-Bathe)
! even if rhoInf (beta1, beta2) is not given.

SUBROUTINE BatheMethodImportFromToml(obj, table, astr)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  TYPE(toml_table), INTENT(INOUT) :: table
  !! table of the algorithm
  TYPE(String), INTENT(IN) :: astr
  !! value of methodName, name of the table of the method

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "BatheMethodImportFromToml()"
#endif
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat
  REAL(DFP), ALLOCATABLE :: splittingRatio, rhoInf, beta1, beta2
  CHARACTER(4) :: variant

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=math%no, stat=stat)

  IF (ASSOCIATED(node)) THEN
    CALL ReadOptionalReal(node, "splittingRatio", splittingRatio)
    IF (.NOT. ALLOCATED(splittingRatio)) THEN
      CALL ReadOptionalReal(node, "gamma", splittingRatio)
    END IF
    CALL ReadOptionalReal(node, "rhoInf", rhoInf)
    CALL ReadOptionalReal(node, "beta1", beta1)
    CALL ReadOptionalReal(node, "beta2", beta2)
  END IF

  variant = UpperCase(astr%slice(1, 4))
  SELECT CASE (variant)
  CASE ("RHOB", "RBAT")
    IF (.NOT. ALLOCATED(rhoInf)) rhoInf = math%zero
  CASE ("BETA", "BBAT")
    IF (.NOT. (ALLOCATED(beta1) .OR. ALLOCATED(beta2))) THEN
      beta1 = math%one / 3.0_DFP
    END IF
  CASE DEFAULT
    ! the parameters given in the table select the variant
  END SELECT

  CALL obj%BatheMethod(splittingRatio=splittingRatio, rhoInf=rhoInf, &
                       beta1=beta1, beta2=beta2)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE BatheMethodImportFromToml

!----------------------------------------------------------------------------
!                                              NewmarkBetaMethodImportFromToml
!----------------------------------------------------------------------------

! Read beta and gamma of Newmark-beta method from the table astr

SUBROUTINE NewmarkBetaMethodImportFromToml(obj, table, astr)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  !! algorithm object
  TYPE(toml_table), INTENT(INOUT) :: table
  !! table of the algorithm
  TYPE(String), INTENT(IN) :: astr
  !! value of methodName, name of the table of the method

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "NewmarkBetaMethodImportFromToml()"
#endif
  TYPE(toml_table), POINTER :: node
  INTEGER(I4B) :: origin, stat
  REAL(DFP), ALLOCATABLE :: beta, gamma

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=math%no, stat=stat)

  IF (ASSOCIATED(node)) THEN
    CALL ReadOptionalReal(node, "beta", beta)
    CALL ReadOptionalReal(node, "gamma", gamma)
  END IF

  CALL obj%NewmarkBetaMethod(beta=beta, gamma=gamma)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE NewmarkBetaMethodImportFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: found
TYPE(String) :: astr
REAL(DFP) :: dt

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL GetValue(table=table, key="methodName", VALUE=astr, &
              default_value="Bathe", origin=origin, stat=stat)

SELECT CASE (UpperCase(astr%slice(1, 4)))
CASE ("BATH", "GAMM", "GBAT", "BETA", "BBAT", "RHOB", "RBAT")
  CALL BatheMethodImportFromToml(obj, table, astr)
CASE ("NEWM", "TRAP")
  CALL NewmarkBetaMethodImportFromToml(obj, table, astr)
CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'unknown methodName '//astr%chars()// &
                    ', it should be Bathe or Newmark')
#endif
END SELECT

CALL GetValue(table=table, key="form", VALUE=astr, &
              default_value="U", origin=origin, stat=stat)
CALL obj%SetForm(astr%chars())

CALL GetValue(table=table, key="forceEvaluation", VALUE=astr, &
              default_value="interpolate", origin=origin, stat=stat)
CALL obj%SetForceEvaluation(astr%chars())

CALL GetValue(table=table, key="dt", VALUE=dt, &
              default_value=math%zero, origin=origin, stat=stat, &
              isfound=found)
IF (found) CALL obj%Initiate(dt=dt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
LOGICAL(LGT) :: isok
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=math%no, &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading the toml file :: &
                  &cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
isok = PRESENT(printToml)
IF (isok) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

node => NULL()
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                             Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

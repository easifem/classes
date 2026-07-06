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

USE GlobalData, ONLY: CHAR_LF, stdout

USE String_Class, ONLY: String

USE StringUtility, ONLY: UpperCase

USE TomlUtility, ONLY: GetValue

USE tomlf, ONLY: toml_serialize,  &
  & toml_get => get_value, &
  & toml_len => len, &
  & toml_array,  &
  & toml_stat

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_GammaBatheMethodImportFromToml(obj, table, nodeName)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(IN) :: nodeName

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: gamma
  REAL(DFP), PARAMETER :: default_gamma = math%half

  CALL toml_get(table, nodeName%chars(), node, &
                origin=origin, requested=.FALSE., stat=stat)

  gamma = default_gamma
  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(node, "gamma", gamma, &
                  default_value=default_gamma, &
                  origin=origin, stat=stat)
  END IF

  node => NULL()

  CALL obj%BatheMethod(gamma=gamma)

END SUBROUTINE Help_GammaBatheMethodImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_BetaBatheMethodImportFromToml(obj, table, nodeName)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(IN) :: nodeName

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: gamma, beta1, beta2
  REAL(DFP), PARAMETER :: default_gamma = math%half, &
                          default_beta1 = math%one / 3.0_DFP, &
                          default_beta2 = math%one - default_beta1

  CALL toml_get(table, nodeName%chars(), node, &
                origin=origin, requested=.FALSE., stat=stat)

  gamma = default_gamma
  beta1 = default_beta1
  beta2 = default_beta2

  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(node, "gamma", gamma, &
                  default_value=default_gamma, &
                  origin=origin, stat=stat)
    CALL GetValue(node, "beta1", beta1, &
                  default_value=default_beta1, &
                  origin=origin, stat=stat)
    CALL GetValue(node, "beta2", beta2, &
                  default_value=default_beta2, &
                  origin=origin, stat=stat)
  END IF

  CALL obj%BatheMethod(gamma=gamma, beta1=beta1, beta2=beta2)

END SUBROUTINE Help_BetaBatheMethodImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_RhoBatheMethodImportFromToml(obj, table, nodeName)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(IN) :: nodeName

  INTEGER(I4B) :: origin, stat
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: gamma, rhoInf
  REAL(DFP), PARAMETER :: default_gamma = math%half, &
                          default_rhoInf = math%zero

  CALL toml_get(table, nodeName%chars(), node, &
                origin=origin, requested=.FALSE., stat=stat)

  gamma = default_gamma
  rhoInf = default_rhoInf

  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(node, "gamma", gamma, &
                  default_value=default_gamma, &
                  origin=origin, stat=stat)
    CALL GetValue(node, "rhoInf", rhoInf, &
                  default_value=default_rhoInf, &
                  origin=origin, stat=stat)
  END IF

  CALL obj%BatheMethod(gamma=gamma, rhoInf=rhoInf)

END SUBROUTINE Help_RhoBatheMethodImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

SUBROUTINE Help_SingleStepMethodImportFromToml(obj, table)
  CLASS(CSDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

  obj%splitRatios(1) = math%one
  obj%singleStep = .TRUE.

  ALLOCATE (obj%substeps(1)%ptr)

  CALL obj%substeps(1)%ptr%ImportFromToml(table)

END SUBROUTINE Help_SingleStepMethodImportFromToml

!----------------------------------------------------------------------------
!                                                             ImportFromToml1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1

CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: found
TYPE(String) :: astr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL obj%DEALLOCATE()

CALL GetValue(table=table, key="methodName", VALUE=astr, &
              default_value="BATHE", origin=origin, stat=stat, &
              isfound=found)

#ifdef DEBUG_VER
CALL AssertError1(found, myName, &
     'Cannot find methodName field in toml table. &
     & MethodName specifies the name of algorithm.')
#endif

obj%name = UpperCase(astr%slice(1, 4))

SELECT CASE (obj%name)
CASE ("GAMM", "GBAT", "BATH")

  CALL Help_GammaBatheMethodImportFromToml(obj, table, nodeName=astr)

CASE ("BETA", "BBAT")

  CALL Help_BetaBatheMethodImportFromToml(obj, table, nodeName=astr)

CASE ("RHOB", "RBAT")

  CALL Help_RhoBatheMethodImportFromToml(obj, table, nodeName=astr)

CASE ("NEWM", "TRAP", "HHTA", "COLL")

  CALL Help_SingleStepMethodImportFromToml(obj, table)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[WIP ERROR] :: User defined method not supported')

END SELECT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                             ImportFromToml2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

LOGICAL(LGT) :: isok
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, &
              origin=origin, requested=.FALSE., &
              stat=stat)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'following error occured while reading the toml file :: &
                  &cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node)

#ifdef DEBUG_VER
IF (PRESENT(printToml)) THEN
  CALL Display(toml_serialize(node), "toml config = "//CHAR_LF, &
               unitNo=stdout)
END IF
#endif

node => NULL()
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END]')
#endif

END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

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

SUBMODULE(SDAlgorithm2_Class) TomlMethods
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
!                                                  NewmarkBetaImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE NewmarkBetaMethodImportFromToml(obj, table, astr, origin, stat)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: astr
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "NewmarkBetaMethodImportFromToml()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: beta, gamma
  REAL(DFP), PARAMETER :: default_beta = 0.25_DFP, default_gamma = math%half

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=math%no, stat=stat)

  beta = default_beta
  gamma = default_gamma

  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(table=node, key="beta", VALUE=beta, &
                  default_value=default_beta, origin=origin, stat=stat)

    CALL GetValue(table=node, key="gamma", VALUE=gamma, &
                  default_value=default_gamma, origin=origin, stat=stat)
  END IF

  CALL obj%NewmarkBetaMethod(beta=beta, gamma=gamma)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE NewmarkBetaMethodImportFromToml

!----------------------------------------------------------------------------
!                                                HHTAlphaMethodImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE HHTAlphaMethodImportFromToml(obj, table, astr, origin, stat)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: astr
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "HHTAlphaMethodImportFromToml()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: alpha, beta, gamma, default_beta, default_gamma
  REAL(DFP), PARAMETER :: default_alpha = -0.30_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=math%no, stat=stat)

  alpha = default_alpha
  beta = (math%one - alpha)**2 * 0.25_DFP
  gamma = (math%one - math%two * alpha) * math%half
  default_beta = beta
  default_gamma = gamma

  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(table=node, key="alpha", VALUE=alpha, &
                  default_value=default_alpha, origin=origin, stat=stat)

    ! defaults of beta and gamma follow the alpha that was read
    default_beta = (math%one - alpha)**2 * 0.25_DFP
    default_gamma = (math%one - math%two * alpha) * math%half
    beta = default_beta
    gamma = default_gamma

    CALL GetValue(table=node, key="beta", VALUE=beta, &
                  default_value=default_beta, origin=origin, stat=stat)

    CALL GetValue(table=node, key="gamma", VALUE=gamma, &
                  default_value=default_gamma, origin=origin, stat=stat)
  END IF

  CALL obj%HHTAlphaMethod(alpha=alpha, beta=beta, gamma=gamma)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE HHTAlphaMethodImportFromToml

!----------------------------------------------------------------------------
!                                        GeneralizedAlphaMethodImportFromToml
!----------------------------------------------------------------------------

! Only the parameters present in the table are passed to
! GeneralizedAlphaMethod, so its defaults apply to the missing ones. An
! unallocated actual argument is an absent optional argument.

SUBROUTINE GeneralizedAlphaMethodImportFromToml(obj, table, astr, origin, &
                                                stat)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: astr
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = &
    "GeneralizedAlphaMethodImportFromToml()"
#endif
  TYPE(toml_table), POINTER :: node
  REAL(DFP), ALLOCATABLE :: rhoInf, alphaM, alphaF, beta, gamma

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=math%no, stat=stat)

  IF (ASSOCIATED(node)) THEN
    CALL ReadOptional("rhoInf", rhoInf)
    CALL ReadOptional("alphaM", alphaM)
    CALL ReadOptional("alphaF", alphaF)
    CALL ReadOptional("beta", beta)
    CALL ReadOptional("gamma", gamma)
  END IF

  CALL obj%GeneralizedAlphaMethod(rhoInf=rhoInf, alphaM=alphaM, &
                                  alphaF=alphaF, beta=beta, gamma=gamma)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

CONTAINS

  ! val is allocated only if key is in node
  SUBROUTINE ReadOptional(key, val)
    CHARACTER(*), INTENT(IN) :: key
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: val

    REAL(DFP) :: areal
    LOGICAL(LGT) :: isFound

    CALL GetValue(table=node, key=key, VALUE=areal, &
                  default_value=math%zero, origin=origin, stat=stat, &
                  isFound=isFound)
    IF (isFound) val = areal
  END SUBROUTINE ReadOptional

END SUBROUTINE GeneralizedAlphaMethodImportFromToml

!----------------------------------------------------------------------------
!                                             CollocationMethodImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE CollocationMethodImportFromToml(obj, table, astr, origin, stat)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: astr
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "CollocationMethodImportFromToml()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: beta, gamma, theta
  REAL(DFP), PARAMETER :: default_beta = math%one / 6.0_DFP, &
                          default_gamma = math%half, &
                          default_theta = 1.4_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=math%no, stat=stat)

  beta = default_beta
  gamma = default_gamma
  theta = default_theta

  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(table=node, key="beta", VALUE=beta, &
                  default_value=default_beta, origin=origin, stat=stat)

    CALL GetValue(table=node, key="gamma", VALUE=gamma, &
                  default_value=default_gamma, origin=origin, stat=stat)

    CALL GetValue(table=node, key="theta", VALUE=theta, &
                  default_value=default_theta, origin=origin, stat=stat)
  END IF

  CALL obj%CollocationMethod(beta=beta, gamma=gamma, theta=theta)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE CollocationMethodImportFromToml

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
              default_value="NEWM", origin=origin, stat=stat, &
              isfound=found)

#ifdef DEBUG_VER
CALL AssertError1(found, myName, &
     'Cannot find timeIntegration field in toml table. &
     &timeIntegration specifies the name of algorithm.')
#endif

obj%name = UpperCase(astr%slice(1, 4))

SELECT CASE (obj%name)

CASE ("NEWM", "TRAP")
  CALL NewmarkBetaMethodImportFromToml(obj=obj, table=table, astr=astr, &
                                       origin=origin, stat=stat)

CASE ("HHTA")
  CALL HHTAlphaMethodImportFromToml(obj=obj, table=table, astr=astr, &
                                    origin=origin, stat=stat)

CASE ("GENA", "GENE")
  CALL GeneralizedAlphaMethodImportFromToml(obj=obj, table=table, &
                                            astr=astr, origin=origin, &
                                            stat=stat)

CASE ("COLL")
  CALL CollocationMethodImportFromToml(obj=obj, table=table, astr=astr, &
                                       origin=origin, stat=stat)

CASE DEFAULT
#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, &
                    'unknown methodName '//astr%chars()// &
                    ', it should be NEWM, TRAP, HHTA, GENA or COLL')
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
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                            ImportFromToml
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
                        '[START]')
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
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include errors
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

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
USE GlobalData, ONLY: stdout, CHAR_LF
USE String_Class, ONLY: String
USE StringUtility, ONLY: UpperCase
USE Display_Method, ONLY: Display

USE TomlUtility, ONLY: GetValue, GetValue_
USE tomlf, ONLY: toml_serialize, &
                 toml_get => get_value, &
                 toml_len => len, &
                 toml_array, &
                 toml_stat

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
  REAL(DFP), PARAMETER :: default_beta = 0.25_DFP, default_gamma = 0.5_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=.FALSE., stat=stat)

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
                requested=.FALSE., stat=stat)

  alpha = default_alpha
  beta = (1.0_DFP - alpha)**2 * 0.25_DFP
  gamma = (1.0_DFP - 2.0_DFP * alpha) * 0.50_DFP
  default_beta = beta
  default_gamma = gamma

  isok = ASSOCIATED(node)
  IF (isok) THEN
    CALL GetValue(table=node, key="alpha", VALUE=alpha, &
                  default_value=default_alpha, origin=origin, stat=stat)

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
  REAL(DFP), PARAMETER :: default_beta = 1.0_DFP / 6.0_DFP, &
                          default_gamma = 0.5_DFP, &
                          default_theta = 1.4_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=.FALSE., stat=stat)

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
!                                             UserDefinedMethodImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE UserDefinedMethodImportFromToml(obj, table, astr, origin, stat)
  CLASS(SDAlgorithm2_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: astr
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "UserDefinedMethodImportFromToml()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node, node2

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  node => NULL()
  CALL toml_get(table, astr%chars(), node, origin=origin, requested=.FALSE., &
                stat=stat)

#ifdef DEBUG_VER
  isok = ASSOCIATED(node)
  CALL AssertError1(isok, myName, &
                    'following error occured while reading the toml file :: &
                    &cannot find '//astr%chars()//" table")
#endif

  CALL toml_get(node, "tanmat", node2, origin=origin, requested=.FALSE., &
                stat=stat)

  isok = ASSOCIATED(node2)
  obj%tanmat = 0.0_DFP

  IF (isok) THEN
    CALL GetValue(table=node2, key="M", VALUE=obj%tanmat(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="C", VALUE=obj%tanmat(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="K", VALUE=obj%tanmat(3), &
                  default_value=0.0_DFP, origin=origin, stat=stat)
  END IF

  obj%rhs_u1 = 0.0_DFP
  obj%rhs_v1 = 0.0_DFP
  obj%rhs_a1 = 0.0_DFP
  obj%rhs_f1 = 0.0_DFP
  obj%rhs_f2 = 0.0_DFP

  CALL toml_get(node, "rhs", node2, origin=origin, requested=.FALSE., &
                stat=stat)
  isok = ASSOCIATED(node2)

  IF (isok) THEN
    CALL GetValue(table=node2, key="MU", VALUE=obj%rhs_u1(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="CU", VALUE=obj%rhs_u1(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="KU", VALUE=obj%rhs_u1(3), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="MV", VALUE=obj%rhs_v1(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="CV", VALUE=obj%rhs_v1(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="KV", VALUE=obj%rhs_v1(3), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="MA", VALUE=obj%rhs_a1(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="CA", VALUE=obj%rhs_a1(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="KA", VALUE=obj%rhs_a1(3), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="F1", VALUE=obj%rhs_f1, &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="F2", VALUE=obj%rhs_f2, &
                  default_value=0.0_DFP, origin=origin, stat=stat)
  END IF

  node2 => NULL()
  CALL GetValue(table=node, key="alpha", VALUE=obj%alpha, &
                default_value=0.0_DFP, origin=origin, stat=stat)

  CALL toml_get(node, "dis", node2, origin=origin, requested=.FALSE., &
                stat=stat)

  obj%dis = 0.0_DFP
  isok = ASSOCIATED(node2)

  IF (isok) THEN

    CALL GetValue(table=node2, key="U1", VALUE=obj%dis(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="V1", VALUE=obj%dis(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="A1", VALUE=obj%dis(3), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="U2", VALUE=obj%dis(4), &
                  default_value=0.0_DFP, origin=origin, stat=stat)
  END IF

  node2 => NULL()

  CALL toml_get(node, "vel", node2, origin=origin, requested=.FALSE., &
                stat=stat)

  obj%vel = 0.0_DFP
  isok = ASSOCIATED(node2)

  IF (isok) THEN
    CALL GetValue(table=node2, key="U1", VALUE=obj%vel(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="V1", VALUE=obj%vel(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="A1", VALUE=obj%vel(3), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="U2", VALUE=obj%vel(4), &
                  default_value=0.0_DFP, origin=origin, stat=stat)
  END IF

  node2 => NULL()
  CALL toml_get(node, "acc", node2, origin=origin, requested=.FALSE., &
                stat=stat)

  obj%acc = 0.0_DFP
  isok = ASSOCIATED(node2)

  IF (isok) THEN
    CALL GetValue(table=node2, key="U1", VALUE=obj%acc(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="V1", VALUE=obj%acc(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="A1", VALUE=obj%acc(3), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="U2", VALUE=obj%acc(4), &
                  default_value=0.0_DFP, origin=origin, stat=stat)
  END IF

  CALL obj%MakeZeros()

  NULLIFY (node, node2)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE UserDefinedMethodImportFromToml

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

CASE ("COLL")
  CALL CollocationMethodImportFromToml(obj=obj, table=table, astr=astr, &
                                       origin=origin, stat=stat)

CASE DEFAULT
  CALL UserDefinedMethodImportFromToml(obj=obj, table=table, astr=astr, &
                                       origin=origin, stat=stat)

END SELECT

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
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
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

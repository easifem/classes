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

SUBMODULE(SDAlgorithm1_Class) TomlMethods
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
!                                                  AlphaMethodImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE AlphaMethodImportFromToml(obj, table, astr, origin, stat)
  CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  TYPE(String), INTENT(INOUT) :: astr
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "NewmarkBetaImportFromToml()"
#endif
  LOGICAL(LGT) :: isok
  TYPE(toml_table), POINTER :: node
  REAL(DFP) :: alpha
  REAL(DFP), PARAMETER :: default_alpha = 0.5_DFP

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL toml_get(table, astr%chars(), node, origin=origin, &
                requested=.FALSE., stat=stat)

  alpha = default_alpha

  isok = ASSOCIATED(node)
  IF (isok) &
    CALL GetValue(table=node, key="alpha", VALUE=alpha, &
                  default_value=default_alpha, origin=origin, stat=stat)

  CALL obj%AlphaMethod(alpha=alpha)

  node => NULL()

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE AlphaMethodImportFromToml

!----------------------------------------------------------------------------
!                                             UserDefinedMethodImportFromToml
!----------------------------------------------------------------------------

SUBROUTINE UserDefinedMethodImportFromToml(obj, table, astr, origin, stat)
  CLASS(SDAlgorithm1_), INTENT(INOUT) :: obj
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

    CALL GetValue(table=node2, key="K", VALUE=obj%tanmat(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)
  END IF

  CALL toml_get(node, "rhs", node2, origin=origin, requested=.FALSE., &
                stat=stat)

  obj%rhs_u1 = 0.0_DFP
  obj%rhs_v1 = 0.0_DFP
  obj%rhs_f1 = 0.0_DFP
  obj%rhs_f2 = 0.0_DFP

  isok = ASSOCIATED(node2)

  IF (isok) THEN

    CALL GetValue(table=node2, key="MU", VALUE=obj%rhs_u1(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="KU", VALUE=obj%rhs_u1(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="MV", VALUE=obj%rhs_v1(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="KV", VALUE=obj%rhs_v1(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="F1", VALUE=obj%rhs_f1, &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="F2", VALUE=obj%rhs_f2, &
                  default_value=0.0_DFP, origin=origin, stat=stat)
  END IF

  node2 => NULL()
  CALL toml_get(node, "dis", node2, origin=origin, requested=.FALSE., &
                stat=stat)

  obj%dis = 0.0_DFP
  isok = ASSOCIATED(node2)

  IF (isok) THEN
    CALL GetValue(table=node2, key="U1", VALUE=obj%dis(1), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="V1", VALUE=obj%dis(2), &
                  default_value=0.0_DFP, origin=origin, stat=stat)

    CALL GetValue(table=node2, key="U2", VALUE=obj%dis(3), &
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

    CALL GetValue(table=node2, key="U2", VALUE=obj%vel(3), &
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
              default_value="ALPH", origin=origin, stat=stat, &
              isfound=found)

#ifdef DEBUG_VER
CALL AssertError1(found, myName, &
     'Cannot find timeIntegration field in toml table. &
     &timeIntegration specifies the name of algorithm.')
#endif

obj%name = UpperCase(astr%slice(1, 4))

SELECT CASE (obj%name)

CASE ("ALPH")
  CALL AlphaMethodImportFromToml(obj=obj, table=table, astr=astr, &
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

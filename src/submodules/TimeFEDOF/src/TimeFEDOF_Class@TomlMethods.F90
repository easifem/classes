! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
! Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

SUBMODULE(TimeFEDOF_Class) TomlMethods
USE TomlUtility, ONLY: GetValue
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_serialize
USE FEFactoryUtility, ONLY: OneDimFEFactory

#ifdef DEBUG_VER
USE Display_Method, ONLY: Display
#endif

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                             ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: origin, stat

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

obj%isInit = .TRUE.
obj%opt => timeOpt

CALL ImportCellOrderFromToml(obj=obj, table=table, origin=origin, stat=stat)
CALL ImportScaleForQuadOrderFromToml(obj=obj, table=table, &
                                     origin=origin, stat=stat)
obj%tdof = obj%cellOrder + 1

obj%fe => OneDimFEFactory(table=table)

#ifdef DEBUG_VER
isok = ASSOCIATED(obj%fe)
CALL AssertError1(isok, myName, &
                  'fe pointer returned from OneDimFEFactory is NULL pointer.')
#endif

CALL obj%fe%ImportFromToml(table=table)

obj%baseInterpolation = obj%fe%GetBaseInterpolation()
obj%baseContinuity = obj%fe%GetBaseContinuity()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
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
                  'following error occured while reading '// &
             'the toml file :: cannot find ['//tomlName//"] table in config.")
#endif

CALL obj%ImportFromToml(table=node, timeOpt=timeOpt)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                    ImportCellOrderFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportCellOrderFromToml(obj, table, origin, stat)
  CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! Internal variables
  LOGICAL(LGT) :: isFound
  INTEGER(I4B) :: cellOrder

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportCellOrderFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading islocal...')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                    'Reading order...')
#endif

  CALL GetValue(table=table, key="order", VALUE=cellOrder, &
                origin=origin, stat=stat, isFound=isFound, &
                default_value=0_I4B)

  obj%cellOrder = INT(cellOrder, kind=INT8)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportCellOrderFromToml

!----------------------------------------------------------------------------
!                                            ImportScaleForQuadOrderFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportScaleForQuadOrderFromToml(obj, table, origin, stat)
  CLASS(TimeFEDOF_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat

  ! internal variables
  INTEGER(I4B) :: scaleForQuadOrder
  LOGICAL(LGT) :: isFound
  INTEGER(I4B), PARAMETER :: default_value = 2

#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportScaleForQuadOrderFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  CALL GetValue( &
    table=table, key="scaleForQuadOrder", VALUE=scaleForQuadOrder, &
    default_value=default_value, origin=origin, stat=stat, isFound=isFound)

#ifdef DEBUG_VER
  IF (.NOT. isFound) THEN
    CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                      'scaleForQuadOrder not found, using default value 2')
  END IF
#endif

  obj%scaleForQuadOrder = INT(scaleForQuadOrder, kind=INT8)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportScaleForQuadOrderFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

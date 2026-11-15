! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
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

!> authors: Vikas Sharma, Ph. D.
! date: 28 Aug 2021
! summary: This module defines a data type for mesh selection

SUBMODULE(MeshSelection_Class) TomlMethods
USE BoundingBox_Method, ONLY: BoundingBox_GetValue => GetValue
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: EqualLine
USE Display_Method, ONLY: Tostring
USE GlobalData, ONLY: CHAR_LF
USE GlobalData, ONLY: stdout
USE tomlf, ONLY: toml_get => get_value
USE tomlf, ONLY: toml_serialize
USE TomlUtility, ONLY: GetValue

IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "MeshSelection_Class"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                            ReadIsSelectionByMeshIDFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsSelectionByMeshIDFromToml(obj, table, dom)
  TYPE(MeshSelection_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsSelectionByMeshIDFromToml()"
#endif
  INTEGER(I4B) :: origin, STAT
  INTEGER(I4B), ALLOCATABLE :: aintvec(:)
  LOGICAL(LGT), PARAMETER :: default_value = .FALSE.
  TYPE(toml_table), POINTER :: node
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isSelectionByMeshID ...')
#endif

  CALL GetValue(table=table, key="isSelectionByMeshID", &
                VALUE=obj%ms(1), default_value=default_value, origin=origin, &
                STAT=STAT)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading meshID ...')
#endif

  node => NULL()
  CALL toml_get(table, "meshID", node, origin=origin, STAT=STAT, &
                requested=.FALSE.)

  isok = ASSOCIATED(node)

#ifdef DEBUG_VER
  IF (obj%ms(1)) THEN
    CALL AssertError1(isok, myName, &
      'You have set isSelectionByMeshID = .TRUE. but you have not &
          &provided meshID table.')
  END IF
#endif

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  ! If you dont provide isSelectionByMeshID but you
  ! so provide meshID table, then we enforce isSelectionByMeshID = .true.
  obj%ms(1) = .TRUE.

  ! read point
  CALL GetValue(table=node, key="point", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=0_I4B, meshID=aintvec, dom=dom)

  ! read line
  CALL GetValue(table=node, key="line", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=1_I4B, meshID=aintvec, dom=dom)

  ! read surface
  CALL GetValue(table=node, key="surface", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=2_I4B, meshID=aintvec, dom=dom)

  ! read volume
  CALL GetValue(table=node, key="volume", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=3_I4B, meshID=aintvec, dom=dom)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIsSelectionByMeshIDFromToml

!----------------------------------------------------------------------------
!                                            ReadIsSelectionByElemNumFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsSelectionByElemNumFromToml(obj, table, dom)
  TYPE(MeshSelection_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsSelectionByElemNumFromToml()"
#endif
  INTEGER(I4B) :: origin, STAT
  INTEGER(I4B), ALLOCATABLE :: aintvec(:)
  LOGICAL(LGT), PARAMETER :: default_value = .FALSE.
  TYPE(toml_table), POINTER :: node
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isSelectionByElemNum ...')
#endif

  CALL GetValue(table=table, key="isSelectionByElemNum", &
                VALUE=obj%ms(2), default_value=default_value, origin=origin, &
                STAT=STAT)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading elemNum ...')
#endif

  node => NULL()
  CALL toml_get(table, "elemNum", node, origin=origin, STAT=STAT, &
                requested=.FALSE.)

  isok = ASSOCIATED(node)

#ifdef DEBUG_VER
  IF (obj%ms(2)) THEN
    CALL AssertError1(isok, myName, &
      'You have set isSelectionByElemNum = .TRUE. but you have not &
          &provided elemNum table.')
  END IF
#endif

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  obj%ms(2) = .TRUE.

  ! read points
  CALL GetValue(table=node, key="point", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=0_I4B, elemNum=aintvec, dom=dom)

  ! read lines
  CALL GetValue(table=node, key="line", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=1_I4B, elemNum=aintvec, dom=dom)

  ! read surfaces
  CALL GetValue(table=node, key="surface", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=2_I4B, elemNum=aintvec, dom=dom)

  ! read volumes
  CALL GetValue(table=node, key="volume", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=3_I4B, elemNum=aintvec, dom=dom)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIsSelectionByElemNumFromToml

!----------------------------------------------------------------------------
!                                           ReadIsSelectionByNodeNumFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsSelectionByNodeNumFromToml(obj, table, dom)
  TYPE(MeshSelection_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsSelectionByNodeNumFromToml()"
#endif

  INTEGER(I4B) :: origin, STAT
  INTEGER(I4B), ALLOCATABLE :: aintvec(:)
  LOGICAL(LGT), PARAMETER :: default_value = .FALSE.
  TYPE(toml_table), POINTER :: node
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isSelectionByNodeNum ...')
#endif

  CALL GetValue(table=table, key="isSelectionByNodeNum", &
                VALUE=obj%ms(3), default_value=default_value, origin=origin, &
                STAT=STAT)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading nodeNum ...')
#endif

  node => NULL()
  CALL toml_get(table, "nodeNum", node, origin=origin, STAT=STAT, &
                requested=.FALSE.)

  isok = ASSOCIATED(node)

#ifdef DEBUG_VER
  IF (obj%ms(3)) THEN
    CALL AssertError1(isok, myName, &
      'You have set isSelectionByNodeNum = .TRUE. but you have not &
          &provided elemNum table.')
  END IF
#endif

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  obj%ms(3) = .TRUE.

  ! read points
  CALL GetValue(table=node, key="point", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=0_I4B, nodeNum=aintvec, dom=dom)

  ! read lines
  CALL GetValue(table=node, key="line", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=1_I4B, nodeNum=aintvec, dom=dom)

  ! read surfaces
  CALL GetValue(table=node, key="surface", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=2_I4B, nodeNum=aintvec, dom=dom)

  ! read volumes
  CALL GetValue(table=node, key="volume", VALUE=aintvec, &
                origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=3_I4B, nodeNum=aintvec, dom=dom)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIsSelectionByNodeNumFromToml

!----------------------------------------------------------------------------
!                                                ReadIsSelectionByBoxFromToml
!----------------------------------------------------------------------------

SUBROUTINE ReadIsSelectionByBoxFromToml(obj, table, dom)
  TYPE(MeshSelection_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table
  CLASS(AbstractDomain_), OPTIONAL, INTENT(IN) :: dom

  ! Internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ReadIsSelectionByBoxFromToml()"
#endif
  INTEGER(I4B) :: origin, STAT
  LOGICAL(LGT), PARAMETER :: default_value = .FALSE.
  TYPE(toml_table), POINTER :: node
  TYPE(BoundingBox_), ALLOCATABLE :: box(:)
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading isSelectionByBox ...')
#endif

  CALL GetValue(table=table, key="isSelectionByBox", &
                VALUE=obj%ms(4), default_value=default_value, origin=origin, &
                STAT=STAT)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          'Reading box ...')
#endif

  node => NULL()
  CALL toml_get(table, "box", node, origin=origin, STAT=STAT, &
                requested=.FALSE.)

  isok = ASSOCIATED(node)

#ifdef DEBUG_VER
  IF (obj%ms(4)) THEN
    CALL AssertError1(isok, myName, &
    'You have set isSelectionByBox = .TRUE. but you have not provided &
    &[box] table.')
  END IF
#endif

  IF (.NOT. isok) THEN
#ifdef DEBUG_VER
    CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                            '[END] ')
#endif
    RETURN
  END IF

  obj%ms(4) = .TRUE.

  ! read points
  CALL BoundingBox_GetValue(table=node, key="point", VALUE=box, &
                            origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=0_I4B, box=box, dom=dom)

  ! read lines
  CALL BoundingBox_GetValue(table=node, key="line", VALUE=box, &
                            origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=1_I4B, box=box, dom=dom)

  ! read surfaces
  CALL BoundingBox_GetValue(table=node, key="surface", VALUE=box, &
                            origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=2_I4B, box=box, dom=dom)

  ! read volumes
  CALL BoundingBox_GetValue(table=node, key="volume", VALUE=box, &
                            origin=origin, STAT=STAT, isFound=isok)
  IF (isok) CALL obj%Add(dim=3_I4B, box=box, dom=dom)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ReadIsSelectionByBoxFromToml

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
CALL ReadIsSelectionByMeshIDFromToml(obj=obj, table=table)
CALL ReadIsSelectionByNodeNumFromToml(obj=obj, table=table)
CALL ReadIsSelectionByBoxFromToml(obj=obj, table=table)
CALL ReadIsSelectionByElemNumFromToml(obj=obj, table=table)
CALL obj%Set()

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                              ImportFromToml
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
LOGICAL(LGT) :: isok
#endif
TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, STAT

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START]')
#endif

CALL GetValue(table=table, afile=afile, filename=filename)

node => NULL()
CALL toml_get(table, tomlName, node, origin=origin, requested=.FALSE., &
              STAT=STAT)

#ifdef DEBUG_VER
isok = ASSOCIATED(node)
CALL AssertError1(isok, myName, &
                  'Cannot find ['//tomlName//'] table in config.')
#endif

CALL obj%ImportFromToml(table=node)

node => NULL()
DEALLOCATE (table)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END]')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                              Include error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

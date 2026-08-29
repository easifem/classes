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

SUBMODULE(OneDimDomain_Class) TomlMethods
USE TomlUtility, ONLY: GetValue
USE TomlUtility, ONLY: GetValue_
USE tomlf, ONLY: toml_get => get_value
USE Display_Method, ONLY: Display
! USE String_Class, ONLY: String
! USE FEFactoryUtility, ONLY: OneDimFEFactory
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = &
                           "OneDimDomain_Class@TomlMethods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml2()"
#endif

TYPE(toml_table), ALLOCATABLE :: table
TYPE(toml_table), POINTER :: node
INTEGER(I4B) :: origin, stat
LOGICAL(LGT) :: isok

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
IF (.NOT. isok) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    'following error occured while reading '// &
                    'the toml file :: cannot find ['//tomlName// &
                    "] table in config.")
END IF
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
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

INTEGER(I4B) :: origin, stat, case_id, totalElements
REAL(DFP) :: domain(2)
REAL(DFP), ALLOCATABLE :: elemLength(:)
LOGICAL(LGT) :: isElemLength, isDomain, isTotalElements, &
                isElemLengthUniform, abool

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()

CALL ImportDomainFromToml(table=table, domain=domain, origin=origin, &
                          stat=stat, isDomain=isDomain)

CALL ImportTotalElementsFromToml(table=table, totalElements=totalElements, &
                                 origin=origin, stat=stat, &
                                 isTotalElements=isTotalElements)

CALL ImportElemLengthFromToml(table=table, elemLength=elemLength, &
                              origin=origin, stat=stat, &
                              isElemLength=isElemLength, &
                              isElemLengthUniform=isElemLengthUniform)

case_id = 0

abool = isDomain .AND. isTotalElements
IF (abool) case_id = 1

abool = isDomain .AND. isElemLength .AND. isElemLengthUniform
IF (abool) case_id = 2

abool = isDomain .AND. isTotalElements .AND. (.NOT. isElemLengthUniform)
IF (abool) case_id = 3

SELECT CASE (case_id)
CASE (1)
  CALL obj%Initiate(domain=domain, totalElements=totalElements)
CASE (2)
  CALL obj%Initiate(domain=domain, elemLength=elemLength(1))
CASE (3)
  CALL obj%Initiate(domain=domain, totalElements=totalElements, &
                    elemLength=elemLength)
CASE DEFAULT

#ifdef DEBUG_VER
  CALL AssertError1(math%no, myName, "No case found for case_id.")
#endif

END SELECT

abool = ALLOCATED(elemLength)
IF (abool) DEALLOCATE (elemLength)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                       ImportDomainFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportDomainFromToml(table, domain, origin, stat, isDomain)
  REAL(DFP), INTENT(INOUT) :: domain(2)
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  LOGICAL(LGT), INTENT(INOUT) :: isDomain

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportDomainFromToml()"
#endif
  INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isDomain = math%no

  CALL GetValue_(table=table, key="domain", VALUE=domain, &
                 tsize=tsize, origin=origin, stat=stat, &
                 isFound=isDomain)

#ifdef DEBUG_VER
  CALL AssertError1(isDomain, myName, "domain not found.")
#endif

#ifdef DEBUG_VER
  CALL AssertError2(tsize, math%two_i, myName, &
                    "a=tsize, b=2")
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportDomainFromToml

!----------------------------------------------------------------------------
!                                                ImportTotalElementsFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportTotalElementsFromToml(table, totalElements, origin, stat, &
                                       isTotalElements)
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  INTEGER(I4B), INTENT(INOUT) :: totalElements
  LOGICAL(LGT), INTENT(INOUT) :: isTotalElements

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportTotalElementsFromToml()"
#endif

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isTotalElements = math%no

  CALL GetValue(table=table, key="totalElements", &
                VALUE=totalElements, &
                default_value=math%zero_i, &
                origin=origin, stat=stat, isFound=isTotalElements)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportTotalElementsFromToml

!----------------------------------------------------------------------------
!                                                  ImportTotalNodesFromToml
!----------------------------------------------------------------------------

SUBROUTINE ImportElemLengthFromToml(table, elemLength, origin, stat, &
                                    isElemLength, isElemLengthUniform)
  TYPE(toml_table), INTENT(INOUT) :: table
  INTEGER(I4B), INTENT(INOUT) :: origin, stat
  REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: elemLength(:)
  LOGICAL(LGT), INTENT(INOUT) :: isElemLength, isElemLengthUniform

  ! Define internal variables
#ifdef DEBUG_VER
  CHARACTER(*), PARAMETER :: myName = "ImportElemLengthFromToml()"
#endif
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START] ')
#endif

  isElemLength = math%no
  isElemLengthUniform = math%no

  CALL GetValue(table=table, key="elemLength", &
                VALUE=elemLength, &
                origin=origin, stat=stat, &
                isFound=isElemLength, &
                isScalar=isElemLengthUniform)

  IF (isElemLength) THEN
    isok = ALLOCATED(elemLength)
#ifdef DEBUG_VER
    CALL AssertError1(isok, myName, "elemLength not allocated.")
#endif
    IF (SIZE(elemLength) .EQ. 1) isElemLengthUniform = math%yes
  END IF

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
END SUBROUTINE ImportElemLengthFromToml

!----------------------------------------------------------------------------
!                                                                     Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE TomlMethods

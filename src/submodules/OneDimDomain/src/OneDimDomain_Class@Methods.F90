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

SUBMODULE(OneDimDomain_Class) Methods
USE GlobalData, ONLY: CHAR_LF
USE Display_Method, ONLY: ToString, Display
USE ReallocateUtility, ONLY: Reallocate

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Set all parameters of the object

MODULE PROCEDURE obj_SetParam
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetParam()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (PRESENT(domain)) CALL obj_SetDomain(obj, domain)
IF (PRESENT(totalElements)) CALL obj_SetTotalElements(obj, totalElements)
IF (PRESENT(totalNodes)) CALL obj_SetTotalNodes(obj, totalNodes)
IF (PRESENT(elemLength)) CALL obj_SetElemLength1(obj, elemLength)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetParam

!----------------------------------------------------------------------------
!                                                                  SetDomain
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetDomain
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetDomain()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%domain = domain

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetDomain

!----------------------------------------------------------------------------
!                                                           SetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalElements()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%totalElements = totalElements

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTotalElements

!----------------------------------------------------------------------------
!                                                              SetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetTotalNodes()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%totalNodes = totalNodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetTotalNodes

!----------------------------------------------------------------------------
!                                                             SetElemLength1
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemLength1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetElemLength1()"
LOGICAL(LGT) :: isok
#endif

INTEGER(I4B) :: tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

tsize = SIZE(VALUE)

#ifdef DEBUG_VER
isok = .NOT. ALLOCATED(obj%elemLength)
CALL AssertError1(isok, myName, &
               "elemLength array si already allocated. Call deallocate first")

IF (tsize .NE. 1) THEN
  isok = tsize .EQ. obj%totalElements
  CALL AssertError1(isok, myName, &
                    "size of value is not equal to 1, "//CHAR_LF// &
              " in this case size of value should be equal to totalElements" &
                    //CHAR_LF//" tsize = "//ToString(tsize)// &
                    " totalElements = "//ToString(tsize))
END IF
#endif

CALL Reallocate(obj%elemLength, tsize)

obj%elemLength(1:tsize) = VALUE(1:tsize)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_SetElemLength1

!----------------------------------------------------------------------------
!                                                             SetElemLength2
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetElemLength2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetElemLength2()"
#endif

LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
isok = indx .LE. obj%totalElements
CALL AssertError1(isok, myName, &
                  "Index out of bounds: indx = "//ToString(indx))

isok = ALLOCATED(obj%elemLength)
CALL AssertError1(isok, myName, &
                  "elemLength array is not allocated")
#endif

obj%elemLength(indx) = VALUE

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetElemLength2

!----------------------------------------------------------------------------
!                                                                 GetDomain
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetDomain
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetDomain()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans(1:2) = obj%domain(1:2)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetDomain

!----------------------------------------------------------------------------
!                                                            GetTotalElements
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalElements
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalElements()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%totalElements

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalElements

!----------------------------------------------------------------------------
!                                                             GetTotalNodes
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalNodes()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%totalNodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalNodes

!----------------------------------------------------------------------------
!                                                             GetElemLength
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetElemLength
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetElemLength()"
LOGICAL(LGT) :: isok
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

IF (obj%isElemLengthUniform) THEN
  ans = obj%elemLength(1)

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif

  RETURN
END IF

#ifdef DEBUG_VER
isok = indx .LE. obj%totalElements
CALL AssertError1(isok, "obj_GetElemLength", &
                  "Index out of bounds: indx = "//ToString(indx))
#endif

ans = obj%elemLength(indx)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_GetElemLength

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%domain, "domain: ", unitno=unitno)
CALL Display(obj%totalNodes, "totalNodes: ", unitno=unitno)
CALL Display(obj%isElemLengthUniform, "isElemLengthUniform: ", &
             unitno=unitno)
CALL Display(obj%totalElements, "totalElements: ", unitno=unitno)

isok = ALLOCATED(obj%elemLength)
IF (isok) THEN
  CALL Display(isok, "elemLength is allocated: ", unitno=unitno)
  aint = SIZE(obj%elemLength)
  CALL Display(aint, "elemLength size: ", unitno=unitno)
  CALL Display(obj%elemLength, "elemLength: ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                            DisplayMeshInfo
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_DisplayMeshInfo
! Internal variables
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_DisplayMeshInfo()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: aint

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(msg, unitno=unitno)
CALL Display(obj%domain, "domain: ", unitno=unitno)
CALL Display(obj%totalNodes, "totalNodes: ", unitno=unitno)
CALL Display(obj%isElemLengthUniform, "isElemLengthUniform: ", &
             unitno=unitno)
CALL Display(obj%totalElements, "totalElements: ", unitno=unitno)

isok = ALLOCATED(obj%elemLength)
IF (isok) THEN
  CALL Display(isok, "elemLength is allocated: ", unitno=unitno)
  aint = SIZE(obj%elemLength)
  CALL Display(aint, "elemLength size: ", unitno=unitno)
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_DisplayMeshInfo

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_ImportFromToml2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_ImportFromToml1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
obj%isElemLengthUniform = .FALSE.
obj%domain = 0.0_DFP
obj%totalElements = 0
obj%totalNodes = 0
IF (ALLOCATED(obj%elemLength)) DEALLOCATE (obj%elemLength)
obj%xij = 0.0_DFP
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                           IsElementPresent
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_IsElementPresent
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_IsElementPresent()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = globalElement .LE. obj%totalElements

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalElemNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = globalElement

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalElemNumber

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetLocalNodeNumber
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetLocalNodeNumber

!----------------------------------------------------------------------------
!                                                          GetTotalVertexDOF
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetTotalVertexNodes
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetTotalVertexNodes()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

ans = obj%totalNodes

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetTotalVertexNodes

!----------------------------------------------------------------------------
!                                                           GetConnectivity_
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetConnectivity_
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_GetConnectivity_()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
                  '[WIP ERROR] :: This routine is under development')

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_GetConnectivity_

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate1()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%domain(1:2) = domain(1:2)
obj%totalElements = totalElements
obj%totalNodes = totalElements + 1
obj%isElemLengthUniform = .TRUE.
CALL Reallocate(obj%elemLength, 1)
obj%elemLength(1) = (domain(2) - domain(1)) / REAL(totalElements, kind=DFP)
obj%xij = 0.0_DFP

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate1

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate2()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%domain(1:2) = domain(1:2)
obj%isElemLengthUniform = .TRUE.
CALL Reallocate(obj%elemLength, 1)
obj%elemLength(1) = elemLength
obj%totalElements = INT((domain(2) - domain(1)) / elemLength, kind=I4B)
obj%totalNodes = obj%totalElements + 1
obj%xij = 0.0_DFP

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate2

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Initiate3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Initiate3()"
#endif

INTEGER(I4B) :: ii, tsize

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL obj%DEALLOCATE()
obj%isInit = .TRUE.
obj%domain(1:2) = domain(1:2)
obj%totalElements = totalElements
obj%totalNodes = obj%totalElements + 1
obj%xij = 0.0_DFP

tsize = SIZE(elemLength)

IF (tsize .EQ. 1) THEN
  CALL Reallocate(obj%elemLength, 1)
  obj%isElemLengthUniform = .TRUE.
  obj%elemLength(1) = elemLength(1)
  RETURN
END IF

CALL Reallocate(obj%elemLength, totalElements)
obj%isElemLengthUniform = .FALSE.

DO ii = 1, totalElements
  obj%elemLength(ii) = elemLength(ii)
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Initiate3

!----------------------------------------------------------------------------
!                                                                     Error
!----------------------------------------------------------------------------

INCLUDE "../../include/errors.F90"

END SUBMODULE Methods

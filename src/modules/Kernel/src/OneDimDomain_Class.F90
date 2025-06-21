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

MODULE OneDimDomain_Class
USE GlobalData, ONLY: I4B, DFP, LGT, CHAR_LF
USE Display_Method, ONLY: ToString, Display
USE ExceptionHandler_Class, ONLY: e
USE ReallocateUtility, ONLY: Reallocate
USE TxtFile_Class, ONLY: TxtFile_
USE tomlf, ONLY: toml_table

IMPLICIT NONE
PRIVATE
PUBLIC :: OneDimDomain_
CHARACTER(*), PARAMETER :: modName = "OneDimDomain_Class"
INTEGER(I4B), PARAMETER :: MAX_ORDER = 51
!! maximum order of lagrange polynomial in an element of mesh

!----------------------------------------------------------------------------
!                                                             OneDimDomain_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: This class contains the one dimensional domain

TYPE :: OneDimDomain_
  PRIVATE
  LOGICAL(LGT) :: isElemLengthUniform = .FALSE.
  !! is the element length uniform in the domain
  REAL(DFP) :: domain(2) = 0.0_DFP
  !! domain(1) is the start point
  !! domain(2) is the end point
  INTEGER(I4B) :: totalElements = 0
  !! total number of elements in the domain
  INTEGER(I4B) :: totalNodes = 0
  !! total number of nodes in the domain
  REAL(DFP), ALLOCATABLE :: elemLength(:)
  !! length of each element
  !! the size should be equal to totalElements
  !! When isElemLengthUniform is true, then
  !! size of elemLength is 1
  REAL(DFP) :: xij(1, MAX_ORDER + 1) = 0.0_DFP
  !! nodal coordinates in an element of mesh

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import parameters from a TOML file
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import parameters from a TOML file with a different structure
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, &
    ImportFromToml2
  !! Generic method for importing from toml files
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the object

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  !! Set the parameters of the object
  PROCEDURE, PUBLIC, PASS(obj) :: SetDomain => obj_SetDomain
  !! Set obj%domain
  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalElements => obj_SetTotalElements
  !! Set obj%totalElements
  PROCEDURE, PUBLIC, PASS(obj) :: SetTotalNodes => obj_SetTotalNodes
  !! Set obj%totalNodes
  PROCEDURE, PASS(obj) :: SetElemLength1 => obj_SetElemLength1
  !! Set entire elemLength array
  !! obj%elemLength = value
  PROCEDURE, PASS(obj) :: SetElemLength2 => obj_SetElemLength2
  !! Set a single element length
  !! obj%elemLength(indx) = value
  GENERIC, PUBLIC :: SetElemLength => SetElemLength1, &
    SetElemLength2

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetDomain => obj_GetDomain
  !! Get the domain of the object
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalElements => obj_GetTotalElements
  !! Get the total number of elements in the object
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalNodes => obj_GetTotalNodes
  !! Get the total number of nodes in the object
  PROCEDURE, PUBLIC, PASS(obj) :: GetElemLength => obj_GetElemLength
  !! Get an entry from the  element length array

  ! IO:
  ! @IOMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of the object
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshInfo => obj_DisplayMeshInfo
  !! Display mesh info
END TYPE OneDimDomain_

CONTAINS

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Set all parameters of the object

SUBROUTINE obj_SetParam(obj, domain, totalElements, totalNodes, elemLength)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  REAL(DFP), OPTIONAL, INTENT(IN) :: domain(2)
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: totalElements
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: totalNodes
  REAL(DFP), OPTIONAL, INTENT(IN) :: elemLength(:)

  IF (PRESENT(domain)) CALL obj_SetDomain(obj, domain)
  IF (PRESENT(totalElements)) CALL obj_SetTotalElements(obj, totalElements)
  IF (PRESENT(totalNodes)) CALL obj_SetTotalNodes(obj, totalNodes)
  IF (PRESENT(elemLength)) CALL obj_SetElemLength1(obj, elemLength)
END SUBROUTINE obj_SetParam

!----------------------------------------------------------------------------
!                                                                  SetDomain
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  This method sets the domain of the object

SUBROUTINE obj_SetDomain(obj, domain)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: domain(2)
  obj%domain = domain
END SUBROUTINE obj_SetDomain

!----------------------------------------------------------------------------
!                                                           SetTotalElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Set obj%totalElements

SUBROUTINE obj_SetTotalElements(obj, totalElements)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: totalElements
  obj%totalElements = totalElements
END SUBROUTINE obj_SetTotalElements

!----------------------------------------------------------------------------
!                                                              SetTotalNodes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Set obj%totalNodes

SUBROUTINE obj_SetTotalNodes(obj, totalNodes)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: totalNodes
  obj%totalNodes = totalNodes
END SUBROUTINE obj_SetTotalNodes

!----------------------------------------------------------------------------
!                                                             SetElemLength1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Set the entire elemLength array

SUBROUTINE obj_SetElemLength1(obj, VALUE)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  REAL(DFP), INTENT(IN) :: VALUE(:)

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

END SUBROUTINE obj_SetElemLength1

!----------------------------------------------------------------------------
!                                                             SetElemLength2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Set a single element length

SUBROUTINE obj_SetElemLength2(obj, indx, VALUE)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  INTEGER(I4B), INTENT(IN) :: indx
  REAL(DFP), INTENT(IN) :: VALUE

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

END SUBROUTINE obj_SetElemLength2

!----------------------------------------------------------------------------
!                                                                 GetDomain
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Get the domain of the object

FUNCTION obj_GetDomain(obj) RESULT(ans)
  CLASS(OneDimDomain_), INTENT(IN) :: obj
  REAL(DFP) :: ans(2)
  ans(1:2) = obj%domain(1:2)
END FUNCTION obj_GetDomain

!----------------------------------------------------------------------------
!                                                            GetTotalElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Get the tatal number of elements

FUNCTION obj_GetTotalElements(obj) RESULT(ans)
  CLASS(OneDimDomain_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%totalElements
END FUNCTION obj_GetTotalElements

!----------------------------------------------------------------------------
!                                                             GetTotalNodes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Get the total number of nodes

FUNCTION obj_GetTotalNodes(obj) RESULT(ans)
  CLASS(OneDimDomain_), INTENT(IN) :: obj
  INTEGER(I4B) :: ans
  ans = obj%totalNodes
END FUNCTION obj_GetTotalNodes

!----------------------------------------------------------------------------
!                                                             GetElemLength
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Get an entry from the element length array

FUNCTION obj_GetElemLength(obj, indx) RESULT(ans)
  CLASS(OneDimDomain_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: indx
  REAL(DFP) :: ans

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

END FUNCTION obj_GetElemLength

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Display the contents of the object

SUBROUTINE obj_Display(obj, msg, unitno)
  CLASS(OneDimDomain_), INTENT(IN) :: obj
  CHARACTER(*), OPTIONAL, INTENT(IN) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  CALL Display(msg, unitno=unitno)
  CALL Display(obj%domain, "domain: ", unitno=unitno)
  CALL Display(obj%totalNodes, "totalNodes: ", unitno=unitno)
  CALL Display(obj%isElemLengthUniform, "isElemLengthUniform: ", &
               unitno=unitno)
  CALL Display(obj%totalElements, "totalElements: ", unitno=unitno)
  CALL Display(obj%elemLength, "elemLength: ", unitno=unitno)

END SUBROUTINE obj_Display

!----------------------------------------------------------------------------
!                                                            DisplayMeshInfo
!----------------------------------------------------------------------------

SUBROUTINE obj_DisplayMeshInfo(obj, msg, unitno)
  CLASS(OneDimDomain_), INTENT(in) :: obj
  CHARACTER(*), OPTIONAL, INTENT(in) :: msg
  INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_DisplayMeshInfo()"

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

END SUBROUTINE obj_DisplayMeshInfo

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Initialize by importing data from toml config

SUBROUTINE obj_ImportFromToml1(obj, table)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  TYPE(toml_table), INTENT(INOUT) :: table

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

END SUBROUTINE obj_ImportFromToml1

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Initialize by importing data from toml config with a different structure

SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, printToml)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  CHARACTER(*), INTENT(IN) :: tomlName
  TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
  CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml

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
END SUBROUTINE obj_ImportFromToml2

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

SUBROUTINE obj_Deallocate(obj)
  CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  obj%isElemLengthUniform = .FALSE.
  obj%domain = 0.0_DFP
  obj%totalElements = 0
  obj%totalNodes = 0
  IF (ALLOCATED(obj%elemLength)) DEALLOCATE (obj%elemLength)
  obj%xij = 0.0_DFP
END SUBROUTINE obj_Deallocate

!----------------------------------------------------------------------------
!                                                           IsElementPresent
!----------------------------------------------------------------------------

SUBROUTINE obj_IsElementPresent(obj, globalElement, isPresent)
  CLASS(OneDimDomain_), INTENT(in) :: obj
  INTEGER(I4B), INTENT(IN) :: globalElement
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPresent

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName = "obj_IsElementPresent()"

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')

END SUBROUTINE obj_IsElementPresent

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

FUNCTION obj_GetLocalElemNumber(obj, globalElement, islocal) RESULT(ans)
  CLASS(OneDimDomain_), INTENT(IN) :: obj
  INTEGER(I4B), INTENT(IN) :: globalElement
  !! global element number
  LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  !! if islocal = .true. then global element number is local
  INTEGER(I4B) :: ans
  !! local element number

  CHARACTER(*), PARAMETER :: myName = "obj_GetLocalElemNumber()"

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[WIP ERROR] :: This routine is under development')
END FUNCTION obj_GetLocalElemNumber

!----------------------------------------------------------------------------
!                                                                     Error
!----------------------------------------------------------------------------

INCLUDE "../../../submodules/include/errors.F90"

END MODULE OneDimDomain_Class

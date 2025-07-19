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
USE GlobalData, ONLY: I4B, DFP, LGT
USE ExceptionHandler_Class, ONLY: e
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
  LOGICAL(LGT) :: isInit = .FALSE.
  !! is the object initialized
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
  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate the object with totalElements
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate the object with elemLength
  PROCEDURE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate the object with totalElements and elemLength
  GENERIC, PUBLIC :: Initiate => Initiate1, &
    Initiate2, Initiate3

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
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalElemNumber => obj_GetLocalElemNumber
  !! Get the local element number from the global element number
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalNodeNumber => obj_GetLocalNodeNumber
  !! Get the local node number from the global node number
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertexNodes => &
    obj_GetTotalVertexNodes
  !! Get the total number of vertex in object
  PROCEDURE, PUBLIC, PASS(obj) :: GetConnectivity_ => obj_GetConnectivity_
  !! Get connectivity of the element without any allocation
  PROCEDURE, PUBLIC, PASS(obj) :: IsElementPresent => obj_IsElementPresent

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the contents of the object
  PROCEDURE, PUBLIC, PASS(obj) :: DisplayMeshInfo => obj_DisplayMeshInfo
  !! Display mesh info
END TYPE OneDimDomain_

!----------------------------------------------------------------------------
!                                                                 SetParam
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Set all parameters of the object

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, domain, totalElements, totalNodes, &
                                 elemLength)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: domain(2)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: totalElements
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: totalNodes
    REAL(DFP), OPTIONAL, INTENT(IN) :: elemLength(:)
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  SetDomain
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  This method sets the domain of the object

INTERFACE
  MODULE SUBROUTINE obj_SetDomain(obj, domain)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: domain(2)
  END SUBROUTINE obj_SetDomain
END INTERFACE

!----------------------------------------------------------------------------
!                                                           SetTotalElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Set obj%totalElements

INTERFACE
  MODULE SUBROUTINE obj_SetTotalElements(obj, totalElements)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: totalElements
  END SUBROUTINE obj_SetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                              SetTotalNodes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Set obj%totalNodes

INTERFACE
  MODULE SUBROUTINE obj_SetTotalNodes(obj, totalNodes)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: totalNodes
  END SUBROUTINE obj_SetTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                             SetElemLength1
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Set the entire elemLength array

INTERFACE
  MODULE SUBROUTINE obj_SetElemLength1(obj, VALUE)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
  END SUBROUTINE obj_SetElemLength1
END INTERFACE

!---------------------------------------------------------------------------
!                                                             SetElemLength2
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Set a single element length

INTERFACE
  MODULE SUBROUTINE obj_SetElemLength2(obj, indx, VALUE)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
  END SUBROUTINE obj_SetElemLength2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 GetDomain
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Get the domain of the object

INTERFACE
  MODULE FUNCTION obj_GetDomain(obj) RESULT(ans)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    REAL(DFP) :: ans(2)
  END FUNCTION obj_GetDomain
END INTERFACE

!----------------------------------------------------------------------------
!                                                            GetTotalElements
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Get the tatal number of elements

INTERFACE
  MODULE FUNCTION obj_GetTotalElements(obj) RESULT(ans)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetTotalNodes
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Get the total number of nodes

INTERFACE
  MODULE FUNCTION obj_GetTotalNodes(obj) RESULT(ans)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                             GetElemLength
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Get an entry from the element length array

INTERFACE
  MODULE FUNCTION obj_GetElemLength(obj, indx) RESULT(ans)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP) :: ans
  END FUNCTION obj_GetElemLength
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Display the contents of the object

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    CHARACTER(*), OPTIONAL, INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                            DisplayMeshInfo
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_DisplayMeshInfo(obj, msg, unitno)
    CLASS(OneDimDomain_), INTENT(in) :: obj
    CHARACTER(*), OPTIONAL, INTENT(in) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_DisplayMeshInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Initialize by importing data from toml config

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary: Initialize by importing data from toml config with a different structure

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2(obj, tomlName, afile, filename, &
                                        printToml)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                           IsElementPresent
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_IsElementPresent(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsElementPresent
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetLocalElemNumber(obj, globalElement, islocal) &
    RESULT(ans)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal = .true. then global element number is local
    INTEGER(I4B) :: ans
    !! local element number
  END FUNCTION obj_GetLocalElemNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetLocalElemNumber
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetLocalNodeNumber(obj, globalNode, islocal) &
    RESULT(ans)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global element number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if islocal = .true. then global element number is local
    INTEGER(I4B) :: ans
    !! local element number
  END FUNCTION obj_GetLocalNodeNumber
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetTotalVertexDOF
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexNodes(obj) RESULT(ans)
    CLASS(OneDimDomain_), INTENT(in) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalVertexNodes
END INTERFACE

!----------------------------------------------------------------------------
!                                                           GetConnectivity_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-21
! summary:  Get connectivity of the element without any allocation

INTERFACE
  MODULE SUBROUTINE obj_GetConnectivity_(obj, globalElement, ans, tsize, &
                                         opt, islocal)
    CLASS(OneDimDomain_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalElement
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    CHARACTER(*), OPTIONAL, INTENT(IN) :: opt
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
  END SUBROUTINE obj_GetConnectivity_
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  This method is used to initiate the object

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, domain, totalElements)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: domain(2)
    INTEGER(I4B), INTENT(IN) :: totalElements
    !! Total number of elements in the domain
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  This method is used to initiate the object

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, domain, elemLength)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: domain(2)
    REAL(DFP), INTENT(IN) :: elemLength
    !! Total number of elements in the domain
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-22
! summary:  This method is used to initiate the object

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, domain, totalElements, elemLength)
    CLASS(OneDimDomain_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: domain(2)
    !! domain of the object
    INTEGER(I4B), INTENT(IN) :: totalElements
    !! Total number of elements in the domain
    INTEGER(I4B), INTENT(IN) :: elemLength(:)
    !! Total number of elements in the domain
    !! Only 1 to totalElements length are used
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE OneDimDomain_Class

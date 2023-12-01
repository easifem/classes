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
!

MODULE NitscheBC_Class
USE GlobalData
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE Domain_Class, ONLY: Domain_
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class
USE NeumannBC_Class
USE DomainConnectivity_Class, ONLY: DomainConnectivity_, &
  & DomainConnectivityPointer_
USE tomlf, ONLY: toml_table
USE TxtFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "NitscheBC_Class"
CHARACTER(*), PARAMETER :: myprefix = "NitscheBC"
PUBLIC :: DEALLOCATE
PUBLIC :: NitscheBCPointer_
PUBLIC :: NitscheBC_
PUBLIC :: AddNitscheBC
PUBLIC :: GetNitscheBCPointer
PUBLIC :: NitscheBCImportFromToml

!----------------------------------------------------------------------------
!                                                               NitscheBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS(NeumannBC_) :: NitscheBC_
  INTEGER(I4B), ALLOCATABLE :: cellElem(:)
  INTEGER(I4B), ALLOCATABLE :: localFacetID(:)
  INTEGER(I4B), ALLOCATABLE :: cellEntity(:)
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: SetCellData => bc_SetCellData
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinCellEntity => bc_GetMinCellEntity
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxCellEntity => bc_GetMaxCellEntity
  PROCEDURE, PUBLIC, PASS(obj) :: IsCellEntityPresent &
   & => bc_IsCellEntityPresent
  PROCEDURE, PUBLIC, PASS(obj) :: GetStartIndex => bc_GetStartIndex
  PROCEDURE, PUBLIC, PASS(obj) :: GetEndIndex => bc_GetEndIndex
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellElem => bc_GetCellElem
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalFacetID => bc_GetLocalFacetID
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => bc_GetPrefix
  FINAL :: bc_Final
END TYPE NitscheBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NitscheBCPointer_
  CLASS(NitscheBC_), POINTER :: ptr => NULL()
END TYPE NitscheBCPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE bc_Deallocate_Vector(obj)
    TYPE(NitscheBC_), ALLOCATABLE :: obj(:)
  END SUBROUTINE bc_Deallocate_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE bc_Deallocate_Ptr_Vector(obj)
    TYPE(NitscheBCPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE bc_Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Final(obj)
    TYPE(NitscheBC_), INTENT(INOUT) :: obj
  END SUBROUTINE bc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetCellData@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_SetCellData(obj, meshID, localID, &
    & tFacetElements, domConList)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: meshID(:)
    INTEGER(I4B), INTENT(IN) :: localID(:)
    INTEGER(I4B), INTENT(IN) :: tFacetElements(:)
    TYPE(DomainConnectivityPointer_), INTENT(IN) :: domConList(:)
  END SUBROUTINE bc_SetCellData
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetMinCellEntity@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_GetMinCellEntity(obj) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetMinCellEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMaxCellEntity@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_GetMaxCellEntity(obj) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetMaxCellEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                           IsCellEntityPresent@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_IsCellEntityPresent(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    LOGICAL(LGT) :: ans
  END FUNCTION bc_IsCellEntityPresent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_GetStartIndex(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetStartIndex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_GetEndIndex(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetEndIndex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_GetCellElem(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetCellElem
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_GetLocalFacetID(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetLocalFacetID
END INTERFACE

!----------------------------------------------------------------------------
!                                                 addNitscheBC@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Add Nitsche boundary conditions to the vector of pointer

INTERFACE AddNitscheBC
  MODULE SUBROUTINE bc_AddNitscheBC(dbc, dbcNo, param, boundary, dom)
    TYPE(NitscheBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Nitsche boundary to form
    INTEGER(I4B), INTENT(IN) :: dbcNo
    !! Nitsche boundary number
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[NitscheBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    CLASS(Domain_), INTENT(IN) :: dom
  END SUBROUTINE bc_AddNitscheBC
END INTERFACE AddNitscheBC

!----------------------------------------------------------------------------
!                                                 GetNitscheBC@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Get dirichlet boundary conditions to the vector of pointer

INTERFACE GetNitscheBCPointer
  MODULE FUNCTION bc_GetNitscheBCPointer(dbc, dbcNo) RESULT(ans)
    CLASS(NitscheBCPointer_), INTENT(IN) :: dbc(:)
    INTEGER(I4B), INTENT(IN) :: dbcNo
    !! Nitsche boundary nunber
    CLASS(NitscheBC_), POINTER :: ans
  END FUNCTION bc_GetNitscheBCPointer
END INTERFACE GetNitscheBCPointer

!----------------------------------------------------------------------------
!                                                       GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION bc_GetPrefix(obj) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION bc_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE NitscheBCImportFromToml
  MODULE SUBROUTINE bc_ImportFromToml1(obj, table, dom, tomlName)
    TYPE(NitscheBCPointer_), INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE bc_ImportFromToml1
END INTERFACE NitscheBCImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE NitscheBCImportFromToml
  MODULE SUBROUTINE bc_ImportFromToml2(obj, dom, tomlName, afile,  &
    & filename, printToml)
    TYPE(NitscheBCPointer_), INTENT(INOUT) :: obj(:)
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE bc_ImportFromToml2
END INTERFACE NitscheBCImportFromToml

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE NitscheBC_Class

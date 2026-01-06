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
USE GlobalData, ONLY: I4B, LGT, DFP
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE NeumannBC_Class, ONLY: NeumannBC_
USE DomainConnectivity_Class, ONLY: DomainConnectivityPointer_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE

PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "NitscheBC_Class"
#endif

PUBLIC :: NitscheBCDeallocate
PUBLIC :: NitscheBCDisplay
PUBLIC :: NitscheBCPointer_
PUBLIC :: NitscheBC_
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
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate memory occupied by AbstractBC
  PROCEDURE, PUBLIC, PASS(obj) :: SetCellData => obj_SetCellData
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinCellEntity => obj_GetMinCellEntity
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxCellEntity => obj_GetMaxCellEntity
  PROCEDURE, PUBLIC, PASS(obj) :: IsCellEntityPresent => &
    obj_IsCellEntityPresent
  PROCEDURE, PUBLIC, PASS(obj) :: GetStartIndex => obj_GetStartIndex
  PROCEDURE, PUBLIC, PASS(obj) :: GetEndIndex => obj_GetEndIndex
  PROCEDURE, PUBLIC, PASS(obj) :: GetCellElem => obj_GetCellElem
  PROCEDURE, PUBLIC, PASS(obj) :: GetLocalFacetID => obj_GetLocalFacetID
  FINAL :: obj_Final
END TYPE NitscheBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NitscheBCPointer_
  CLASS(NitscheBC_), POINTER :: ptr => NULL()
END TYPE NitscheBCPointer_

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE NitscheBCDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE NitscheBCDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    TYPE(NitscheBC_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE

INTERFACE NitscheBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Vector
END INTERFACE NitscheBCDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(NitscheBCPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE

INTERFACE NitscheBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Ptr_Vector
END INTERFACE NitscheBCDeallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(NitscheBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                    SetCellData@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetCellData(obj, meshID, localID, &
                                    tFacetElements, domConList)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: meshID(:)
    INTEGER(I4B), INTENT(IN) :: localID(:)
    INTEGER(I4B), INTENT(IN) :: tFacetElements(:)
    TYPE(DomainConnectivityPointer_), INTENT(IN) :: domConList(:)
  END SUBROUTINE obj_SetCellData
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetMinCellEntity@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetMinCellEntity(obj) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMinCellEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetMaxCellEntity@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetMaxCellEntity(obj) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxCellEntity
END INTERFACE

!----------------------------------------------------------------------------
!                                           IsCellEntityPresent@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_IsCellEntityPresent(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsCellEntityPresent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetStartIndex(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetStartIndex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetEndIndex(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetEndIndex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetCellElem(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetCellElem
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION obj_GetLocalFacetID(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetLocalFacetID
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetNitscheBC@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Get dirichlet boundary conditions to the vector of pointer

INTERFACE
  MODULE FUNCTION obj_GetNitscheBCPointer(bc, bcNo) RESULT(ans)
    CLASS(NitscheBCPointer_), INTENT(IN) :: bc(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: bcNo
    !! Nitsche boundary nunber
    CLASS(NitscheBC_), POINTER :: ans
  END FUNCTION obj_GetNitscheBCPointer
END INTERFACE

INTERFACE GetNitscheBCPointer
  MODULE PROCEDURE obj_GetNitscheBCPointer
END INTERFACE GetNitscheBCPointer

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom, tomlName)
    TYPE(NitscheBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE NitscheBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE NitscheBCImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, dom, tomlName, afile, filename, printToml)
    TYPE(NitscheBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE NitscheBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE NitscheBCImportFromToml

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Display the vector of NeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(NitscheBC_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Vector
END INTERFACE

INTERFACE NitscheBCDisplay
  MODULE PROCEDURE obj_Display_Vector
END INTERFACE NitscheBCDisplay

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Display the vector of NeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(NitscheBCPointer_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE

INTERFACE NitscheBCDisplay
  MODULE PROCEDURE obj_Display_Ptr_Vector
END INTERFACE NitscheBCDisplay

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE NitscheBC_Class

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
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "NitscheBC_Class"
CHARACTER(*), PARAMETER :: myprefix = "NitscheBC"
PUBLIC :: DEALLOCATE
PUBLIC :: NitscheBCPointer_
PUBLIC :: NitscheBC_
PUBLIC :: AddNitscheBC
PUBLIC :: GetNitscheBCPointer

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
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & bc_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => bc_Initiate
  PROCEDURE, PUBLIC, PASS(obj) :: SetCellData => bc_SetCellData
  PROCEDURE, PUBLIC, PASS(obj) :: GetMinCellEntity => bc_GetMinCellEntity
  PROCEDURE, PUBLIC, PASS(obj) :: GetMaxCellEntity => bc_GetMaxCellEntity
  PROCEDURE, PUBLIC, PASS(obj) :: isCellEntityPresent &
   & => bc_isCellEntityPresent
  PROCEDURE, PUBLIC, PASS(obj) :: getStartIndex => bc_getStartIndex
  PROCEDURE, PUBLIC, PASS(obj) :: getEndIndex => bc_getEndIndex
  PROCEDURE, PUBLIC, PASS(obj) :: getCellElem => bc_getCellElem
  PROCEDURE, PUBLIC, PASS(obj) :: getLocalFacetID => bc_getLocalFacetID
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
!                                      checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 4 Feb 2022
! summary: Check essential parameters

INTERFACE
  MODULE SUBROUTINE bc_checkEssentialParam(obj, param)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                      setNitscheBCParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE setNitscheBCParam(param, name, idof, nodalValueType, &
  & useFunction, isNormal, isTangent, useExternal)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: nodalValueType
    !! Space
    !! Time
    !! SpaceTime
    !! Constant
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: useFunction
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isNormal
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTangent
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: useExternal
  END SUBROUTINE setNitscheBCParam
END INTERFACE

PUBLIC :: setNitscheBCParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Initiate(obj, param, boundary, dom)
    CLASS(NitscheBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    CLASS(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE bc_Initiate
END INTERFACE

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

INTERFACE
  MODULE PURE FUNCTION bc_GetMaxCellEntity(obj) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION bc_GetMaxCellEntity
END INTERFACE

INTERFACE
  MODULE PURE FUNCTION bc_isCellEntityPresent(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    LOGICAL(LGT) :: ans
  END FUNCTION bc_isCellEntityPresent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_getStartIndex(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_getStartIndex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_getEndIndex(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_getEndIndex
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_getCellElem(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_getCellElem
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE PURE FUNCTION bc_getLocalFacetID(obj, entityNum) RESULT(ans)
    CLASS(NitscheBC_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: entityNum
    INTEGER(I4B) :: ans
  END FUNCTION bc_getLocalFacetID
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
!
!----------------------------------------------------------------------------

END MODULE NitscheBC_Class

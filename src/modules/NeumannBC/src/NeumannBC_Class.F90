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

MODULE NeumannBC_Class
USE GlobalData
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE Domain_Class, ONLY: Domain_
USE FEDomain_Class, ONLY: FEDomain_
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class
USE DirichletBC_Class
USE tomlf, ONLY: toml_table
USE TxtFile_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "NeumannBC_CLASS"
CHARACTER(*), PARAMETER :: myprefix = "NeumannBC"
PUBLIC :: NeumannBC_
PUBLIC :: NeumannBCPointer_
PUBLIC :: NeumannBCDeallocate
PUBLIC :: NeumannBCDisplay
PUBLIC :: AddNeumannBC
PUBLIC :: AppendNeumannBC
PUBLIC :: GetNeumannBCPointer
PUBLIC :: NeumannBCImportFromToml

!----------------------------------------------------------------------------
!                                                               NeumannBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS(DirichletBC_) :: NeumannBC_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  FINAL :: obj_Final
END TYPE NeumannBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: NeumannBCPointer_
  CLASS(NeumannBC_), POINTER :: ptr => NULL()
END TYPE NeumannBCPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE NeumannBCDeallocate
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    TYPE(NeumannBC_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE NeumannBCDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE NeumannBCDeallocate
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(NeumannBCPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE NeumannBCDeallocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(NeumannBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                 addNeumannBC@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Add Neumann boundary conditions to the vector of pointer

INTERFACE AddNeumannBC
  MODULE SUBROUTINE obj_AddNeumannBC(nbc, nbcNo, param, boundary, dom)
    TYPE(NeumannBCPointer_), INTENT(INOUT) :: nbc(:)
    !! Dirichlet boundary to form
    INTEGER(I4B), INTENT(IN) :: nbcNo
    !! Dirichlet boundary number
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[DirichletBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    CLASS(FEDomain_), INTENT(IN) :: dom
  END SUBROUTINE obj_AddNeumannBC
END INTERFACE AddNeumannBC

!----------------------------------------------------------------------------
!                                                AppendNeumannBC@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Add Neumann boundary conditions to the vector of pointer

INTERFACE AppendNeumannBC
  MODULE SUBROUTINE obj_AppendNeumannBC(nbc, param, boundary, dom, nbcNo)
    TYPE(NeumannBCPointer_), ALLOCATABLE, INTENT(INOUT) :: nbc(:)
    !! Dirichlet boundary to form
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[DirichletBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    CLASS(FEDomain_), INTENT(IN) :: dom
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nbcNo
    !! Dirichlet boundary number
  END SUBROUTINE obj_AppendNeumannBC
END INTERFACE AppendNeumannBC

!----------------------------------------------------------------------------
!                                                 GetNeumannBC@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Get dirichlet boundary conditions to the vector of pointer

INTERFACE GetNeumannBCPointer
  MODULE FUNCTION obj_GetNeumannBCPointer(nbc, nbcNo) RESULT(ans)
    CLASS(NeumannBCPointer_), INTENT(IN) :: nbc(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nbcNo
    !! Neumann boundary nunber
    CLASS(NeumannBC_), POINTER :: ans
  END FUNCTION obj_GetNeumannBCPointer
END INTERFACE GetNeumannBCPointer

!----------------------------------------------------------------------------
!                                                     GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-14
! summary:  This function returns the prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(NeumannBC_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate param from the toml file

INTERFACE NeumannBCImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom, tomlName)
    TYPE(NeumannBCPointer_), INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CLASS(FEDomain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE NeumannBCImportFromToml

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary:  Initiate kernel from the toml file

INTERFACE NeumannBCImportFromToml
  MODULE SUBROUTINE obj_ImportFromToml2(obj, dom, tomlName, afile,  &
    & filename, printToml)
    TYPE(NeumannBCPointer_), INTENT(INOUT) :: obj(:)
    CLASS(FEDomain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE NeumannBCImportFromToml

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Display the vector of NeumannBC_

INTERFACE NeumannBCDisplay
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(NeumannBC_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Vector
END INTERFACE NeumannBCDisplay

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Display the vector of NeumannBC_

INTERFACE NeumannBCDisplay
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(NeumannBCPointer_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE NeumannBCDisplay

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE NeumannBC_Class

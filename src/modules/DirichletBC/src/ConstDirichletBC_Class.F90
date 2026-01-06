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

MODULE ConstDirichletBC_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class, ONLY: AbstractBC_
USE DirichletBC_Class, ONLY: DirichletBC_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE
PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "ConstDirichletBC_Class"
#endif

PUBLIC :: ConstDirichletBCDeallocate
PUBLIC :: ConstDirichletBCDisplay
PUBLIC :: ConstDirichletBC_
PUBLIC :: ConstDirichletBCPointer_
PUBLIC :: AddConstDirichletBC
PUBLIC :: AppendConstDirichletBC
PUBLIC :: GetConstDirichletBCPointer
PUBLIC :: ConstDirichletBCImportFromToml

!----------------------------------------------------------------------------
!                                                           ConstDirichletBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS(DirichletBC_) :: ConstDirichletBC_
CONTAINS
  PRIVATE
  FINAL :: obj_Final
END TYPE ConstDirichletBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: ConstDirichletBCPointer_
  CLASS(ConstDirichletBC_), POINTER :: ptr => NULL()
END TYPE ConstDirichletBCPointer_

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of ConstDirichletBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    TYPE(ConstDirichletBC_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE

INTERFACE ConstDirichletBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Vector
END INTERFACE ConstDirichletBCDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of ConstDirichletBCPointer_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(ConstDirichletBCPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE

INTERFACE ConstDirichletBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Ptr_Vector
END INTERFACE ConstDirichletBCDeallocate

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ConstDirichletBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                              AddConstDirichletBC@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Add ConstDirichletBC to the vector of ConstDirichletBCPointer_

INTERFACE
  MODULE SUBROUTINE obj_AddConstDirichletBC(dbc, dbcNo, param, boundary, dom)
    TYPE(ConstDirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary to form
    INTEGER(I4B), INTENT(IN) :: dbcNo
    !! Dirichlet boundary number
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[ConstDirichletBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    CLASS(AbstractDomain_), INTENT(IN) :: dom
  END SUBROUTINE obj_AddConstDirichletBC
END INTERFACE

INTERFACE AddConstDirichletBC
  MODULE PROCEDURE obj_AddConstDirichletBC
END INTERFACE AddConstDirichletBC

!----------------------------------------------------------------------------
!                                           AppendConstDirichletBC@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Append ConstDirichletBC to the vector of ConstDirichletBCPointer_

INTERFACE
  MODULE SUBROUTINE obj_AppendConstDirichletBC( &
    dbc, param, boundary, dom, dbcNo)
    TYPE(ConstDirichletBCPointer_), ALLOCATABLE, INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary to form
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[ConstDirichletBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    CLASS(AbstractDomain_), INTENT(IN) :: dom
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcNo
    !! Dirichlet boundary number
  END SUBROUTINE obj_AppendConstDirichletBC
END INTERFACE

INTERFACE AppendConstDirichletBC
  MODULE PROCEDURE obj_AppendConstDirichletBC
END INTERFACE AppendConstDirichletBC

!----------------------------------------------------------------------------
!                                              GetConstDirichletBC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Get pointer to ConstDirichletBCPointer_

INTERFACE
  MODULE FUNCTION obj_GetConstDirichletBCPointer(dbc, dbcNo) RESULT(ans)
    CLASS(ConstDirichletBCPointer_), INTENT(IN) :: dbc(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcNo
    !! Dirichlet boundary nunber
    CLASS(ConstDirichletBC_), POINTER :: ans
  END FUNCTION obj_GetConstDirichletBCPointer
END INTERFACE

INTERFACE GetConstDirichletBCPointer
  MODULE PROCEDURE obj_GetConstDirichletBCPointer
END INTERFACE GetConstDirichletBCPointer

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary:  Initiate a vector of ConstDirichletBCPointer_ from the toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom, tomlName)
    TYPE(ConstDirichletBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE ConstDirichletBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE ConstDirichletBCImportFromToml

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Initiate a vector of ConstDirichletBCPointer_ from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, dom, tomlName, afile, filename, printToml)
    TYPE(ConstDirichletBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE ConstDirichletBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE ConstDirichletBCImportFromToml

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Display a vector of ConstDirichletBC_

INTERFACE
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(ConstDirichletBC_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Vector
END INTERFACE

INTERFACE ConstDirichletBCDisplay
  MODULE PROCEDURE obj_Display_Vector
END INTERFACE ConstDirichletBCDisplay

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Display a vector of ConstDirichletBCPointer_

INTERFACE
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(ConstDirichletBCPointer_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE

INTERFACE ConstDirichletBCDisplay
  MODULE PROCEDURE obj_Display_Ptr_Vector
END INTERFACE ConstDirichletBCDisplay

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConstDirichletBC_Class

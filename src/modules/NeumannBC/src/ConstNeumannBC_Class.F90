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

MODULE ConstNeumannBC_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE NeumannBC_Class, ONLY: NeumannBC_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
IMPLICIT NONE

PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "ConstNeumannBC_Class"
#endif

PUBLIC :: ConstNeumannBC_
PUBLIC :: ConstNeumannBCPointer_
PUBLIC :: ConstNeumannBCDeallocate
PUBLIC :: ConstNeumannBCDisplay
PUBLIC :: GetConstNeumannBCPointer
PUBLIC :: ConstNeumannBCImportFromToml

!----------------------------------------------------------------------------
!                                                               ConstNeumannBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS(NeumannBC_) :: ConstNeumannBC_
CONTAINS
  PRIVATE
  FINAL :: obj_Final
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportConstBCFromToml
  !! Initiate ConstNeumannBC from toml table (overriding)
END TYPE ConstNeumannBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: ConstNeumannBCPointer_
  CLASS(ConstNeumannBC_), POINTER :: ptr => NULL()
END TYPE ConstNeumannBCPointer_

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of ConstNeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    TYPE(ConstNeumannBC_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE

INTERFACE ConstNeumannBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Vector
END INTERFACE ConstNeumannBCDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of ConstNeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(ConstNeumannBCPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE

INTERFACE ConstNeumannBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Ptr_Vector
END INTERFACE ConstNeumannBCDeallocate

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(ConstNeumannBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetConstNeumannBC@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Get dirichlet boundary conditions to the vector of pointer

INTERFACE
  MODULE FUNCTION obj_GetConstNeumannBCPointer(bc, bcNo) RESULT(ans)
    CLASS(ConstNeumannBCPointer_), INTENT(IN) :: bc(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: bcNo
    !! Neumann boundary nunber
    CLASS(ConstNeumannBC_), POINTER :: ans
  END FUNCTION obj_GetConstNeumannBCPointer
END INTERFACE

INTERFACE GetConstNeumannBCPointer
  MODULE PROCEDURE obj_GetConstNeumannBCPointer
END INTERFACE GetConstNeumannBCPointer

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-06
! summary: Import ConstNeumannBC_ from the toml table (overriding)

INTERFACE
  MODULE SUBROUTINE obj_ImportConstBCFromToml(obj, table, dom)
    CLASS(ConstNeumannBC_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_ImportConstBCFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate ConstNeumannBCPointer_ from the toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom, tomlName)
    TYPE(ConstNeumannBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE ConstNeumannBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE ConstNeumannBCImportFromToml

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate ConstNeumannBCPointer_ from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, dom, tomlName, afile, filename, printToml)
    TYPE(ConstNeumannBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE ConstNeumannBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE ConstNeumannBCImportFromToml

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Display the vector of ConstNeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(ConstNeumannBC_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Vector
END INTERFACE

INTERFACE ConstNeumannBCDisplay
  MODULE PROCEDURE obj_Display_Vector
END INTERFACE ConstNeumannBCDisplay

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Display the vector of ConstNeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(ConstNeumannBCPointer_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE

INTERFACE ConstNeumannBCDisplay
  MODULE PROCEDURE obj_Display_Ptr_Vector
END INTERFACE ConstNeumannBCDisplay

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE ConstNeumannBC_Class

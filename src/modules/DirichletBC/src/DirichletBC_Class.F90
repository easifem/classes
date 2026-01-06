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

MODULE DirichletBC_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE AbstractBC_Class, ONLY: AbstractBC_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_

IMPLICIT NONE
PRIVATE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "DirichletBC_Class"
#endif

PUBLIC :: DirichletBCDeallocate
PUBLIC :: DirichletBCDisplay
PUBLIC :: DirichletBC_
PUBLIC :: DirichletBCPointer_
PUBLIC :: GetDirichletBCPointer
PUBLIC :: DirichletBCImportFromToml

!----------------------------------------------------------------------------
!                                                               DirichletBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: Dirichlet boundary condition class

TYPE, EXTENDS(AbstractBC_) :: DirichletBC_
CONTAINS
  PRIVATE
  FINAL :: obj_Final
END TYPE DirichletBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: DirichletBCPointer_
  CLASS(DirichletBC_), POINTER :: ptr => NULL()
END TYPE DirichletBCPointer_

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Deallocate a vector of DirichletBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    TYPE(DirichletBC_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE

INTERFACE DirichletBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Vector
END INTERFACE DirichletBCDeallocate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Deallocate a vector of DirichletBCPointer_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(DirichletBCPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE

INTERFACE DirichletBCDeallocate
  MODULE PROCEDURE obj_Deallocate_Ptr_Vector
END INTERFACE DirichletBCDeallocate

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(DirichletBC_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetDirichletBC@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! summary: Get a pointer to DirichletBC from vector of DirichletBCPointer_

INTERFACE
  MODULE FUNCTION obj_GetDirichletBCPointer(bc, bcNo) RESULT(ans)
    CLASS(DirichletBCPointer_), INTENT(IN) :: bc(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: bcNo
    !! Dirichlet boundary nunber
    CLASS(DirichletBC_), POINTER :: ans
  END FUNCTION obj_GetDirichletBCPointer
END INTERFACE

INTERFACE GetDirichletBCPointer
  MODULE PROCEDURE obj_GetDirichletBCPointer
END INTERFACE GetDirichletBCPointer

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate a vector of DirichletBCPointer_ from the toml table

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, dom, tomlName)
    TYPE(DirichletBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! Should be allocated outside
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table to returned
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    !! domain
    CHARACTER(*), INTENT(IN) :: tomlName
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

INTERFACE DirichletBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml1
END INTERFACE DirichletBCImportFromToml

!----------------------------------------------------------------------------
!                                                    ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-08
! summary: Initiate a vector of DirichletBCPointer_ from the toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, dom, tomlName, afile, filename, printToml)
    TYPE(DirichletBCPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    CLASS(AbstractDomain_), TARGET, INTENT(IN) :: dom
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

INTERFACE DirichletBCImportFromToml
  MODULE PROCEDURE obj_ImportFromToml2
END INTERFACE DirichletBCImportFromToml

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Display the vector of DirichletBC_

INTERFACE
  MODULE SUBROUTINE obj_Display_Vector(obj, msg, unitNo)
    TYPE(DirichletBC_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Vector
END INTERFACE

INTERFACE DirichletBCDisplay
  MODULE PROCEDURE obj_Display_Vector
END INTERFACE DirichletBCDisplay

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary: Display the vector of DirichletBCPointer_

INTERFACE
  MODULE SUBROUTINE obj_Display_Ptr_Vector(obj, msg, unitNo)
    TYPE(DirichletBCPointer_) :: obj(:)
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display_Ptr_Vector
END INTERFACE

INTERFACE DirichletBCDisplay
  MODULE PROCEDURE obj_Display_Ptr_Vector
END INTERFACE DirichletBCDisplay

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DirichletBC_Class

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
USE GlobalData
USE BaseType
USE ExceptionHandler_Class, ONLY: e
USE MeshSelection_Class, ONLY: MeshSelection_
USE Domain_Class, ONLY: Domain_
USE FPL, ONLY: ParameterList_
USE AbstractBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "DirichletBC_Class"
CHARACTER(*), PARAMETER :: myprefix = "DirichletBC"
PUBLIC :: DEALLOCATE
PUBLIC :: DirichletBC_
PUBLIC :: DirichletBCPointer_
PUBLIC :: AddDirichletBC
PUBLIC :: GetDirichletBCPointer

!----------------------------------------------------------------------------
!                                                               DirichletBC_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 1 Sept 2021
! summary: This is an abstract data type for boundary conditions

TYPE, EXTENDS(AbstractBC_) :: DirichletBC_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & bc_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate => bc_Initiate
  FINAL :: bc_Final
END TYPE DirichletBC_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: DirichletBCPointer_
  CLASS(DirichletBC_), POINTER :: ptr => NULL()
END TYPE DirichletBCPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-09
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE bc_Deallocate_Vector(obj)
    TYPE(DirichletBC_), ALLOCATABLE :: obj(:)
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
    TYPE(DirichletBCPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE bc_Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                    checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_checkEssentialParam(obj, param)
    CLASS(DirichletBC_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE bc_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                       setDirichletParam@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE SetDirichletBCParam(param, name, idof, nodalValueType, &
    & useFunction, isNormal, isTangent)
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
  END SUBROUTINE SetDirichletBCParam
END INTERFACE

PUBLIC :: SetDirichletBCParam

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE bc_Initiate(obj, param, boundary, dom)
    CLASS(DirichletBC_), INTENT(INOUT) :: obj
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
    TYPE(DirichletBC_), INTENT(INOUT) :: obj
  END SUBROUTINE bc_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                 addDirichletBC@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Add dirichlet boundary conditions to the vector of pointer

INTERFACE AddDirichletBC
  MODULE SUBROUTINE bc_AddDirichletBC(dbc, dbcNo, param, boundary, dom)
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary to form
    INTEGER(I4B), INTENT(IN) :: dbcNo
    !! Dirichlet boundary number
    TYPE(ParameterList_), INTENT(IN) :: param
    !! parameter for constructing [[DirichletBC_]].
    TYPE(MeshSelection_), INTENT(IN) :: boundary
    !! Boundary region
    CLASS(Domain_), INTENT(IN) :: dom
  END SUBROUTINE bc_AddDirichletBC
END INTERFACE AddDirichletBC

!----------------------------------------------------------------------------
!                                                 GetDirichletBC@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2022-04-27
! update: 2023-09-10
! summary: Get dirichlet boundary conditions to the vector of pointer

INTERFACE GetDirichletBCPointer
  MODULE FUNCTION bc_GetDirichletBCPointer(dbc, dbcNo) RESULT(ans)
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    INTEGER(I4B), INTENT(IN) :: dbcNo
    !! Dirichlet boundary nunber
    CLASS(DirichletBC_), POINTER :: ans
  END FUNCTION bc_GetDirichletBCPointer
END INTERFACE GetDirichletBCPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE DirichletBC_Class

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

MODULE ScalarMeshField_Class
USE GlobalData
USE BaSetype
USE FPL, ONLY: ParameterList_
USE Mesh_Class, ONLY: Mesh_
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class
USE AbstractMeshField_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "ScalarMeshField_Class"
PUBLIC :: ScalarMeshField_
PUBLIC :: ScalarMeshFieldPointer_
PUBLIC :: SetScalarMeshFieldParam
PUBLIC :: ScalarMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                                     ScalarMeshField_Class
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Feb 2022
! summary: Scalar mesh field

TYPE, EXTENDS(AbstractMeshField_) :: ScalarMeshField_
END TYPE ScalarMeshField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: ScalarMeshFieldPointer_
  CLASS(ScalarMeshField_), POINTER :: ptr => NULL()
END TYPE ScalarMeshFieldPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE ScalarMeshFieldDeallocate
  MODULE SUBROUTINE aField_Deallocate_Vector(obj)
    TYPE(ScalarMeshField_), ALLOCATABLE :: obj(:)
  END SUBROUTINE aField_Deallocate_Vector
END INTERFACE ScalarMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE ScalarMeshFieldDeallocate
  MODULE SUBROUTINE aField_Deallocate_Ptr_Vector(obj)
    TYPE(ScalarMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE aField_Deallocate_Ptr_Vector
END INTERFACE ScalarMeshFieldDeallocate

!----------------------------------------------------------------------------
!                              SetAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE SetScalarMeshFieldParam(param, name, &
    & fieldType, varType, engine, defineOn, nns)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
  !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: nns
  !! Number of node in space
  END SUBROUTINE SetScalarMeshFieldParam
END INTERFACE

END MODULE ScalarMeshField_Class

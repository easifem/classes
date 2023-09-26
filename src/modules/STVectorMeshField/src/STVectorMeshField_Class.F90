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

MODULE STVectorMeshField_Class
USE GlobalData
USE BaSetype
USE FPL, ONLY: ParameterList_
USE Mesh_Class, ONLY: Mesh_
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class
USE AbstractMeshField_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STVectorMeshField_Class"
PUBLIC :: DEALLOCATE

!----------------------------------------------------------------------------
!                                                     STVectorMeshField_Class
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Feb 2022
! summary: Scalar mesh field

TYPE, EXTENDS(AbstractMeshField_) :: STVectorMeshField_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & aField_checkEssentialParam
    !! check essential parameters
  PROCEDURE, PASS(obj) :: Initiate1 => aField_Initiate1
    !! Initiate the field by reading param and a given mesh
END TYPE STVectorMeshField_

PUBLIC :: STVectorMeshField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: STVectorMeshFieldPointer_
  CLASS(STVectorMeshField_), POINTER :: ptr => NULL()
END TYPE STVectorMeshFieldPointer_

PUBLIC :: STVectorMeshFieldPointer_

!----------------------------------------------------------------------------
!                              SetAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE SetSTVectorMeshFieldParam(param, name, &
    & fieldType, varType, engine, defineOn, spaceCompo, nns, nnt)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
  !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), INTENT(IN) :: nns
    INTEGER(I4B), INTENT(IN) :: nnt
  !! Number of node in space
  END SUBROUTINE SetSTVectorMeshFieldParam
END INTERFACE

PUBLIC :: SetSTVectorMeshFieldParam

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE aField_checkEssentialParam(obj, param)
    CLASS(STVectorMeshField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE aField_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate the field by reading param and given domain

INTERFACE
  MODULE SUBROUTINE aField_Initiate1(obj, param, mesh)
    CLASS(STVectorMeshField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Mesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE aField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE aField_Deallocate_Vector(obj)
    TYPE(STVectorMeshField_), ALLOCATABLE :: obj(:)
  END SUBROUTINE aField_Deallocate_Vector
END INTERFACE DEALLOCATE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE DEALLOCATE
  MODULE SUBROUTINE aField_Deallocate_Ptr_Vector(obj)
    TYPE(STVectorMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE aField_Deallocate_Ptr_Vector
END INTERFACE DEALLOCATE

END MODULE STVectorMeshField_Class

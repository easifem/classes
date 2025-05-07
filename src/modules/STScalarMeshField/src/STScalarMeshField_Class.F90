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

MODULE STScalarMeshField_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE UserFunction_Class, ONLY: UserFunction_
USE AbstractMeshField_Class, ONLY: AbstractScalarMeshField_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "STScalarMeshField_Class"
CHARACTER(*), PARAMETER :: myprefix = "STScalarMeshField"

PUBLIC :: STScalarMeshFieldDeallocate
PUBLIC :: STScalarMeshField_
PUBLIC :: SetSTScalarMeshFieldParam
PUBLIC :: STScalarMeshFieldPointer_

!----------------------------------------------------------------------------
!                                                     STScalarMeshField_Class
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Feb 2022
! summary: Scalar mesh field

TYPE, EXTENDS(AbstractScalarMeshField_) :: STScalarMeshField_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  PROCEDURE, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate from user function
END TYPE STScalarMeshField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: STScalarMeshFieldPointer_
  CLASS(STScalarMeshField_), POINTER :: ptr => NULL()
END TYPE STScalarMeshFieldPointer_

!----------------------------------------------------------------------------
!                              setAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE SetSTScalarMeshFieldParam(param, name, &
                               fieldType, varType, engine, defineOn, nns, nnt)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
    INTEGER(I4B), INTENT(IN) :: nnt
    !! Number of node in time
  END SUBROUTINE SetSTScalarMeshFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate from UserFunction_

INTERFACE
  MODULE SUBROUTINE obj_Initiate4(obj, mesh, func, name, engine, nnt)
    CLASS(STScalarMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE obj_Initiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE STScalarMeshFieldDeallocate
  MODULE SUBROUTINE aField_Deallocate_Vector(obj)
    TYPE(STScalarMeshField_), ALLOCATABLE :: obj(:)
  END SUBROUTINE aField_Deallocate_Vector
END INTERFACE STScalarMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE STScalarMeshFieldDeallocate
  MODULE SUBROUTINE aField_Deallocate_Ptr_Vector(obj)
    TYPE(STScalarMeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE aField_Deallocate_Ptr_Vector
END INTERFACE STScalarMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                                              GetPrefix
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-04
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(STScalarMeshField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

END MODULE STScalarMeshField_Class

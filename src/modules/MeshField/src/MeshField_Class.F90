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

MODULE MeshField_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE FPL, ONLY: ParameterList_
USE ExceptionHandler_Class, ONLY: e
USE UserFunction_Class, ONLY: UserFunction_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractMeshField_Class, ONLY: AbstractMeshField_
USE FEDOF_Class, ONLY: FEDOF_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "MeshField_Class"
CHARACTER(*), PARAMETER :: myprefix = "MeshField"

PUBLIC :: MeshField_
PUBLIC :: MeshFieldPointer_
PUBLIC :: MeshFieldDeallocate
PUBLIC :: ScalarMeshFieldInitiate
PUBLIC :: STScalarMeshFieldInitiate
PUBLIC :: VectorMeshFieldInitiate
PUBLIC :: STVectorMeshFieldInitiate
PUBLIC :: TensorMeshFieldInitiate
PUBLIC :: STTensorMeshFieldInitiate
PUBLIC :: InitiateInterpolationPoints
PUBLIC :: SetInterpolationPoints
PUBLIC :: InitiateQuadraturePoints
PUBLIC :: SetQuadraturePoints

!----------------------------------------------------------------------------
!                                                            MeshField_Class
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Mesh Field

TYPE, EXTENDS(AbstractMeshField_) :: MeshField_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
END TYPE MeshField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: MeshFieldPointer_
  CLASS(MeshField_), POINTER :: ptr => NULL()
END TYPE MeshFieldPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    TYPE(MeshField_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE

INTERFACE MeshFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_Vector
END INTERFACE MeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of NeumannBC_

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(MeshFieldPointer_), ALLOCATABLE :: obj(:)
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE

INTERFACE MeshFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_Ptr_Vector
END INTERFACE MeshFieldDeallocate

!----------------------------------------------------------------------------
!                              SetAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine Check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE SetScalarMeshFieldParam( &
    param, name, fieldType, varType, engine, defineOn, nns)
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

!----------------------------------------------------------------------------
!                              SetAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate ScalarMeshField

INTERFACE
  MODULE SUBROUTINE ScalarMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, nns, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE ScalarMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                SetSTScalarMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Set parameters for space-time scalar mesh field

INTERFACE
  MODULE SUBROUTINE SetSTScalarMeshFieldParam( &
    param, name, fieldType, varType, engine, defineOn, nns, nnt)
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
!                                SetSTScalarMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Set space-time scalar mesh field

INTERFACE
  MODULE SUBROUTINE STScalarMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, nns, nnt, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
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
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE STScalarMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                 SetVectorMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Set parameters for vector mesh field

INTERFACE
  MODULE SUBROUTINE SetVectorMeshFieldParam( &
    param, name, fieldType, varType, engine, defineOn, spaceCompo, nns)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space compo
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
  END SUBROUTINE SetVectorMeshFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                 VectorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Initiate vector mesh field

INTERFACE
  MODULE SUBROUTINE VectorMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, spaceCompo, nns, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type, normal, constant
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type: constant, space, spaceTime, time, etc
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space compo
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space with in the element
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE VectorMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                SetSTVectorMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Set parameters for space-time vector mesh field

INTERFACE
  MODULE SUBROUTINE SetSTVectorMeshFieldParam( &
    param, name, fieldType, varType, engine, defineOn, spaceCompo, nns, nnt)
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

!----------------------------------------------------------------------------
!                                STVectorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate space-time vector mesh field

INTERFACE
  MODULE SUBROUTINE STVectorMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, spaceCompo, nns, &
    nnt, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
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
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE STVectorMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                 SetTensorMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Set parameters for tensor mesh field

INTERFACE
  MODULE SUBROUTINE SetTensorMeshFieldParam( &
    param, name, fieldType, varType, engine, defineOn, dim1, dim2, nns)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
  END SUBROUTINE SetTensorMeshFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                  TensorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate tensor mesh field

INTERFACE
  MODULE SUBROUTINE TensorMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, dim1, dim2, &
    nns, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE TensorMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                SetSTTensorMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary:  Set param for space-time tensor mesh field

INTERFACE
  MODULE SUBROUTINE SetSTTensorMeshFieldParam( &
    param, name, fieldType, varType, engine, defineOn, dim1, dim2, nns, nnt)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: dim1
    !! size in dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    !! size in dim2
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in space
    INTEGER(I4B), INTENT(IN) :: nnt
    !! Number of node in space
  END SUBROUTINE SetSTTensorMeshFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                STTensorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Space-Time tensor mesh field

INTERFACE
  MODULE SUBROUTINE STTensorMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, dim1, dim2, nns, nnt, &
    mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: dim1
    !! size in dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    !! size in dim2
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in space
    INTEGER(I4B), INTENT(IN) :: nnt
    !! Number of node in space
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE STTensorMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Scalar Mesh Field Constructor

INTERFACE
  MODULE SUBROUTINE ScalarMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE ScalarMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Space time mesh field constructor

INTERFACE
  MODULE SUBROUTINE STScalarMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE STScalarMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Vector mesh field constructor

INTERFACE
  MODULE SUBROUTINE VectorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE VectorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Space-time vector mesh field constructor

INTERFACE
  MODULE SUBROUTINE STVectorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE STVectorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Tensor mesh field constructor

INTERFACE
  MODULE SUBROUTINE TensorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE TensorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Space-time tensor mesh field constructor

INTERFACE
  MODULE SUBROUTINE STTensorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE STTensorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-04
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(MeshField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                            GenerateInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-27
! summary:  Generate interpolation points

INTERFACE
  MODULE SUBROUTINE InitiateInterpolationPoints( &
    obj, order, ipType, fedof, mesh, engine)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order(3), ipType(3)
    TYPE(FEDOF_), INTENT(INOUT) :: fedof
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    CHARACTER(*), INTENT(IN) :: engine
  END SUBROUTINE InitiateInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                             UpdateInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-27
! summary: Generate interpolation points

INTERFACE
  MODULE SUBROUTINE SetInterpolationPoints(obj, order, ipType, fedof, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: order(3), ipType(3)
    TYPE(FEDOF_), INTENT(INOUT) :: fedof
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE SetInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                 GenerateQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-07
! summary:  Generate quadrature points

INTERFACE
  MODULE SUBROUTINE InitiateQuadraturePoints(obj, fedof, mesh, &
                                             geofedof, engine)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    TYPE(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    CHARACTER(*), INTENT(IN) :: engine
  END SUBROUTINE InitiateQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                             UpdateInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-07
! summary:  Generate quadrature points

INTERFACE
  MODULE SUBROUTINE SetQuadraturePoints(obj, fedof, geofedof, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    TYPE(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE SetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MeshField_Class

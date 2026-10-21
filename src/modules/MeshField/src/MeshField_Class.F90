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

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Mesh field, concrete class of AbstractMeshField_
!
! This module defines [[MeshField_]], the concrete class of
! [[AbstractMeshField_]]. All the methods of the mesh field are
! inherited from [[AbstractMeshField_]].
!
! This module also provides the following helper routines:
!
! - Constructors for scalar, vector, and tensor mesh fields, and their
!   space-time versions: [[ScalarMeshFieldInitiate]],
!   [[STScalarMeshFieldInitiate]], [[VectorMeshFieldInitiate]],
!   [[STVectorMeshFieldInitiate]], [[TensorMeshFieldInitiate]],
!   [[STTensorMeshFieldInitiate]]. These routines compute the shape of
!   the data from simple arguments (nns, nnt, spaceCompo, dim1, dim2)
!   and call Initiate of [[AbstractMeshField_]].
! - Vector mesh fields of interpolation points and quadrature points:
!   [[InitiateInterpolationPoints]], [[SetInterpolationPoints]],
!   [[InitiateQuadraturePoints]], [[SetQuadraturePoints]].
! - [[MeshFieldDeallocate]] for vectors of mesh fields.
!
! The examples are also available as tests in
! `src/modules/MeshField/examples/`.

MODULE MeshField_Class
USE GlobalData, ONLY: I4B, DFP, LGT
USE ExceptionHandler_Class, ONLY: e
USE UserFunction_Class, ONLY: UserFunction_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE AbstractMeshField_Class, ONLY: AbstractMeshField_
USE FEDOF_Class, ONLY: FEDOF_
IMPLICIT NONE

PRIVATE

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
!
! MeshField_ is the concrete class of [[AbstractMeshField_]]. It does
! not add any new data or method. It stores the data (scalar, vector,
! or matrix) of a finite element variable for each element of a mesh.
! See [[AbstractMeshField_]] for the data layout and the methods.
!
! Examples
!
! See the examples of [[ScalarMeshFieldInitiate]] and
! [[VectorMeshFieldInitiate]].
!
! See also: [[AbstractMeshField_]], [[MeshFieldPointer_]]

TYPE, EXTENDS(AbstractMeshField_) :: MeshField_
END TYPE MeshField_

!----------------------------------------------------------------------------
!                                                         MeshFieldPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Pointer to MeshField_
!
! A container of pointer to [[MeshField_]], which is used to make
! vectors of pointers to mesh fields.
!
! See also: [[MeshFieldDeallocate]]

TYPE :: MeshFieldPointer_
  CLASS(MeshField_), POINTER :: ptr => NULL()
  !! pointer to mesh field
END TYPE MeshFieldPointer_

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of MeshField_
!
! Deallocate every mesh field in the vector, and then deallocate the
! vector itself. If obj is not allocated then nothing is done.
!
! Arguments:
!   obj : TYPE(MeshField_), ALLOCATABLE(:) - vector of mesh fields
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: MeshFieldDeallocate
! USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2, tfields = 3
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_), ALLOCATABLE :: obj(:)
! INTEGER(I4B) :: ii
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! ALLOCATE (obj(tfields))
! DO ii = 1, tfields
!   CALL ScalarMeshFieldInitiate( &
!     obj=obj(ii), name="field", fieldType=fieldopt%normal, &
!     varType=fevaropt%constant, engine=engineopt%native_serial, &
!     defineOn=fevaropt%nodal, nns=1, mesh=amesh)
! END DO
!
! CALL OK(obj(tfields)%IsInitiated(), "IsInitiated")
!
! CALL MeshFieldDeallocate(obj)
! CALL OK(.NOT. ALLOCATED(obj), "MeshFieldDeallocate")
!
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate_Ptr_Vector]]

INTERFACE MeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_Vector(obj)
    TYPE(MeshField_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of mesh fields
  END SUBROUTINE obj_Deallocate_Vector
END INTERFACE MeshFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-12
! summary:  Deallocate the vector of MeshFieldPointer_
!
! For every entry of the vector, the data of the mesh field pointed
! by ptr is deallocated (ptr%DEALLOCATE()) and ptr is nullified. The
! target of ptr itself is not freed. Then the vector is deallocated.
!
! Arguments:
!   obj : TYPE(MeshFieldPointer_), ALLOCATABLE(:) - vector of pointers
!
! Examples
!
! The usage is the same as [[obj_Deallocate_Vector]].
!
! See also: [[obj_Deallocate_Vector]]

INTERFACE MeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_Ptr_Vector(obj)
    TYPE(MeshFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of pointers to mesh fields
  END SUBROUTINE obj_Deallocate_Ptr_Vector
END INTERFACE MeshFieldDeallocate

!----------------------------------------------------------------------------
!                                ScalarMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate ScalarMeshField
!
! Initiate a scalar mesh field (rank = scalar). The shape of data in
! an element is:
!
! - s = [1] if varType is constant
! - s = [nns] otherwise (varType is space)
!
! For varType time or spaceTime use [[STScalarMeshFieldInitiate]].
! Only the memory is allocated, use Insert to fill the values.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   name : CHARACTER(*) - name of the field
!   fieldType : INTEGER(I4B) - normal or constant (TypeFieldOpt)
!   varType : INTEGER(I4B) - constant or space
!   engine : CHARACTER(*) - engine, for example NATIVE_SERIAL
!   defineOn : INTEGER(I4B) - nodal or quadrature
!   nns : INTEGER(I4B) - maximum number of nodes (or quadrature
!     points) in an element, not used when varType is constant
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE BaseType, ONLY: math => TypeMathOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: ScalarMeshFieldInitiate
! USE Test_Method, ONLY: OK
! USE UserFunction_Class, ONLY: UserFunction_
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! TYPE(UserFunction_) :: func1, func2
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: fptr => NULL()
! INTEGER(I4B) :: iel, telements, nns, nrow, ncol, tsize
! REAL(DFP), ALLOCATABLE :: xij(:, :), found(:)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! nns = amesh%GetMaxNNE()
! telements = amesh%GetTotalElements()
!
! ! func1 = x + y
! CALL func1%Initiate(name="func1", returnType=fevaropt%scalar, &
!                     argType=fevaropt%space, numArgs=nsd)
! fptr => SumXY
! CALL func1%SetScalarFunctionPointer(func=fptr)
!
! ! func2 = 2 * (x + y)
! CALL func2%Initiate(name="func2", returnType=fevaropt%scalar, &
!                     argType=fevaropt%space, numArgs=nsd)
! fptr => TwiceSumXY
! CALL func2%SetScalarFunctionPointer(func=fptr)
!
! CALL ScalarMeshFieldInitiate( &
!   obj=obj, name="scalarMeshField", fieldType=fieldopt%normal, &
!   varType=fevaropt%space, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, nns=nns, mesh=amesh)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%scalar, "GetRank")
! CALL OK(obj%GetVarType() .EQ. fevaropt%space, "GetVarType")
! CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
!
! ! Insert element by element
! DO iel = 1, telements
!   CALL obj%Insert(func=func1, globalElement=iel, islocal=math%yes)
! END DO
!
! ALLOCATE (xij(3, nns), found(nns))
! xij = 0.0_DFP
!
! CALL OK(Check(1.0_DFP), "Insert(func, globalElement)")
!
! ! Update element 1 by func2
! CALL obj%Set(func=func2, globalElement=1, islocal=math%yes)
! CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
!                         globalElement=1, islocal=math%yes)
! CALL obj%GetSpaceScalarField_(globalElement=1, ans=found, tsize=tsize, &
!                               islocal=math%yes)
! isok = ALL(ABS(found(1:tsize) - &
!                2.0_DFP * (xij(1, 1:ncol) + xij(2, 1:ncol))) .LT. tol)
! CALL OK(isok, "Set(func, globalElement)")
!
! ! Update all the elements by func2
! CALL obj%Set(func=func2)
! CALL OK(Check(2.0_DFP), "Set(func)")
!
! CALL obj%DEALLOCATE()
! CALL func1%DEALLOCATE()
! CALL func2%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! SUBROUTINE SumXY(args, nargs, ans)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans
!   ans = args(1) + args(2)
! END SUBROUTINE SumXY
!
! SUBROUTINE TwiceSumXY(args, nargs, ans)
!   REAL(DFP), INTENT(IN) :: args(:)
!   INTEGER(I4B), INTENT(IN) :: nargs
!   REAL(DFP), INTENT(INOUT) :: ans
!   ans = 2.0_DFP * (args(1) + args(2))
! END SUBROUTINE TwiceSumXY
!
! ! Check that every element contains scale * (x + y) at its nodes
! FUNCTION Check(scale) RESULT(ans)
!   REAL(DFP), INTENT(IN) :: scale
!   LOGICAL(LGT) :: ans
!   INTEGER(I4B) :: jj
!
!   ans = .TRUE.
!   DO jj = 1, telements
!     CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
!                             globalElement=jj, islocal=math%yes)
!     CALL obj%GetSpaceScalarField_(globalElement=jj, ans=found, &
!                                   tsize=tsize, islocal=math%yes)
!     ans = ans .AND. (tsize .EQ. ncol)
!     ans = ans .AND. ALL(ABS(found(1:tsize) - scale * &
!                             (xij(1, 1:ncol) + xij(2, 1:ncol))) .LT. tol)
!   END DO
! END FUNCTION Check
!
! END PROGRAM main
!```
!
! See also: [[STScalarMeshFieldInitiate]], [[obj_Initiate4]]

INTERFACE
  MODULE SUBROUTINE ScalarMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, nns, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type, normal, constant
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type: constant, space
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE ScalarMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                              STScalarMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate space-time scalar mesh field
!
! Initiate a space-time scalar mesh field (rank = scalar). The shape
! of data in an element is:
!
! - s = [1] if varType is constant
! - s = [nnt] if varType is time
! - s = [nns, nnt] otherwise (varType is spaceTime)
!
! Only the memory is allocated, use Insert to fill the values.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   name : CHARACTER(*) - name of the field
!   fieldType : INTEGER(I4B) - normal or constant (TypeFieldOpt)
!   varType : INTEGER(I4B) - constant, time, or spaceTime
!   engine : CHARACTER(*) - engine, for example NATIVE_SERIAL
!   defineOn : INTEGER(I4B) - nodal or quadrature
!   nns : INTEGER(I4B) - maximum number of nodes in space
!   nnt : INTEGER(I4B) - number of nodes in time
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableScalar
! USE BaseType, ONLY: TypeFEVariableSpaceTime
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FEVariable_Method, ONLY: NodalVariable
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: STScalarMeshFieldInitiate
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2, nnt = 2
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! TYPE(FEVariable_) :: fevar, found
! INTEGER(I4B) :: iel, telements, nns, s(4), tsize
! REAL(DFP), ALLOCATABLE :: val(:, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! nns = amesh%GetMaxNNE()
! telements = amesh%GetTotalElements()
!
! CALL STScalarMeshFieldInitiate( &
!   obj=obj, name="stScalar", fieldType=fieldopt%normal, &
!   varType=fevaropt%spaceTime, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, nns=nns, nnt=nnt, mesh=amesh)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%scalar, "GetRank")
! CALL OK(obj%GetVarType() .EQ. fevaropt%spaceTime, "GetVarType")
! CALL OK(obj%GetTotalShape() .EQ. 2, "GetTotalShape")
! CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
!
! ALLOCATE (val(nns, nnt))
! DO iel = 1, telements
!   val = REAL(iel, DFP)
!   fevar = NodalVariable(val, TypeFEVariableScalar, &
!                         TypeFEVariableSpaceTime)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (found%len .EQ. nns * nnt)
!   isok = isok .AND. &
!          ALL(ABS(found%val(1:found%len) - REAL(iel, DFP)) .LT. tol)
! END DO
! CALL OK(isok, "Get")
!
! CALL obj%GetShape(globalElement=1, islocal=math%yes, ans=s, tsize=tsize)
! isok = (tsize .EQ. 2) .AND. (s(1) .EQ. nns) .AND. (s(2) .EQ. nnt)
! CALL OK(isok, "GetShape")
!
! ! varType = time: only nnt values in each element
! CALL STScalarMeshFieldInitiate( &
!   obj=obj, name="tScalar", fieldType=fieldopt%normal, &
!   varType=fevaropt%time, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, nns=nns, nnt=nnt, mesh=amesh)
!
! CALL OK(obj%GetTotalShape() .EQ. 1, "GetTotalShape (time)")
! CALL OK(obj%GetMaxNNE() .EQ. 1, "GetMaxNNE (time)")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[ScalarMeshFieldInitiate]]

INTERFACE
  MODULE SUBROUTINE STScalarMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, nns, nnt, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type, normal, constant
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type: constant, time, spaceTime
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
    INTEGER(I4B), INTENT(IN) :: nnt
    !! Number of node in time
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE STScalarMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                 VectorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate vector mesh field
!
! Initiate a vector mesh field (rank = vector). The shape of data in
! an element is:
!
! - s = [spaceCompo] if varType is constant
! - s = [spaceCompo, nns] otherwise (varType is space)
!
! For varType spaceTime use [[STVectorMeshFieldInitiate]].
! Only the memory is allocated, use Insert to fill the values.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   name : CHARACTER(*) - name of the field
!   fieldType : INTEGER(I4B) - normal or constant (TypeFieldOpt)
!   varType : INTEGER(I4B) - constant or space
!   engine : CHARACTER(*) - engine, for example NATIVE_SERIAL
!   defineOn : INTEGER(I4B) - nodal or quadrature
!   spaceCompo : INTEGER(I4B) - number of space components
!   nns : INTEGER(I4B) - maximum number of nodes (or quadrature
!     points) in an element, not used when varType is constant
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableSpace
! USE BaseType, ONLY: TypeFEVariableVector
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FEVariable_Method, ONLY: NodalVariable
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: VectorMeshFieldInitiate
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! TYPE(FEVariable_) :: fevar
! INTEGER(I4B) :: iel, telements, nns, nrow, ncol, mrow, mcol
! INTEGER(I4B), ALLOCATABLE :: s(:)
! REAL(DFP), ALLOCATABLE :: xij(:, :), found(:, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! nns = amesh%GetMaxNNE()
! telements = amesh%GetTotalElements()
!
! CALL VectorMeshFieldInitiate( &
!   obj=obj, name="xij", fieldType=fieldopt%normal, &
!   varType=fevaropt%space, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, spaceCompo=nsd, nns=nns, mesh=amesh)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
! CALL OK(obj%GetTotalShape() .EQ. 2, "GetTotalShape")
! CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
!
! ALLOCATE (xij(3, nns), found(nsd, nns))
! xij = 0.0_DFP
!
! DO iel = 1, telements
!   CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
!                           globalElement=iel, islocal=math%yes)
!   fevar = NodalVariable(xij(1:nsd, 1:ncol), TypeFEVariableVector, &
!                         TypeFEVariableSpace)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
!                           globalElement=iel, islocal=math%yes)
!   CALL obj%GetSpaceVectorField_(globalElement=iel, ans=found, &
!                                 nrow=mrow, ncol=mcol, islocal=math%yes)
!   isok = isok .AND. (mrow .EQ. nsd) .AND. (mcol .EQ. ncol)
!   isok = isok .AND. ALL(ABS(found(1:mrow, 1:mcol) - &
!                             xij(1:nsd, 1:ncol)) .LT. tol)
! END DO
! CALL OK(isok, "GetSpaceVectorField_")
!
! s = obj%Shape(globalElement=1, islocal=math%yes)
! isok = (SIZE(s) .EQ. 2) .AND. (s(1) .EQ. nsd) .AND. (s(2) .EQ. nns)
! CALL OK(isok, "Shape")
!
! ! A constant vector field stores spaceCompo values in each element
! CALL VectorMeshFieldInitiate( &
!   obj=obj, name="constVec", fieldType=fieldopt%normal, &
!   varType=fevaropt%constant, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, spaceCompo=nsd, nns=nns, mesh=amesh)
! CALL OK(obj%GetTotalShape() .EQ. 1, "GetTotalShape (constant)")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[STVectorMeshFieldInitiate]], [[obj_Initiate4]]

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
    !! variable type: constant, space
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
!                                STVectorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate space-time vector mesh field
!
! Initiate a space-time vector mesh field (rank = vector). The shape
! of data in an element is:
!
! - s = [spaceCompo] if varType is constant
! - s = [spaceCompo, nns, nnt] otherwise (varType is spaceTime)
!
! Only the memory is allocated, use Insert to fill the values.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   name : CHARACTER(*) - name of the field
!   fieldType : INTEGER(I4B) - normal or constant (TypeFieldOpt)
!   varType : INTEGER(I4B) - constant or spaceTime
!   engine : CHARACTER(*) - engine, for example NATIVE_SERIAL
!   defineOn : INTEGER(I4B) - nodal or quadrature
!   spaceCompo : INTEGER(I4B) - number of space components
!   nns : INTEGER(I4B) - maximum number of nodes in space
!   nnt : INTEGER(I4B) - number of nodes in time
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableSpaceTime
! USE BaseType, ONLY: TypeFEVariableVector
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FEVariable_Method, ONLY: NodalVariable
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: STVectorMeshFieldInitiate
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2, nnt = 2
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! TYPE(FEVariable_) :: fevar, found
! INTEGER(I4B) :: iel, telements, nns
! INTEGER(I4B), ALLOCATABLE :: s(:)
! REAL(DFP), ALLOCATABLE :: val(:, :, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! nns = amesh%GetMaxNNE()
! telements = amesh%GetTotalElements()
!
! CALL STVectorMeshFieldInitiate( &
!   obj=obj, name="stVector", fieldType=fieldopt%normal, &
!   varType=fevaropt%spaceTime, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, spaceCompo=nsd, nns=nns, nnt=nnt, &
!   mesh=amesh)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
! CALL OK(obj%GetTotalShape() .EQ. 3, "GetTotalShape")
! CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
!
! ALLOCATE (val(nsd, nns, nnt))
! DO iel = 1, telements
!   val = REAL(iel, DFP)
!   fevar = NodalVariable(val, TypeFEVariableVector, &
!                         TypeFEVariableSpaceTime)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (found%len .EQ. nsd * nns * nnt)
!   isok = isok .AND. &
!          ALL(ABS(found%val(1:found%len) - REAL(iel, DFP)) .LT. tol)
! END DO
! CALL OK(isok, "Get")
!
! s = obj%Shape(globalElement=1, islocal=math%yes)
! isok = (SIZE(s) .EQ. 3) .AND. (s(1) .EQ. nsd) .AND. &
!        (s(2) .EQ. nns) .AND. (s(3) .EQ. nnt)
! CALL OK(isok, "Shape")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[VectorMeshFieldInitiate]]

INTERFACE
  MODULE SUBROUTINE STVectorMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, spaceCompo, nns, &
    nnt, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type, normal, constant
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type: constant, spaceTime
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! number of space components
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
    INTEGER(I4B), INTENT(IN) :: nnt
    !! Number of node in time
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE STVectorMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                  TensorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-30
! summary: Initiate tensor mesh field
!
! Initiate a tensor mesh field (rank = matrix). The shape of data in
! an element is:
!
! - s = [dim1, dim2] if varType is constant
! - s = [dim1, dim2, nns] otherwise (varType is space)
!
! For varType spaceTime use [[STTensorMeshFieldInitiate]].
! Only the memory is allocated, use Insert to fill the values.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   name : CHARACTER(*) - name of the field
!   fieldType : INTEGER(I4B) - normal or constant (TypeFieldOpt)
!   varType : INTEGER(I4B) - constant or space
!   engine : CHARACTER(*) - engine, for example NATIVE_SERIAL
!   defineOn : INTEGER(I4B) - nodal or quadrature
!   dim1, dim2 : INTEGER(I4B) - number of rows and columns of matrix
!   nns : INTEGER(I4B) - maximum number of nodes (or quadrature
!     points) in an element, not used when varType is constant
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableMatrix
! USE BaseType, ONLY: TypeFEVariableSpace
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FEVariable_Method, ONLY: NodalVariable
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: TensorMeshFieldInitiate
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2, dim1 = 2, dim2 = 2
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! TYPE(FEVariable_) :: fevar, found
! INTEGER(I4B) :: iel, telements, nns
! INTEGER(I4B), ALLOCATABLE :: s(:)
! REAL(DFP), ALLOCATABLE :: val(:, :, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! nns = amesh%GetMaxNNE()
! telements = amesh%GetTotalElements()
!
! CALL TensorMeshFieldInitiate( &
!   obj=obj, name="tensor", fieldType=fieldopt%normal, &
!   varType=fevaropt%space, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, dim1=dim1, dim2=dim2, nns=nns, mesh=amesh)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%matrix, "GetRank")
! CALL OK(obj%GetTotalShape() .EQ. 3, "GetTotalShape")
! CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
!
! ALLOCATE (val(dim1, dim2, nns))
! DO iel = 1, telements
!   val = REAL(iel, DFP)
!   fevar = NodalVariable(val, TypeFEVariableMatrix, TypeFEVariableSpace)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (found%len .EQ. dim1 * dim2 * nns)
!   isok = isok .AND. &
!          ALL(ABS(found%val(1:found%len) - REAL(iel, DFP)) .LT. tol)
! END DO
! CALL OK(isok, "Get")
!
! s = obj%Shape(globalElement=1, islocal=math%yes)
! isok = (SIZE(s) .EQ. 3) .AND. (s(1) .EQ. dim1) .AND. &
!        (s(2) .EQ. dim2) .AND. (s(3) .EQ. nns)
! CALL OK(isok, "Shape")
!
! CALL TensorMeshFieldInitiate( &
!   obj=obj, name="constTensor", fieldType=fieldopt%normal, &
!   varType=fevaropt%constant, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, dim1=dim1, dim2=dim2, nns=nns, mesh=amesh)
! CALL OK(obj%GetTotalShape() .EQ. 2, "GetTotalShape (constant)")
! CALL OK(obj%GetMaxNNE() .EQ. 1, "GetMaxNNE (constant)")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[STTensorMeshFieldInitiate]], [[obj_Initiate4]]

INTERFACE
  MODULE SUBROUTINE TensorMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, dim1, dim2, &
    nns, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type, normal, constant
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type: constant, space
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: dim1
    !! size in dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    !! size in dim2
    INTEGER(I4B), INTENT(IN) :: nns
    !! Number of node in space
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE TensorMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                STTensorMeshFieldInitiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Space-Time tensor mesh field
!
! Initiate a space-time tensor mesh field (rank = matrix). The shape
! of data in an element is:
!
! - s = [dim1, dim2] if varType is constant
! - s = [dim1, dim2, nns, nnt] otherwise (varType is spaceTime)
!
! Only the memory is allocated, use Insert to fill the values.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   name : CHARACTER(*) - name of the field
!   fieldType : INTEGER(I4B) - normal or constant (TypeFieldOpt)
!   varType : INTEGER(I4B) - constant or spaceTime
!   engine : CHARACTER(*) - engine, for example NATIVE_SERIAL
!   defineOn : INTEGER(I4B) - nodal or quadrature
!   dim1, dim2 : INTEGER(I4B) - number of rows and columns of matrix
!   nns : INTEGER(I4B) - maximum number of nodes in space
!   nnt : INTEGER(I4B) - number of nodes in time
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: STTensorMeshFieldInitiate
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2, dim1 = 2, dim2 = 2, nnt = 2
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! INTEGER(I4B) :: nns
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! nns = amesh%GetMaxNNE()
!
! CALL STTensorMeshFieldInitiate( &
!   obj=obj, name="stTensor", fieldType=fieldopt%normal, &
!   varType=fevaropt%spaceTime, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, dim1=dim1, dim2=dim2, nns=nns, nnt=nnt, &
!   mesh=amesh)
!
! CALL OK(obj%IsInitiated(), "IsInitiated")
! CALL OK(obj%GetRank() .EQ. fevaropt%matrix, "GetRank")
! CALL OK(obj%GetVarType() .EQ. fevaropt%spaceTime, "GetVarType")
! CALL OK(obj%GetTotalShape() .EQ. 4, "GetTotalShape")
! CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
!
! CALL STTensorMeshFieldInitiate( &
!   obj=obj, name="constTensor", fieldType=fieldopt%normal, &
!   varType=fevaropt%constant, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, dim1=dim1, dim2=dim2, nns=nns, nnt=nnt, &
!   mesh=amesh)
! CALL OK(obj%GetTotalShape() .EQ. 2, "GetTotalShape (constant)")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[TensorMeshFieldInitiate]]

INTERFACE
  MODULE SUBROUTINE STTensorMeshFieldInitiate( &
    obj, name, fieldType, varType, engine, defineOn, dim1, dim2, nns, nnt, &
    mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! Mesh field object
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type, normal, constant
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type: constant, spaceTime
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: dim1
    !! size in dim1
    INTEGER(I4B), INTENT(IN) :: dim2
    !! size in dim2
    INTEGER(I4B), INTENT(IN) :: nns
    !! number of nodes in space
    INTEGER(I4B), INTENT(IN) :: nnt
    !! Number of node in time
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE STTensorMeshFieldInitiate
END INTERFACE

!----------------------------------------------------------------------------
!                                ScalarMeshFieldInitiate4@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Scalar Mesh Field Constructor from user function
!
! Initiate a scalar mesh field from a user function by calling
! [[ScalarMeshFieldInitiate]] with:
!
! - fieldType = constant and varType = constant if the argType of
!   func is constant, otherwise fieldType = normal and
!   varType = func%GetArgType()
! - defineOn = nodal
! - nns = mesh%GetMaxNNE()
!
! The return type of func must be scalar. Only the memory is
! allocated, use Insert to fill the values.
!
! Note: this routine is not public at present.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine of the field
!   nnt : INTEGER(I4B), OPTIONAL - not used
!
! See also: [[ScalarMeshFieldInitiate]], [[obj_Initiate3]]

INTERFACE
  MODULE SUBROUTINE ScalarMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE ScalarMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                              STScalarMeshFieldInitiate4@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Space time scalar mesh field constructor from user function
!
! Initiate a space-time scalar mesh field from a user function by
! calling [[STScalarMeshFieldInitiate]] with:
!
! - fieldType = constant and varType = constant if the argType of
!   func is constant, otherwise fieldType = normal and
!   varType = func%GetArgType()
! - defineOn = nodal
! - nns = mesh%GetMaxNNE()
!
! The return type of func must be scalar, and nnt must be present.
!
! Note: this routine is not public at present.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine of the field
!   nnt : INTEGER(I4B), OPTIONAL - number of nodes in time (required)
!
! See also: [[STScalarMeshFieldInitiate]], [[obj_Initiate3]]

INTERFACE
  MODULE SUBROUTINE STScalarMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE STScalarMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                VectorMeshFieldInitiate4@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Vector mesh field constructor from user function
!
! Initiate a vector mesh field from a user function by calling
! [[VectorMeshFieldInitiate]] with:
!
! - fieldType = constant and varType = constant if the argType of
!   func is constant, otherwise fieldType = normal and
!   varType = func%GetArgType()
! - defineOn = nodal
! - spaceCompo = func%GetNumReturns()
! - nns = mesh%GetMaxNNE()
!
! The return type of func must be vector.
!
! Note: this routine is not public at present.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine of the field
!   nnt : INTEGER(I4B), OPTIONAL - not used
!
! See also: [[VectorMeshFieldInitiate]], [[obj_Initiate3]]

INTERFACE
  MODULE SUBROUTINE VectorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE VectorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                              STVectorMeshFieldInitiate4@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Space-time vector mesh field constructor from user function
!
! Initiate a space-time vector mesh field from a user function by
! calling [[STVectorMeshFieldInitiate]] with:
!
! - fieldType = constant and varType = constant if the argType of
!   func is constant, otherwise fieldType = normal and
!   varType = func%GetArgType()
! - defineOn = nodal
! - spaceCompo = func%GetNumReturns()
! - nns = mesh%GetMaxNNE()
!
! The return type of func must be vector, and nnt must be present.
!
! Note: this routine is not public at present.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine of the field
!   nnt : INTEGER(I4B), OPTIONAL - number of nodes in time (required)
!
! See also: [[STVectorMeshFieldInitiate]], [[obj_Initiate3]]

INTERFACE
  MODULE SUBROUTINE STVectorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE STVectorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                                TensorMeshFieldInitiate4@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Tensor mesh field constructor from user function
!
! Initiate a tensor mesh field from a user function by calling
! [[TensorMeshFieldInitiate]] with:
!
! - fieldType = constant and varType = constant if the argType of
!   func is constant, otherwise fieldType = normal and
!   varType = func%GetArgType()
! - defineOn = nodal
! - [dim1, dim2] = func%GetReturnShape()
! - nns = mesh%GetMaxNNE()
!
! The return type of func must be matrix.
!
! Note: this routine is not public at present.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine of the field
!   nnt : INTEGER(I4B), OPTIONAL - not used
!
! See also: [[TensorMeshFieldInitiate]], [[obj_Initiate3]]

INTERFACE
  MODULE SUBROUTINE TensorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE TensorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                              STTensorMeshFieldInitiate4@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary: Space-time tensor mesh field constructor from user function
!
! Initiate a space-time tensor mesh field from a user function by
! calling [[STTensorMeshFieldInitiate]] with:
!
! - fieldType = constant and varType = constant if the argType of
!   func is constant, otherwise fieldType = normal and
!   varType = func%GetArgType()
! - defineOn = nodal
! - [dim1, dim2] = func%GetReturnShape()
! - nns = mesh%GetMaxNNE()
!
! The return type of func must be matrix, and nnt must be present.
!
! Note: this routine is not public at present.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the field
!   engine : CHARACTER(*) - engine of the field
!   nnt : INTEGER(I4B), OPTIONAL - number of nodes in time (required)
!
! See also: [[STTensorMeshFieldInitiate]], [[obj_Initiate3]]

INTERFACE
  MODULE SUBROUTINE STTensorMeshFieldInitiate4( &
    obj, mesh, func, name, engine, nnt)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE STTensorMeshFieldInitiate4
END INTERFACE

!----------------------------------------------------------------------------
!                            InitiateInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-27
! summary:  Generate interpolation points
!
! Create a vector mesh field which contains the interpolation points
! of each element. The following steps are performed:
!
! 1. Get the maximum number of interpolation points in an element,
!    maxCon, from the finite elements of fedof for the given order and
!    ipType.
! 2. Initiate obj by [[VectorMeshFieldInitiate]] with name = "xij",
!    fieldType = normal, varType = space, defineOn = nodal,
!    spaceCompo = 3, and nns = maxCon.
! 3. Fill obj by [[SetInterpolationPoints]].
!
! The data of an element is a matrix of shape (3, n), where n is the
! number of interpolation points in the element. This field can be
! used as nodeCoordField in WriteData.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   order : INTEGER(I4B)(3) - order of interpolation in x, y, z
!   ipType : INTEGER(I4B)(3) - interpolation point type in x, y, z
!     (TypeInterpolationOpt)
!   fedof : TYPE(FEDOF_) - finite element degree of freedom
!   mesh : CLASS(AbstractMesh_) - mesh
!   engine : CHARACTER(*) - engine of the field
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractFE_Class, ONLY: AbstractFE_
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MeshField_Class, ONLY: InitiateInterpolationPoints
! USE MeshField_Class, ONLY: MeshField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
! INTEGER(I4B), PARAMETER :: order(3) = 2, &
!                            ipType(3) = TypeInterpolationOpt%Equidistance
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MeshField_) :: obj
! CLASS(AbstractMesh_), POINTER :: mesh
! CLASS(AbstractFE_), POINTER :: feptr
! INTEGER(I4B) :: npts, nrow, ncol, mrow, mcol, nsd
! REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!
! CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       fileName=tomlFileName, tomlName="u")
! mesh => dom%GetMeshPointer()
! nsd = mesh%GetNSD()
!
! CALL InitiateInterpolationPoints( &
!   obj=obj, order=order, ipType=ipType, fedof=fedof, mesh=mesh, &
!   engine=engineopt%native_serial)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
! CALL OK(obj%GetVarType() .EQ. fevaropt%space, "GetVarType")
! CALL OK(obj%GetDefineOn() .EQ. fevaropt%nodal, "GetDefineOn")
! CALL OK(obj%GetTotalElements() .EQ. mesh%GetTotalElements(), &
!         "GetTotalElements")
!
! feptr => fedof%GetFEPointer(globalElement=1, islocal=math%yes)
! npts = feptr%GetTotalInterpolationPoints(order=order, ipType=ipType)
!
! ALLOCATE (xij(3, obj%GetMaxNNE()), elemCoord(3, mesh%GetMaxNNE()))
! elemCoord = 0.0_DFP
!
! CALL obj%GetSpaceVectorField_(globalElement=1, ans=xij, nrow=mrow, &
!                               ncol=mcol, islocal=math%yes)
! CALL OK((mrow .EQ. 3) .AND. (mcol .EQ. npts), "shape of element data")
!
! ! The first points are the vertices of the element
! CALL mesh%GetNodeCoord(nodeCoord=elemCoord, nrow=nrow, ncol=ncol, &
!                        globalElement=1, islocal=math%yes)
! isok = ALL(ABS(xij(1:nsd, 1:ncol) - elemCoord(1:nsd, 1:ncol)) .LT. tol)
! CALL OK(isok, "vertices are interpolation points")
!
! NULLIFY (mesh, feptr)
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[SetInterpolationPoints]], [[InitiateQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE InitiateInterpolationPoints( &
    obj, order, ipType, fedof, mesh, engine)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: order(3), ipType(3)
    !! order and interpolation point type in x, y, z
    TYPE(FEDOF_), INTENT(INOUT) :: fedof
    !! finite element degree of freedom
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
  END SUBROUTINE InitiateInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                SetInterpolationPoints@InterpolationMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-27
! summary: Generate interpolation points
!
! Fill an initiated (but empty) vector mesh field with the
! interpolation points. For each element the nodal coordinates of the
! element are obtained from mesh, and the interpolation points are
! computed by the finite element of fedof. Then the points are
! inserted in obj.
!
! obj must be a vector mesh field with spaceCompo = 3,
! varType = space, and nns not less than the maximum number of
! interpolation points in an element.
!
! Arguments:
!   obj : CLASS(MeshField_) - vector mesh field
!   order : INTEGER(I4B)(3) - order of interpolation in x, y, z
!   ipType : INTEGER(I4B)(3) - interpolation point type in x, y, z
!   fedof : TYPE(FEDOF_) - finite element degree of freedom
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractFE_Class, ONLY: AbstractFE_
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MeshField_Class, ONLY: InitiateInterpolationPoints
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: SetInterpolationPoints
! USE MeshField_Class, ONLY: VectorMeshFieldInitiate
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
! INTEGER(I4B), PARAMETER :: order(3) = 2, &
!                            ipType(3) = TypeInterpolationOpt%Equidistance
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MeshField_) :: obj, want
! CLASS(AbstractMesh_), POINTER :: mesh
! CLASS(AbstractFE_), POINTER :: feptr
! INTEGER(I4B) :: iel, telements, maxpts, nrow, ncol, mrow, mcol
! REAL(DFP), ALLOCATABLE :: x1(:, :), x2(:, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!
! CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       fileName=tomlFileName, tomlName="u")
! mesh => dom%GetMeshPointer()
! telements = mesh%GetTotalElements()
!
! ! maximum number of interpolation points in an element
! maxpts = 0
! DO iel = 1, telements
!   feptr => fedof%GetFEPointer(globalElement=iel, islocal=math%yes)
!   maxpts = MAX(maxpts, &
!                feptr%GetTotalInterpolationPoints(order=order, &
!                                                  ipType=ipType))
! END DO
!
! CALL VectorMeshFieldInitiate( &
!   obj=obj, name="xij", fieldType=fieldopt%normal, &
!   varType=fevaropt%space, engine=engineopt%native_serial, &
!   defineOn=fevaropt%nodal, spaceCompo=3, nns=maxpts, mesh=mesh)
!
! CALL SetInterpolationPoints(obj=obj, order=order, ipType=ipType, &
!                             fedof=fedof, mesh=mesh)
!
! CALL InitiateInterpolationPoints( &
!   obj=want, order=order, ipType=ipType, fedof=fedof, mesh=mesh, &
!   engine=engineopt%native_serial)
!
! ALLOCATE (x1(3, maxpts), x2(3, maxpts))
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%GetSpaceVectorField_(globalElement=iel, ans=x1, nrow=nrow, &
!                                 ncol=ncol, islocal=math%yes)
!   CALL want%GetSpaceVectorField_(globalElement=iel, ans=x2, nrow=mrow, &
!                                  ncol=mcol, islocal=math%yes)
!   isok = isok .AND. (nrow .EQ. mrow) .AND. (ncol .EQ. mcol)
!   isok = isok .AND. &
!          ALL(ABS(x1(1:nrow, 1:ncol) - x2(1:nrow, 1:ncol)) .LT. tol)
! END DO
! CALL OK(isok, "SetInterpolationPoints")
!
! NULLIFY (mesh, feptr)
! CALL obj%DEALLOCATE()
! CALL want%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[InitiateInterpolationPoints]]

INTERFACE
  MODULE SUBROUTINE SetInterpolationPoints(obj, order, ipType, fedof, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! vector mesh field
    INTEGER(I4B), INTENT(IN) :: order(3), ipType(3)
    !! order and interpolation point type in x, y, z
    TYPE(FEDOF_), INTENT(INOUT) :: fedof
    !! finite element degree of freedom
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE SetInterpolationPoints
END INTERFACE

!----------------------------------------------------------------------------
!                                 InitiateQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-07
! summary:  Generate quadrature points
!
! Create a vector mesh field which contains the physical coordinates
! of the quadrature points of each element. The following steps are
! performed:
!
! 1. Get the maximum number of quadrature points in an element,
!    maxCon = fedof%GetMaxTotalQuadraturePoints().
! 2. Initiate obj by [[VectorMeshFieldInitiate]] with name = "xij",
!    fieldType = normal, varType = space, defineOn = quadrature,
!    spaceCompo = 3, and nns = maxCon.
! 3. Fill obj by [[SetQuadraturePoints]].
!
! This field can be used as quadField in Initiate and Insert of
! [[AbstractMeshField_]], and as nodeCoordField in WriteData.
!
! Arguments:
!   obj : CLASS(MeshField_) - mesh field
!   fedof : TYPE(FEDOF_) - finite element degree of freedom, it
!     gives the quadrature points
!   mesh : CLASS(AbstractMesh_) - mesh
!   geofedof : TYPE(FEDOF_) - finite element degree of freedom for
!     geometry, it maps the quadrature points to the physical element
!   engine : CHARACTER(*) - engine of the field
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MeshField_Class, ONLY: InitiateQuadraturePoints
! USE MeshField_Class, ONLY: MeshField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MeshField_) :: obj
! CLASS(AbstractMesh_), POINTER :: mesh
! INTEGER(I4B) :: iel, telements, nsd, nrow, ncol, mrow, mcol, ii
! REAL(DFP), ALLOCATABLE :: xij(:, :), elemCoord(:, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!
! CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       fileName=tomlFileName, tomlName="u")
! mesh => dom%GetMeshPointer()
! telements = mesh%GetTotalElements()
! nsd = mesh%GetNSD()
!
! CALL InitiateQuadraturePoints(obj=obj, fedof=fedof, mesh=mesh, &
!                               geofedof=geofedof, &
!                               engine=engineopt%native_serial)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%vector, "GetRank")
! CALL OK(obj%GetDefineOn() .EQ. fevaropt%quadrature, "GetDefineOn")
! CALL OK(obj%GetMaxNNE() .EQ. fedof%GetMaxTotalQuadraturePoints(), &
!         "GetMaxNNE")
!
! ALLOCATE (xij(3, obj%GetMaxNNE()), elemCoord(3, mesh%GetMaxNNE()))
! elemCoord = 0.0_DFP
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL mesh%GetNodeCoord(nodeCoord=elemCoord, nrow=nrow, ncol=ncol, &
!                          globalElement=iel, islocal=math%yes)
!   CALL obj%GetSpaceVectorField_(globalElement=iel, ans=xij, nrow=mrow, &
!                                 ncol=mcol, islocal=math%yes)
!   DO ii = 1, nsd
!     isok = isok .AND. &
!            ALL(xij(ii, 1:mcol) .GE. MINVAL(elemCoord(ii, 1:ncol)) - tol)
!     isok = isok .AND. &
!            ALL(xij(ii, 1:mcol) .LE. MAXVAL(elemCoord(ii, 1:ncol)) + tol)
!   END DO
! END DO
! CALL OK(isok, "quadrature points inside the element")
!
! NULLIFY (mesh)
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[SetQuadraturePoints]], [[InitiateInterpolationPoints]]

INTERFACE
  MODULE SUBROUTINE InitiateQuadraturePoints( &
    obj, fedof, mesh, geofedof, engine)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! mesh field
    TYPE(FEDOF_), INTENT(INOUT) :: fedof
    !! finite element degree of freedom
    TYPE(FEDOF_), INTENT(INOUT) :: geofedof
    !! finite element degree of freedom for geometry
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! abstract mesh
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the field
  END SUBROUTINE InitiateQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!                                      SetQuadraturePoints@QuadratureMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2025-11-07
! summary:  Generate quadrature points
!
! Fill an initiated (but empty) vector mesh field with the physical
! coordinates of quadrature points. For each element:
!
! 1. Get the quadrature points from the finite element of fedof.
! 2. Get the shape functions of the geometry element (geofedof) at
!    these quadrature points.
! 3. Interpolate the nodal coordinates of the element to get the
!    physical coordinates of the quadrature points.
! 4. Insert these coordinates in obj.
!
! obj must be a vector mesh field with spaceCompo = 3,
! varType = space, and nns = fedof%GetMaxTotalQuadraturePoints().
!
! Formula:
!
! $$\mathbf{x}(\xi_q) = \sum_{a} N_a(\xi_q) \, \mathbf{x}_a$$
!
! where \(N_a\) are the shape functions of geofedof, \(\mathbf{x}_a\)
! are the nodal coordinates, and \(\xi_q\) are the quadrature points.
!
! Arguments:
!   obj : CLASS(MeshField_) - vector mesh field
!   fedof : TYPE(FEDOF_) - finite element degree of freedom
!   geofedof : TYPE(FEDOF_) - finite element degree of freedom for
!     geometry
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MeshField_Class, ONLY: InitiateQuadraturePoints
! USE MeshField_Class, ONLY: MeshField_
! USE MeshField_Class, ONLY: SetQuadraturePoints
! USE MeshField_Class, ONLY: VectorMeshFieldInitiate
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MeshField_) :: obj, want
! CLASS(AbstractMesh_), POINTER :: mesh
! INTEGER(I4B) :: iel, telements, maxpts, nrow, ncol, mrow, mcol
! REAL(DFP), ALLOCATABLE :: x1(:, :), x2(:, :)
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!
! CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       fileName=tomlFileName, tomlName="u")
! mesh => dom%GetMeshPointer()
! telements = mesh%GetTotalElements()
! maxpts = fedof%GetMaxTotalQuadraturePoints()
!
! CALL VectorMeshFieldInitiate( &
!   obj=obj, name="xij", fieldType=fieldopt%normal, &
!   varType=fevaropt%space, engine=engineopt%native_serial, &
!   defineOn=fevaropt%quadrature, spaceCompo=3, nns=maxpts, mesh=mesh)
!
! CALL SetQuadraturePoints(obj=obj, fedof=fedof, geofedof=geofedof, &
!                          mesh=mesh)
!
! CALL InitiateQuadraturePoints(obj=want, fedof=fedof, mesh=mesh, &
!                               geofedof=geofedof, &
!                               engine=engineopt%native_serial)
!
! ALLOCATE (x1(3, maxpts), x2(3, maxpts))
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%GetSpaceVectorField_(globalElement=iel, ans=x1, nrow=nrow, &
!                                 ncol=ncol, islocal=math%yes)
!   CALL want%GetSpaceVectorField_(globalElement=iel, ans=x2, nrow=mrow, &
!                                  ncol=mcol, islocal=math%yes)
!   isok = isok .AND. (nrow .EQ. mrow) .AND. (ncol .EQ. mcol)
!   isok = isok .AND. &
!          ALL(ABS(x1(1:nrow, 1:ncol) - x2(1:nrow, 1:ncol)) .LT. tol)
! END DO
! CALL OK(isok, "SetQuadraturePoints")
!
! NULLIFY (mesh)
! CALL obj%DEALLOCATE()
! CALL want%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[InitiateQuadraturePoints]]

INTERFACE
  MODULE SUBROUTINE SetQuadraturePoints(obj, fedof, geofedof, mesh)
    CLASS(MeshField_), INTENT(INOUT) :: obj
    !! vector mesh field
    TYPE(FEDOF_), INTENT(INOUT) :: fedof, geofedof
    !! finite element degree of freedom for field and geometry
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE SetQuadraturePoints
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE MeshField_Class

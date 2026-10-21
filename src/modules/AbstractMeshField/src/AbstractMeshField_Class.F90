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

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-11
! summary: Abstract mesh field, data defined element by element on a mesh
!
! This module defines [[AbstractMeshField_]], an abstract class which
! stores finite element data (scalar, vector, or matrix) element by
! element on a mesh. The data in an element can be constant, or it can
! vary in space, in time, or in space-time. The data can be defined at
! nodes or at quadrature points.
!
! The concrete class is [[MeshField_]] in `MeshField_Class`. All the
! examples in this module use `MeshField_`. The examples are also
! available as tests in `src/modules/MeshField/examples/`.

MODULE AbstractMeshField_Class
USE AbstractMaterial_Class, ONLY: AbstractMaterial_
USE AbstractMaterial_Class, ONLY: AbstractMaterialPointer_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE BaseType, ONLY: FEVariable_
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FieldOpt_Class, ONLY: typefield => TypeFieldOpt
USE GlobalData, ONLY: DFP, I4B, LGT
USE HDF5File_Class, ONLY: HDF5File_
USE String_Class, ONLY: String
USE UserFunction_Class, ONLY: UserFunction_
USE UserFunction_Class, ONLY: UserFunctionPointer_
IMPLICIT NONE

PRIVATE
PUBLIC :: AbstractMeshField_
PUBLIC :: AbstractMeshFieldDeallocate
PUBLIC :: AbstractMeshFieldInitiate
PUBLIC :: AbstractMeshFieldGetShapeAndSize

!----------------------------------------------------------------------------
!                                                         AbstractMeshField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-11
! summary: Abstract mesh field
!
! AbstractMeshField_ stores data of a finite element variable for each
! element of a mesh.
!
! The behavior of the field is defined by the following components:
!
! - fieldType: how the data changes from one element to another.
!   If fieldType is constant then only one element data is stored and
!   it is used for every element (tSize = 1). If fieldType is normal
!   then every element has its own data (tSize = total elements).
! - varType: how the data changes inside an element; constant, space,
!   time, or spaceTime.
! - rank: scalar, vector, or matrix.
! - defineOn: nodal or quadrature.
!
! The data of all elements is stored in a single vector `val`, and the
! shape of the data of all elements is stored in a single vector `ss`.
! The data of local element \(e\) is
! `val(indxVal(e) : indxVal(e+1)-1)`, and its shape is
! `ss(indxShape(e) : indxShape(e+1)-1)`. The data of an element is
! stored in column major order, for example for a vector field which
! varies in space the data is a matrix of shape (spaceCompo, nns).
!
! The size of the shape vector of an element is `totalShape`:
!
! | rank   | constant | space or time | spaceTime |
! |--------|----------|---------------|-----------|
! | scalar | 1        | 1             | 2         |
! | vector | 1        | 2             | 3         |
! | matrix | 2        | 3             | 4         |
!
! Formula:
!
! $$\mathrm{size}(e) = \mathrm{indxVal}(e+1) - \mathrm{indxVal}(e)
!   = \prod_{i=1}^{\mathrm{totalShape}} s_i(e)$$
!
! Notes:
!
! - The data is filled by [[obj_Insert1]] (or other Insert methods).
!   Insert must be called element by element in increasing order of
!   local element number, because it computes `indxVal(e+1)` from
!   `indxVal(e)`.
! - [[obj_Set1]] (and other Set methods) update an element which is
!   already filled by Insert. The size of data must not change.
!
! See also: [[MeshField_]], [[obj_Initiate4]], [[obj_Insert1]],
! [[obj_Get]]

TYPE, ABSTRACT :: AbstractMeshField_
  PRIVATE
  LOGICAL(LGT) :: isInit = .FALSE.
  !! It is true if the object is initiated
  INTEGER(I4B) :: fieldType = typefield%normal
  !! How the data changes from one element to another;
  !! normal (every element has its own data) or constant (same data in
  !! all elements)
  TYPE(String) :: name
  !! name of the field
  TYPE(String) :: engine
  !! Engine of the field, for example
  !! NATIVE_SERIAL, NATIVE_OMP, NATIVE_MPI, PETSC, LIS_SERIAL,
  !! LIS_OMP, LIS_MPI
  INTEGER(I4B) :: tSize = 0
  !! Number of elements for which data is stored;
  !! 1 if fieldType is constant, else total number of elements
  INTEGER(I4B) :: defineOn = 0
  !! Nodal: nodal values
  !! Quadrature: quadrature values
  INTEGER(I4B) :: rank = 0
  !! Scalar, Vector, Matrix
  INTEGER(I4B) :: varType = 0
  !! Constant, Space, Time, SpaceTime
  INTEGER(I4B) :: totalShape = 0
  !! total shape of the data, following rules are used for totalShape
  !! for (scalar, constant) totalShape  = 1
  !! for (scalar, space) totalShape  =  1
  !! for (scalar, time) totalShape  =  1
  !! for (scalar, spaceTime) totalShape  =  2
  !! for (vector, constant) totalShape  = 1
  !! for (vector, space) totalShape  =  2
  !! for (vector, time) totalShape  =  2
  !! for (vector, spaceTime) totalShape  =  3
  !! for (matrix, constant) totalShape  = 2
  !! for (matrix, space) totalShape  =  3
  !! for (matrix, time) totalShape  =  3
  !! for (matrix, spaceTime) totalShape  = 4
  INTEGER(I4B) :: maxShape(fevaropt%maxRank)
  !! maxShape denotes the upper bound of data in each dimension.
  !! it is upper bound for shape of data in a given element.
  INTEGER(I4B), ALLOCATABLE :: ss(:)
  !! shape of the data stored in the MeshField,
  !! shape of element e is ss(indxShape(e):indxShape(e+1)-1)
  INTEGER(I4B), ALLOCATABLE :: indxShape(:)
  !! Index for shape, the size of indxShape is tSize+1
  INTEGER(I4B), ALLOCATABLE :: indxVal(:)
  !! Index for value, The size of indxVal is equal to tSize+1
  !! indxVal(iel) gives the starting index of the element iel
  !! indxVal(iel+1)-indxVal(iel) gives the total number of values in element
  !! iel
  REAL(DFP), ALLOCATABLE :: val(:)
  !! values, val(indxVal(iel):indxVal(iel+1)-1) corresponds to
  !! local element number iel.
  !! These values are decoded by using the shape stored in ss(:)
  CLASS(AbstractMesh_), POINTER :: mesh => NULL()
  !! Mesh which contains the information of the finite element.

CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copying other fields, and different options
  PROCEDURE, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate from Abstract materials
  PROCEDURE, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate from user function
  PROCEDURE, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate from arguments
  PROCEDURE, PASS(obj) :: Initiate5 => obj_Initiate5
  !! Initiate from Abstract materials and quadrature field
  PROCEDURE, PASS(obj) :: Initiate6 => obj_Initiate6
  !! Initiate from user function and quadrature field
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3, &
    Initiate4, Initiate5, Initiate6
  !! Generic initiate method
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: DEALLOCATE => &
    obj_Deallocate
  !! Deallocate the field

  ! @IOMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from hdf5 file
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data in hdf5 file
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: WriteData_vtk => &
    obj_WriteData_vtk
  !! Export data in vtkFile
  GENERIC, PUBLIC :: WriteData => WriteData_vtk
  !! Write data in VTK file

  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Shape => obj_Shape
  !! Return shape
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Get => obj_Get
  !! Getting the value
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Get_ => obj_Get_
  !! Getting the value in FEVariable without allocation
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: IsInitiated => &
    obj_IsInitiated
  !! Returns true if the object is initiated
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetMeshPointer => &
    obj_GetMeshPointer
  !! get mesh pointer
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetMaxNNE => &
    obj_GetMaxNNE
  !! Get maximum number of nodes in space, it gets appropiate entry from
  !! maxShape.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetSpaceVectorField_ => &
    obj_GetSpaceVectorField_
  !! get the space values from vector mesh field without allocation.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetSpaceScalarField_ => &
    obj_GetSpaceScalarField_
  !! get the space values from Scalar mesh field without allocation.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: &
    GetConstantScalarField_ => obj_GetConstantScalarField_
  !! get the Constant values from Scalar mesh field without allocation.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetRank => &
    obj_GetRank
  !! Get rank of the field
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalElements => &
    obj_GetTotalElements
  !! Get total number of elements tSize
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetVarType => &
    obj_GetVarType
  !! Get varType of the field
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetDefineOn => &
    obj_GetDefineOn
  !! Get defineOn of the field
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalShape => &
    obj_GetTotalShape
  !! Get total shape of data, this the size of shape vector
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetShape => &
    obj_GetShape
  !! Get the shape of data for a given element

  ! @AddMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Add1 => obj_Add1
  !! Adding a value to an element
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Add2 => obj_Add2
  !! Add a value to all the elements
  GENERIC, PUBLIC :: Add => Add1, Add2

  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Setting the value by using FEVariable_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Setting the value by using UserFunction_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Setting the value by using material
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set all values by using the FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set the value of an element by using the user function
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set the values by using AbstractMaterialPointer_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set the values by using user function pointers

  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, Set7

  ! @InsertMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert1 => obj_Insert1
  !! Insertting the value by using FEVariable_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert2 => obj_Insert2
  !! Insertting the value by using UserFunction_
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert3 => obj_Insert3
  !! Insertting the value by using material
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert4 => obj_Insert4
  !! Insert all values by using the FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert5 => obj_Insert5
  !! Insert globalElement value by using user function
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert6 => obj_Insert6
  !!  Insert the values by using AbstractMaterialPointer
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert7 => obj_Insert7
  !! Insert the values by using user functions
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert8 => obj_Insert8
  !! Insert the values by using user functions and quadField
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Insert9 => obj_Insert9
  !! Insert the values in a element using user functions and
  !! quadrature points

  GENERIC, PUBLIC :: Insert => Insert1, Insert2, Insert3, Insert4, &
    Insert5, Insert6, Insert7, Insert8, Insert9
END TYPE AbstractMeshField_

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-24
! summary: Initiate by copying other fields, and different options
!
! Initiate obj by copying all the data of obj2. The arrays (val,
! indxVal, ss, indxShape) are copied, and the mesh pointer of obj
! points to the mesh of obj2.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field to initiate
!   obj2 : CLASS(AbstractMeshField_) - mesh field to copy from
!   copyFull, copyStructure, usePointer : LOGICAL(LGT), OPTIONAL -
!     reserved for future use, they are not used at present
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableConstant
! USE BaseType, ONLY: TypeFEVariableScalar
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
! TYPE(FEMesh_), TARGET :: amesh
! TYPE(MeshField_) :: obj, obj2
! TYPE(FEVariable_) :: fevar, found
! CLASS(AbstractMesh_), POINTER :: meshptr
! INTEGER(I4B) :: iel, telements
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
! telements = amesh%GetTotalElements()
!
! CALL obj2%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%normal, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)
!
! DO iel = 1, telements
!   fevar = NodalVariable(REAL(iel, DFP), TypeFEVariableScalar, &
!                         TypeFEVariableConstant)
!   CALL obj2%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! CALL obj%Initiate(obj2=obj2)
!
! CALL OK(obj%IsInitiated(), "IsInitiated")
! CALL OK(obj%GetTotalElements() .EQ. telements, "GetTotalElements")
!
! meshptr => obj%GetMeshPointer()
! CALL OK(ASSOCIATED(meshptr, amesh), "GetMeshPointer")
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (ABS(found%val(1) - REAL(iel, DFP)) .LT. tol)
! END DO
! CALL OK(isok, "copied values")
!
! NULLIFY (meshptr)
! CALL obj%DEALLOCATE()
! CALL obj2%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate4]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field to initiate
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj2
    !! mesh field to copy from
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    !! not used at present
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    !! not used at present
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
    !! not used at present
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-24
! summary: Initiate from abstractMaterials
!
! We first search the name in material.
! If the name is found in the material then we get the pointer to
! user function corresponding to the material name.
! Then we call [[obj_Initiate3]] method with this user function.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   material : CLASS(AbstractMaterial_) - material which contains
!     the user function `name`
!   name : CHARACTER(*) - name of the material property
!   engine : CHARACTER(*) - engine of the mesh field
!   nnt : INTEGER(I4B), OPTIONAL - number of nodes in time
!
! Examples
!
! The usage is the same as [[obj_Initiate3]], but the user function is
! taken from the material.
!
! See also: [[obj_Initiate3]], [[obj_Initiate5]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, mesh, material, name, engine, nnt)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the material
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-24
! summary: Initiate from UserFunction_
!
! This method initiates AbstractMeshField from a user function.
! Only the memory is allocated, the values are not filled. Use
! [[obj_Insert2]] to fill the values.
!
! The following options are used:
!
! - fieldType = normal
! - varType = func%GetArgType()
! - rank = func%GetReturnType()
! - engine = engine
! - defineOn = nodal
! - nns = mesh%GetMaxNNE()
!
! The shape of data is obtained from
! [[AbstractMeshFieldGetShapeAndSize]].
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   mesh : CLASS(AbstractMesh_) - mesh
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the mesh field
!   engine : CHARACTER(*) - engine of the mesh field
!   nnt : INTEGER(I4B), OPTIONAL - number of nodes in time, needed
!     when argType of func is time or spaceTime
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
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
! TYPE(UserFunction_) :: func
! TYPE(FEVariable_) :: found
! INTEGER(I4B) :: iel, telements
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
! telements = amesh%GetTotalElements()
!
! CALL func%Initiate(name="func", returnType=fevaropt%scalar, &
!                    argType=fevaropt%constant)
! CALL func%SetScalarConstantVal(val=2.0_DFP)
!
! CALL obj%Initiate(mesh=amesh, func=func, name="func", &
!                   engine=engineopt%native_serial)
!
! CALL OK(obj%GetRank() .EQ. fevaropt%scalar, "GetRank")
! CALL OK(obj%GetVarType() .EQ. fevaropt%constant, "GetVarType")
! CALL OK(obj%GetDefineOn() .EQ. fevaropt%nodal, "GetDefineOn")
! CALL OK(obj%GetTotalElements() .EQ. telements, "GetTotalElements")
!
! ! Fill all the elements
! CALL obj%Insert(func=func)
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (ABS(found%val(1) - 2.0_DFP) .LT. tol)
! END DO
! CALL OK(isok, "Insert(func)")
!
! ! Update all the elements
! CALL func%SetScalarConstantVal(val=3.0_DFP)
! CALL obj%Set(func=func)
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (ABS(found%val(1) - 3.0_DFP) .LT. tol)
! END DO
! CALL OK(isok, "Set(func)")
!
! CALL obj%DEALLOCATE()
! CALL func%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Initiate2]], [[obj_Insert2]], [[obj_Set2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, mesh, func, name, engine, nnt)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE obj_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-29
! summary:  Initiate AbstractMeshField_ from arguments
!
! This routine is used to initiate the AbstractMeshField_ from
! arguments. Only the memory is allocated, the values are not filled.
! Use Insert methods to fill the values.
!
! - If fieldType is constant then data is stored for one element
!   (tSize = 1), otherwise tSize = mesh%GetTotalElements().
! - The size of s is totalShape, and s is the maximum shape of data in
!   an element (maxShape). The memory for val is
!   PRODUCT(s) * tSize.
! - The correct s for given rank and varType can be obtained from
!   [[AbstractMeshFieldGetShapeAndSize]].
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   name : CHARACTER(*) - name of the field
!   fieldType : INTEGER(I4B) - normal or constant (TypeFieldOpt)
!   engine : CHARACTER(*) - engine, for example NATIVE_SERIAL
!   defineOn : INTEGER(I4B) - nodal or quadrature
!   varType : INTEGER(I4B) - constant, space, time, or spaceTime
!   rank : INTEGER(I4B) - scalar, vector, or matrix
!   s : INTEGER(I4B)(:) - maximum shape of data in an element
!   mesh : CLASS(AbstractMesh_) - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableConstant
! USE BaseType, ONLY: TypeFEVariableScalar
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
! TYPE(FEVariable_) :: fevar, found
! INTEGER(I4B) :: iel, telements, s(4), tsize
! REAL(DFP) :: aval
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%normal, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)
!
! telements = amesh%GetTotalElements()
! CALL OK(obj%GetTotalElements() .EQ. telements, "GetTotalElements")
!
! ! Insert must be called element by element, in increasing order
! DO iel = 1, telements
!   fevar = NodalVariable(REAL(iel, DFP), TypeFEVariableScalar, &
!                         TypeFEVariableConstant)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (ABS(found%val(1) - REAL(iel, DFP)) .LT. tol)
!
!   CALL obj%GetConstantScalarField_(globalElement=iel, ans=aval, &
!                                    islocal=math%yes)
!   isok = isok .AND. (ABS(aval - REAL(iel, DFP)) .LT. tol)
! END DO
! CALL OK(isok, "Get and GetConstantScalarField_")
!
! CALL obj%GetShape(globalElement=1, islocal=math%yes, ans=s, tsize=tsize)
! CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. 1), "GetShape")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Insert1]], [[AbstractMeshFieldGetShapeAndSize]]

INTERFACE AbstractMeshFieldInitiate
  MODULE SUBROUTINE obj_Initiate4( &
    obj, name, fieldType, engine, defineOn, varType, rank, s, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    INTEGER(I4B), INTENT(IN) :: fieldType
    !! field type
    CHARACTER(*), INTENT(IN) :: engine
    !! engine
    INTEGER(I4B), INTENT(IN) :: defineOn
    !! define on Nodal or Quadrature
    INTEGER(I4B), INTENT(IN) :: varType
    !! variable type
    !! how the field varies inside the element
    !! space, time, spaceTime, constant
    INTEGER(I4B), INTENT(IN) :: rank
    !! rank of the field, scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: s(:)
    !! shape of the field
    CLASS(AbstractMesh_), TARGET, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE obj_Initiate4
END INTERFACE AbstractMeshFieldInitiate

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-24
! summary: Initiate from abstract materials and quadrature fields
!
! We first search the name in material.
! If the name is found in the material then we get the pointer to
! user function corresponding to the material name.
! Then we call [[obj_Initiate6]] method with this user function.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   quadField : CLASS(AbstractMeshField_) - vector mesh field which
!     contains the quadrature points
!   material : CLASS(AbstractMaterial_) - material which contains
!     the user function `name`
!   name : CHARACTER(*) - name of the material property
!   engine : CHARACTER(*) - engine of the mesh field
!   nnt : INTEGER(I4B), OPTIONAL - number of nodes in time
!
! Examples
!
! The usage is the same as [[obj_Initiate6]], but the user function is
! taken from the material.
!
! See also: [[obj_Initiate6]], [[obj_Initiate2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate5(obj, quadField, material, name, &
                                  engine, nnt)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMeshField_), INTENT(INOUT) :: quadField
    !! vector mesh field for quadrature fields
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the material
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE obj_Initiate5
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-24
! summary: Initiate from UserFunction_ at quadrature points
!
! This method initiates AbstractMeshField from a user function. The
! data is defined at the quadrature points stored in quadField.
! Only the memory is allocated, the values are not filled. Use
! [[obj_Insert9]] to fill the values.
!
! The following options are used:
!
! - fieldType = normal
! - varType = func%GetArgType()
! - rank = func%GetReturnType()
! - engine = engine
! - defineOn = quadrature
! - nns = quadField%GetMaxNNE(), maximum number of quadrature points
! - mesh = quadField%GetMeshPointer()
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   quadField : CLASS(AbstractMeshField_) - vector mesh field which
!     contains the quadrature points, see InitiateQuadraturePoints
!   func : CLASS(UserFunction_) - user function
!   name : CHARACTER(*) - name of the mesh field
!   engine : CHARACTER(*) - engine of the mesh field
!   nnt : INTEGER(I4B), OPTIONAL - number of nodes in time
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
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
! USE UserFunction_Class, ONLY: UserFunction_
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml"
! INTEGER(I4B), PARAMETER :: nsd = 2
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MeshField_) :: quadField, obj
! TYPE(UserFunction_) :: func
! PROCEDURE(InterfaceScalarSubroutine), POINTER :: fptr => NULL()
! CLASS(AbstractMesh_), POINTER :: mesh
! INTEGER(I4B) :: iel, telements, nrow, ncol, tsize
! REAL(DFP), ALLOCATABLE :: xij(:, :), found(:)
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
! CALL InitiateQuadraturePoints(obj=quadField, fedof=fedof, mesh=mesh, &
!                               geofedof=geofedof, &
!                               engine=engineopt%native_serial)
!
! CALL func%Initiate(name="func", returnType=fevaropt%scalar, &
!                    argType=fevaropt%space, numArgs=nsd)
! fptr => SumXY
! CALL func%SetScalarFunctionPointer(func=fptr)
!
! CALL obj%Initiate(quadField=quadField, func=func, name="func", &
!                   engine=engineopt%native_serial)
!
! CALL OK(obj%GetDefineOn() .EQ. fevaropt%quadrature, "GetDefineOn")
! CALL OK(obj%GetVarType() .EQ. fevaropt%space, "GetVarType")
! CALL OK(obj%GetMaxNNE() .EQ. quadField%GetMaxNNE(), "GetMaxNNE")
!
! DO iel = 1, telements
!   CALL obj%Insert(quadField=quadField, func=func, globalElement=iel, &
!                   islocal=math%yes)
! END DO
!
! ALLOCATE (xij(3, quadField%GetMaxNNE()), found(quadField%GetMaxNNE()))
! isok = .TRUE.
! DO iel = 1, telements
!   CALL quadField%GetSpaceVectorField_(globalElement=iel, ans=xij, &
!                                       nrow=nrow, ncol=ncol, &
!                                       islocal=math%yes)
!   CALL obj%GetSpaceScalarField_(globalElement=iel, ans=found, &
!                                 tsize=tsize, islocal=math%yes)
!   isok = isok .AND. (tsize .EQ. ncol)
!   isok = isok .AND. ALL(ABS(found(1:tsize) - &
!                             (xij(1, 1:ncol) + xij(2, 1:ncol))) .LT. tol)
! END DO
! CALL OK(isok, "Insert(quadField, func, globalElement)")
!
! NULLIFY (mesh)
! CALL obj%DEALLOCATE()
! CALL quadField%DEALLOCATE()
! CALL func%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
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
! END PROGRAM main
!```
!
! See also: [[obj_Initiate5]], [[obj_Insert9]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate6(obj, quadField, func, name, engine, nnt)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMeshField_), INTENT(INOUT) :: quadField
    !! vector mesh field for quadrature fields
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    CHARACTER(*), INTENT(IN) :: engine
    !! engine of the AbstractMeshField
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes in time
  END SUBROUTINE obj_Initiate6
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-21
! summary: Deallocates data in [[AbstractMeshField_]]
!
! Deallocate all the data, reset all the options to the default
! values, and nullify the mesh pointer. After this call
! obj%IsInitiated() returns false.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableConstant
! USE BaseType, ONLY: TypeFEVariableScalar
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
! TYPE(FEVariable_) :: fevar, found
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%constant, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)
!
! CALL OK(obj%IsInitiated(), "IsInitiated")
! CALL OK(obj%GetTotalElements() .EQ. 1, "GetTotalElements (constant)")
! CALL OK(obj%GetRank() .EQ. fevaropt%scalar, "GetRank")
! CALL OK(obj%GetVarType() .EQ. fevaropt%constant, "GetVarType")
! CALL OK(obj%GetDefineOn() .EQ. fevaropt%nodal, "GetDefineOn")
! CALL OK(obj%GetTotalShape() .EQ. 1, "GetTotalShape")
! CALL OK(obj%GetMaxNNE() .EQ. 1, "GetMaxNNE")
!
! fevar = NodalVariable(10.0_DFP, TypeFEVariableScalar, &
!                       TypeFEVariableConstant)
! CALL obj%Insert(globalElement=1, islocal=math%yes, fevar=fevar)
!
! ! For a constant field every element returns the same value
! CALL obj%Get(globalElement=5, islocal=math%yes, fevar=found)
! isok = (found%len .EQ. 1) .AND. &
!        (ABS(found%val(1) - 10.0_DFP) .LT. tol)
! CALL OK(isok, "Get value of constant field")
!
! CALL obj%DEALLOCATE()
! CALL OK(.NOT. obj%IsInitiated(), "Deallocate")
!
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_IsInitiated]]

INTERFACE AbstractMeshFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractMeshFieldDeallocate

!----------------------------------------------------------------------------
!                                                           Shape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: This function returns the shape of data in an element
!
! Returns the shape of the data stored in globalElement. The size of
! the returned value is totalShape:
!
! - scalar: [nns] for space, [nnt] for time, [nns, nnt] for spaceTime,
!   [1] for constant
! - vector: [spaceCompo] for constant, [spaceCompo, nns] for space,
!   [spaceCompo, nns, nnt] for spaceTime
! - matrix: [dim1, dim2] for constant, [dim1, dim2, nns] for space,
!   [dim1, dim2, nns, nnt] for spaceTime
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT) - if true globalElement is local
!
! Returns:
!   ans : INTEGER(I4B), ALLOCATABLE(:) - shape of data in the element
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
! See also: [[obj_GetShape]], [[obj_GetTotalShape]]

INTERFACE
  MODULE FUNCTION obj_Shape(obj, globalElement, islocal) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalElement is local element
    INTEGER(I4B), ALLOCATABLE :: ans(:)
    !! shape of data in the element, size of ans is totalShape
  END FUNCTION obj_Shape
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Get the values from AbstractMeshField_
!
! Get the data of globalElement in an FEVariable. The memory of
! fevar%val is reallocated if needed. The rank, varType, defineOn, and
! shape of fevar are set from obj. The values are in
! fevar%val(1:fevar%len). If fieldType is constant then the data of
! the only stored element is returned for every globalElement.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   globalElement : INTEGER(I4B) - global or local element number
!   fevar : TYPE(FEVariable_) - data of the element
!   islocal : LOGICAL(LGT) - if true globalElement is local
!
! Examples
!
! See the example of [[obj_Initiate4]].
!
! See also: [[obj_Get_]], [[obj_Insert1]]

INTERFACE
  MODULE SUBROUTINE obj_Get(obj, globalElement, fevar, islocal)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then global element is local element
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Get
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-24
! summary: Get the values from AbstractMeshField_ without allocation
!
! This method is like [[obj_Get]], but it does not allocate memory.
! fevar must be initiated before calling this method, and:
!
! - fevar%capacity must not be less than the size of element data
! - fevar%defineOn must be equal to obj%defineOn
! - fevar%rank must be equal to obj%rank
!
! These conditions are checked only in the debug mode. fevar%varType
! is set from obj.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   globalElement : INTEGER(I4B) - global or local element number
!   fevar : TYPE(FEVariable_) - data of the element
!   islocal : LOGICAL(LGT) - if true globalElement is local
!
! Examples
!
! The usage is the same as [[obj_Get]].
!
! See also: [[obj_Get]]

INTERFACE
  MODULE SUBROUTINE obj_Get_(obj, globalElement, fevar, islocal)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true, then global element is local element
    TYPE(FEVariable_), INTENT(INOUT) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Get_
END INTERFACE

!----------------------------------------------------------------------------
!                                                   IsInitiated@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-28
! summary:  Returns obj%isInit
!
! Returns true if obj is initiated by one of the Initiate methods.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : LOGICAL(LGT) - true if obj is initiated
!
! Examples
!
! See the example of [[obj_Deallocate]].

INTERFACE
  MODULE FUNCTION obj_IsInitiated(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    LOGICAL(LGT) :: ans
    !! true if obj is initiated
  END FUNCTION obj_IsInitiated
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetMeshPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary:  Returns mesh pointer stored inside the AbstractMeshField
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : CLASS(AbstractMesh_), POINTER - mesh of the mesh field
!
! Examples
!
! See the example of [[obj_Initiate1]].

INTERFACE
  MODULE FUNCTION obj_GetMeshPointer(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    CLASS(AbstractMesh_), POINTER :: ans
    !! mesh pointer
  END FUNCTION obj_GetMeshPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetMaxNNE@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get maximum number of nodes in element
!
! Get the maximum number of spatial nodes (or quadrature points) in an
! element. This method returns an appropiate entry from maxShape.
! Following rule has been used.
!
! | rank   | constant or time | space or spaceTime |
! |--------|------------------|--------------------|
! | scalar | 1                | maxShape(1)        |
! | vector | 1                | maxShape(2)        |
! | matrix | 1                | maxShape(3)        |
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : INTEGER(I4B) - maximum number of spatial nodes in an element
!
! Examples
!
! See the example of [[obj_Shape]].

INTERFACE
  MODULE FUNCTION obj_GetMaxNNE(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B) :: ans
    !! maximum number of spatial nodes in an element
  END FUNCTION obj_GetMaxNNE
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetSpaceVectorField_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the element value of vector meshField
!
! Get the value of vector mesh field which has vartype = space.
! The result will be a two dimension array. The first index of ans
! represents the space components, and second index represents the
! spatial points. Only ans(1:nrow, 1:ncol) is written, so ans should
! be large enough, for example ans(spaceCompo, obj%GetMaxNNE()).
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - vector mesh field (varType=space)
!   globalElement : INTEGER(I4B) - global or local element number
!   ans : REAL(DFP)(:, :) - element value
!   nrow, ncol : INTEGER(I4B) - number of rows and columns written in
!     ans
!   islocal : LOGICAL(LGT), OPTIONAL - if true globalElement is local
!
! Examples
!
! See the example of [[obj_Shape]].
!
! See also: [[obj_GetSpaceScalarField_]]

INTERFACE
  MODULE SUBROUTINE obj_GetSpaceVectorField_( &
    obj, globalElement, ans, nrow, ncol, islocal)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! abstract mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global element number
    REAL(DFP), INTENT(INOUT) :: ans(:, :)
    !! element level value of vector mesh field
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and cols in ans
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! is globalElement local element
  END SUBROUTINE obj_GetSpaceVectorField_
END INTERFACE

!----------------------------------------------------------------------------
!                                            GetSpaceScalarField_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the element value of Scalar meshField
!
! Get the value of scalar mesh field which has vartype = space.
! The result will be a one dimension array, which represents the value
! at spatial nodes (or quadrature points). Only ans(1:tsize) is
! written, so ans should be large enough, for example
! ans(obj%GetMaxNNE()).
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - scalar mesh field (varType=space)
!   globalElement : INTEGER(I4B) - global or local element number
!   ans : REAL(DFP)(:) - element value
!   tsize : INTEGER(I4B) - number of values written in ans
!   islocal : LOGICAL(LGT), OPTIONAL - if true globalElement is local
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableScalar
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
! INTEGER(I4B) :: iel, telements, nns, nrow, ncol, tsize, s(4)
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
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%normal, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%space, rank=fevaropt%scalar, s=[nns], mesh=amesh)
!
! CALL OK(obj%GetMaxNNE() .EQ. nns, "GetMaxNNE")
! CALL OK(obj%GetTotalShape() .EQ. 1, "GetTotalShape")
!
! ALLOCATE (xij(3, nns), found(nns))
! xij = 0.0_DFP
!
! DO iel = 1, telements
!   CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
!                           globalElement=iel, islocal=math%yes)
!   fevar = NodalVariable(xij(1, 1:ncol), TypeFEVariableScalar, &
!                         TypeFEVariableSpace)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
!                           globalElement=iel, islocal=math%yes)
!   CALL obj%GetSpaceScalarField_(globalElement=iel, ans=found, &
!                                 tsize=tsize, islocal=math%yes)
!   isok = isok .AND. (tsize .EQ. ncol)
!   isok = isok .AND. ALL(ABS(found(1:tsize) - xij(1, 1:ncol)) .LT. tol)
! END DO
! CALL OK(isok, "GetSpaceScalarField_")
!
! CALL obj%GetShape(globalElement=1, islocal=math%yes, ans=s, tsize=tsize)
! CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. nns), "GetShape")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetSpaceVectorField_]],
! [[obj_GetConstantScalarField_]]

INTERFACE
  MODULE SUBROUTINE obj_GetSpaceScalarField_( &
    obj, globalElement, ans, tsize, islocal)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! abstract mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    REAL(DFP), INTENT(INOUT) :: ans(:)
    !! Element level value of Scalar mesh field
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written in ans
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! Is globalElement local element
  END SUBROUTINE obj_GetSpaceScalarField_
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetConstantScalarField_@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the element value of Scalar meshField
!
! Get the value of scalar mesh field which has vartype = constant.
! The result will be a scalar.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - scalar mesh field
!     (varType=constant)
!   globalElement : INTEGER(I4B) - global or local element number
!   ans : REAL(DFP) - element value
!   islocal : LOGICAL(LGT), OPTIONAL - if true globalElement is local
!
! Examples
!
! See the example of [[obj_Initiate4]].
!
! See also: [[obj_GetSpaceScalarField_]]

INTERFACE
  MODULE SUBROUTINE obj_GetConstantScalarField_( &
    obj, globalElement, ans, islocal)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! abstract mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! Global element number
    REAL(DFP), INTENT(INOUT) :: ans
    !! Element level value of Scalar mesh field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! Is globalElement local element
  END SUBROUTINE obj_GetConstantScalarField_
END INTERFACE

!----------------------------------------------------------------------------
!                                                         GetRank@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the rank of mesh field
!
! Get the rank of data stored in obj; scalar, vector, or matrix
! (see TypeFEVariableOpt).
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : INTEGER(I4B) - rank of the field
!
! Examples
!
! See the example of [[obj_Deallocate]].

INTERFACE
  MODULE FUNCTION obj_GetRank(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B) :: ans
    !! rank of the field
  END FUNCTION obj_GetRank
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalElements@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the total number of elements in obj
!
! Get the number of elements for which data is stored (tSize). It is
! 1 if fieldType is constant, otherwise it is the total number of
! elements in the mesh.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : INTEGER(I4B) - tSize
!
! Examples
!
! See the examples of [[obj_Deallocate]] and [[obj_Initiate4]].

INTERFACE
  MODULE FUNCTION obj_GetTotalElements(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B) :: ans
    !! total number of elements stored in obj
  END FUNCTION obj_GetTotalElements
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetVarType@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the varType of obj
!
! Get the varType of obj; constant, space, time, or spaceTime.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : INTEGER(I4B) - varType
!
! Examples
!
! See the example of [[obj_Deallocate]].

INTERFACE
  MODULE FUNCTION obj_GetVarType(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B) :: ans
    !! varType
  END FUNCTION obj_GetVarType
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetDefineOn@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the DefineOn of obj
!
! Get the defineOn of obj; nodal or quadrature.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : INTEGER(I4B) - defineOn
!
! Examples
!
! See the example of [[obj_Deallocate]].

INTERFACE
  MODULE FUNCTION obj_GetDefineOn(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B) :: ans
    !! defineOn
  END FUNCTION obj_GetDefineOn
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTotalShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the total shape of obj
!
! Get the total shape of data. Ans is the size of shape vector of
! data in an element.
!
! | rank   | constant | space or time | spaceTime |
! |--------|----------|---------------|-----------|
! | scalar | 1        | 1             | 2         |
! | vector | 1        | 2             | 3         |
! | matrix | 2        | 3             | 4         |
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!
! Returns:
!   ans : INTEGER(I4B) - size of shape vector
!
! Examples
!
! See the example of [[obj_Deallocate]].
!
! See also: [[obj_Shape]], [[obj_GetShape]]

INTERFACE
  MODULE FUNCTION obj_GetTotalShape(obj) RESULT(ans)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B) :: ans
    !! size of shape vector
  END FUNCTION obj_GetTotalShape
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetShape@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-02-27
! summary: Get the shape of data in an element
!
! Get the shape of data in an element without allocation. This method
! is like [[obj_Shape]], but the result is written in ans(1:tsize).
! The size of ans should not be less than obj%GetTotalShape().
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT) - if true globalElement is local
!   ans : INTEGER(I4B)(:) - shape of data
!   tsize : INTEGER(I4B) - number of entries written in ans
!
! Examples
!
! See the example of [[obj_GetSpaceScalarField_]].
!
! See also: [[obj_Shape]]

INTERFACE
  MODULE SUBROUTINE obj_GetShape(obj, globalElement, islocal, ans, tsize)
    CLASS(AbstractMeshField_), INTENT(IN) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true globalElement is a local element
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! shape of data in the element
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in ans
  END SUBROUTINE obj_GetShape
END INTERFACE

!----------------------------------------------------------------------------
!                                  ScalarMeshFieldGetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of scalar and stscalar within the element
!
! Get the shape s(1:tsize) of the scalar data in an element:
!
! - constant: s = [1]
! - space: s = [nns]
! - time: s = [nnt]
! - spaceTime: s = [nns, nnt]
!
! Arguments:
!   varType : INTEGER(I4B) - constant, space, time, or spaceTime
!   s : INTEGER(I4B)(4) - shape of data
!   tsize : INTEGER(I4B) - number of entries written in s
!   nns : INTEGER(I4B), OPTIONAL - number of spatial nodes
!   nnt : INTEGER(I4B), OPTIONAL - number of time nodes
!
! Examples
!
! See the example of [[AbstractMeshFieldGetShapeAndSize]].

INTERFACE
  MODULE SUBROUTINE ScalarMeshFieldGetShapeAndSize(varType, s, tsize, &
                                                   nns, nnt)
    INTEGER(I4B), INTENT(IN) :: varType
    !! constant, space, time, spaceTime
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    !! shape of data
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in s
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns
    !! number of nodes or quadrature points in an element
    !! nns should be present when varType is space or spaceTime
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes or quadrature points in a time element
    !! nnt should be present when varType is time or spaceTime
  END SUBROUTINE ScalarMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                  VectorMeshFieldGetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of vector and stvector within the element
!
! Get the shape s(1:tsize) of the vector data in an element:
!
! - constant: s = [spaceCompo]
! - space: s = [spaceCompo, nns]
! - time: s = [spaceCompo, nnt]
! - spaceTime: s = [spaceCompo, nns, nnt]
!
! Arguments:
!   varType : INTEGER(I4B) - constant, space, time, or spaceTime
!   s : INTEGER(I4B)(4) - shape of data
!   tsize : INTEGER(I4B) - number of entries written in s
!   spaceCompo : INTEGER(I4B), OPTIONAL - number of space components
!   nns : INTEGER(I4B), OPTIONAL - number of spatial nodes
!   nnt : INTEGER(I4B), OPTIONAL - number of time nodes
!
! Examples
!
! See the example of [[AbstractMeshFieldGetShapeAndSize]].

INTERFACE
  MODULE SUBROUTINE VectorMeshFieldGetShapeAndSize( &
    varType, s, tsize, spaceCompo, nns, nnt)
    INTEGER(I4B), INTENT(IN) :: varType
    !! constant, space, time, spaceTime
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    !! shape of data
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in s
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space components
    !! spaceCompo should be present
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns
    !! number of nodes or quadrature points in an element
    !! nns should be present when varType is space or spaceTime
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes or quadrature points in a time element
    !! nnt should be present when varType is time or spaceTime
  END SUBROUTINE VectorMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                  TensorMeshFieldGetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of tensor and sttensor within the element
!
! Get the shape s(1:tsize) of the matrix data in an element:
!
! - constant: s = [dim1, dim2]
! - space: s = [dim1, dim2, nns]
! - time: s = [dim1, dim2, nnt]
! - spaceTime: s = [dim1, dim2, nns, nnt]
!
! Arguments:
!   varType : INTEGER(I4B) - constant, space, time, or spaceTime
!   s : INTEGER(I4B)(4) - shape of data
!   tsize : INTEGER(I4B) - number of entries written in s
!   dim1, dim2 : INTEGER(I4B), OPTIONAL - size of the matrix
!   nns : INTEGER(I4B), OPTIONAL - number of spatial nodes
!   nnt : INTEGER(I4B), OPTIONAL - number of time nodes
!
! Examples
!
! See the example of [[AbstractMeshFieldGetShapeAndSize]].

INTERFACE
  MODULE SUBROUTINE TensorMeshFieldGetShapeAndSize( &
    varType, s, tsize, dim1, dim2, nns, nnt)
    INTEGER(I4B), INTENT(IN) :: varType
    !! constant, space, time, spaceTime
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    !! shape of data
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in s
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim1, dim2
    !! dim1 and dim2 of the tensor mesh field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns
    !! number of nodes or quadrature points in an element
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nnt
    !! number of nodes or quadrature points in a time element
  END SUBROUTINE TensorMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetShapeAndSize@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-31
! summary:  Get shape and size of data within the element
!
! Get the shape s(1:tsize) of data in an element for a given rank and
! varType. This is the value of `s` which should be given to
! [[obj_Initiate4]]. This routine calls
! [[ScalarMeshFieldGetShapeAndSize]],
! [[VectorMeshFieldGetShapeAndSize]], or
! [[TensorMeshFieldGetShapeAndSize]] depending on the rank.
!
! Arguments:
!   rank : INTEGER(I4B) - scalar, vector, or matrix
!   varType : INTEGER(I4B) - constant, space, time, or spaceTime
!   s : INTEGER(I4B)(4) - shape of data
!   tsize : INTEGER(I4B) - number of entries written in s
!   spaceCompo : INTEGER(I4B), OPTIONAL - needed for vector
!   dim1, dim2 : INTEGER(I4B), OPTIONAL - needed for matrix
!   nns : INTEGER(I4B), OPTIONAL - needed for space and spaceTime
!   nnt : INTEGER(I4B), OPTIONAL - needed for time and spaceTime
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMeshField_Class, ONLY: AbstractMeshFieldGetShapeAndSize
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! INTEGER(I4B), PARAMETER :: nns = 3, nnt = 2, spaceCompo = 2, &
!                            dim1 = 2, dim2 = 3
! INTEGER(I4B) :: s(4), tsize
! LOGICAL(LGT) :: isok
!
! ! scalar
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%scalar, varType=fevaropt%constant, s=s, tsize=tsize)
! CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. 1), "scalar, constant")
!
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%scalar, varType=fevaropt%space, s=s, tsize=tsize, &
!   nns=nns)
! CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. nns), "scalar, space")
!
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%scalar, varType=fevaropt%spaceTime, s=s, tsize=tsize, &
!   nns=nns, nnt=nnt)
! isok = (tsize .EQ. 2) .AND. ALL(s(1:2) .EQ. [nns, nnt])
! CALL OK(isok, "scalar, spaceTime")
!
! ! vector
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%vector, varType=fevaropt%constant, s=s, tsize=tsize, &
!   spaceCompo=spaceCompo)
! CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. spaceCompo), "vector, constant")
!
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%vector, varType=fevaropt%space, s=s, tsize=tsize, &
!   spaceCompo=spaceCompo, nns=nns)
! isok = (tsize .EQ. 2) .AND. ALL(s(1:2) .EQ. [spaceCompo, nns])
! CALL OK(isok, "vector, space")
!
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%vector, varType=fevaropt%spaceTime, s=s, tsize=tsize, &
!   spaceCompo=spaceCompo, nns=nns, nnt=nnt)
! isok = (tsize .EQ. 3) .AND. ALL(s(1:3) .EQ. [spaceCompo, nns, nnt])
! CALL OK(isok, "vector, spaceTime")
!
! ! matrix
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%matrix, varType=fevaropt%constant, s=s, tsize=tsize, &
!   dim1=dim1, dim2=dim2)
! isok = (tsize .EQ. 2) .AND. ALL(s(1:2) .EQ. [dim1, dim2])
! CALL OK(isok, "matrix, constant")
!
! CALL AbstractMeshFieldGetShapeAndSize( &
!   rank=fevaropt%matrix, varType=fevaropt%spaceTime, s=s, tsize=tsize, &
!   dim1=dim1, dim2=dim2, nns=nns, nnt=nnt)
! isok = (tsize .EQ. 4) .AND. ALL(s(1:4) .EQ. [dim1, dim2, nns, nnt])
! CALL OK(isok, "matrix, spaceTime")
!
! END PROGRAM main
!```
!
! See also: [[obj_Initiate4]]

INTERFACE
  MODULE SUBROUTINE AbstractMeshFieldGetShapeAndSize( &
    rank, varType, s, tsize, spaceCompo, dim1, dim2, nns, nnt)
    INTEGER(I4B), INTENT(IN) :: rank
    !! scalar, vector, matrix
    INTEGER(I4B), INTENT(IN) :: varType
    !! constant, space, time, spaceTime
    INTEGER(I4B), INTENT(INOUT) :: s(4)
    !! shape of data
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of entries written in s
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space components
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dim1, dim2
    !! dim1 and dim2 of the tensor mesh field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: nns, nnt
    !! number of nodes in space and time
  END SUBROUTINE AbstractMeshFieldGetShapeAndSize
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Display the content
!
! Display the name, fieldType, engine, tSize, defineOn, rank, varType,
! and the internal arrays (val, indxVal, ss, indxShape) of obj.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   msg : CHARACTER(*) - message to print before the content
!   unitNo : INTEGER(I4B), OPTIONAL - unit number, default is stdout
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableConstant
! USE BaseType, ONLY: TypeFEVariableScalar
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEMesh_Class, ONLY: FEMesh_
! USE FEVariable_Method, ONLY: NodalVariable
! USE FieldOpt_Class, ONLY: fieldopt => TypeFieldOpt
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE MeshField_Class, ONLY: MeshField_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2
!
! TYPE(HDF5File_) :: meshfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! TYPE(FEVariable_) :: fevar
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
!
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%constant, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)
!
! fevar = NodalVariable(10.0_DFP, TypeFEVariableScalar, &
!                       TypeFEVariableConstant)
! CALL obj%Insert(globalElement=1, islocal=math%yes, fevar=fevar)
!
! CALL obj%Display(msg="obj: ")
! CALL OK(obj%IsInitiated(), "Display")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
    !! unit number
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         IMPORT@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Import from hdf5file
!
! Import the mesh field from the group of hdf5 file. The datasets
! written by [[obj_Export]] are read:
!
! - BoolR0/isInit (optional, default is true)
! - IntR0/fieldType (stored as string, for example NORMAL),
!   IntR0/tSize, IntR0/defineOn, IntR0/rank, IntR0/varType,
!   IntR0/totalShape
! - StringR0/name, StringR0/engine
! - IntR1/maxShape
! - IntR1/ss, IntR1/indxShape, IntR1/indxVal, RealR1/val (read only
!   if present)
!
! obj is deallocated before reading. The mesh is not stored in the
! hdf5 file, therefore the mesh should be given. If mesh is not present
! then the mesh pointer of obj is not associated, and the methods which
! need the mesh (for example Get with fieldType normal) can not be
! used. In the debug mode, if fieldType is not constant then tSize is
! checked against mesh%GetTotalElements().
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   hdf5 : TYPE(HDF5File_) - hdf5 file opened for reading
!   group : CHARACTER(*) - group in hdf5 file
!   mesh : CLASS(AbstractMesh_), OPTIONAL - mesh
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableScalar
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
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5", &
!   outfilename = "Import_test_1.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2
! REAL(DFP), PARAMETER :: tol = 1.0E-10_DFP
!
! TYPE(HDF5File_) :: meshfile, outfile
! TYPE(FEMesh_), TARGET :: amesh
! TYPE(MeshField_) :: obj, obj2
! TYPE(FEVariable_) :: fevar, found1, found2
! CLASS(AbstractMesh_), POINTER :: meshptr
! INTEGER(I4B) :: iel, telements, nns, nrow, ncol
! REAL(DFP), ALLOCATABLE :: xij(:, :)
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
! ! A scalar field which varies in space, value = x coordinate of node
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%normal, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%space, rank=fevaropt%scalar, s=[nns], mesh=amesh)
!
! ALLOCATE (xij(3, nns))
! xij = 0.0_DFP
! DO iel = 1, telements
!   CALL amesh%GetNodeCoord(nodeCoord=xij, nrow=nrow, ncol=ncol, &
!                           globalElement=iel, islocal=math%yes)
!   fevar = NodalVariable(xij(1, 1:ncol), TypeFEVariableScalar, &
!                         TypeFEVariableSpace)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! ! Export
! CALL outfile%Initiate(fileName=outfilename, MODE="NEW")
! CALL outfile%OPEN()
! CALL obj%Export(hdf5=outfile, group="/meshField")
! CALL outfile%CLOSE()
! CALL outfile%DEALLOCATE()
!
! ! Import
! CALL outfile%Initiate(fileName=outfilename, MODE="READ")
! CALL outfile%OPEN()
! CALL obj2%IMPORT(hdf5=outfile, group="/meshField", mesh=amesh)
! CALL outfile%CLOSE()
! CALL outfile%DEALLOCATE()
!
! CALL OK(obj2%IsInitiated(), "IsInitiated")
! CALL OK(obj2%GetTotalElements() .EQ. obj%GetTotalElements(), &
!         "GetTotalElements")
! CALL OK(obj2%GetRank() .EQ. obj%GetRank(), "GetRank")
! CALL OK(obj2%GetVarType() .EQ. obj%GetVarType(), "GetVarType")
! CALL OK(obj2%GetDefineOn() .EQ. obj%GetDefineOn(), "GetDefineOn")
! CALL OK(obj2%GetTotalShape() .EQ. obj%GetTotalShape(), "GetTotalShape")
! CALL OK(obj2%GetMaxNNE() .EQ. obj%GetMaxNNE(), "GetMaxNNE")
!
! meshptr => obj2%GetMeshPointer()
! CALL OK(ASSOCIATED(meshptr, amesh), "GetMeshPointer")
!
! isok = .TRUE.
! DO iel = 1, telements
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found1)
!   CALL obj2%Get(globalElement=iel, islocal=math%yes, fevar=found2)
!   isok = isok .AND. (found1%len .EQ. found2%len)
!   isok = isok .AND. ALL(ABS(found1%val(1:found1%len) - &
!                             found2%val(1:found2%len)) .LT. tol)
! END DO
! CALL OK(isok, "imported values")
!
! NULLIFY (meshptr)
! CALL obj%DEALLOCATE()
! CALL obj2%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, mesh)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! hdf5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group name
    CLASS(AbstractMesh_), TARGET, OPTIONAL, INTENT(IN) :: mesh
    !! mesh
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Export to hdf5file
!
! Export the mesh field to the group of a hdf5 file. The hdf5 file must
! be opened with write permission (mode NEW, WRITE, or OVERWRITE). The
! following datasets are written inside the group:
!
! - BoolR0/isInit
! - IntR0/fieldType, IntR0/tSize, IntR0/defineOn, IntR0/rank,
!   IntR0/varType, IntR0/totalShape
! - StringR0/name, StringR0/engine
! - IntR1/maxShape, IntR1/ss, IntR1/indxShape, IntR1/indxVal
! - RealR1/val
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   hdf5 : TYPE(HDF5File_) - hdf5 file opened for writing
!   group : CHARACTER(*) - group in hdf5 file
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableConstant
! USE BaseType, ONLY: TypeFEVariableScalar
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
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: &
!   filename = "./meshdata/small_tri3_mesh.h5", &
!   outfilename = "Export_test_1.h5"
! INTEGER(I4B), PARAMETER :: nsd = 2
!
! TYPE(HDF5File_) :: meshfile, outfile
! TYPE(FEMesh_) :: amesh
! TYPE(MeshField_) :: obj
! TYPE(FEVariable_) :: fevar
! INTEGER(I4B) :: iel, telements
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
! telements = amesh%GetTotalElements()
!
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%normal, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)
!
! DO iel = 1, telements
!   fevar = NodalVariable(REAL(iel, DFP), TypeFEVariableScalar, &
!                         TypeFEVariableConstant)
!   CALL obj%Insert(globalElement=iel, islocal=math%yes, fevar=fevar)
! END DO
!
! CALL outfile%Initiate(fileName=outfilename, MODE="NEW")
! CALL outfile%OPEN()
! CALL obj%Export(hdf5=outfile, group="/meshField")
! CALL outfile%CLOSE()
! CALL outfile%DEALLOCATE()
!
! INQUIRE (FILE=outfilename, EXIST=isok)
! CALL OK(isok, "Export")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! hdf5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group name
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                      WriteData@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Export to vtk file
!
! Write the mesh field to a VTK PolyData (.vtp) file. nodeCoordField
! is a vector mesh field which contains the coordinates of the points
! where the data of obj is defined, for example the interpolation
! points (see InitiateInterpolationPoints) or the quadrature points
! (see InitiateQuadraturePoints).
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   nodeCoordField : CLASS(AbstractMeshField_) - coordinates of points
!   filename : CHARACTER(*) - name of the vtk file
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeInterpolationOpt
! USE EngineOpt_Class, ONLY: engineopt => TypeEngineOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: LGT
! USE MeshField_Class, ONLY: InitiateInterpolationPoints
! USE MeshField_Class, ONLY: MeshField_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
!
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "MeshField_test.toml", &
!                            vtkFileName = "WriteData_test_1.vtp"
! INTEGER(I4B), PARAMETER :: order(3) = 2, &
!                            ipType(3) = TypeInterpolationOpt%Equidistance
!
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! TYPE(MeshField_) :: obj
! CLASS(AbstractMesh_), POINTER :: mesh
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
!
! CALL dom%ImportFromToml(fileName=tomlFileName, tomlName="domain")
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       fileName=tomlFileName, tomlName="u")
! mesh => dom%GetMeshPointer()
!
! CALL InitiateInterpolationPoints( &
!   obj=obj, order=order, ipType=ipType, fedof=fedof, mesh=mesh, &
!   engine=engineopt%native_serial)
!
! CALL obj%WriteData(filename=vtkFileName, nodeCoordField=obj)
!
! INQUIRE (FILE=vtkFileName, EXIST=isok)
! CALL OK(isok, "WriteData")
!
! NULLIFY (mesh)
! CALL obj%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk(obj, nodeCoordField, filename)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CLASS(AbstractMeshField_), INTENT(INOUT) :: nodeCoordField
    !! vector mesh field of point coordinates
    CHARACTER(*), INTENT(IN) :: filename
    !! name of vtk file
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Set values in AbstractMeshField_ from FEVariable
!
! This routine sets the values of globalElement in AbstractMeshField_
! from FEVariable. The element must be filled before by an Insert
! method, and the size of data must not change.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT) - if true globalElement is local
!   fevar : TYPE(FEVariable_) - new data of the element
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableScalar
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
! TYPE(FEVariable_) :: fevar, found
! INTEGER(I4B) :: telements, nns
! REAL(DFP), ALLOCATABLE :: val(:)
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
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%normal, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%space, rank=fevaropt%scalar, s=[nns], mesh=amesh)
!
! ALLOCATE (val(nns))
!
! ! Fill all elements with 1.0
! val = 1.0_DFP
! fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
! CALL obj%Insert(fevar=fevar)
! CALL OK(Check(1.0_DFP, 1.0_DFP), "Insert(fevar)")
!
! ! Update element 1 with 5.0
! val = 5.0_DFP
! fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
! CALL obj%Set(globalElement=1, islocal=math%yes, fevar=fevar)
! CALL OK(Check(5.0_DFP, 1.0_DFP), "Set(globalElement, fevar)")
!
! ! Update all elements with 7.0
! val = 7.0_DFP
! fevar = NodalVariable(val, TypeFEVariableScalar, TypeFEVariableSpace)
! CALL obj%Set(fevar=fevar)
! CALL OK(Check(7.0_DFP, 7.0_DFP), "Set(fevar)")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
!
! CONTAINS
!
! ! Element 1 should contain first, all other elements should contain rest
! FUNCTION Check(first, rest) RESULT(ans)
!   REAL(DFP), INTENT(IN) :: first, rest
!   LOGICAL(LGT) :: ans
!   INTEGER(I4B) :: jj
!   REAL(DFP) :: want
!
!   ans = .TRUE.
!   DO jj = 1, telements
!     want = rest
!     IF (jj .EQ. 1) want = first
!     CALL obj%Get(globalElement=jj, islocal=math%yes, fevar=found)
!     ans = ans .AND. (found%len .EQ. nns)
!     ans = ans .AND. ALL(ABS(found%val(1:found%len) - want) .LT. tol)
!   END DO
! END FUNCTION Check
!
! END PROGRAM main
!```
!
! See also: [[obj_Set4]], [[obj_Insert1]]

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalElement, islocal, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global element is local element
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Set values in AbstractMeshField_
!
! This routine sets the values in AbstractMeshField_ from UserFunction.
! The mesh field must be filled before by an Insert method.
!
! - If fieldType is constant then we get the value from user function
!   and set it.
! - Otherwise, for each element of the mesh we get nodal coordinates
! - Then we get value from userfunction by using the coordinates
! - Then we set the value in AbstractMeshField_ by using [[obj_Set1]]
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   func : CLASS(UserFunction_) - user function
!   times : REAL(DFP)(:), OPTIONAL - time values, needed when varType
!     is time or spaceTime
!
! Examples
!
! See the example of [[obj_Initiate3]].
!
! See also: [[obj_Set5]], [[obj_Insert2]]

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Set values in AbstractMeshField_ by AbstractMaterial
!
! This routine sets the values in AbstractMeshField_ from
! AbstractMaterial.
!
! 1. First we get the user function from material by using the name
! 2. Then we call [[obj_Set2]] method to set the values
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   material : CLASS(AbstractMaterial_) - material
!   name : CHARACTER(*) - name of the material property
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! The usage is the same as [[obj_Set2]], but the user function is
! taken from the material.
!
! See also: [[obj_Set2]], [[obj_Insert3]]

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Set values in AbstractMeshField_ using FeVariable
!
! This routine sets the same FEVariable in all the elements of
! AbstractMeshField_ by calling [[obj_Set1]] for each element. The mesh
! field must be filled before by an Insert method.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   fevar : TYPE(FEVariable_) - data for all the elements
!
! Examples
!
! See the example of [[obj_Set1]].
!
! See also: [[obj_Set1]], [[obj_Insert4]]

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Set values in AbstractMeshField_ by UserFunction
!
! This routine sets the value of globalElement in AbstractMeshField_
! from user function. This function is like [[obj_Set2]], but in this
! case we set the value of a single element. The element must be
! filled before by an Insert method.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   func : CLASS(UserFunction_) - user function
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT), OPTIONAL - if true globalElement is local
!   times : REAL(DFP)(:), OPTIONAL - time values
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
! See also: [[obj_Set2]], [[obj_Insert5]]

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, func, globalElement, islocal, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Set AbstractMeshField_ using AbstractMaterial and medium
!
! This routine sets the values in AbstractMeshField_ from
! AbstractMaterial. The following steps are performed:
!
! 1. From each material(ii) we get the user function by using the name
! 2. Then we call [[obj_Set7]] with these user functions
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   medium : INTEGER(I4B) - medium number
!   material : CLASS(AbstractMaterialPointer_)(:) - materials
!   name : CHARACTER(*) - name of the material property
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! The usage is the same as [[obj_Set7]], but the user functions are
! taken from the materials.
!
! See also: [[obj_Set7]], [[obj_Insert6]]

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, medium, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(AbstractMaterialPointer_), INTENT(INOUT) :: material(:)
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the material
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set6
END INTERFACE

INTERFACE AbstractMeshFieldSet
  MODULE PROCEDURE obj_Set6
END INTERFACE AbstractMeshFieldSet

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Set AbstractMeshField_ using user functions and medium
!
! This routine sets the values in AbstractMeshField_ from a vector of
! user functions. The following steps are performed for each element:
!
! 1. The elements of mesh contain the medium and material information
! 2. We get the material number of the element for the given medium,
!    which acts as an index for func.
! 3. Then we set the values in that element by using this function,
!    see [[obj_Set5]].
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   medium : INTEGER(I4B) - medium number
!   func : CLASS(UserFunctionPointer_)(:) - user functions, one for
!     each material
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! The usage is like [[obj_Set5]], it needs a mesh with material
! information.
!
! See also: [[obj_Set6]], [[obj_Insert7]]

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, medium, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(UserFunctionPointer_), INTENT(INOUT) :: func(:)
    !! User functions
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Set7
END INTERFACE

INTERFACE AbstractMeshFieldSet
  MODULE PROCEDURE obj_Set7
END INTERFACE AbstractMeshFieldSet

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-14
! summary: Insert values in AbstractMeshField_
!
! This routine inserts the values in AbstractMeshField_ for
! globalElement from FEVariable. The size of data can be different in
! each element, but it must not exceed the maximum shape given in
! Initiate.
!
! Insert computes indxVal(e+1) from indxVal(e), therefore elements must
! be inserted in increasing order of local element number, starting
! from 1. If fieldType is constant then only element 1 is used.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT) - if true globalElement is local
!   fevar : TYPE(FEVariable_) - data of the element
!
! Examples
!
! See the example of [[obj_Initiate4]].
!
! See also: [[obj_Set1]], [[obj_Get]]

INTERFACE
  MODULE SUBROUTINE obj_Insert1(obj, globalElement, islocal, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global element is local element
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Insert1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Insert values in AbstractMeshField_
!
! This routine inserts the values in AbstractMeshField_ from
! UserFunction for all the elements.
! It performs following steps:
!
! 1. If fieldType is constant then get the FEVariable_ from
!    UserFunction and insert it in element 1.
! 2. Otherwise, for each element get the nodal coordinates, get the
!    FEVariable_ from UserFunction at these coordinates, and call
!    [[obj_Insert1]].
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   func : CLASS(UserFunction_) - user function
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! See the example of [[obj_Initiate3]].
!
! See also: [[obj_Insert5]], [[obj_Set2]]

INTERFACE
  MODULE SUBROUTINE obj_Insert2(obj, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Insert values in AbstractMeshField_ by AbstractMaterial
!
! This routine inserts the values in AbstractMeshField_ from
! AbstractMaterial.
!
! 1. First we get the user function from material by using the name
! 2. Then we call [[obj_Insert2]] method to insert the values
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   material : CLASS(AbstractMaterial_) - material
!   name : CHARACTER(*) - name of the material property
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! The usage is the same as [[obj_Insert2]], but the user function is
! taken from the material.
!
! See also: [[obj_Insert2]], [[obj_Set3]]

INTERFACE
  MODULE SUBROUTINE obj_Insert3(obj, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CLASS(AbstractMaterial_), INTENT(INOUT) :: material
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the AbstractMeshField
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert3
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Insert values in AbstractMeshField_
!
! This routine inserts the same FEVariable in all the elements by
! calling [[obj_Insert1]] for each element. If fieldType is constant
! then only element 1 is inserted.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   fevar : TYPE(FEVariable_) - data for all the elements
!
! Examples
!
! See the example of [[obj_Set1]].
!
! See also: [[obj_Insert1]], [[obj_Set4]]

INTERFACE
  MODULE SUBROUTINE obj_Insert4(obj, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! FEVariable
  END SUBROUTINE obj_Insert4
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-18
! summary: Insert values in AbstractMeshField from userFunction
!
! This routine inserts the value of globalElement in AbstractMeshField_
! from user function. This function is like [[obj_Insert2]], but in
! this case we insert the value of a single element.
!
! - If obj%fieldType equals constant then we just get the constant
!   value and insert it in element 1.
! - Otherwise we use the nodal coordinates of the element, and get
!   FEVariable from user function at these nodal coordinates.
!
! Then we insert this FEVariable by using [[obj_Insert1]]. Elements
! must be inserted in increasing order of local element number.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   func : CLASS(UserFunction_) - user function
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT), OPTIONAL - if true globalElement is local
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! See the example of [[obj_Set5]].
!
! See also: [[obj_Insert2]], [[obj_Set5]]

INTERFACE
  MODULE SUBROUTINE obj_Insert5(obj, func, globalElement, islocal, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert5
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Insert AbstractMeshField_ using AbstractMaterial and medium
!
! This routine inserts the values in AbstractMeshField_ from
! AbstractMaterial. The following steps are performed:
!
! 1. From each material(ii) we get the user function by using the name
! 2. Then we call [[obj_Insert7]] with these user functions
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   medium : INTEGER(I4B) - medium number
!   material : CLASS(AbstractMaterialPointer_)(:) - materials
!   name : CHARACTER(*) - name of the material property
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! The usage is the same as [[obj_Insert7]], but the user functions are
! taken from the materials.
!
! See also: [[obj_Insert7]], [[obj_Set6]]

INTERFACE
  MODULE SUBROUTINE obj_Insert6(obj, medium, material, name, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(AbstractMaterialPointer_), INTENT(INOUT) :: material(:)
    !! Abstract material
    CHARACTER(*), INTENT(IN) :: name
    !! name of the material
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert6
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Insert AbstractMeshField_ using user functions and medium
!
! This routine inserts the values in AbstractMeshField_ from a vector
! of user functions. The following steps are performed for each
! element:
!
! 1. The elements of mesh contain the medium and material information
! 2. We get the material number of the element for the given medium,
!    which acts as an index for func.
! 3. Then we insert the values in that element by using this function,
!    see [[obj_Insert5]].
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   medium : INTEGER(I4B) - medium number
!   func : CLASS(UserFunctionPointer_)(:) - user functions, one for
!     each material
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! The usage is like [[obj_Insert5]], it needs a mesh with material
! information.
!
! See also: [[obj_Insert6]], [[obj_Set7]]

INTERFACE
  MODULE SUBROUTINE obj_Insert7(obj, medium, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(UserFunctionPointer_), INTENT(INOUT) :: func(:)
    !! User functions
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert7
END INTERFACE

!----------------------------------------------------------------------------
!                                                        Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-13
! summary: Insert AbstractMeshField_ using user functions and quadField
!
! This routine inserts the values in AbstractMeshField_ from a vector
! of user functions at quadrature points. The following steps are
! performed for each element:
!
! 1. The elements of mesh contain the medium and material information
! 2. We get the material number of the element for the given medium,
!    which acts as an index for func.
! 3. Then we insert the values in that element by using this function
!    at the quadrature points stored in quadField,
!    see [[obj_Insert9]].
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   quadField : CLASS(AbstractMeshField_) - vector mesh field which
!     contains the quadrature points
!   medium : INTEGER(I4B) - medium number
!   func : CLASS(UserFunctionPointer_)(:) - user functions, one for
!     each material
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! The usage is like [[obj_Insert9]], it needs a mesh with material
! information.
!
! See also: [[obj_Insert9]], [[obj_Insert7]]

INTERFACE
  MODULE SUBROUTINE obj_Insert8(obj, quadField, medium, func, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! AbstractMeshField
    CLASS(AbstractMeshField_), INTENT(INOUT) :: quadField
    !! Vector mesh field for quadrature points
    INTEGER(I4B), INTENT(IN) :: medium
    !! Medium number
    CLASS(UserFunctionPointer_), INTENT(INOUT) :: func(:)
    !! User functions
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert8
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Insert@InsertMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-18
! summary: Insert values in an element from userFunction at quad points
!
! This routine inserts the value of globalElement in AbstractMeshField_
! from user function, at the quadrature points stored in quadField.
! This function is like [[obj_Insert5]], but the user function is
! evaluated at the quadrature points instead of the nodal coordinates.
!
! - If obj%fieldType equals constant then we just get the constant
!   value and insert it in element 1.
! - Otherwise we get the quadrature points of the element from
!   quadField, and get FEVariable from user function at these points.
!
! Then we insert this FEVariable by using [[obj_Insert1]]. Elements
! must be inserted in increasing order of local element number.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   quadField : CLASS(AbstractMeshField_) - vector mesh field which
!     contains the quadrature points
!   func : CLASS(UserFunction_) - user function
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT), OPTIONAL - if true globalElement is local
!   times : REAL(DFP)(:), OPTIONAL - time values
!
! Examples
!
! See the example of [[obj_Initiate6]].
!
! See also: [[obj_Initiate6]], [[obj_Insert8]]

INTERFACE
  MODULE SUBROUTINE obj_Insert9(obj, quadField, func, globalElement, &
                                islocal, times)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! abstract mesh field
    CLASS(AbstractMeshField_), INTENT(INOUT) :: quadField
    !! vector mesh field which contains quadrature points
    CLASS(UserFunction_), INTENT(INOUT) :: func
    !! User function
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then global element is local element
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! time vector when the var type is `Time` or `SpaceTime`
  END SUBROUTINE obj_Insert9
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@AddMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Add values to AbstractMeshField_
!
! Add values to an element of AbstractMeshField_ from FEVariable.
! This routine can be used while updating the abstractMeshField. The
! element must be filled before by an Insert method, and the size of
! fevar must be equal to the size of element data.
!
! Formula:
!
! $$\mathbf{v}_e \leftarrow \mathbf{v}_e + \alpha \, \mathbf{f}$$
!
! where \(\mathbf{v}_e\) is the data of element e, \(\alpha\) is scale,
! and \(\mathbf{f}\) is fevar%val.
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   globalElement : INTEGER(I4B) - global or local element number
!   islocal : LOGICAL(LGT) - if true globalElement is local
!   scale : REAL(DFP) - scale
!   fevar : TYPE(FEVariable_) - value to add
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: FEVariable_
! USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableConstant
! USE BaseType, ONLY: TypeFEVariableScalar
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
! TYPE(FEVariable_) :: fevar, found
! INTEGER(I4B) :: iel, telements
! REAL(DFP) :: want
! LOGICAL(LGT) :: isok
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! CALL meshfile%Initiate(fileName=filename, MODE="READ")
! CALL meshfile%OPEN()
! CALL amesh%Initiate(hdf5=meshfile, dim=nsd)
! telements = amesh%GetTotalElements()
!
! CALL obj%Initiate( &
!   name="scalarMeshField", fieldType=fieldopt%normal, &
!   engine=engineopt%native_serial, defineOn=fevaropt%nodal, &
!   varType=fevaropt%constant, rank=fevaropt%scalar, s=[1], mesh=amesh)
!
! fevar = NodalVariable(1.0_DFP, TypeFEVariableScalar, &
!                       TypeFEVariableConstant)
!
! ! obj = 1 in all elements
! CALL obj%Insert(fevar=fevar)
!
! ! obj = 1 + 2 * 1 = 3 in all elements
! CALL obj%Add(scale=2.0_DFP, fevar=fevar)
!
! ! obj = 3 + 1 * 1 = 4 in element 1
! CALL obj%Add(globalElement=1, islocal=math%yes, scale=1.0_DFP, &
!              fevar=fevar)
!
! isok = .TRUE.
! DO iel = 1, telements
!   want = 3.0_DFP
!   IF (iel .EQ. 1) want = 4.0_DFP
!   CALL obj%Get(globalElement=iel, islocal=math%yes, fevar=found)
!   isok = isok .AND. (ABS(found%val(1) - want) .LT. tol)
! END DO
! CALL OK(isok, "Add")
!
! CALL obj%DEALLOCATE()
! CALL amesh%DEALLOCATE()
! CALL meshfile%CLOSE()
! CALL meshfile%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Add2]], [[obj_Set1]]

INTERFACE
  MODULE SUBROUTINE obj_Add1(obj, globalElement, islocal, scale, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    INTEGER(I4B), INTENT(IN) :: globalElement
    !! global or local element number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true globalElement is local element
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! value to add
  END SUBROUTINE obj_Add1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Add@AddMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-02-17
! summary: Add values to AbstractMeshField_
!
! Add the same FEVariable to all the elements of the mesh. This is like
! [[obj_Add1]] for every element.
!
! Formula:
!
! $$\mathbf{v}_e \leftarrow \mathbf{v}_e + \alpha \, \mathbf{f}
!   \quad \forall e$$
!
! Arguments:
!   obj : CLASS(AbstractMeshField_) - mesh field
!   scale : REAL(DFP) - scale
!   fevar : TYPE(FEVariable_) - value to add
!
! Examples
!
! See the example of [[obj_Add1]].
!
! See also: [[obj_Add1]]

INTERFACE
  MODULE SUBROUTINE obj_Add2(obj, scale, fevar)
    CLASS(AbstractMeshField_), INTENT(INOUT) :: obj
    !! mesh field
    REAL(DFP), INTENT(IN) :: scale
    !! scale
    TYPE(FEVariable_), INTENT(IN) :: fevar
    !! value to add
  END SUBROUTINE obj_Add2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractMeshField_Class

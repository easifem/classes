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
! date: 2021-06-28
! summary: Block node field with the LIS engine
!
! This module is built only with USE_LIS; otherwise the stub
! NO_BlockNodeFieldLis_Class.F90 is used.
!
! Changes:
! - 2026-11-16: Documentation of the current state (issue #653)
! - 2026-11-16: Set18 added to the Set generic (issue #654); modName
!   moved to the submodules (issue #655)
! - 2026-11-16: Initiate1, Initiate2 and DEALLOCATE create and destroy
!   the LIS vector (issue #653, step 1)
! - 2026-11-16: SetSingle, SetAll and SetMultiple1-5 write to the LIS
!   vector (issue #653, step 2)
! - 2026-11-16: GetSingle and GetMultiple1-4 read the LIS vector;
!   GetPointer returns NULL (issue #653, step 3)
! - 2026-11-16: BLAS methods call the LIS vector routines
!   (issue #653, step 4)

MODULE BlockNodeFieldLis_Class
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BlockNodeField_Class, ONLY: BlockNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_

IMPLICIT NONE

PRIVATE

PUBLIC :: BlockNodeFieldLis_
PUBLIC :: BlockNodeFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         BlockNodeFieldLis_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Block node field with the LIS engine
!
! BlockNodeFieldLis_ is a [[BlockNodeField_]] whose values are stored in
! a LIS vector (obj%lis_ptr) instead of the native vector realVec, as in
! [[ScalarFieldLis_]]. The DOF data, the FEDOFs and the layout are the
! same as in [[BlockNodeField_]], and its Set, Get and Dirichlet BC
! methods can be used: they reach the values through the methods based
! on the index of a value (SetSingle, SetAll, SetMultiple, GetSingle,
! GetMultiple), which this class overrides.
!
! realVec is allocated but it is not kept up to date: it is filled from
! the LIS vector (lis_vector_gather) only by Display and Export.
! GetPointer is not available.
!
! Initiate (from arguments or by copying another node field) creates the
! LIS vector and sets the engine to LIS_OMP; DEALLOCATE destroys it. The
! LIS library must be started before a field is initiated, by
! LisOmpEngine_%Initiate (lis_initialize), and stopped after the fields
! are deallocated, by LisOmpEngine_%DEALLOCATE (lis_finalize).
!
! The class has these limits.
!
!- The BLAS methods (AXPY, SCAL, COPY, Swap, norms, DOT_PRODUCT, PMUL,
!  Reciprocal) call the LIS vector routines, so their field arguments
!  must also use the LIS engine. To move values between engines use
!  Set(VALUE=field) or SetMultiple with a field.
!- Import always raises an error, and Export calls the Export of
!  [[BlockNodeField_]], which is under development.
!
! See also: [[BlockNodeField_]], [[ScalarFieldLis_]]

TYPE, EXTENDS(BlockNodeField_) :: BlockNodeFieldLis_

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate by copying another node field, create the LIS vector
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate from arguments, create the LIS vector
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Destroy the LIS vector and deallocate the data
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Returns the local size of the field (local_n)
  FINAL :: obj_Final
  !! Finalizer

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from HDF5File_ (under construction, raises an error)
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Gather the LIS vector and export to HDF5File_
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Gather the LIS vector and display the content

  ! BLAS:
  ! @BlasMethods
  PROCEDURE, PUBLIC, PASS(obj) :: AXPY1 => obj_AXPY1
  !! obj = obj + scale * x
  PROCEDURE, PUBLIC, PASS(obj) :: AXPY2 => obj_AXPY2
  !! obj = obj + a1 * x1 + a2 * x2
  PROCEDURE, PUBLIC, PASS(obj) :: AXPY3 => obj_AXPY3
  !! obj = obj + a1 * x1 + a2 * x2 + a3 * x3
  PROCEDURE, PUBLIC, PASS(obj) :: SCAL => obj_SCAL
  !! obj = scale * obj
  PROCEDURE, PUBLIC, PASS(obj) :: COPY => obj_Copy
  !! obj = obj2
  PROCEDURE, PUBLIC, PASS(obj) :: Swap => obj_Swap
  !! Swap the values of obj and obj2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => obj_Norm2
  !! Returns the L2 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => obj_Norm1
  !! Returns the L1 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => obj_Normi
  !! Returns the infinity norm
  PROCEDURE, PUBLIC, PASS(obj) :: DOT_PRODUCT => obj_DOT_PRODUCT
  !! Dot product of obj and obj2
  PROCEDURE, PUBLIC, PASS(obj) :: PMUL => obj_PMUL
  !! obj = obj1 * obj2, entry by entry
  PROCEDURE, PUBLIC, PASS(obj) :: Reciprocal => obj_Reciprocal
  !! obj = 1 / obj, entry by entry

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  !! Set or add one entry of the LIS vector
  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set or add one value to all entries of the LIS vector
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple1 => obj_SetMultiple1
  !! Set or add entries given by indices
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple2 => obj_SetMultiple2
  !! Set or add entries in a range from a vector
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple3 => obj_SetMultiple3
  !! Set or add entries in a range from a range of a vector
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple4 => obj_SetMultiple4
  !! Set or add one value to entries in a range
  PROCEDURE, PUBLIC, PASS(obj) :: SetMultiple5 => obj_SetMultiple5
  !! Set or add entries in a range from a range of another node field
  PROCEDURE, PASS(obj) :: Set18 => obj_Set18
  !! Set one DOF of ivar from another node field
  GENERIC, PUBLIC :: Set => Set18
  !! Adds Set18 to the Set generic of BlockNodeField_

  ! GET:
  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! Not available, returns NULL
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  !! Get one entry of the LIS vector
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple1 => obj_GetMultiple1
  !! Get entries given by indices
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple2 => obj_GetMultiple2
  !! Get entries in a range
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple3 => obj_GetMultiple3
  !! Get entries in a range into a range of VALUE
  PROCEDURE, PUBLIC, PASS(obj) :: GetMultiple4 => obj_GetMultiple4
  !! Set or add scale times entries in a range to a range of VALUE

END TYPE BlockNodeFieldLis_

!----------------------------------------------------------------------------
!                                                  BlockNodeFieldLisPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Pointer to a block node field with the LIS engine

TYPE :: BlockNodeFieldLisPointer_
  CLASS(BlockNodeFieldLis_), POINTER :: ptr => NULL()
  !! pointer to a block node field with the LIS engine
END TYPE BlockNodeFieldLisPointer_

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethod
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Finalizer, deallocates the field
!
! Calls obj%DEALLOCATE(), which destroys the LIS vector.

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! block node field with the LIS engine
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Initiate a LIS block node field by copying another node field
!
! If obj is initiated it is deallocated first (its LIS vector is
! destroyed). Then the data of obj2 are copied by
! AbstractNodeFieldInitiate (see [[BlockNodeField_:obj_Initiate1]]), a
! new LIS vector of size local_n (global size global_n) is created, and
! the values are copied into it:
!
!- if obj2 is a BlockNodeFieldLis_, from the LIS vector of obj2
!  (lis_vector_copy);
!- otherwise (a native node field), from the native vector of values
!  copied from obj2 (lis_vector_scatter).
!
! obj2 is not changed, and obj has its own LIS vector.
!
! Examples
!
!```fortran
! TYPE(LisOmpEngine_) :: lis
! TYPE(BlockNodeFieldLis_) :: u, v
! CALL lis%Initiate()
! ! u is initiated with Initiate(name, ...), see [[obj_Initiate2]]
! CALL v%Initiate(obj2=u)
! CALL OK(v%SIZE() .EQ. u%SIZE(), "Initiate1: Size")
! CALL v%DEALLOCATE()
! CALL u%DEALLOCATE()
! CALL lis%DEALLOCATE()
!```
!
! Full examples:
! src/modules/BlockNodeFieldLis/examples/Initiate_test_1.F90,
! src/modules/BlockNodeFieldLis/examples/Initiate_test_2.F90
!
! See also: [[obj_Initiate2]], [[obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field to be initiated
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! field to copy, it should be a child of AbstractNodeField_
    !! (BlockNodeFieldLis_ or a native BlockNodeField_)
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyFull
    !! passed to AbstractNodeFieldInitiate
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyStructure
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: usePointer
    !! not used
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Initiate a LIS block node field from arguments
!
! If obj is initiated it is deallocated first (its LIS vector is
! destroyed). Then the DOF data, the DOF object and the FEDOF pointers
! are set as in [[BlockNodeField_:obj_Initiate2]], the engine is set to
! LIS_OMP (whatever engine is given), and a LIS vector of size local_n
! (global size global_n) is created. The values are not set.
!
! Examples
!
!```fortran
! TYPE(LisOmpEngine_) :: lis
! TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
! TYPE(BlockNodeFieldLis_) :: u
! CALL lis%Initiate()
! ! fedofs(1)%ptr, geofedofs(1)%ptr point to FEDOFs with six nodes
! ! V has two space components, P has one: 6 * 2 + 6 * 1 = 18 values
! CALL u%Initiate( &
!   name="u", engine="LIS_OMP", storageFMT=DOF_FMT, &
!   spaceCompo=[2, 1], isSpaceCompo=math%yes, &
!   isSpaceCompoScalar=math%no, timeCompo=[1, 1], isTimeCompo=math%yes, &
!   isTimeCompoScalar=math%no, tPhysicalVarNames=2, &
!   physicalVarNames=["V", "P"], isPhysicalVarNames=math%yes, &
!   isPhysicalVarNamesScalar=math%no, tNodes=[6, 6], isTNodes=math%yes, &
!   isTNodesScalar=math%no, tSize=18, fedof=fedofs, geofedof=geofedofs)
! CALL OK(u%SIZE() .EQ. 18, "Initiate2: Size")
! CALL u%DEALLOCATE()
! CALL lis%DEALLOCATE()
!```
!
! Full example:
! src/modules/BlockNodeFieldLis/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate1]], [[obj_Deallocate]],
! [[BlockNodeField_:obj_Initiate2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, name, engine, storageFMT, spaceCompo, isSpaceCompo, &
    isSpaceCompoScalar, timeCompo, isTimeCompo, isTimeCompoScalar, &
    tPhysicalVarNames, physicalVarNames, isPhysicalVarNames, &
    isPhysicalVarNamesScalar, tNodes, isTNodes, isTNodesScalar, &
    tSize, fedof, geofedof, fieldType, comm, local_n, global_n, timefedof)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field defined in [[AbstractField_]]
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine; not used, the engine is set to LIS_OMP
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format, see [[FieldOpt_Class:TypeFieldOpt]]
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space components, size tPhysicalVarNames
    LOGICAL(LGT), INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo
    LOGICAL(LGT), INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! time components, size tPhysicalVarNames
    LOGICAL(LGT), INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    LOGICAL(LGT), INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    INTEGER(I4B), INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    CHARACTER(*), INTENT(IN) :: physicalVarNames(:)
    !! names of the physical variables, size tPhysicalVarNames
    LOGICAL(LGT), INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    LOGICAL(LGT), INTENT(IN) :: isPhysicalVarNamesScalar
    !! if true then we physicalVarNames is scalar
    INTEGER(I4B), INTENT(IN)  :: tNodes(:)
    !! total number of nodes in each physical variable
    LOGICAL(LGT), INTENT(IN) :: isTNodes
    !! if true we will try to access tNodes
    LOGICAL(LGT), INTENT(IN) :: isTNodesScalar
    !! is tNodes scalar
    INTEGER(I4B), INTENT(IN) :: tSize
    !! total size of node field
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:)
    !! FEDOF of each physical variable, size 1 (same FEDOF for all)
    !! or tPhysicalVarNames
    TYPE(FEDOFPointer_), INTENT(IN) :: geofedof(:)
    !! geometry FEDOF of each physical variable, size 1 or
    !! tPhysicalVarNames
    INTEGER(I4B), INTENT(IN), OPTIONAL :: fieldType
    !! field type, see TypeFieldOpt for the names, default is
    !! TypeFieldOpt%normal, defined in [[AbstractField_]]
    INTEGER(I4B), INTENT(IN), OPTIONAL :: comm
    !! communication group
    !! Only needed for parallel environment, defined in [[AbstractField_]]
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_n
    !! local size of field on each processor, default tSize
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! global size of field on distributed on processors, default tSize
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedof(:)
    !! TimeFEDOF of each physical variable, size 1 or
    !! tPhysicalVarNames
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Destroy the LIS vector and deallocate the object
!
! The LIS vector is destroyed (lis_vector_destroy), then the data of the
! node field are deallocated (AbstractNodeFieldDeallocate). Call it
! before LisOmpEngine_%DEALLOCATE (lis_finalize).
!
! See also: [[obj_Initiate2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
  END SUBROUTINE obj_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-25
! summary: Returns the local size of the field
!
! Returns obj%local_n, the number of values stored on this process.
! dims is not used.
!
! Returns:
!   ans : INTEGER(I4B) - obj%local_n

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    !! block node field with the LIS engine
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used
    INTEGER(I4B) :: ans
    !! local size, obj%local_n
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Display the content
!
! If the LIS vector exists, gather it into the native vector of values
! and display the field with AbstractNodeFieldDisplay. If obj%lis_ptr
! is null (the field is not initiated) nothing is displayed.

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! block node field with the LIS engine
    CHARACTER(*), INTENT(IN) :: msg
    !! message printed before the content
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Import the field from HDF5File_
!
! This routine is under construction; it always raises an error.

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! block node field with the LIS engine
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group name in the file
    CLASS(FEDOF_), TARGET, INTENT(IN), OPTIONAL :: fedof, geofedof
    !! FEDOF of the field and of the geometry (not used)
    TYPE(FEDOFPointer_), INTENT(IN), OPTIONAL :: fedofs(:), geofedofs(:)
    !! FEDOF of each physical variable and of its geometry (not used)
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! FEDOF in time (not used)
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! FEDOF in time of each physical variable (not used)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Export data into HDF5File_
!
! If the LIS vector exists, gather it into the native vector of values
! and call the Export of [[BlockNodeField_]], which is still under
! development and raises an error. If obj%lis_ptr is null (the field is
! not initiated) nothing is exported.

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! block node field with the LIS engine
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group name in the file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: obj = obj + scale * x
!
! Calls lis_vector_axpy. x must use the LIS engine and have the same
! size as obj.
!
! Formula:
!   $$ obj = obj + s*x $$
!
! Examples
!
!```fortran
! w = 0.0_DFP
! CALL w%AXPY(x=x, scale=2.0_DFP)
! CALL w%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "AXPY: w + s * x")
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/AXPY_test_1.F90
!
! See also: [[obj_AXPY2]], [[obj_AXPY3]], [[obj_SCAL]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY1(obj, x, scale)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field, obj = obj + scale * x
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x
    !! node field with the LIS engine
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_AXPY1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-16
! summary: obj = obj + a1 * x1 + a2 * x2
!
! Calls lis_vector_axpy twice. x1 and x2 must use the LIS engine.
!
! Formula:
!   $$ obj = obj + a_1 x_1 + a_2 x_2 $$
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_AXPY2(obj, x1, x2, a1, a2)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    !! first node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    !! second node field with the LIS engine
    REAL(DFP), INTENT(IN) :: a1
    !! scale of x1
    REAL(DFP), INTENT(IN) :: a2
    !! scale of x2
  END SUBROUTINE obj_AXPY2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2026-11-16
! summary: obj = obj + a1 * x1 + a2 * x2 + a3 * x3
!
! Calls lis_vector_axpy three times. x1, x2 and x3 must use the LIS
! engine.
!
! Formula:
!   $$ obj = obj + a_1 x_1 + a_2 x_2 + a_3 x_3 $$
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_AXPY3(obj, x1, x2, x3, a1, a2, a3)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    !! first node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    !! second node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x3
    !! third node field with the LIS engine
    REAL(DFP), INTENT(IN) :: a1
    !! scale of x1
    REAL(DFP), INTENT(IN) :: a2
    !! scale of x2
    REAL(DFP), INTENT(IN) :: a3
    !! scale of x3
  END SUBROUTINE obj_AXPY3
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SCAL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Scaling, obj = scale * obj
!
! Calls lis_vector_scale.
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_SCAL(obj, scale)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_SCAL
END INTERFACE

!----------------------------------------------------------------------------
!                                                          COPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Copy the values, obj = obj2
!
! Calls lis_vector_copy. obj2 must use the same engine (LIS_OMP) and
! have the same size as obj; the engine is checked in debug mode. To
! copy a field of another engine use Set(VALUE=obj2).
!
! See also: [[obj_AXPY1]] (contains an example), [[obj_Swap]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field, obj = obj2
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! node field of the same size and engine
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Swap@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Swap the values of two node fields, obj <=> obj2
!
! Exchange the nodal values stored in the LIS vectors of obj and obj2
! by calling lis_vector_swap. Only the values are exchanged; the other
! components (name, fedof, dof data, ...) of obj and obj2 are not
! changed. This method overrides the Swap method of AbstractNodeField_.
!
! Both fields must be initiated with the LIS engine and have the same
! size. In debug mode an error is raised when the LIS vector of obj or
! obj2 is null, or when obj%Size() and obj2%Size() differ.
!
! See also: [[obj_AXPY1]] (contains an example), [[obj_Copy]]

INTERFACE
  MODULE SUBROUTINE obj_Swap(obj, obj2)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! first node field, on return it contains the values of obj2
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj2
    !! second node field (LIS engine), on return it contains the values
    !! of obj
  END SUBROUTINE obj_Swap
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Returns the L2 norm
!
! Calls lis_vector_nrm2.
!
! Returns:
!   ans : REAL(DFP) - L2 norm of the values
!
! Formula:
!   $$ ans = \sqrt{\sum_i x_i^2} $$
!
! Examples
!
!```fortran
! TYPE(BlockNodeFieldLis_) :: x, y, z
! ! x, y and z are initiated with the same layout, see [[obj_Initiate2]]
! CALL OK(ABS(x%Norm2() - SQRT(2109.0_DFP)) .LT. tol, "Norm2")
! CALL OK(ABS(x%DOT_PRODUCT(obj2=y) + 23.0_DFP) .LT. tol, "DOT_PRODUCT")
! CALL z%PMUL(obj1=x, obj2=y)
! CALL z%Reciprocal()
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/Norm2_test_1.F90
!
! See also: [[obj_Norm1]], [[obj_Normi]]

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    !! LIS block node field
    REAL(DFP) :: ans
    !! L2 norm
  END FUNCTION obj_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Norm1@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Returns the L1 norm (sum of absolute values)
!
! Calls lis_vector_nrm1.
!
! Returns:
!   ans : REAL(DFP) - L1 norm of the values
!
! Formula:
!   $$ ans = \sum_i |x_i| $$
!
! See also: [[obj_Norm2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Norm1(obj) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    !! LIS block node field
    REAL(DFP) :: ans
    !! L1 norm
  END FUNCTION obj_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Normi@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Returns the infinity norm (largest absolute value)
!
! Calls lis_vector_nrmi.
!
! With the OpenMP build of LIS this needs a LIS built after the fix of
! lis_vector_nrmi in the easifem fork of LIS (lis-latest).
!
! Returns:
!   ans : REAL(DFP) - infinity norm of the values
!
! Formula:
!   $$ ans = \max_i |x_i| $$
!
! See also: [[obj_Norm2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Normi(obj) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    !! LIS block node field
    REAL(DFP) :: ans
    !! infinity norm
  END FUNCTION obj_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                   DOT_PRODUCT@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Dot product of obj and obj2
!
! Calls lis_vector_dot. obj2 must use the LIS engine and have the same
! size.
!
! Returns:
!   ans : REAL(DFP) - dot product
!
! Formula:
!   $$ ans = \sum_i x_i y_i $$
!
! See also: [[obj_Norm2]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_DOT_PRODUCT(obj, obj2) RESULT(ans)
    CLASS(BlockNodeFieldLis_), INTENT(IN) :: obj
    !! LIS block node field
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! node field with the LIS engine
    REAL(DFP) :: ans
    !! dot product
  END FUNCTION obj_DOT_PRODUCT
END INTERFACE

!----------------------------------------------------------------------------
!                                                          PMUL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Entry by entry product, obj = obj1 * obj2
!
! Calls lis_vector_pmul. obj1 and obj2 must use the LIS engine and have
! the same size as obj.
!
! Formula:
!   $$ obj_i = (obj1)_i (obj2)_i $$
!
! See also: [[obj_Norm2]] (contains an example), [[obj_Reciprocal]]

INTERFACE
  MODULE SUBROUTINE obj_PMUL(obj, obj1, obj2)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field, result
    CLASS(AbstractNodeField_), INTENT(IN) :: obj1
    !! first node field with the LIS engine
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! second node field with the LIS engine
  END SUBROUTINE obj_PMUL
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Reciprocal@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Entry by entry reciprocal, obj = 1 / obj
!
! Calls lis_vector_reciprocal. The entries must not be zero.
!
! Formula:
!   $$ obj_i = 1 / obj_i $$
!
! See also: [[obj_Norm2]] (contains an example), [[obj_PMUL]]

INTERFACE
  MODULE SUBROUTINE obj_Reciprocal(obj)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
  END SUBROUTINE obj_Reciprocal
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set single entry
!
! Sets entry indx of the LIS vector to scale * VALUE
! (lis_vector_set_value with LIS_INS_VALUE), or adds scale * VALUE when
! addContribution is true (LIS_ADD_VALUE).
!
! Examples
!
!```fortran
! CALL x%SetSingle(indx=3, VALUE=2.0_DFP, scale=4.0_DFP)
! CALL x%GetSingle(indx=3, VALUE=val)
! CALL OK(ABS(val - 8.0_DFP) .LT. tol, "SetSingle: scale * value")
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_GetSingle]], [[obj_SetAll]]

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: indx
    !! index of the entry, 1 <= indx <= obj%Size()
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set all the values
!
! Sets all entries to scale * VALUE (lis_vector_set_all), or adds
! scale * VALUE to each entry when addContribution is true
! (lis_vector_shift with -scale * VALUE, since LIS computes
! x_i - sigma). Set(VALUE) and the assignment obj = VALUE of
! [[BlockNodeField_]] call this method.
!
! Changes:
!   - 2026-11-17: add with lis_vector_shift; before, a loop of
!     lis_vector_set_value over indices 1..local_n, which was slow and
!     used local indices as global ones under MPI
!
! Examples
!
!```fortran
! TYPE(BlockNodeFieldLis_) :: u
! ! u is initiated, see [[obj_Initiate2]]
! CALL u%Set(VALUE=2.0_DFP)
! CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/Set_test_1.F90
!
! See also: [[obj_SetSingle]]

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set or add
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set multiple entries using indices
!
! Sets entry indx(i) to scale * VALUE(i) for i = 1, ..., SIZE(indx)
! (lis_vector_set_values4), or adds scale * VALUE(i) when
! addContribution is true.
!
! Examples
!
!```fortran
! CALL x%SetMultiple(indx=[2, 4], VALUE=[10.0_DFP, 20.0_DFP], &
!                    scale=2.0_DFP)
! CALL x%GetMultiple(indx=[4, 2], VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found(1:2) - [40.0_DFP, 20.0_DFP]) .LT. tol), &
!         "SetMultiple indx: scale * value")
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple1(obj, indx, VALUE, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! indices, SIZE(indx) = SIZE(VALUE)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, obj(indx(i)) = scale * VALUE(i)
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set multiple entries in a range
!
! Sets the entries istart, istart + stride, ..., iend to scale * VALUE(1),
! scale * VALUE(2), ... (lis_vector_set_values5), or adds them when
! addContribution is true. The number of entries is
! (iend - istart) / stride + 1.
!
! Examples
!
!```fortran
! CALL x%SetMultiple(istart=1, iend=5, stride=2, &
!                    VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], scale=3.0_DFP)
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple1]], [[obj_GetMultiple2]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple2(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, SIZE(VALUE) >= (iend - istart) / stride + 1
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set multiple entries in a range from a range of value
!
! Sets the entries istart:iend:stride of obj to scale times the entries
! istart_value:iend_value:stride_value of VALUE
! (lis_vector_set_values8), or adds them when addContribution is true.
! Both ranges have (iend - istart) / stride + 1 entries.
!
! Examples
!
!```fortran
! ! x(2:6:2) = -1 * src(4:6)
! CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=src, &
!                    istart_value=4, iend_value=6, stride_value=1, &
!                    scale=-1.0_DFP)
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_GetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE, same number of entries as obj range
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set multiple entries in a range to one value
!
! Sets the entries istart:iend:stride to scale * VALUE
! (lis_vector_set_values6), or adds scale * VALUE when addContribution
! is true.
!
! Examples
!
!```fortran
! ! x(1:6:3) = 0.5 * 8
! CALL x%SetMultiple(istart=1, iend=6, stride=3, VALUE=8.0_DFP, &
!                    scale=0.5_DFP)
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple2]], [[obj_SetAll]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple4(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! one value for all entries in the range
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple4
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Set multiple entries in a range from a range of another field
!
! obj(istart:iend:stride) is set to scale * VALUE(istart_value::
! stride_value), or scale times these values is added when
! addContribution is true. The number of values is given by the range
! of obj. VALUE can have any engine:
!
!- NATIVE: its vector of values is passed to [[obj_SetMultiple3]],
!  no temporary array,
!- LIS: scale times the range of VALUE is set (or added) in the range
!  of obj (lis_vector_set_values9), no temporary array,
!- otherwise: the range of VALUE is copied into a temporary array by
!  GetMultiple of VALUE, then set by [[obj_SetMultiple3]].
!
! @note
!   obj and VALUE can be the same field, but then the two ranges should
!   not overlap.
! @endnote
!
! Examples
!
!```fortran
! ! x(1:3) = x(1:3) + 2 * y(4:6)
! CALL x%SetMultiple(istart=1, iend=3, stride=1, VALUE=y, &
!                    istart_value=4, iend_value=6, stride_value=1, &
!                    scale=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_2.F90
!
! See also: [[obj_SetMultiple3]], [[obj_GetMultiple4]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple5(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(BlockNodeFieldLis_), TARGET, INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    CLASS(AbstractNodeField_), TARGET, INTENT(INOUT) :: VALUE
    !! node field from which the values are taken
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_SetMultiple5
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Not available, returns NULL
!
! The values are stored in the LIS vector, not in realVec, so no
! pointer to them can be returned. The result is NULL and in debug mode
! an error is raised. Use Get or GetMultiple instead.
!
! Returns:
!   ans : REAL(DFP), POINTER - NULL
!
! See also: [[obj_GetMultiple2]]

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(BlockNodeFieldLis_), TARGET, INTENT(IN) :: obj
    !! LIS block node field
    REAL(DFP), POINTER :: ans(:)
    !! always NULL
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Get single entry
!
! Returns entry indx of the LIS vector (lis_vector_get_value).
!
! See also: [[obj_SetSingle]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: indx
    !! index of the entry, 1 <= indx <= obj%Size()
    REAL(DFP), INTENT(OUT) :: VALUE
    !! value of the entry
  END SUBROUTINE obj_GetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Get multiple entries using indices
!
! VALUE(i) is entry indx(i) of the LIS vector, i = 1, ..., SIZE(indx)
! (lis_vector_get_values_from_index); tsize = SIZE(indx).
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple1(obj, indx, VALUE, tsize)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! indices of the entries
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! values, SIZE(VALUE) >= SIZE(indx)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE, SIZE(indx)
  END SUBROUTINE obj_GetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Get multiple entries in a range
!
! VALUE(1:tsize) are the entries istart:iend:stride of the LIS vector
! (lis_vector_get_values_from_range), with
! tsize = (iend - istart) / stride + 1. Get(VALUE, tsize) of
! [[BlockNodeField_]] calls this method.
!
! See also: [[obj_SetMultiple2]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple2(obj, istart, iend, stride, VALUE, tsize)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! values, SIZE(VALUE) >= tsize
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE
  END SUBROUTINE obj_GetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Get multiple entries in a range into a range of value
!
! The entries istart:iend:stride of the LIS vector are written in
! VALUE(istart_value:iend_value:stride_value)
! (lis_vector_get_values_from_range2); the other entries of VALUE are
! not changed. tsize = (iend - istart) / stride + 1.
!
! Changes:
!   - 2026-11-18: VALUE is INTENT(INOUT) (it was INTENT(OUT)), so the
!     entries outside the range keep their values (#620).
!
! Examples
!
!```fortran
! ! found(4:6) = x(1:3)
! CALL x%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
!                    istart_value=4, iend_value=6, stride_value=1, &
!                    tsize=tsize)
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_1.F90
!
! See also: [[obj_SetMultiple3]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     tsize)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values, entries istart_value:iend_value:stride_value are written
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE, same number of entries as obj range
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE
  END SUBROUTINE obj_GetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Scale entries in a range and set or add them to VALUE
!
! VALUE(istart_value::stride_value) = scale * obj(istart:iend:stride),
! or scale * obj(istart:iend:stride) is added when addContribution is
! true. The number of values is given by the range of obj; the other
! entries of VALUE are not changed. With scale 1 and without
! addContribution the values are written directly
! (lis_vector_get_values_from_range2); otherwise they are first copied
! into obj%tempVec.
!
! Examples
!
!```fortran
! ! found(2:6:2) = found(2:6:2) + 2 * x(1:5:2)
! CALL x%GetMultiple(istart=1, iend=5, stride=2, VALUE=found, &
!                    istart_value=2, iend_value=6, stride_value=2, &
!                    scale=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/BlockNodeFieldLis/examples/SetMultiple_test_2.F90
!
! See also: [[obj_GetMultiple3]], [[obj_SetMultiple5]]

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple4(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! LIS block node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! range of entries of obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values are set or added in the range of VALUE
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of entries of VALUE
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add (true) or set (false, default)
  END SUBROUTINE obj_GetMultiple4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-08
! summary: Set one DOF of a physical variable from another node field
!
! Set local DOF idof of physical variable ivar at all nodes from local
! DOF idof_value of physical variable ivar_value of VALUE. VALUE can be
! a native or LIS scalar, vector, space-time scalar or space-time
! vector field. If addContribution is true, obj = obj + scale * VALUE
! for these entries.
!
! It is called through the generic Set (keyword arguments ivar, idof,
! VALUE, ivar_value, idof_value). Until the LIS layer exists
! (issue #653) the values go to the native vector of values for the
! native VALUE types, and to obj%lis_ptr (which is 0) for the LIS ones.
!
! Changes:
!   - 2026-11-16: Added to the Set generic (issue #654).

INTERFACE
  MODULE SUBROUTINE obj_Set18(obj, ivar, idof, VALUE, ivar_value, &
                              idof_value, scale, addContribution)
    CLASS(BlockNodeFieldLis_), INTENT(INOUT) :: obj
    !! block node field with the LIS engine
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    !! node field to copy from
    INTEGER(I4B), INTENT(IN) :: ivar_value
    !! physical variable number in value
    INTEGER(I4B), INTENT(IN) :: idof_value
    !! local degree of freedom number in value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set18
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeFieldLis_Class

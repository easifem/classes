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
! summary: Block node field data type is defined
!
! Changes:
! - 2026-11-16: Documentation and examples of all public methods

MODULE BlockNodeField_Class
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE BaseType, ONLY: FEVariable_
USE DirichletBC_Class, ONLY: DirichletBC_
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: DOF_FMT
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: NodesToDOF
USE HDF5File_Class, ONLY: HDF5File_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
USE tomlf, ONLY: toml_table
USE UserFunction_Class, ONLY: UserFunction_
IMPLICIT NONE
PRIVATE

INTEGER(I4B), PARAMETER :: MYCONVERSION = TypeFieldOpt%conversionNodesToDOF

PUBLIC :: BlockNodeFieldPointer_
PUBLIC :: BlockNodeField_
PUBLIC :: BlockNodeFieldDeallocate

!----------------------------------------------------------------------------
!                                                           BlockNodeField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Node field with several physical variables (multiphysics)
!
! A block node field holds several physical variables in one vector,
! for example velocity and pressure. Physical variable ivar has
! spaceCompo(ivar) space components and timeCompo(ivar) time
! components, so it has tdof(ivar) = spaceCompo(ivar) * timeCompo(ivar)
! degrees of freedom (DOFs) at each of its tNodes(ivar) nodes. The local
! DOF of space component s and time component t of ivar is
! s + (t - 1) * spaceCompo(ivar).
!
! Each physical variable has its own FEDOF, geometry FEDOF and
! (optionally) TimeFEDOF, stored in obj%fedofs(ivar), obj%geofedofs(ivar)
! and obj%timefedofs(ivar). The single pointers obj%fedof, obj%geofedof
! and obj%timefedof are not used and stay NULL.
!
! Initiate the field with Initiate (from arguments, normally with
! storageFMT = DOF_FMT) or Initiate(obj2) (copy of another block field),
! and free it with DEALLOCATE. In DOF format the values are stored DOF by
! DOF: all nodes of DOF 1 of variable 1, all nodes of DOF 2 of
! variable 1, ..., then the DOFs of variable 2, and so on.
!
! The values are set by Set (all values, by ivar and idof, by ivar,
! by ivar, spaceCompo and timeCompo, from another block field) and the
! Dirichlet boundary condition methods, and read by Get.
!
! In all Set and Get methods globalNode is a local node number, and
! islocal must be true.
!
! Import and Export (HDF5) and ImportFromToml are not implemented yet;
! they raise an error.
!
! Changes:
!   - 2026-11-16: Added Initiate (obj_Initiate1, obj_Initiate2);
!     removed the unused component idofs.
!
! Examples
!
!```fortran
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
! TYPE(BlockNodeField_) :: u
! ! fedof and geofedof are initiated with six nodes
! fedofs(1)%ptr => fedof
! geofedofs(1)%ptr => geofedof
! ! V has two space components, P has one
! CALL u%Initiate( &
!   name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
!   spaceCompo=[2, 1], isSpaceCompo=math%yes, &
!   isSpaceCompoScalar=math%no, timeCompo=[1, 1], isTimeCompo=math%yes, &
!   isTimeCompoScalar=math%no, tPhysicalVarNames=2, &
!   physicalVarNames=["V", "P"], isPhysicalVarNames=math%yes, &
!   isPhysicalVarNamesScalar=math%no, tNodes=[6, 6], isTNodes=math%yes, &
!   isTNodesScalar=math%no, tSize=18, fedof=fedofs, geofedof=geofedofs)
!```
!
! Full example: src/modules/BlockNodeField/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate2]], [[obj_Initiate1]]

TYPE, EXTENDS(AbstractNodeField_) :: BlockNodeField_
CONTAINS
  ! @ConstructorMethods
  FINAL :: obj_Final
  !! Finalizer
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate from another node field (copy)
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate from arguments
  GENERIC, PUBLIC :: Initiate => Initiate2
  !! Generic method to initiate (Initiate1 is added by AbstractField_)

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Initiate from a toml table (under development)
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import from HDF5File_ (under development)
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export to HDF5File_ (under development)

  ! @SetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set all values to a scalar value
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values of obj by using a rank-1 vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set a single entry, filter: ivar, idof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set all DOFs of ivar at nodes to a scalar, filter: ivar
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set all DOFs of ivar at nodes from a vector, filter: ivar
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set entries at nodes to a scalar, filter: ivar, idof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set entries at nodes from a vector, filter: ivar, idof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set entries at nodes from a vector
  !! filter: ivar, spaceCompo, timeCompo
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set entries at nodes to a scalar
  !! filter: ivar, spaceCompo, timeCompo
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set entries at nodes from a vector
  !! filter: ivar, spaceCompo, timeCompo(:)
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set entries at nodes to a scalar
  !! filter: ivar, spaceCompo, timeCompo(:)
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! Set entries at nodes from a vector
  !! filter: ivar, spaceCompo(:), timeCompo
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set13 => obj_Set13
  !! Set entries at nodes to a scalar
  !! filter: ivar, spaceCompo(:), timeCompo
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set14 => obj_Set14
  !! Set a single entry, filter: ivar, spaceCompo, timeCompo
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set15 => obj_Set15
  !! Set entries of one node to a scalar
  !! filter: ivar, spaceCompo, timeCompo(:)
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set16 => obj_Set16
  !! Set entries of one node to a scalar
  !! filter: ivar, spaceCompo(:), timeCompo
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Set17 => obj_Set17
  !! Set (copy or add) from another block node field
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, &
    Set5, Set6, Set7, Set8, Set9, Set10, Set11, &
    Set12, Set13, Set14, Set15, Set16, Set17
  !! Generic method to set values
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ASSIGN => obj_Assign
  !! Set all values to a scalar value
  GENERIC, PUBLIC :: ASSIGNMENT(=) => ASSIGN
  !! Assignment, obj = scalar

  ! @GetMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get a single entry, filter: ivar, idof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all the values in a rank-1 vector
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get entries at nodes, filter: ivar, idof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get entries at nodes in FEVariable, filter: ivar, idof
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get all DOFs of ivar at nodes in FEVariable, filter: ivar
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get entries at nodes in a rank-1 vector
  !! filter: ivar, spaceCompo, timeCompo
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: Get7 => obj_Get7
  !! Get entries at nodes in FEVariable
  !! filter: ivar, spaceCompo, timeCompo
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, &
    Get5, Get6, Get7
  !! Generic method to get values
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: GetFEVariable => &
    obj_GetFEVariable
  !! Get all DOFs of ivar at nodes in FEVariable (default ivar = 1)

  ! @DirichletBCMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC1 => &
    obj_ApplyDirichletBC1
  !! Apply one Dirichlet BC to ivar
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC2 => &
    obj_ApplyDirichletBC2
  !! Apply a vector of Dirichlet BCs to ivar
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC3 => &
    obj_ApplyDirichletBC3
  !! Apply one space-time Dirichlet BC to ivar
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC4 => &
    obj_ApplyDirichletBC4
  !! Apply a vector of space-time Dirichlet BCs to ivar
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3, ApplyDirichletBC4
  !! Generic method to apply Dirichlet BCs
END TYPE BlockNodeField_

!----------------------------------------------------------------------------
!                                                    BlockNodeFieldPointer_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Pointer to a block node field

TYPE :: BlockNodeFieldPointer_
  CLASS(BlockNodeField_), POINTER :: ptr => NULL()
  !! pointer to a block node field
END TYPE BlockNodeFieldPointer_

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Finalizer, deallocates the field
!
! See also: [[AbstractNodeField_Class:obj_Deallocate]]

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Initiate a block node field by copying another node field
!
! If obj is initiated it is deallocated first. Then the data of obj2 are
! copied by [[AbstractNodeField_Class:obj_Initiate1]]: the data of
! AbstractField_ (name, engine, ...), the DOF object, the values, and
! the pointers fedofs, geofedofs and timefedofs (obj points to the same
! FEDOF objects as obj2). obj2 is not changed.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u, v
! ! u is initiated with Initiate(name, ...), see [[obj_Initiate2]]
! CALL v%Initiate(obj2=u)
! CALL OK(v%SIZE() .EQ. u%SIZE(), "Initiate1: Size")
!```
!
! Full example: src/modules/BlockNodeField/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate2]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! node field
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyFull
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: copyStructure
    !! not used
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: usePointer
    !! not used
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-16
! summary: Initiate a block node field from arguments
!
! If obj is initiated it is deallocated first. Then
! [[AbstractNodeField_Class:obj_Initiate3]] sets the DOF data, the DOF
! object, the vector of values (tSize entries), local_n and global_n,
! and stores the FEDOF pointers in obj%fedofs, obj%geofedofs and
! (if timefedof is present) obj%timefedofs.
!
! All the DOF arguments are required. The sizes of spaceCompo,
! timeCompo, physicalVarNames and tNodes must be tPhysicalVarNames, and
! tSize must be the sum of tNodes(i) * spaceCompo(i) * timeCompo(i).
! Only the first character of each physical variable name is stored.
!
! Examples
!
!```fortran
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
! TYPE(BlockNodeField_) :: u
! ! fedof and geofedof are initiated with six nodes
! fedofs(1)%ptr => fedof
! geofedofs(1)%ptr => geofedof
! ! V has two space components, P has one: 6 * 2 + 6 * 1 = 18 values
! CALL u%Initiate( &
!   name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
!   spaceCompo=[2, 1], isSpaceCompo=math%yes, &
!   isSpaceCompoScalar=math%no, timeCompo=[1, 1], isTimeCompo=math%yes, &
!   isTimeCompoScalar=math%no, tPhysicalVarNames=2, &
!   physicalVarNames=["V", "P"], isPhysicalVarNames=math%yes, &
!   isPhysicalVarNamesScalar=math%no, tNodes=[6, 6], isTNodes=math%yes, &
!   isTNodesScalar=math%no, tSize=18, fedof=fedofs, geofedof=geofedofs)
! CALL OK(u%SIZE() .EQ. 18, "Initiate2: Size")
!```
!
! Full example: src/modules/BlockNodeField/examples/Initiate_test_1.F90
!
! See also: [[obj_Initiate1]], [[AbstractNodeField_Class:obj_Initiate3]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, name, engine, storageFMT, spaceCompo, isSpaceCompo, &
    isSpaceCompoScalar, timeCompo, isTimeCompo, isTimeCompoScalar, &
    tPhysicalVarNames, physicalVarNames, isPhysicalVarNames, &
    isPhysicalVarNamesScalar, tNodes, isTNodes, isTNodesScalar, &
    tSize, fedof, geofedof, fieldType, comm, local_n, global_n, timefedof)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! node field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field defined in [[AbstractField_]]
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine, defined in [[AbstractField_]]
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format, see [[FieldOpt_Class:TypeFieldOpt]]
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space components, tsize of spaceCompo should be same as
    !! tPhysicalVarNames
    LOGICAL(LGT), INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo
    LOGICAL(LGT), INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! Time components
    LOGICAL(LGT), INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    LOGICAL(LGT), INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    INTEGER(I4B), INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    CHARACTER(*), INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables, size of physicalVarNames should
    !! be same as tPhysicalVarNames
    LOGICAL(LGT), INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
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
    !! local size of field on each processor
    !! Only needed for parallel environment
    !! Defined in [[AbstractField_]]
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    !! Defined in [[AbstractField_]]
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedof(:)
    !! TimeFEDOF of each physical variable, size 1 or
    !! tPhysicalVarNames
  END SUBROUTINE obj_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Deallocate a vector of block node field pointers
!
! Deallocate every associated field, nullify its pointer, and then
! deallocate the vector.
!
! Examples
!
!```fortran
! TYPE(BlockNodeFieldPointer_), ALLOCATABLE :: fields(:)
! ALLOCATE (fields(2))
! ALLOCATE (fields(1)%ptr, fields(2)%ptr)
! ! ... initiate and use the fields
! CALL BlockNodeFieldDeallocate(fields)
! CALL OK(.NOT. ALLOCATED(fields), "BlockNodeFieldDeallocate")
!```

INTERFACE BlockNodeFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(BlockNodeFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! vector of pointers; deallocated on return
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE BlockNodeFieldDeallocate

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-15
! summary: Initiate a block node field from a toml table
!
! This routine is under development; it raises an error in debug
! mode and does nothing otherwise. Initiate the field with
! Initiate instead.
!
! See also: [[AbstractNodeField_:obj_ImportFromToml1]]

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, fedof, geofedof, &
                                        timefedof, dom)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table of the field
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof, geofedof
    !! FEDOF of the field and of the geometry
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(INOUT) :: timefedof
    !! FEDOF in time
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! domain
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-16
! summary: Import the field from HDF5File_
!
! This routine is under construction; it always raises an error.

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group name in the file
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    !! FEDOF of the field and of the geometry (not used)
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    !! FEDOF of each physical variable and of its geometry (not used)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    !! FEDOF in time (not used)
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
    !! FEDOF in time of each physical variable (not used)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Export@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-24
! summary: Export data into HDF5File_
!
! This routine is under development; it always raises an error.

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group name in the file
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set all values to a scalar
!
! If addContribution is true, obj = obj + scale * VALUE; otherwise all
! values are set to VALUE.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BlockNodeField_Class, ONLY: BlockNodeField_
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDOF_Class, ONLY: FEDOFPointer_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: DOF_FMT
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
! TYPE(BlockNodeField_) :: u
! REAL(DFP) :: src(18), found(18)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
!                           dom=dom)
! CALL geofedof%ImportFromToml(tomlName="geofedof", &
!                              filename=tomlFileName, dom=dom)
! fedofs(1)%ptr => fedof
! geofedofs(1)%ptr => geofedof
!
! CALL u%Initiate( &
!   name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
!   spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
!   timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
!   tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
!   isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
!   tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
!   fedof=fedofs, geofedof=geofedofs)
!
! CALL OK(u%IsInitiated(), "Initiate: IsInitiated")
! CALL OK(u%SIZE() .EQ. 18, "Initiate: Size")
!
! ! Set all values to a scalar (obj_Set1), Get all values (obj_Get2)
! CALL u%Set(VALUE=2.0_DFP)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(tsize .EQ. 18, "Get all: tsize")
! CALL OK(ALL(ABS(found - 2.0_DFP) .LT. tol), "Set all to a scalar")
!
! ! Assignment (obj_Assign)
! u = 3.0_DFP
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 3.0_DFP) .LT. tol), "Assignment u = 3")
!
! ! Add a scalar: u = u + 2 * 1
! CALL u%Set(VALUE=1.0_DFP, scale=2.0_DFP, addContribution=math%yes)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 5.0_DFP) .LT. tol), "Add a scalar")
!
! ! Set all values from a vector (obj_Set2)
! src = [(REAL(ii, DFP), ii=1, 18)]
! CALL u%Set(VALUE=src)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "Set all from a vector")
!
! ! Add a vector: u = u + 2 * src
! CALL u%Set(VALUE=src, scale=2.0_DFP, addContribution=math%yes)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 3.0_DFP * src) .LT. tol), "Add a vector")
!
! CALL u%DEALLOCATE()
! CALL OK(.NOT. u%IsInitiated(), "DEALLOCATE: not initiated")
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Assign]], [[obj_Set2]], [[obj_Get2]]

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set all values from a vector
!
! VALUE(i) is the value of entry i of the field, in the storage order
! of the field (see [[BlockNodeField_]]). If addContribution is true,
! obj = obj + scale * VALUE.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! REAL(DFP) :: src(18)
! ! u has 18 values
! src = [(REAL(ii, DFP), ii=1, 18)]
! CALL u%Set(VALUE=src)
! CALL u%Set(VALUE=src, scale=2.0_DFP, addContribution=math%yes)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_1.F90
!
! See also: [[obj_Set1]], [[obj_Get2]]

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, SIZE(VALUE) = obj%SIZE()
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set a single entry by ivar and idof
!
! Set local DOF idof of physical variable ivar at node globalNode. If
! addContribution is true, the entry becomes entry + scale * VALUE.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_y of node 3
! CALL u%Set(globalNode=3, islocal=math%yes, VALUE=4.0_DFP, ivar=1, &
!            idof=2)
! CALL u%Get(VALUE=val, globalNode=3, islocal=math%yes, ivar=1, idof=2)
! CALL OK(ABS(val - 4.0_DFP) .LT. tol, "Set3")
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_2.F90
!
! See also: [[obj_Set6]], [[obj_Set7]], [[obj_Get1]]

INTERFACE
  MODULE SUBROUTINE obj_Set3( &
    obj, globalNode, islocal, VALUE, ivar, idof, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar, 1 to tdof(ivar)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set all DOFs of a physical variable at nodes to a scalar
!
! Set all tdof(ivar) DOFs of physical variable ivar at every node in
! globalNode to VALUE.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_x and V_y of nodes 2, 3
! CALL u%Set(globalNode=[2, 3], islocal=math%yes, VALUE=5.0_DFP, ivar=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_3.F90
!
! See also: [[obj_Set5]]

INTERFACE
  MODULE SUBROUTINE obj_Set4( &
    obj, globalNode, islocal, VALUE, ivar, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set all DOFs of a physical variable at nodes from a vector
!
! Set all tdof(ivar) DOFs of physical variable ivar at the nodes in
! globalNode. In DOF format VALUE is in DOF order: all nodes of DOF 1
! of ivar, then all nodes of DOF 2, and so on, so the value of local DOF
! k at globalNode(i) is VALUE((k - 1) * SIZE(globalNode) + i).
!
! Note that the order of the arguments differs from the other Set
! methods (VALUE comes before islocal); use keyword arguments.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_x = 1, 2 and V_y = 3, 4 at nodes 4, 5
! CALL u%Set(globalNode=[4, 5], islocal=math%yes, ivar=1, &
!            VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP])
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_3.F90
!
! See also: [[obj_Set4]]

INTERFACE
  MODULE SUBROUTINE obj_Set5( &
    obj, globalNode, VALUE, islocal, ivar, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values of all DOFs of ivar at globalNode, in DOF order,
    !! SIZE(VALUE) = SIZE(globalNode) * tdof(ivar)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set one DOF of a physical variable at nodes to a scalar
!
! Calls [[obj_Set3]] for each node in globalNode.
!
! See also: [[obj_Set3]], [[obj_Set7]]

INTERFACE
  MODULE SUBROUTINE obj_Set6( &
    obj, globalNode, islocal, VALUE, ivar, idof, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! constant value which will be assigned to the globalnode
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set one DOF of a physical variable at nodes from a vector
!
! Set local DOF idof of physical variable ivar at globalNode(i) to
! VALUE(i).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_x of nodes 4, 5, 6
! CALL u%Set(globalNode=[4, 5, 6], islocal=math%yes, &
!            VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], ivar=1, idof=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_2.F90
!
! See also: [[obj_Set3]], [[obj_Set6]], [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Set7( &
    obj, globalNode, islocal, VALUE, ivar, idof, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, SIZE(VALUE) = SIZE(globalNode)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set one space-time component at nodes from a vector
!
! Set space component spaceCompo and time component timeCompo of
! physical variable ivar at globalNode(i) to VALUE(i).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_y of nodes 1, 2, 3
! CALL u%Set(globalNode=[1, 2, 3], islocal=math%yes, &
!            VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP], ivar=1, spaceCompo=2, &
!            timeCompo=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_4.F90
!
! See also: [[obj_Set9]], [[obj_Get6]]

INTERFACE
  MODULE SUBROUTINE obj_Set8( &
    obj, globalNode, islocal, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values, SIZE(VALUE) = SIZE(globalNode)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component number in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component number in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set one space-time component at nodes to a scalar
!
! Calls [[obj_Set8]] with VALUE repeated SIZE(globalNode) times.
!
! Full example: src/modules/BlockNodeField/examples/Set_test_4.F90
!
! See also: [[obj_Set8]]

INTERFACE
  MODULE SUBROUTINE obj_Set9( &
    obj, globalNode, islocal, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set several time components at nodes from a vector
!
! Set space component spaceCompo and the time components timeCompo(:)
! of physical variable ivar at the nodes in globalNode. VALUE is in node
! order: the value of timeCompo(j) at globalNode(i) is
! VALUE((i - 1) * SIZE(timeCompo) + j).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has U (two space, two time components) on 6 nodes
! ! U_y at nodes 1, 2: node 1 (t1, t2) = 1, 2; node 2 (t1, t2) = 3, 4
! CALL u%Set(globalNode=[1, 2], islocal=math%yes, &
!            VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], ivar=1, &
!            spaceCompo=2, timeCompo=[1, 2])
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_5.F90
!
! See also: [[obj_Set11]], [[obj_Set15]]

INTERFACE
  MODULE SUBROUTINE obj_Set10( &
    obj, globalNode, islocal, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values in node order,
    !! SIZE(VALUE) = SIZE(globalNode) * SIZE(timeCompo)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! time components in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set several time components at nodes to a scalar
!
! Calls [[obj_Set10]] with VALUE repeated
! SIZE(globalNode) * SIZE(timeCompo) times.
!
! Full example: src/modules/BlockNodeField/examples/Set_test_5.F90
!
! See also: [[obj_Set10]]

INTERFACE
  MODULE SUBROUTINE obj_Set11( &
    obj, globalNode, islocal, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! time components in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set several space components at nodes from a vector
!
! Set the space components spaceCompo(:) and time component timeCompo
! of physical variable ivar at the nodes in globalNode. VALUE is in node
! order: the value of spaceCompo(j) at globalNode(i) is
! VALUE((i - 1) * SIZE(spaceCompo) + j).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has U (two space, two time components) on 6 nodes
! ! U at time 2, nodes 5, 6: node 5 (x, y) = 1, 2; node 6 (x, y) = 3, 4
! CALL u%Set(globalNode=[5, 6], islocal=math%yes, &
!            VALUE=[1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP], ivar=1, &
!            spaceCompo=[1, 2], timeCompo=2)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_5.F90
!
! See also: [[obj_Set13]], [[obj_Set16]]

INTERFACE
  MODULE SUBROUTINE obj_Set12( &
    obj, globalNode, islocal, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values in node order,
    !! SIZE(VALUE) = SIZE(globalNode) * SIZE(spaceCompo)
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space components in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set several space components at nodes to a scalar
!
! Calls [[obj_Set12]] with VALUE repeated
! SIZE(globalNode) * SIZE(spaceCompo) times.
!
! Full example: src/modules/BlockNodeField/examples/Set_test_5.F90
!
! See also: [[obj_Set12]]

INTERFACE
  MODULE SUBROUTINE obj_Set13( &
    obj, globalNode, islocal, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space components in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set a single entry by space and time components
!
! Set space component spaceCompo and time component timeCompo of
! physical variable ivar at node globalNode. This method has no islocal
! argument; globalNode is a local node number.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_x of node 6
! CALL u%Set(globalNode=6, VALUE=5.0_DFP, ivar=1, spaceCompo=1, &
!            timeCompo=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_4.F90
!
! See also: [[obj_Set8]], [[obj_Set3]]

INTERFACE
  MODULE SUBROUTINE obj_Set14( &
    obj, globalNode, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set several time components of one node to a scalar
!
! Set space component spaceCompo and the time components timeCompo(:)
! of physical variable ivar at node globalNode to VALUE. This method has
! no islocal argument; globalNode is a local node number.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has U (two space, two time components) and T (one space, three
! ! time components) on 6 nodes
! ! T at node 2, time components 2, 3
! CALL u%Set(globalNode=2, VALUE=7.0_DFP, ivar=2, spaceCompo=1, &
!            timeCompo=[2, 3])
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_5.F90
!
! See also: [[obj_Set10]], [[obj_Set16]]

INTERFACE
  MODULE SUBROUTINE obj_Set15( &
    obj, globalNode, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! time components in the physical variable
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set15
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set several space components of one node to a scalar
!
! Set the space components spaceCompo(:) and time component timeCompo
! of physical variable ivar at node globalNode to VALUE. This method has
! no islocal argument; globalNode is a local node number.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! ! u has U (two space, two time components) on 6 nodes
! ! U_x and U_y of node 4 at time component 2
! CALL u%Set(globalNode=4, VALUE=8.0_DFP, ivar=1, spaceCompo=[1, 2], &
!            timeCompo=2)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_5.F90
!
! See also: [[obj_Set12]], [[obj_Set15]]

INTERFACE
  MODULE SUBROUTINE obj_Set16( &
    obj, globalNode, VALUE, ivar, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! local node number
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! space component numbers in ivar
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component number in ivar
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, add scale * VALUE, else set; default false
  END SUBROUTINE obj_Set16
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Set a block node field from another block node field
!
! If addContribution is true, obj = obj + scale * VALUE (AXPY);
! otherwise the values of VALUE are copied into obj (Copy). Both fields
! must have the same layout.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BlockNodeField_Class, ONLY: BlockNodeField_
! USE ExceptionHandler_Class, ONLY: e
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDOF_Class, ONLY: FEDOFPointer_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: DOF_FMT
! USE GlobalData, ONLY: I4B
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "BlockNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(FEDOFPointer_) :: fedofs(1), geofedofs(1)
! TYPE(BlockNodeField_) :: u, v
! REAL(DFP) :: src(18), found(18)
! INTEGER(I4B) :: ii, tsize
!
! CALL e%SetQuietMode(math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL fedof%ImportFromToml(tomlName="space", filename=tomlFileName, &
!                           dom=dom)
! CALL geofedof%ImportFromToml(tomlName="geofedof", &
!                              filename=tomlFileName, dom=dom)
! fedofs(1)%ptr => fedof
! geofedofs(1)%ptr => geofedof
!
! CALL u%Initiate( &
!   name="u", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
!   spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
!   timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
!   tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
!   isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
!   tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
!   fedof=fedofs, geofedof=geofedofs)
!
! CALL v%Initiate( &
!   name="v", engine="NATIVE_SERIAL", storageFMT=DOF_FMT, &
!   spaceCompo=[2, 1], isSpaceCompo=math%yes, isSpaceCompoScalar=math%no, &
!   timeCompo=[1, 1], isTimeCompo=math%yes, isTimeCompoScalar=math%no, &
!   tPhysicalVarNames=2, physicalVarNames=["V", "P"], &
!   isPhysicalVarNames=math%yes, isPhysicalVarNamesScalar=math%no, &
!   tNodes=[6, 6], isTNodes=math%yes, isTNodesScalar=math%no, tSize=18, &
!   fedof=fedofs, geofedof=geofedofs)
!
! src = [(REAL(ii, DFP), ii=1, 18)]
! CALL v%Set(VALUE=src)
! u = 0.0_DFP
!
! ! copy (obj_Set17)
! CALL u%Set(VALUE=v)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "Set17: copy")
!
! ! add: u = u + 2 * v
! CALL u%Set(VALUE=v, scale=2.0_DFP, addContribution=math%yes)
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - 3.0_DFP * src) .LT. tol), "Set17: add")
!
! ! v is not changed
! CALL v%Get(VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "Set17: v unchanged")
!
! CALL u%DEALLOCATE()
! CALL v%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[AbstractNodeField_Class:obj_Copy]]

INTERFACE
  MODULE SUBROUTINE obj_Set17(obj, VALUE, scale, addContribution)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    CLASS(BlockNodeField_), INTENT(INOUT) :: VALUE
    !! block node field with the same layout as obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale, used only if addContribution is true, default 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true, obj = obj + scale * VALUE, else copy; default false
  END SUBROUTINE obj_Set17
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set one DOF from another node field (disabled)

! INTERFACE
!   MODULE SUBROUTINE obj_Set18( &
!     obj, ivar, idof, VALUE, ivar_value, idof_value, scale, addContribution)
!     CLASS(BlockNodeField_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: ivar
!     !! physical variable number
!     INTEGER(I4B), INTENT(IN) :: idof
!     !! local degree of freedom number in ivar
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     !! AbstractNodeField_
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     !! physical variable number in value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!     !! local degree of freedom number in value
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     !! scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!     !! add or set
!   END SUBROUTINE obj_Set18
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Assignment, set all values to a scalar (obj = VALUE)
!
! Full example: src/modules/BlockNodeField/examples/Set_test_1.F90
!
! See also: [[obj_Set1]]

INTERFACE
  MODULE SUBROUTINE obj_Assign(obj, VALUE)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value
  END SUBROUTINE obj_Assign
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get a single entry by ivar and idof
!
! Return local DOF idof of physical variable ivar at node globalNode.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! REAL(DFP) :: val
! ! u has V (two space components) and P (one component) on 6 nodes
! ! P of node 5
! CALL u%Get(VALUE=val, globalNode=5, islocal=math%yes, ivar=2, idof=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_2.F90
!
! See also: [[obj_Set3]], [[obj_Get3]]

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, islocal, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value of the entry
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get all values
!
! Return all values of the field in storage order (see
! [[BlockNodeField_]]).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! REAL(DFP) :: found(18)
! INTEGER(I4B) :: tsize
! ! u has 18 values
! CALL u%Get(VALUE=found, tsize=tsize)
! CALL OK(tsize .EQ. 18, "Get all: tsize")
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_1.F90
!
! See also: [[obj_Set2]]

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, tsize)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! all values, SIZE(VALUE) >= obj%SIZE()
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE, obj%SIZE()
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get one DOF of a physical variable at nodes
!
! VALUE(i) is local DOF idof of physical variable ivar at
! globalNode(i).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! REAL(DFP) :: vec(3)
! INTEGER(I4B) :: tsize
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_x of nodes 4, 5, 6
! CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[4, 5, 6], &
!            islocal=math%yes, ivar=1, idof=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_2.F90
!
! See also: [[obj_Set7]], [[obj_Get4]]

INTERFACE
  MODULE SUBROUTINE obj_Get3( &
    obj, VALUE, tsize, globalNode, islocal, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values, SIZE(VALUE) = SIZE(globalNode)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE, SIZE(globalNode)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get one DOF of a physical variable at nodes in FEVariable
!
! Same as [[obj_Get3]], but the values are returned as a nodal scalar,
! space FEVariable (val(i) is the value at globalNode(i)).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! TYPE(FEVariable_) :: fevar
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_y at nodes 2, 4
! CALL u%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=1, &
!            idof=2)
!```
!
! Full example: src/modules/BlockNodeField/examples/Get_test_1.F90
!
! See also: [[obj_Get3]], [[obj_Get5]]

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, islocal, ivar, idof)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! nodal FEVariable, TypeFEVariableScalar, TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom number in ivar
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get all DOFs of a physical variable at nodes in FEVariable
!
! Return all DOFs of physical variable ivar at the nodes in globalNode
! as a nodal FEVariable. The type depends on the components of ivar:
!
!- spaceCompo = 1, timeCompo = 1: scalar, space; val(i) at node i.
!- spaceCompo > 1, timeCompo = 1: vector, space; val(s, i).
!- spaceCompo = 1, timeCompo > 1: scalar, space-time; val(i, t).
!- spaceCompo > 1, timeCompo > 1: vector, space-time; val(s, i, t).
!
! Here i is the position in globalNode, s the space component and t the
! time component.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! TYPE(FEVariable_) :: fevar
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V at nodes 2, 4: vector, space FEVariable, shape (2, 2)
! CALL u%Get(VALUE=fevar, globalNode=[2, 4], islocal=math%yes, ivar=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Get_test_1.F90
!
! See also: [[obj_GetFEVariable]], [[obj_Get4]], [[obj_Get7]]

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, islocal, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! nodal FEVariable; scalar if spaceCompo is 1, otherwise vector;
    !! space if timeCompo is 1, otherwise space-time
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get one space-time component at nodes
!
! VALUE(i) is space component spaceCompo and time component timeCompo
! of physical variable ivar at globalNode(i).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! REAL(DFP) :: vec(3)
! INTEGER(I4B) :: tsize
! ! u has V (two space components) and P (one component) on 6 nodes
! ! V_y of nodes 1, 2, 3
! CALL u%Get(VALUE=vec, tsize=tsize, globalNode=[1, 2, 3], &
!            islocal=math%yes, ivar=1, spaceCompo=2, timeCompo=1)
!```
!
! Full example: src/modules/BlockNodeField/examples/Set_test_4.F90
!
! See also: [[obj_Set8]], [[obj_Get7]]

INTERFACE
  MODULE SUBROUTINE obj_Get6( &
    obj, VALUE, tsize, globalNode, islocal, ivar, spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values, SIZE(VALUE) = SIZE(globalNode)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! number of values written in VALUE, SIZE(globalNode)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get one space-time component at nodes in FEVariable
!
! Same as [[obj_Get6]], but the values are returned as a nodal scalar,
! space FEVariable (val(i) is the value at globalNode(i)).
!
! Full example: src/modules/BlockNodeField/examples/Get_test_1.F90
!
! See also: [[obj_Get6]], [[obj_Get4]]

INTERFACE
  MODULE SUBROUTINE obj_Get7( &
    obj, VALUE, globalNode, islocal, ivar, spaceCompo, timeCompo)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! nodal FEVariable, TypeFEVariableScalar, TypeFEVariableSpace
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component in the physical variable
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in the physical variable
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-06
! summary: Get one DOF into another node field (disabled)

! INTERFACE
!   MODULE SUBROUTINE obj_Get8(obj, ivar, idof, VALUE, ivar_value, idof_value)
!     CLASS(BlockNodeField_), INTENT(IN) :: obj
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
!     INTEGER(I4B), INTENT(IN) :: ivar
!     INTEGER(I4B), INTENT(IN) :: idof
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!   END SUBROUTINE obj_Get8
! END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get all DOFs of a physical variable at nodes in FEVariable
!
! Calls [[obj_Get5]] with ivar (default 1).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! TYPE(FEVariable_) :: fevar
! ! u has V (two space components) and P (one component) on 6 nodes
! ! P at nodes 2, 4
! CALL u%GetFEVariable(globalNode=[2, 4], islocal=math%yes, &
!                      VALUE=fevar, ivar=2)
!```
!
! Full example: src/modules/BlockNodeField/examples/Get_test_1.F90
!
! See also: [[obj_Get5]]

INTERFACE
  MODULE SUBROUTINE obj_GetFEVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(BlockNodeField_), INTENT(IN) :: obj
    !! block node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! must be true, globalNode is a local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! nodal FEVariable, see [[obj_Get5]]
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical variable number, default 1
  END SUBROUTINE obj_GetFEVariable
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply one Dirichlet boundary condition to a physical variable
!
! Get the boundary nodes and nodal values of dbc (with
! obj%fedofs(ivar) and obj%geofedofs(ivar)) and set space component
! dbc%GetDOFNo() of physical variable ivar at these nodes. ivar must
! have one time component. All other entries are unchanged.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
! ! u has V (two space components) and P (one component) on 6 nodes
! CALL DirichletBCImportFromToml(obj=dbc, dom=dom, tomlName="dbc", &
!                                filename=tomlFileName)
! CALL u%ApplyDirichletBC(dbc=dbc(1)%ptr, ivar=1, times=0.0_DFP)
!```
!
! Full example:
! src/modules/BlockNodeField/examples/ApplyDirichletBC_test_1.F90
!
! See also: [[obj_ApplyDirichletBC2]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, ivar, times)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet BC, dbc%GetDOFNo() is the space component of ivar
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), OPTIONAL, INTENT(IN) :: times
    !! time, used when the BC values depend on time
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a vector of Dirichlet boundary conditions to a
!   physical variable
!
! Same as [[obj_ApplyDirichletBC1]] for each associated pointer in dbc;
! entries that are not associated are skipped.
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
! CALL u%ApplyDirichletBC(dbc=dbc, ivar=1, times=0.0_DFP)
!```
!
! Full example:
! src/modules/BlockNodeField/examples/ApplyDirichletBC_test_1.F90
!
! See also: [[obj_ApplyDirichletBC1]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, ivar, times)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet BCs, dbc(i)%ptr%GetDOFNo() is the space component
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), OPTIONAL, INTENT(IN) :: times
    !! time, used when the BC values depend on time
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply one space-time Dirichlet boundary condition to a
!   physical variable
!
! Get the boundary nodes and the nodal values at the time components
! (with obj%fedofs(ivar), obj%geofedofs(ivar) and obj%timefedofs(ivar))
! and set space component dbc%GetDOFNo() of every time component of
! ivar at these nodes. obj%timefedofs(ivar) must be associated, that is,
! the field must be initiated with timefedof.
!
! Changes:
!   - 2026-11-16: Bound to the generic ApplyDirichletBC (issue #645).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
! ! u is initiated with timefedof; ivar 1 has two time components
! CALL u%ApplyDirichletBC(dbc=dbc(1)%ptr, ivar=1, &
!                         times=[0.0_DFP, 1.0_DFP])
!```
!
! Full example:
! src/modules/BlockNodeField/examples/ApplyDirichletBC_test_2.F90
!
! See also: [[obj_ApplyDirichletBC1]], [[obj_ApplyDirichletBC4]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, dbc, ivar, times)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    !! Dirichlet BC, dbc%GetDOFNo() is the space component of ivar
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), INTENT(IN) :: times(:)
    !! times, size is the number of time components of ivar
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-01-22
! summary: Apply a vector of space-time Dirichlet boundary conditions
!   to a physical variable
!
! Same as [[obj_ApplyDirichletBC3]] for each associated pointer in dbc;
! entries that are not associated are skipped.
!
! Changes:
!   - 2026-11-16: Bound to the generic ApplyDirichletBC (issue #645).
!
! Examples
!
!```fortran
! TYPE(BlockNodeField_) :: u
! TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
! ! u is initiated with timefedof; ivar 1 has two time components
! CALL u%ApplyDirichletBC(dbc=dbc, ivar=1, times=[0.0_DFP, 1.0_DFP])
!```
!
! Full example:
! src/modules/BlockNodeField/examples/ApplyDirichletBC_test_2.F90
!
! See also: [[obj_ApplyDirichletBC2]], [[obj_ApplyDirichletBC3]]

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC4(obj, dbc, ivar, times)
    CLASS(BlockNodeField_), INTENT(INOUT) :: obj
    !! block node field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet BCs, dbc(i)%ptr%GetDOFNo() is the space component
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable number
    REAL(DFP), INTENT(IN) :: times(:)
    !! times, size is the number of time components of ivar
  END SUBROUTINE obj_ApplyDirichletBC4
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeField_Class

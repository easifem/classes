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

!> authors: Vikas Sharma, Ph. D.
! date: 16 Jul 2021
! summary: Abstract field is designed to handle fields in FEM
!
!# Introduction
! - In FEM, we use variables of different ranks. These varibles will be
! designated as the field. These fields can be defined at:
!   - Spatial-temporal nodal points
!   - Quadrature points inside the element
! - In addition, global matrices can also be described as the field.
! - In this way, Fields are high level objects in finite element modeling.
!
! [[AbstractField_]] defines an abstract class. This class will be extended
! to [[AbstractNodeField_]], [[AbstractElementField_]],
! [[AbstractMatrixField_]].

MODULE AbstractField_Class
USE GlobalData, ONLY: DFP, I4B, LGT, stdout, stdin, INT64, Constant, &
                      Space, Time, SpaceTime
USE BaseType, ONLY: RealVector_, DOF_
USE String_Class, ONLY: String
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE VTKFile_Class, ONLY: VTKFile_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_
USE TimeOpt_Class, ONLY: TimeOpt_
USE TxtFile_Class, ONLY: TxtFile_
USE FieldOpt_Class, ONLY: TypeField => TypeFieldOpt
USE EngineOpt_Class, ONLY: TypeEngineName => TypeEngineOpt
USE AbstractDomain_Class, ONLY: AbstractDomain_, AbstractDomainPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE tomlf, ONLY: toml_table
USE DirichletBC_Class, ONLY: DirichletBCPointer_, DirichletBC_
USE NeumannBC_Class, ONLY: NeumannBCPointer_, NeumannBC_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "AbstractField_Class"
CHARACTER(*), PARAMETER :: myprefix = "AbstractField"

PUBLIC :: AbstractFieldInitiate
PUBLIC :: AbstractFieldDisplay
PUBLIC :: AbstractFieldImport
PUBLIC :: AbstractFieldExport
PUBLIC :: AbstractFieldDeallocate
PUBLIC :: SetAbstractFieldParam
PUBLIC :: AbstractFieldCheckEssentialParam
PUBLIC :: AbstractField_
PUBLIC :: SetAbstractFieldParamFromToml
PUBLIC :: AbstractFieldReadFEDOFFromToml
PUBLIC :: AbstractFieldReadTimeFEDOFFromToml

!----------------------------------------------------------------------------
!                                                           AbstractField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 Jul 2021
! summary: Abstract field is designed to handle fields in FEM

TYPE, ABSTRACT :: AbstractField_
  LOGICAL(LGT) :: isInitiated = .FALSE.
  !! It is true if the object is initiated
  INTEGER(I4B) :: fieldType = TypeField%normal
  !! fieldType can be normal, constant, can vary in space and/ or both.
  TYPE(String) :: name
  !! name of the field
  TYPE(String) :: engine
  !! Engine of the field, for example
  !! NATIVE_SERIAL, NATIVE_OMP, NATIVE_MPI, PETSC, LIS_OMP, LIS_MPI
  INTEGER(I4B) :: comm = 0_I4B
  !! communication group (MPI)
  INTEGER(I4B) :: myRank = 0_I4B
  !! rank of current processor (MPI)
  INTEGER(I4B) :: numProcs = 1_I4B
  !! Total number of processors (MPI)
  INTEGER(I4B) :: global_n = 0_I4B
  !! total number of nodes on all processors (MPI)
  INTEGER(I4B) :: local_n = 0_I4B
  !! local number of nodes on a given processor (MPI)
  INTEGER(I4B) :: is = 0_I4B
  !! starting index (MPI)
  INTEGER(I4B) :: ie = 0_I4B
  !! end index + 1 (MPI)
  INTEGER(INT64) :: lis_ptr = 0_INT64
  !! lis_ptr is pointer returned by the LIS library
  !! It is used when engine is LIS_OMP or LIS_MPI
  CLASS(FEDOF_), POINTER :: fedof => NULL(), geofedof => NULL()
  !! pointer to fedof and geometric fedof
  TYPE(FEDOFPointer_), ALLOCATABLE :: fedofs(:), geofedofs(:)
  !! pointer to fedof
  CLASS(TimeFEDOF_), POINTER :: timefedof => NULL()
  !! pointer to time fedof
  TYPE(TimeFEDOFPointer_), ALLOCATABLE :: timefedofs(:)
  !! pointer to time fedofs

  TYPE(UserFunction_), POINTER :: exact => NULL()
  !! reference function for displacement
  !! Reference displacement denotes the exact solution
  LOGICAL(LGT) :: saveErrorNorm = .FALSE.
  !! save error norm
  CHARACTER(4) :: errorType = "NONE"
  !! errorType
  LOGICAL(LGT) :: plotWithResult = .FALSE.
  !! do you want to plot exact solution with result
  LOGICAL(LGT) :: plotErrorNorm = .FALSE.

  TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
  !! Dirichlet boundary conditions

  TYPE(NeumannBCPointer_), ALLOCATABLE :: nbc(:)
  !! Neumann boundary conditions on surfaces and edges

  TYPE(NeumannBCPointer_), ALLOCATABLE :: nbc_point(:)
  !! Neumann boundary conditions for point

CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE(obj_CheckEssentialParam), DEFERRED, PUBLIC, PASS(obj) :: &
    CheckEssentialParam
  !! Check essential parameters
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate the field by reading param and given domain
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copying other fields, and different options
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate  block fields (different physical variables) defined
  !! over different order of domain.
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate the field by arguments
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate5 => obj_Initiate5
  !! Initiate the field by arguments
  GENERIC, PUBLIC :: Initiate => Initiate1, Initiate2, Initiate3, &
    Initiate4, Initiate5
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the field

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the field
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data From hdf5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data in hdf5 file
  PROCEDURE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import data From toml file
  PROCEDURE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import data From toml file
  PROCEDURE, PASS(obj) :: ImportFromToml3 => obj_ImportFromToml3
  !! Import data From toml file
  PROCEDURE, PASS(obj) :: ImportFromToml4 => obj_ImportFromToml4
  !! Import data From toml file
  PROCEDURE, PASS(obj) :: ImportFromToml5 => obj_ImportFromToml5
  !! Import data From toml file
  PROCEDURE, PASS(obj) :: ImportFromToml6 => obj_ImportFromToml6
  !! Import data From toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2, &
    ImportFromToml3, ImportFromToml4, ImportFromToml5, ImportFromToml6
  !! Generic method to import data From toml file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => obj_WriteData_vtk
  !! Write data in vtk file
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_hdf5 => obj_WriteData_hdf5
  !! Write data in hdf5 file
  GENERIC, PUBLIC :: WriteData => WriteData_vtk, WriteData_hdf5

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj), NON_OVERRIDABLE, PUBLIC :: GetParam => obj_GetParam
  !! Get the parameters of AbstractField
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalPhysicalVars => &
    obj_GetTotalPhysicalVars
  !! Returns the total number of physical variables
  !!  This routine should be implemented by child classes
  !! For block matrices the physical variables are more than one,
  !! for example, presesure and velocity.
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetName => obj_GetName
  !! Returns the name
  PROCEDURE, PUBLIC, PASS(obj) :: GetPhysicalNames => obj_GetPhysicalNames
  !! Returns the names of physical variables
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetSpaceCompo => obj_GetSpaceCompo
  !! Return space component
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeCompo => obj_GetTimeCompo
  !! Return time component
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetStorageFMT => obj_GetStorageFMT
  !! Return storage format
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalDOF => obj_GetTotalDOF
  !! Returns the total number of degree of freedoms
  !! This is same as calling Size
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalVertexDOF => &
    obj_GetTotalVertexDOF
  !! Returns the total number of vertex degree of freedoms
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalEdgeDOF => obj_GetTotalEdgeDOF
  !! Returns the total number of edge degree of freedoms
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalFaceDOF => obj_GetTotalFaceDOF
  !! Returns the total number of face degree of freedoms
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalCellDOF => obj_GetTotalCellDOF
  !! Returns the total number of cell degree of freedoms
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj), NON_OVERRIDABLE :: isConstant => &
    obj_isConstant
  !! It returns true if the field is constant field
  !!  This routine should be implemented by child classes
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix
  !! Get the prefix of the field, it is necessary for Setting essential param
  !!  This routine should be implemented by child classes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetFEDOFPointer1 => &
    obj_GetFEDOFPointer1
  !! Get the FEDOF pointer, a single pointer is returned
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetFEDOFPointer2 => &
    obj_GetFEDOFPointer2
  !! Get the fedof pointer, a vector of pointers is returned
  GENERIC, PUBLIC :: GetFEDOFPointer => GetFEDOFPointer1, &
    GetFEDOFPointer2
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetTimeFEDOFPointer1 => &
    obj_GetTimeFEDOFPointer1
  !! Get the timefedof pointer, a single pointer is returned
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: GetTimeFEDOFPointer2 => &
    obj_GetTimeFEDOFPointer2
  !! Get the TimeFEDOF pointer, a vector of pointers is returned
  GENERIC, PUBLIC :: GetTimeFEDOFPointer => GetTimeFEDOFPointer1, &
    GetTimeFEDOFPointer2
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetEngineName => &
    obj_GetEngineName
  !! Get the engine name
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalNBC => &
    obj_GetTotalNBC
  !! Get size of nbc
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetTotalPointNBC => &
    obj_GetTotalPointNBC
  !! Get the size of nbc_point
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetNBCPointer => &
    obj_GetNBCPointer
  !! Get the pointer to nbc
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetPointNBCPointer => &
    obj_GetPointNBCPointer
  !! Get the pointer to nbc_point

  ! SET:
  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetParam => obj_SetParam
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetName => obj_SetName
  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set all the values to a constant scalar value

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: ApplyDirichletBC2 => obj_ApplyDirichletBC2
  PROCEDURE, PASS(obj) :: ApplyDirichletBC3 => obj_ApplyDirichletBC3
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3

  !SET:
  ! @NeumannBCMethods
  PROCEDURE, PASS(obj) :: ApplyPointNeumannBC1 => obj_ApplyPointNeumannBC1
  !! Apply point Neumann BC to field
  GENERIC, PUBLIC :: ApplyPointNeumannBC => ApplyPointNeumannBC1

END TYPE AbstractField_

!----------------------------------------------------------------------------
!                                   CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Check the essential parameters in param.

ABSTRACT INTERFACE
  SUBROUTINE obj_CheckEssentialParam(obj, param)
    IMPORT :: AbstractField_, ParameterList_
    CLASS(AbstractField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                  SetAbstractFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-04
! summary:  Set AbstractField_ parameters

INTERFACE
  MODULE SUBROUTINE SetAbstractFieldParam(param, prefix, name, engine, &
                                          fieldType, comm, local_n, &
                                          global_n, spaceCompo, &
                                          isSpaceCompo, isSpaceCompoScalar, &
                                          timeCompo, isTimeCompo, &
                                          isTimeCompoScalar, &
                                          tPhysicalVarNames, &
                                          physicalVarNames, &
                                          isPhysicalVarNames)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: prefix
    !! prefix
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! space components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    CHARACTER(*), OPTIONAL, INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
  END SUBROUTINE SetAbstractFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                     CheckEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Check essential param

INTERFACE
  MODULE SUBROUTINE AbstractFieldCheckEssentialParam(obj, param, prefix)
    CLASS(AbstractField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CHARACTER(*), INTENT(IN) :: prefix
  END SUBROUTINE AbstractFieldCheckEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate the field by reading param and given domain

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof, geofedof, timefedof)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    !! FEDOF object
    CLASS(FEDOF_), TARGET, INTENT(IN) :: geofedof
    !! Geometric Fedof object
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE AbstractFieldInitiate
  MODULE PROCEDURE obj_Initiate1
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate by copying other fields, and different options

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE AbstractFieldInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate by reading options From [[ParameterList_]]

INTERFACE
  MODULE SUBROUTINE obj_Initiate3(obj, param, fedof, geofedof, timefedof)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:), geofedof(:)
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedof(:)
    !! Vector of TimeFEDOFPointers
    !! All timefedofs should be initiated
  END SUBROUTINE obj_Initiate3
END INTERFACE

INTERFACE AbstractFieldInitiate
  MODULE PROCEDURE obj_Initiate3
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate the field by reading param and given domain
!
!# Introduction
!  This method is like obj_Initiate1, but it works with arugments
!  instead of parameter list.

INTERFACE
  MODULE SUBROUTINE obj_Initiate4( &
    obj, name, engine, fieldType, storageFMT, comm, local_n, global_n, &
    spaceCompo, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
    isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
    isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes, &
    isTNodesScalar, tSize, fedof, geofedof, timefedof)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: storageFMT
    !! Storage format of the field
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! space components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    CHARACTER(*), OPTIONAL, INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNamesScalar
    !! if true then physicalVarNames acts as scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tNodes(:)
    !! total number of nodes in each physical variable
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodes
    !! if true we will try to access tNodes
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodesScalar
    !! is tNodes scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
    !! total size of node field
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof, geofedof
    !! FEDOF object and geometric FEDOF object
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate4
END INTERFACE

INTERFACE AbstractFieldInitiate
  MODULE PROCEDURE obj_Initiate4
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Initiate the field by reading param and given domain
!
!# Introduction
!  This method is like obj_Initiate3, but it works with arugments
!  instead of parameter list.

INTERFACE
  MODULE SUBROUTINE obj_Initiate5( &
    obj, name, engine, fieldType, comm, local_n, global_n, spaceCompo, &
    isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
    isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
    isPhysicalVarNames, fedof, geofedof, timefedof)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! space components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    CHARACTER(*), OPTIONAL, INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:), geofedof(:)
    !! Vector of FEDOF pointers
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedof(:)
    !! Vector of TimeFEDOFPointers
    !! All timefedofs should be initiated
  END SUBROUTINE obj_Initiate5
END INTERFACE

INTERFACE AbstractFieldInitiate
  MODULE PROCEDURE obj_Initiate5
END INTERFACE AbstractFieldInitiate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE AbstractFieldDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE AbstractFieldDeallocate

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE AbstractFieldDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE AbstractFieldDisplay

!----------------------------------------------------------------------------
!                                                         IMPORT@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: fedof, geofedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

INTERFACE AbstractFieldImport
  MODULE PROCEDURE obj_Import
END INTERFACE AbstractFieldImport

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE AbstractFieldExport
  MODULE PROCEDURE obj_Export
END INTERFACE AbstractFieldExport

!----------------------------------------------------------------------------
!                                  AbstractFieldReadOptsFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-24
! summary:  Read options for intiating AbstractField from toml table
!
!# Introduction
! This method reads options for intiating AbstractField from toml table.
! The arguments details are given in Initiate4 and Initiate5 methods.
! This method will be called internally by ImportFromToml methods.
! We do not want to initiate AbstractField from param.

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadOptsFromToml( &
    table, name, engine, fieldType, spaceCompo, isSpaceCompo, &
    isSpaceCompoScalar, timeCompo, isTimeCompo, isTimeCompoScalar, &
    tPhysicalVarNames, physicalVarNames, isPhysicalVarNames, &
    isPhysicalVarNamesScalar)
    TYPE(toml_table), INTENT(INOUT) :: table
    !! Toml table
    TYPE(String), INTENT(OUT) :: name
    TYPE(String), INTENT(OUT) :: engine
    INTEGER(I4B), INTENT(OUT) :: fieldType
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: spaceCompo(:)
    LOGICAL(LGT), INTENT(OUT) :: isSpaceCompo
    LOGICAL(LGT), INTENT(OUT) :: isSpaceCompoScalar
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: timeCompo(:)
    LOGICAL(LGT), INTENT(OUT) :: isTimeCompo
    LOGICAL(LGT), INTENT(OUT) :: isTimeCompoScalar
    INTEGER(I4B), INTENT(OUT) :: tPhysicalVarNames
    CHARACTER(1), ALLOCATABLE, INTENT(INOUT) :: physicalVarNames(:)
    LOGICAL(LGT), INTENT(OUT) :: isPhysicalVarNames
    LOGICAL(LGT), INTENT(OUT) :: isPhysicalVarNamesScalar
  END SUBROUTINE AbstractFieldReadOptsFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data From toml file
!
!# Introduction
!
! This method is used to import data from toml table.
! fedof: If fedof is not initiated then it will be initiated by
!        calling fedof%ImportFromToml(node) method.
!        the node name is specified by fedofName
!
! timefedof: If timefedof is not initiated then it will be initiated by
!            calling timefedof%ImportFromToml(node) method.
!            the node name is specified by timefedofName.
!            It is needed for space-time fields.
!
! dom: dom is needed to initiate fedof
! timeOpt: It is needed to initiate timefedof

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml1(obj, table, fedof, geofedof, &
                                        timefedof, dom, timeOpt)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof, geofedof
    !! if fedof is not initiated then it will be initiated by
    !! calling fedof%ImportFromToml(node) method.
    !! where node is the table field called "space".
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(INOUT) :: timefedof
    !! timefedof is needed for space-time fields
    !! if  it is present then following operations are performed
    !! - If timefedof is not initiated then it will be initiated by
    !!   calling timefedof%ImportFromToml(node) method, where node
    !!   is the table field specified by "timefedofName".
    !!   In this case we need to provide timeOpt.
    !!   (Read more at TimeFEDOF_Class.F90)
    !! - If timefedof is already initiated then it will be used. In
    !!   this case we do not need use timeOpt
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Abstract domain object
    !! It is needed when fedof is not initiated.
    !! When we call ImportFromToml method of fedof
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    !! TimeOpt_ is needed when timefedof is not initiated
    !! Read more at TimeOpt_Class.F90
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data From toml file
!
!# Introduction
!
! In this method we call obj_ImportFromToml1 method.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, fedof, geofedof, timefedof, dom, timeOpt, afile, &
    filename, printToml)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof, geofedof
    !! if fedof is not initiated then it will be initiated by
    !! calling fedof%ImportFromToml(node) method.
    !! where node is the table field called "space".
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(INOUT) :: timefedof
    !! timefedof is needed for space-time fields
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Abstract domain object
    !! It is needed when fedof is not initiated.
    !! When we call ImportFromToml method of fedof
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    !! TimeOpt_ is needed when timefedof is not intiated
    !! Read more at obj_ImportFromToml1
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! txt file where toml config is stored
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! you can pass the filename, then we will make
    !! the file, open it and read the toml config and
    !! close the file.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! if it is true then we will print the toml config
  END SUBROUTINE obj_ImportFromToml2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-06
! summary:  Import data From toml file for block node fields
!
!# Introduction
!
! This method is like obj_ImportFromToml1 but this method is for
! BlockNodeField_ and BlockMatrixField_
!
! fedof: fedof is a vector of FEDOFPointer_ objects.
!        If it is not initiated then it will be initiated by calling
!        fedof(ii)%ImportFromToml(node) method.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml3( &
    obj, table, fedof, geofedof, timefedof, dom, timeOpt)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:), geofedof(:)
    !! The size of fedof should be total number of physical variables.
    !! If fedof is not allocated then we will allocate it.
    !! If fedof(ii) not initiated then we will intiate it From
    !! toml table, in this case we will call improt From toml method
    !! of fedof. We will need domain in this case
    TYPE(TimeFEDOFPointer_), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: &
      timefedof(:)
    !! The size of timefedof should be total number of physical variables.
    !! timefedof is needed for space-time fields
    !! If timefedof is not allocated then we will allocate it.
    !! If timefedof(ii) not initiated then we will intiate it From
    !! toml table, in this case we will call improt From toml method
    !! of timefedof. We will need timeOpt in this case
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Abstract domain object
    !! It is needed when fedof is not initiated.
    !! When we call ImportFromToml method of fedof
    !! In this case all fedofs will have same domain
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    !! TimeOpt_ is needed when timefedof is not initiated
    !! Read more at TimeOpt_Class.F90
  END SUBROUTINE obj_ImportFromToml3
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data From toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml4( &
    obj, tomlName, fedof, geofedof, timefedof, dom, timeOpt, afile, &
    filename, printToml)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:), geofedof(:)
    !! Read the docs of obj_ImportFromToml3
    TYPE(TimeFEDOFPointer_), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: &
      timefedof(:)
    !! Read the docs of obj_ImportFromToml3
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Read the docs of obj_ImportFromToml3
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    !! Read the docs of obj_ImportFromToml1
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    !! text file where toml config is stored
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    !! you can pass the filename, then we will make
    !! the file, open it and read the toml config and
    !! close the file.
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
    !! if it is true then we will print the toml config
  END SUBROUTINE obj_ImportFromToml4
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-06
! summary:  Import data From toml file for block node fields
!
!# Introduction
!   This methos is like obj_ImportFromToml3 but in this case
! we can provide different domain for each physical variable.
!
! In this case domain is NOT optional, it is required.
! This is because in this case we will initiate fedof
! If fedof is already initiated then we do not need domain. Hence
! you should call obj_ImportFromToml3 (without domain) instead of this method.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml5( &
    obj, table, fedof, geofedof, timefedof, dom, timeOpt)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:), geofedof(:)
    !! Read the docs of obj_ImportFromToml3
    !! Geometric FEDOF object
    TYPE(AbstractDomainPointer_), INTENT(IN) :: dom(:)
    !! The size of domain should be total number of physical variables.
    !! Read the docs of obj_ImportFromToml3
    TYPE(TimeFEDOFPointer_), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: &
      timefedof(:)
    !! Read the docs of obj_ImportFromToml3
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    !! Read the docs of obj_ImportFromToml1
  END SUBROUTINE obj_ImportFromToml5
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data From toml file

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml6( &
    obj, tomlName, fedof, geofedof, timefedof, dom, timeOpt, afile, &
    filename, printToml)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:), geofedof(:)
    TYPE(AbstractDomainPointer_), INTENT(IN) :: dom(:)
    TYPE(TimeFEDOFPointer_), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: &
      timefedof(:)
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml6
END INTERFACE

!----------------------------------------------------------------------------
!                                    SetAbstractFieldParamFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-15
! summary:  Make param From toml file

INTERFACE
  MODULE SUBROUTINE SetAbstractFieldParamFromToml( &
    param, table, prefix, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    !! Parameter list to be set
    TYPE(toml_table), INTENT(INOUT) :: table
    !! toml table From which parameters are read
    CHARACTER(*), INTENT(IN) :: prefix
    !! prefix
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
  END SUBROUTINE SetAbstractFieldParamFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                      AbstractFieldReadUserFunctionFromToml
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-06
! summary:  Abstract field read user function from toml

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadUserFunctionFromToml(obj, table)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE AbstractFieldReadUserFunctionFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                   AbstractFieldReadFEDOFFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-15
! summary:  Safely read fedof From toml file,
!
!# Introduction
!   This method is used by ReadFromToml methods
! If fedof is not initiated then it will be initiated by
! calling fedof%ImportFromToml(node) method.

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadFEDOFFromToml1(table, fedof, dom)
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof
    !! if fedof is not initiated then it will be initiated by
    !! calling fedof%ImportFromToml(node) method.
    !! where node is the table field called "space".
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Abstract domain object
    !! It is needed when fedof is not initiated.
    !! When we call ImportFromToml method of fedof
  END SUBROUTINE AbstractFieldReadFEDOFFromToml1
END INTERFACE

INTERFACE AbstractFieldReadFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadFEDOFFromToml1
END INTERFACE AbstractFieldReadFEDOFFromToml

!----------------------------------------------------------------------------
!                                    AbstractFieldReadFEDOFFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-06
! summary: Safely read fedof From toml file for block node fields
!
!# Introduction
!   This method is used by ReadFromToml methods
! If fedof is not initiated then it will be initiated by
!   calling fedof%ImportFromToml(node) method.
! In this case we will initiate different fedof for each physical
! variables

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadFEDOFFromToml2(table, fedof, dom)
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:)
    !! The size of fedof should be total number of physical variables
    !! If fedof is allocated then all its ptr should be associated
    !! Read docs of AbstractFieldReadFEDOFFromToml1
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Read docs of AbstractFieldReadFEDOFFromToml1
  END SUBROUTINE AbstractFieldReadFEDOFFromToml2
END INTERFACE

INTERFACE AbstractFieldReadFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadFEDOFFromToml2
END INTERFACE AbstractFieldReadFEDOFFromToml

!----------------------------------------------------------------------------
!                                    AbstractFieldReadFEDOFFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-06
! summary: Safely read fedof From toml file for block node fields
!
!# Introduction
!   This method is used by ReadFromToml methods
! If fedof is not initiated then it will be initiated by
!   calling fedof%ImportFromToml(node) method.
! In this case we will initiate different fedof for each physical
! variables

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadFEDOFFromToml3(table, fedof, dom)
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:)
    !! The size of fedof should be total number of physical variables
    !! Read docs of AbstractFieldReadFEDOFFromToml1
    TYPE(AbstractDomainPointer_), INTENT(IN) :: dom(:)
    !! Read docs of AbstractFieldReadFEDOFFromToml1
  END SUBROUTINE AbstractFieldReadFEDOFFromToml3
END INTERFACE

INTERFACE AbstractFieldReadFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadFEDOFFromToml3
END INTERFACE AbstractFieldReadFEDOFFromToml

!----------------------------------------------------------------------------
!                              AbstractFieldReadGeoFEDOFFromToml@TomlMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-15
! summary:  Safely read fedof From toml file,
!
!# Introduction
!   This method is used by ReadFromToml methods
! If fedof is not initiated then it will be initiated by
! calling fedof%ImportFromToml(node) method.

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadGeoFEDOFFromToml1(table, fedof, dom)
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(FEDOF_), TARGET, INTENT(INOUT) :: fedof
    !! if fedof is not initiated then it will be initiated by
    !! calling fedof%ImportFromToml(node) method.
    !! where node is the table field called "space".
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Abstract domain object
    !! It is needed when fedof is not initiated.
    !! When we call ImportFromToml method of fedof
  END SUBROUTINE AbstractFieldReadGeoFEDOFFromToml1
END INTERFACE

INTERFACE AbstractFieldReadGeoFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadGeoFEDOFFromToml1
END INTERFACE AbstractFieldReadGeoFEDOFFromToml

!----------------------------------------------------------------------------
!                                 AbstractFieldReadGeoFEDOFFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-06
! summary: Safely read fedof From toml file for block node fields
!
!# Introduction
!   This method is used by ReadFromToml methods
! If fedof is not initiated then it will be initiated by
!   calling fedof%ImportFromToml(node) method.
! In this case we will initiate different fedof for each physical
! variables

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadGeoFEDOFFromToml2(table, fedof, dom)
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:)
    !! The size of fedof should be total number of physical variables
    !! If fedof is allocated then all its ptr should be associated
    !! Read docs of AbstractFieldReadFEDOFFromToml1
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Read docs of AbstractFieldReadFEDOFFromToml1
  END SUBROUTINE AbstractFieldReadGeoFEDOFFromToml2
END INTERFACE

INTERFACE AbstractFieldReadGeoFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadGeoFEDOFFromToml2
END INTERFACE AbstractFieldReadGeoFEDOFFromToml

!----------------------------------------------------------------------------
!                                 AbstractFieldReadGeoFEDOFFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-06
! summary: Safely read fedof From toml file for block node fields
!
!# Introduction
!   This method is used by ReadFromToml methods
! If fedof is not initiated then it will be initiated by
!   calling fedof%ImportFromToml(node) method.
! In this case we will initiate different fedof for each physical
! variables

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadGeoFEDOFFromToml3(table, fedof, dom)
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:)
    !! The size of fedof should be total number of physical variables
    !! Read docs of AbstractFieldReadFEDOFFromToml1
    TYPE(AbstractDomainPointer_), INTENT(IN) :: dom(:)
    !! Read docs of AbstractFieldReadFEDOFFromToml1
  END SUBROUTINE AbstractFieldReadGeoFEDOFFromToml3
END INTERFACE

INTERFACE AbstractFieldReadGeoFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadGeoFEDOFFromToml3
END INTERFACE AbstractFieldReadGeoFEDOFFromToml

!----------------------------------------------------------------------------
!                               AbstractFieldReadTimeFEDOFFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-15
! summary:  Safely read timefedof From toml file,
!
!# Introduction
!   This method is used by ReadFromToml methods
! - If timefedof is not present then this method does nothing
! - If timefedof is present and it is initiated then it will do nothing
! - If timefedof is present and it is not initiated then it will be initiated
! by calling timefedof%ImportFromToml(node) method.

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadTimeFEDOFFromToml1( &
    table, timefedof, timeOpt)
    TYPE(toml_table), INTENT(INOUT) :: table
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(INOUT) :: timefedof
    !! Timefedof, read the docs above
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    !! TimeOpt is need when timefedof is being initiated
    !! Read more at TimeOpt_Class.F90
  END SUBROUTINE AbstractFieldReadTimeFEDOFFromToml1
END INTERFACE

INTERFACE AbstractFieldReadTimeFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadTimeFEDOFFromToml1
END INTERFACE AbstractFieldReadTimeFEDOFFromToml

!----------------------------------------------------------------------------
!                               AbstractFieldReadTimeFEDOFFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-25
! summary:  Safely read timefedof From toml file,
!
!# Introduction
!   This method is used by ReadFromToml methods
! - If timefedof is not present then this method does nothing
! - If timefedof is present and it is initiated then it will do nothing
! - If timefedof is present and it is not initiated then it will be initiated
! by calling timefedof%ImportFromToml(node) method.

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadTimeFEDOFFromToml2(table, timefedof, &
                                                        timeOpt)
    TYPE(toml_table), INTENT(INOUT) :: table
    TYPE(TimeFEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: &
      timefedof(:)
    !! Timefedof, read the docs above
    CLASS(TimeOpt_), OPTIONAL, TARGET, INTENT(IN) :: timeOpt
    !! TimeOpt is need when timefedof is being initiated
    !! Read more at TimeOpt_Class.F90
  END SUBROUTINE AbstractFieldReadTimeFEDOFFromToml2
END INTERFACE

INTERFACE AbstractFieldReadTimeFEDOFFromToml
  MODULE PROCEDURE AbstractFieldReadTimeFEDOFFromToml2
END INTERFACE AbstractFieldReadTimeFEDOFFromToml

!----------------------------------------------------------------------------
!                                     AbstractFieldReadDBCFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-15
! summary: Read DBC from toml file

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadDBCFromToml(obj, table)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE AbstractFieldReadDBCFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                     AbstractFieldReadNBCFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-24
! summary: Read NBC from toml file

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadNBCFromToml(obj, table)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE AbstractFieldReadNBCFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                 AbstractFieldReadPointNBCFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-24
! summary: Read NBC from toml file

INTERFACE
  MODULE SUBROUTINE AbstractFieldReadPointNBCFromToml(obj, table)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(toml_table), INTENT(INOUT) :: table
  END SUBROUTINE AbstractFieldReadPointNBCFromToml
END INTERFACE

!----------------------------------------------------------------------------
!                                                     WriteData@IOMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_WriteData_hdf5(obj, hdf5, group)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_WriteData_hdf5
END INTERFACE

INTERFACE AbstractFieldWriteData
  MODULE PROCEDURE obj_WriteData_hdf5
END INTERFACE AbstractFieldWriteData

!----------------------------------------------------------------------------
!                                                      WriteData@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-24
! summary:  Export data in vrkfile

INTERFACE
  MODULE SUBROUTINE obj_WriteData_vtk(obj, vtk)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
  END SUBROUTINE obj_WriteData_vtk
END INTERFACE

INTERFACE AbstractFieldWriteData
  MODULE PROCEDURE obj_WriteData_vtk
END INTERFACE AbstractFieldWriteData

!----------------------------------------------------------------------------
!                                                       SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Set field variables of abstract field

INTERFACE
  MODULE SUBROUTINE obj_SetParam( &
    obj, isInitiated, fieldType, name, engine, comm, myRank, numProcs, &
    global_n, local_n, is, ie, lis_ptr, tSize, realVec, dof, &
    isPMatInitiated, fedof, fedofs)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isInitiated
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    CHARACTER(*), OPTIONAL, INTENT(IN) :: name
    CHARACTER(*), OPTIONAL, INTENT(IN) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: myRank
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: numProcs
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: is
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ie
    INTEGER(INT64), OPTIONAL, INTENT(IN) :: lis_ptr
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
    TYPE(RealVector_), OPTIONAL, INTENT(IN) :: realVec
    TYPE(DOF_), OPTIONAL, INTENT(IN) :: dof
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPMatInitiated
    CLASS(FEDOF_), OPTIONAL, TARGET, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
  END SUBROUTINE obj_SetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetName@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-20
! summary: Set the name of the field

INTERFACE
  MODULE SUBROUTINE obj_SetName(obj, name)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
  END SUBROUTINE obj_SetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                       SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-10-27
! summary: Set all the values

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set or add
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalNBC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-12
! summary: Get the size of nbc

INTERFACE
  MODULE FUNCTION obj_GetTotalNBC(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
    !! if nbc is not allocated then ans is zero
  END FUNCTION obj_GetTotalNBC
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalNBC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-12
! summary: Get the size of nbc

INTERFACE
  MODULE FUNCTION obj_GetTotalPointNBC(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
    !! if nbc_point is not allocated then ans is zero
  END FUNCTION obj_GetTotalPointNBC
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetNBCPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-12
! summary:  Return a poitner to nbc

INTERFACE
  MODULE FUNCTION obj_GetNBCPointer(obj, indx) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
  !! AbstractField
    INTEGER(I4B), INTENT(IN) :: indx
  !! pointer number
    CLASS(NeumannBC_), POINTER :: ans
  !! if nbc is not allocated then ans%ptr is null()
  !! if nbc is allocated but nbc(ii)%ptr is not associated
  !! then ans(ii)%ptr is null()
  END FUNCTION obj_GetNBCPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetPointNBCPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-12
! summary:  Return a poitner to nbc_point

INTERFACE
  MODULE FUNCTION obj_GetPointNBCPointer(obj, indx) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
  !! AbstractField
    INTEGER(I4B), INTENT(IN) :: indx
  !! pointer number
    CLASS(NeumannBC_), POINTER :: ans
  !! if nbc is not allocated then ans%ptr is null()
  !! if nbc is allocated but nbc(ii)%ptr is not associated
  !! then ans(ii)%ptr is null()
  END FUNCTION obj_GetPointNBCPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetParam@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-25
! summary:  Get the field variables

INTERFACE
  MODULE SUBROUTINE obj_GetParam( &
    obj, isInitiated, fieldType, name, engine, comm, myRank, numProcs, &
    global_n, local_n, is, ie, lis_ptr, tSize, realVec, dof, &
    isPMatInitiated, fedof, fedofs)
    CLASS(AbstractField_), INTENT(IN) :: obj
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isInitiated
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: fieldType
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: name
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: myRank
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: numProcs
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: global_n
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: is
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: ie
    INTEGER(INT64), OPTIONAL, INTENT(OUT) :: lis_ptr
    INTEGER(I4B), OPTIONAL, INTENT(OUT) :: tSize
    TYPE(RealVector_), OPTIONAL, INTENT(OUT) :: realVec
    TYPE(DOF_), OPTIONAL, INTENT(OUT) :: dof
    LOGICAL(LGT), OPTIONAL, INTENT(OUT) :: isPMatInitiated
    CLASS(FEDOF_), OPTIONAL, POINTER, INTENT(OUT) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(OUT) :: fedofs(:)
  END SUBROUTINE obj_GetParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           GetTotalPhysicalVars@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-03
! summary:  Returns the total number of physical variables

INTERFACE
  MODULE FUNCTION obj_GetTotalPhysicalVars(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalPhysicalVars
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetPhysicalNames@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns the names of physical variables

INTERFACE
  MODULE SUBROUTINE obj_GetPhysicalNames(obj, ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(*), INTENT(INOUT) :: ans(:)
  END SUBROUTINE obj_GetPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                                          GetName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns the name of the field

INTERFACE
  MODULE FUNCTION obj_GetName(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetName
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetSpaceCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns space components

INTERFACE
  MODULE FUNCTION obj_GetSpaceCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
      !! Total number of physical variables
      !! This can be obtained From GetTotalPhysicalVars method
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetSpaceCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns Time components

INTERFACE
  MODULE FUNCTION obj_GetTimeCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTimeCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetStorageFMT@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns storage format

INTERFACE
  MODULE FUNCTION obj_GetStorageFMT(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetStorageFMT
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetTotalDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of degree of freedoms
!
!# Introduction
! This method is same as calling the size function.

INTERFACE
  MODULE FUNCTION obj_GetTotalDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTotalVertexDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of vertex degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalVertexDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalVertexDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalEdgeDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Edge degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalEdgeDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalEdgeDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetTotalFaceDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Face degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalFaceDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalFaceDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                GetTotalCellDOF@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary:  Returns the total number of Cell degree of freedoms

INTERFACE
  MODULE FUNCTION obj_GetTotalCellDOF(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    INTEGER(I4B) :: ans(tPhysicalVars)
  END FUNCTION obj_GetTotalCellDOF
END INTERFACE

!----------------------------------------------------------------------------
!                                                      isConstant@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-09-22
! summary:  Returns true if the field is constant
INTERFACE
  MODULE FUNCTION obj_IsConstant(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    LOGICAL(LGT) :: ans
  END FUNCTION obj_IsConstant
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetFEDOFPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary: Get the FEDOF pointer
!
! # Introduction
!
! This method returns the fedof pointer. It workds as follows:
!
! - if indx is not given then it will return fedof
! - if indx is given then it will check if the fedofs is allocated
! -  if fedofs is allocated then it will return the pointer fedof(indx)
! - if fedofs is not allocated then it will return the pointer to fedof
! - User should check if the pointer is associated or not.

INTERFACE
  MODULE FUNCTION obj_GetFEDOFPointer1(obj, indx) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: indx
  !! indx is the index of physical variable
  !! for which FEDOF pointer is requested
    CLASS(FEDOF_), POINTER :: ans
  !! ans is the FEDOF pointer, it is a single pointer
  !! to FEDOF object.
  END FUNCTION obj_GetFEDOFPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetFEDOFPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-14
! summary: Get the FEDOF pointer

INTERFACE
  MODULE FUNCTION obj_GetFEDOFPointer2(obj, indx) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(1)
  !! This is just a dummy argument to have a unique interface
    TYPE(FEDOFPointer_), ALLOCATABLE :: ans(:)
  !! List of FEDOF pointers
  END FUNCTION obj_GetFEDOFPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetTimeFEDOFPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Get pointer to timefedof
!
! # Introduction
!
! This method returns the timefedof pointer. It workds as follows:
!
! - if indx is not given then it will return timefedof
! - if indx is given then it will check if the timefedofs is allocated
! - if timefedofs is allocated
!   then it will return the pointer timefedof(indx)
! - if timefedofs is not allocated
!   then it will return the pointer to fedof
! - User should check if the pointer is associated or not.

INTERFACE
  MODULE FUNCTION obj_GetTimeFEDOFPointer1(obj, indx) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: indx
    CLASS(TimeFEDOF_), POINTER :: ans
  END FUNCTION obj_GetTimeFEDOFPointer1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 GetFEDOFPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-01
! summary:  Get timefedof pointers

INTERFACE
  MODULE FUNCTION obj_GetTimeFEDOFPointer2(obj, indx) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(1)
    !! This is just a dummy argument to have a unique interface
    TYPE(TimeFEDOFPointer_), ALLOCATABLE :: ans(:)
    !! List of FEDOF pointers
  END FUNCTION obj_GetTimeFEDOFPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetEngineName@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary:  Get the engine name of the field

INTERFACE
  MODULE FUNCTION obj_GetEngineName(obj) RESULT(ans)
    CLASS(AbstractField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetEngineName
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-05
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times, ivar, extField)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times, ivar, extField)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, dbcPtrs)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: dbcPtrs(:)
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyPointNeumannBC@NBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add contribution of point Neumann boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyPointNeumannBC1(obj, scale, times, ivar, &
                                             extField)
    CLASS(AbstractField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: scale
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyPointNeumannBC1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE AbstractField_Class

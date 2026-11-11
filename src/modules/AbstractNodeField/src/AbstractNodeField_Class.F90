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

MODULE AbstractNodeField_Class
USE AbstractBC_Class, ONLY: AbstractBC_
USE AbstractDomain_Class, ONLY: AbstractDomainPointer_
USE AbstractDomain_Class, ONLY: AbstractDomain_
USE AbstractField_Class, ONLY: AbstractField_
USE Basetype, ONLY: DOF_
USE Basetype, ONLY: FEVariable_
USE Basetype, ONLY: RealVector_
USE BaseType, ONLY: math => TypeMathOpt
USE DirichletBC_Class, ONLY: DirichletBCPointer_
USE DirichletBC_Class, ONLY: DirichletBC_
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDOF_Class, ONLY: FEDOFPointer_
USE FieldOpt_Class, ONLY: TypeFieldOpt
USE GlobalData, ONLY: DFP
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE GlobalData, ONLY: INT8
USE HDF5File_Class, ONLY: HDF5File_
USE NeumannBC_Class, ONLY: NeumannBCPointer_
USE NeumannBC_Class, ONLY: NeumannBC_
USE TimeFEDOF_Class, ONLY: TimeFEDOFPointer_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_
USE TimeOpt_Class, ONLY: TimeOpt_
USE tomlf, ONLY: toml_table
USE TxtFile_Class, ONLY: TxtFile_
USE UserFunction_Class, ONLY: UserFunction_
USE VTKFile_Class, ONLY: VTKFile_
IMPLICIT NONE
PRIVATE

PUBLIC :: AbstractNodeFieldDisplay
PUBLIC :: AbstractNodeField_
PUBLIC :: AbstractNodeFieldPointer_
PUBLIC :: AbstractNodeFieldImport
PUBLIC :: AbstractNodeFieldExport
PUBLIC :: AbstractNodeFieldGetPointer
PUBLIC :: AbstractNodeFieldDeallocate
PUBLIC :: AbstractNodeFieldSetSingle
PUBLIC :: AbstractNodeFieldGetSingle
PUBLIC :: AbstractNodeFieldInitiate
PUBLIC :: AbstractNodeFieldSetParam
PUBLIC :: AbstractNodeFieldGetFEVariable
PUBLIC :: NodeFieldsWriteData

INTEGER(I4B), PARAMETER :: MYSTORAGEFORMAT = TypeFieldOpt%storageFormatDOF

!----------------------------------------------------------------------------
!                                                         AbstractNodeField_
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-24
! summary: Abstract node field
!
! A node field stores one value for each DOF of each component of
! each physical variable in a vector (realVec), in the DOF storage
! format: the values are grouped by physical variable and component,
! and within a group they follow the DOF numbers of fedof. The DOF
! layout is given by the DOF_ object dof and by the components below.
!
! The child classes (ScalarField_, VectorField_, ...) set the DOF data
! in Initiate and provide the methods based on node numbers (Set,
! Get, ...). This class provides the methods based on the index of a
! value (SetSingle, SetMultiple, GetSingle, GetMultiple), the BLAS
! methods (AXPY, SCAL, COPY, Swap, norms, ...) and HDF5 and VTK
! output.
!
! Components:
!   dof_tPhysicalVars - number of physical variables
!   dof_storageFMT - storage format (DOF format)
!   tSize - number of values, SIZE(realVec)
!   dof_spaceCompo, dof_timeCompo - space and time components of each
!     physical variable
!   dof_tNodes - number of nodes (DOFs) of each physical variable
!   dof_names_char - one character name of each physical variable
!   realVec - vector of values
!   dof - DOF object
!   fedof, geofedof, fedofs, geofedofs, timefedof, timefedofs - FEDOF
!     pointers
!   dbc, nbc, nbc_point - boundary conditions
!
! Changes:
!   - 2026-11-11: Moved dof, the FEDOF pointers, the boundary conditions
!     and the methods that use them from [[AbstractField_]].
!   - 2026-11-11: Added SetLocal_n and SetGlobal_n.

TYPE, ABSTRACT, EXTENDS(AbstractField_) :: AbstractNodeField_
  LOGICAL(LGT) :: isMaxTotalNodeNumForBCSet = math%no
  !! It is true when we set the maxTotalNodeNumForBC
  !! see, nodeNum, nodalValue, and GetMaxTotalNodeNumForBC
  INTEGER(I4B) :: maxTotalNodeNumForBC = math%zero_i
  !! maximum total node num for applying boundary conditions
  !! see, nodeNum, nodalValue, and GetMaxTotalNodeNumForBC
  TYPE(DOF_) :: dof
  !! Degree of freedom object,
  !! which contains the information about how the different
  !! components of the fields are stored inside the realVec
  !! or column space of the matrix
  !! note: This variable is only for internal use
  CLASS(FEDOF_), POINTER :: fedof => NULL(), geofedof => NULL()
  !! pointer to fedof and geometric fedof
  TYPE(FEDOFPointer_), ALLOCATABLE :: fedofs(:), geofedofs(:)
  !! pointer to fedof
  CLASS(TimeFEDOF_), POINTER :: timefedof => NULL()
  !! pointer to time fedof
  TYPE(TimeFEDOFPointer_), ALLOCATABLE :: timefedofs(:)
  !! pointer to time fedofs
  TYPE(DirichletBCPointer_), ALLOCATABLE :: dbc(:)
  !! Dirichlet boundary conditions
  TYPE(NeumannBCPointer_), ALLOCATABLE :: nbc(:)
  !! Neumann boundary conditions on surfaces and edges
  TYPE(NeumannBCPointer_), ALLOCATABLE :: nbc_point(:)
  !! Neumann boundary conditions for point
  REAL(DFP), ALLOCATABLE :: nodalValue(:, :)
  !! This array is used internally to store the nodal values of
  !! for dirichletBC, NeumannBC,
  !! The entries are obtained from Get method of AbstractBC
  !! The number of rows is maxTotalNodeNumForBC
  INTEGER(I4B), ALLOCATABLE :: nodeNum(:)
  !! This vector is used internally to apply Dirichlet boundary condition
  !! It stores the node numbers (fedof) where the DBC, NBC, or nbcPoint
  !! are applied.
  INTEGER(I4B) :: dof_tPhysicalVars = math%zero_i
  !! Total number of physical variables
  !! note - This variable is only for internal use
  INTEGER(I4B) :: dof_storageFMT = MYSTORAGEFORMAT
  !! Storage format
  !! note - This variable is only for internal use
  INTEGER(I4B) :: tSize = math%zero_i
  !! Total length of the nodal field = tdof * tNodes
  !! note - This variable is only for internal use
  INTEGER(I4B), ALLOCATABLE :: dof_spaceCompo(:)
  !! Spatial components
  !! note - This variable is only for internal use
  INTEGER(I4B), ALLOCATABLE :: dof_timeCompo(:)
  !! note - This variable is only for internal use
  INTEGER(I4B), ALLOCATABLE :: dof_tNodes(:)
  !! Total number of nodes
  !! note - This variable is only for internal use
  CHARACTER(1), ALLOCATABLE :: dof_names_char(:)
  !! Single character name of physical variable
  !! note - This variable is only for internal use
  TYPE(RealVector_) :: realVec
  !! Vector of reals for internal use only
  !! note - This variable is only for internal use

CONTAINS
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate from another node field (copy)
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by passing arguments
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate3 => obj_Initiate3
  !! Initiate with a FEDOF for each variable (under development)
  GENERIC, PUBLIC :: Initiate => Initiate2, Initiate3
  !! Initiate by passing arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored inside

  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of AbstractNodeField

  !@HDF5Methods
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import AbstractNodeField from HDF5File_
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export AbstractNodeField to HDF5File_

  ! @TomlMethods
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml1 => obj_ImportFromToml1
  !! Import data From toml file
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml2 => obj_ImportFromToml2
  !! Import data From toml file
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml3 => obj_ImportFromToml3
  !! Import data From toml file
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml4 => obj_ImportFromToml4
  !! Import data From toml file
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml5 => obj_ImportFromToml5
  !! Import data From toml file
  PROCEDURE, PRIVATE, PASS(obj) :: ImportFromToml6 => obj_ImportFromToml6
  !! Import data From toml file
  GENERIC, PUBLIC :: ImportFromToml => ImportFromToml1, ImportFromToml2, &
    ImportFromToml3, ImportFromToml4, ImportFromToml5, ImportFromToml6
  !! Generic method to import data From toml file

  ! @DOFMethods
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: GetFEDOFPointer1 => &
    obj_GetFEDOFPointer1
  !! Get the FEDOF pointer, a single pointer is returned
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: GetFEDOFPointer2 => &
    obj_GetFEDOFPointer2
  !! Get the fedof pointer, a vector of pointers is returned
  GENERIC, PUBLIC :: GetFEDOFPointer => GetFEDOFPointer1, &
    GetFEDOFPointer2
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: GetTimeFEDOFPointer1 => &
    obj_GetTimeFEDOFPointer1
  !! Get the timefedof pointer, a single pointer is returned
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: GetTimeFEDOFPointer2 => &
    obj_GetTimeFEDOFPointer2
  !! Get the TimeFEDOF pointer, a vector of pointers is returned
  GENERIC, PUBLIC :: GetTimeFEDOFPointer => GetTimeFEDOFPointer1, &
    GetTimeFEDOFPointer2
  !! Generic method to get the TimeFedof_ pointer
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalNodeLoc1 => &
    obj_GetTotalNodeLoc1
  !! Get size of NodeLoc(globalNode, spaceCompo, timeCompo, ivar)
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalNodeLoc2 => &
    obj_GetTotalNodeLoc2
  !! Get size of NodeLoc(AbstractBC, ivar)
  PROCEDURE, PRIVATE, PASS(obj) :: GetTotalNodeLoc3 => &
    obj_GetTotalNodeLoc3
  !! Get size of NodeLoc(Vector of AbstractBCPointer, ivar)
  GENERIC, PUBLIC :: GetTotalNodeLoc => GetTotalNodeLoc1, GetTotalNodeLoc2, &
    GetTotalNodeLoc3
  !! Generic method to get the size of NodeLoc
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeLoc1 => obj_GetNodeLoc1
  !! Get location of global node number
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeLoc2 => obj_GetNodeLoc2
  !! Get location of global node number from AbstractBC
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeLoc3 => obj_GetNodeLoc3
  !! Get location of global node number from DirichletBCPointer
  GENERIC, PUBLIC :: GetNodeLoc => GetNodeLoc1, GetNodeLoc2, &
    GetNodeLoc3
  !! Generic method for getting location of nodes

  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeLoc_1 => obj_GetNodeLoc_1
  !! Like GetNodeLoc1 but without allocation
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeLoc_2 => obj_GetNodeLoc_2
  !! Like GetNodeLoc1 but without allocation
  PROCEDURE, PRIVATE, PASS(obj) :: GetNodeLoc_3 => obj_GetNodeLoc_3
  !! Like GetNodeLoc1 but without allocation
  GENERIC, PUBLIC :: GetNodeLoc_ => GetNodeLoc_1, GetNodeLoc_2, &
    GetNodeLoc_3
   !! Generic method to get node location

  ! @BCMethods
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: GetTotalNBC => &
    obj_GetTotalNBC
  !! Get size of nbc
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: GetTotalPointNBC => &
    obj_GetTotalPointNBC
  !! Get the size of nbc_point
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: GetNBCPointer => &
    obj_GetNBCPointer
  !! Get the pointer to nbc
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: GetPointNBCPointer => &
    obj_GetPointNBCPointer
  !! Get the pointer to nbc_point
  PROCEDURE, PRIVATE, PASS(obj) :: GetMaxTotalNodeNumForBC1 => &
    obj_GetMaxTotalNodeNumForBC1
  PROCEDURE, PRIVATE, PASS(obj) :: GetMaxTotalNodeNumForBC2 => &
    obj_GetMaxTotalNodeNumForBC2
  !! Get the maximum node number for applying boundary conditions
  !! This will call GetTotalNodeNum on every dbc, nbc, nbc_point
  !! and return the maximum value. This method is necessary for
  !! allocating the nodeNum and nodalValue arrays
  GENERIC, PUBLIC :: GetMaxTotalNodeNumForBC => &
    GetMaxTotalNodeNumForBC1, GetMaxTotalNodeNumForBC2
   !! Generic method to get total number of nodes for boundary conditions
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetDirichletBCIndex => &
    obj_GetDirichletBCIndex
  !! Get the Dirichlet BC indices
  PROCEDURE, PRIVATE, PASS(obj) :: GetDirichletBCIndex1_ => &
    obj_GetDirichletBCIndex1_
  !! Get the Dirichlet BC indices from dbc stored internally
  PROCEDURE, PRIVATE, PASS(obj) :: GetDirichletBCIndex2_ => &
    obj_GetDirichletBCIndex2_
  !! Get the Dirichlet BC indices from given DirichletBCPointer_
  GENERIC, PUBLIC :: GetDirichletBCIndex_ => GetDirichletBCIndex1_, &
    GetDirichletBCIndex2_
  PROCEDURE, PRIVATE, PASS(obj) :: &
    GetTotalDirichletBCIndex1 => obj_GetTotalDirichletBCIndex1, &
    GetTotalDirichletBCIndex2 => obj_GetTotalDirichletBCIndex2
  !! Get the size of Dirichlet BC indices
  GENERIC, PUBLIC :: GetTotalDirichletBCIndex => &
    GetTotalDirichletBCIndex1, GetTotalDirichletBCIndex2
  !! Generic method to get total number of dirichlet boundary condition
  !! indices
  PROCEDURE, PRIVATE, NON_OVERRIDABLE, PASS(obj) :: &
    SetMaxTotalNodeNumForBC1 => obj_SetMaxTotalNodeNumForBC1, &
    SetMaxTotalNodeNumForBC2 => obj_SetMaxTotalNodeNumForBC2
  GENERIC, PUBLIC :: SetMaxTotalNodeNumForBC => &
    SetMaxTotalNodeNumForBC1, SetMaxTotalNodeNumForBC2
  !! Set maximum total node numbers for boundary conditions

  !@VTKMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
  !! Write the values in a VTK file (child class)
  PROCEDURE, PUBLIC, PASS(obj) :: WriteData_vtk => obj_WriteData_vtk1
  !! Write the mesh and the values in a VTK file
  PROCEDURE, NOPASS :: WriteData_vtk2 => &
    obj_WriteData_vtk2
  !! Write several node fields in a VTK file
  GENERIC, PUBLIC :: WriteData => WriteData_vtk2
  !! Write several node fields in a VTK file
  PROCEDURE, PRIVATE, PASS(obj) :: GetVtkUnstructuredGridData1 => &
    obj_GetVtkUnstructuredGridData1
  !! Get vtk data for unstructured grid
  PROCEDURE, PRIVATE, PASS(obj) :: GetVtkUnstructuredGridData2 => &
    obj_GetVtkUnstructuredGridData2
  !! Get vtk data for unstructured grid
  GENERIC, PUBLIC :: GetVtkUnstructuredGridData => &
    GetVtkUnstructuredGridData1, GetVtkUnstructuredGridData2
  !! VTK unstructured grid of Lagrange cells of the field

  ! @GetMethods, deferred methods from AbstractField_
  PROCEDURE, PUBLIC, PASS(obj) :: GetStorageFMT => obj_GetStorageFMT
  !! Get storage format of the field

  ! @GetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! Pointer to the vector of values (NATIVE_SERIAL engine only)
  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  !! Returns the length of data stored inside the fortran vector
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle
  !! Get single entry
  PROCEDURE, PRIVATE, PASS(obj) :: GetMultiple1 => obj_GetMultiple1
  !! get many values from indices
  PROCEDURE, PRIVATE, PASS(obj) :: GetMultiple2 => obj_GetMultiple2
  !! get many values in a range
  PROCEDURE, PRIVATE, PASS(obj) :: GetMultiple3 => obj_GetMultiple3
  !! get many values in a range into a range of value
  GENERIC, PUBLIC :: GetMultiple => GetMultiple1, GetMultiple2, &
    GetMultiple3
  !! get many values
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get Finite Element variable
  PROCEDURE, PUBLIC, PASS(obj) :: GetPhysicalNames => obj_GetPhysicalNames
  !! Get physical names
  PROCEDURE, PUBLIC, PASS(obj) :: GetTotalPhysicalVars => &
    obj_GetTotalPhysicalVars
  !! Get total physical variables
  PROCEDURE, PUBLIC, PASS(obj) :: GetSpaceCompo => obj_GetSpaceCompo
  !! Get the space components
  PROCEDURE, PUBLIC, PASS(obj) :: GetTimeCompo => obj_GetTimeCompo
  !! Get the time components

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetFromHDF5 => obj_SetFromHDF5
  !! Set values from hdf5
  PROCEDURE, PUBLIC, PASS(obj) :: SetLocal_n => obj_SetLocal_n
  !! Set the local size, local_n
  PROCEDURE, PUBLIC, PASS(obj) :: SetGlobal_n => obj_SetGlobal_n
  !! Set the global size, global_n
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  !! Set single entry
  PROCEDURE, PUBLIC, PASS(obj) :: SetAll => obj_SetAll
  !! Set all the values to a constant scalar value
  PROCEDURE, PRIVATE, PASS(obj) :: SetMultiple1 => obj_SetMultiple1
  !! Set multiple entries using indices
  PROCEDURE, PRIVATE, PASS(obj) :: SetMultiple2 => obj_SetMultiple2
  !! Set multiple entries using range
  PROCEDURE, PRIVATE, PASS(obj) :: SetMultiple3 => obj_SetMultiple3
  !! Set multiple entries in a range from a range of value
  PROCEDURE, PRIVATE, PASS(obj) :: SetMultiple4 => obj_SetMultiple4
  !! Set multiple entries in a range to one value
  GENERIC, PUBLIC :: SetMultiple => SetMultiple1, SetMultiple2, &
    SetMultiple3, SetMultiple4
  !! Set multiple entries

  ! @BlasMethods
  PROCEDURE, PRIVATE, PASS(obj) :: AXPY1 => obj_AXPY1
  !! obj = obj + scale * x
  PROCEDURE, PRIVATE, PASS(obj) :: AXPY2 => obj_AXPY2
  !! obj = obj + a1 * x1 + a2 * x2
  PROCEDURE, PRIVATE, PASS(obj) :: AXPY3 => obj_AXPY3
  !! obj = obj + a1 * x1 + a2 * x2 + a3 * x3
  GENERIC, PUBLIC :: AXPY => AXPY1, AXPY2, AXPY3
  !! Y = Y + scale * X ...
  PROCEDURE, PUBLIC, PASS(obj) :: SCAL => obj_SCAL
  !! X = scale * X
  PROCEDURE, PUBLIC, PASS(obj) :: COPY => obj_Copy
  !! Y = X
  PROCEDURE, PUBLIC, PASS(obj) :: Swap => obj_Swap
  !! Swap the values of obj and obj2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => obj_Norm2
  !! Returns the L2 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => obj_Norm1
  !! Returns the L1 norm
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => obj_Normi
  !! Returns the infinity norm
  PROCEDURE, PUBLIC, PASS(obj) :: Norm => obj_Norm
  !! Returns the L2, L1, LInfinity norm
  PROCEDURE, PUBLIC, PASS(obj) :: DOT_PRODUCT => obj_DOT_PRODUCT
  !! dot product
  PROCEDURE, PUBLIC, PASS(obj) :: PMUL => obj_PMUL
  !! z = x * y
  PROCEDURE, PUBLIC, PASS(obj) :: Reciprocal => obj_Reciprocal
  !! y = 1/x
END TYPE AbstractNodeField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: AbstractNodeFieldPointer_
  CLASS(AbstractNodeField_), POINTER :: ptr => NULL()
END TYPE AbstractNodeFieldPointer_

!----------------------------------------------------------------------------
!                                               CheckError@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-28
! summary: Check the DOF data of a node field
!
! Raises an error if dof_tPhysicalVars is zero, or if dof_spaceCompo,
! dof_timeCompo, dof_tNodes or dof_names_char is not allocated or its
! size is not dof_tPhysicalVars.
!

INTERFACE
  MODULE SUBROUTINE AbstractNodeFieldCheckError(obj)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
  END SUBROUTINE AbstractNodeFieldCheckError
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-25
! summary: Initiate a node field from another node field
!
! obj is deallocated, and then the data of AbstractField_ (name, engine,
! FEDOF pointers, ...) are copied from obj2 by Initiate of
! AbstractField_. If obj2 is a node field, its DOF data
! (dof_tPhysicalVars, dof_storageFMT, dof_spaceCompo, dof_timeCompo,
! dof_tNodes, dof_names_char, tSize) and its values are copied too.
! FEDOFs are always shared by pointers.
!
! copyFull, copyStructure and usePointer are passed to AbstractField_;
! they are not used at present.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x, y
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
! CALL y%Initiate(obj2=x)
! CALL OK(y%IsInitiated(), "Initiate: IsInitiated")
! CALL OK(y%Size() .EQ. x%Size(), "Initiate: Size")
! CALL OK(y%GetTotalPhysicalVars() .EQ. 1, "Initiate: physical vars")
! CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "Initiate: values")
!
! CALL y%DEALLOCATE()
! CALL OK(.NOT. y%IsInitiated(), "DEALLOCATE: not initiated")
! CALL OK(y%Size() .EQ. 0, "DEALLOCATE: Size = 0")
! CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "DEALLOCATE: x unchanged")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Deallocate]], [[obj_Copy]]

INTERFACE AbstractNodeFieldInitiate
  MODULE SUBROUTINE obj_Initiate1(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
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
END INTERFACE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary: Initiate a node field by passing arguments
!
! The data of AbstractField_ are set by Initiate of AbstractField_.
! Then the DOF data are set from the arguments: tPhysicalVarNames
! physical variables with the names physicalVarNames (first character),
! the space components spaceCompo, the time components timeCompo and
! the number of nodes tNodes. The DOF object and the vector of values
! (size tSize, set to zero) are made. If local_n or global_n is zero,
! it is set to tSize.
!
! In this routine all the optional DOF arguments are required: an
! error is raised if one of them is missing, or if the size of
! spaceCompo, timeCompo, physicalVarNames or tNodes is not
! tPhysicalVarNames, or if the size of the vector is not tSize. The
! child classes (e.g., ScalarField_) compute these arguments and call
! this routine.
!
! See also: [[ScalarField_:obj_Initiate2]] (contains an example)

INTERFACE AbstractNodeFieldInitiate
  MODULE SUBROUTINE obj_Initiate2(obj, name, engine, fieldType, storageFMT, &
                                  comm, local_n, global_n, spaceCompo, &
                                  isSpaceCompo, isSpaceCompoScalar, &
                                  timeCompo, isTimeCompo, isTimeCompoScalar, &
                                  tPhysicalVarNames, physicalVarNames, &
                                  isPhysicalVarNames, &
                                  isPhysicalVarNamesScalar, tNodes, &
                                  isTNodes, isTNodesScalar, tSize, fedof, &
                                  geofedof, timefedof)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), INTENT(IN), OPTIONAL :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), INTENT(IN), OPTIONAL :: storageFMT
    !! storage format
    INTEGER(I4B), INTENT(IN), OPTIONAL :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: spaceCompo(:)
    !! space components
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompo
    !! if true we will try to access spaceCompo
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeCompo(:)
    !! Time components
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompo
    !! if true we will try to access TimeCompo
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    CHARACTER(*), INTENT(IN), OPTIONAL :: physicalVarNames(:)
    !! Names of the physical variables
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isPhysicalVarNamesScalar
    !! if true then we physicalVarNames is scalar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tNodes(:)
    !! total number of nodes in each physical variable
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTNodes
    !! if true we will try to access tNodes
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTNodesScalar
    !! is tNodes scalar
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tSize
    !! total size of node field
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof, geofedof
    !! FEDOF object
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate2
END INTERFACE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-29
! summary: Initiate a node field with a FEDOF for each variable
!
! Initiate a node field with a FEDOF for each variable. The FEDOF is
! defined in fedofs which is a vector of FEDOFPointer_.
!
! Each fedofs has associated geodofs defined inside geofedos.
!
!@note
!   This routine is under development; it raises an error.
!@endnote
!
! See also: [[obj_Initiate2]]

INTERFACE AbstractNodeFieldInitiate
  MODULE SUBROUTINE obj_Initiate3(obj, name, engine, fieldType, comm, &
                                  local_n, global_n, spaceCompo, &
                                  isSpaceCompo, isSpaceCompoScalar, &
                                  timeCompo, isTimeCompo, isTimeCompoScalar, &
                                  tPhysicalVarNames, physicalVarNames, &
                                  isPhysicalVarNames, fedof, geofedof, &
                                  timefedof)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), INTENT(IN), OPTIONAL :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL
    !! following options are available
    !! FIELD_TYPE_NORMAL
    !! FIELD_TYPE_CONSTANT
    INTEGER(I4B), INTENT(IN), OPTIONAL :: comm
    !! communication group
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: local_n
    !! local size of field on each processor
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: global_n
    !! global size of field on distributed on processors
    !! Only needed for parallel environment
    INTEGER(I4B), INTENT(IN), OPTIONAL :: spaceCompo(:)
    !! space components
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompo
    !! if true we will try to access spaceCompo
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: timeCompo(:)
    !! Time components
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompo
    !! if true we will try to access TimeCompo
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    CHARACTER(*), INTENT(IN), OPTIONAL :: physicalVarNames(:)
    !! Names of the physical variables
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    TYPE(FEDOFPointer_), INTENT(IN) :: fedof(:), geofedof(:)
    !! FEDOFs of the physical variables and of the geometry
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedof(:)
    !! Vector of TimeFEDOFPointers
    !! All timefedofs should be initiated
  END SUBROUTINE obj_Initiate3
END INTERFACE AbstractNodeFieldInitiate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-10-21
! summary: Deallocate the data of a node field
!
! Deallocates the data of AbstractField_ and the DOF data, and resets
! the vector of values; after the call obj%IsInitiated() is false and
! obj%Size() is zero.
!
! See also: [[obj_Initiate1]] (contains an example)

INTERFACE AbstractNodeFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
  END SUBROUTINE obj_Deallocate
END INTERFACE AbstractNodeFieldDeallocate

!----------------------------------------------------------------------------
!                                                           Display@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-26
! summary: Display the content of a node field
!
! Displays the data of AbstractField_, the DOF data (dof_tPhysicalVars,
! dof_storageFMT, dof_spaceCompo, dof_timeCompo, dof_tNodes, tSize) and
! the values.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
! REAL(DFP) :: src(6)
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
! CALL x%Display("x: ")
! CALL OK(.TRUE., "Display")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE AbstractNodeFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    CHARACTER(*), INTENT(IN) :: msg
    !! message
    INTEGER(I4B), INTENT(IN), OPTIONAL :: unitNo
    !! unit number, default is stdout
  END SUBROUTINE obj_Display
END INTERFACE AbstractNodeFieldDisplay

!----------------------------------------------------------------------------
!                                                      ExportToVTK@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-07-29
! summary: Write the values of a node field in a VTK file
!
! It is called by [[obj_WriteData_vtk1]] after the mesh is written. It
! should be implemented by the child class; this routine raises an
! error.
!
! See also: [[ScalarField_:obj_ExportToVTK]]

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtk)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field object
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! vtkfile object
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                        WriteData@VTKMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-24
! summary: Write a node field in a VTK file
!
! 1. The mesh of fedof is written in vtk.
! 2. The point data section is opened, and the values are written by
!    ExportToVTK of the child class.
! 3. The point data section and the piece are closed, and the file is
!    closed.
!
! See also: [[obj_WriteData_vtk2]], [[obj_ExportToVTK]]

INTERFACE AbstractNodeFieldWriteData
  MODULE SUBROUTINE obj_WriteData_vtk1(obj, vtk)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! VTK file
  END SUBROUTINE obj_WriteData_vtk1
END INTERFACE AbstractNodeFieldWriteData

!----------------------------------------------------------------------------
!                                                        WriteData@VTKMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-21
! summary: Write several node fields in a VTK file
!
! The node fields obj(:)%ptr (at most 24, not associated pointers are
! skipped) must be initiated and have the same mesh. The mesh of the
! last field is written, and then the vertex values of all physical
! variables of all fields are written as point data. vtk must be open;
! it is not closed on return.
!
! See also: [[obj_WriteData_vtk1]]

INTERFACE AbstractNodeFieldWriteData
  MODULE SUBROUTINE obj_WriteData_vtk2(obj, vtk)
    CLASS(AbstractNodeFieldPointer_), INTENT(INOUT) :: obj(:)
    !! node fields
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! VTK file
  END SUBROUTINE obj_WriteData_vtk2
END INTERFACE AbstractNodeFieldWriteData

INTERFACE NodeFieldsWriteData
  MODULE PROCEDURE obj_WriteData_vtk2
END INTERFACE NodeFieldsWriteData

!----------------------------------------------------------------------------
!                                                    GetStorageFMT@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-01-07
! summary: Returns the storage format of a node field
!
! Returns TypeFieldOpt%storageFormatDOF: the values are stored by
! physical variables and components (DOF format). It is a deferred
! method of AbstractField_.
!
! Returns:
!   ans : INTEGER(I4B) - storage format
!
! See also: [[obj_GetPhysicalNames]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_GetStorageFMT(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B) :: ans
    !! storage format
  END FUNCTION obj_GetStorageFMT
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-07-20
! summary: Pointer to the vector of values
!
! Returns a pointer to the Fortran vector which stores the values of
! obj. It should be used only with the NATIVE_SERIAL engine. obj must
! have the TARGET attribute in the caller.
!
! Returns:
!   ans : REAL(DFP), POINTER - vector of values, SIZE(ans) = obj%Size()
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_), TARGET :: x
! REAL(DFP), POINTER :: ptr(:)
! REAL(DFP) :: src(6), val
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
! ptr => x%GetPointer()
! CALL OK(ASSOCIATED(ptr), "GetPointer: associated")
! CALL OK(SIZE(ptr) .EQ. 6, "GetPointer: size")
! CALL OK(ALL(ABS(ptr - src) .LT. tol), "GetPointer: values")
!
! ptr(2) = -2.0_DFP
! CALL x%GetSingle(indx=2, VALUE=val)
! CALL OK(ABS(val + 2.0_DFP) .LT. tol, "GetPointer: change values")
! NULLIFY (ptr)
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(AbstractNodeField_), TARGET, INTENT(IN) :: obj
    !! node field
    REAL(DFP), POINTER :: ans(:)
    !! pointer to the values
  END FUNCTION obj_GetPointer
END INTERFACE

INTERFACE AbstractNodeFieldGetPointer
  MODULE PROCEDURE obj_GetPointer
END INTERFACE AbstractNodeFieldGetPointer

!----------------------------------------------------------------------------
!                                                         SetParam@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-10-25
! summary: Set the DOF data of a node field
!
! Each present argument is copied into the matching component of obj;
! the DOF object and the vector of values are not changed.
!

INTERFACE
  MODULE SUBROUTINE obj_SetParam(obj, dof_tPhysicalVars, dof_storageFMT, &
                                 dof_spaceCompo, dof_timeCompo, dof_tNodes, &
                                 dof_names_char, tSize)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dof_tPhysicalVars
    !! total number of physical variables
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dof_storageFMT
    !! Storage pattern, FMT_DOF or FMT_NODES
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dof_spaceCompo(:)
    !! Space components of each physical variable
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dof_timeCompo(:)
    !! Time components of each physical variable
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dof_tNodes(:)
    !! Total number of nodes in each physical variable
    CHARACTER(*), INTENT(IN), OPTIONAL :: dof_names_char(:)
    !! single character name of each physical varible
    INTEGER(I4B), INTENT(IN), OPTIONAL :: tSize
    !! Total size of the field
  END SUBROUTINE obj_SetParam
END INTERFACE

INTERFACE AbstractNodeFieldSetParam
  MODULE PROCEDURE obj_SetParam
END INTERFACE AbstractNodeFieldSetParam

!----------------------------------------------------------------------------
!                                                       SetLocal_n@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Set the local size of a node field
!
! local_n is the number of entries of obj stored on this process.
! [[obj_Initiate2]] sets it to tSize when it is zero. SIZE of a scalar
! field returns local_n.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL OK(x%local_n .EQ. 6, "after Initiate: local_n = 6")
! CALL OK(x%global_n .EQ. 6, "after Initiate: global_n = 6")
!
! CALL x%SetLocal_n(local_n=4)
! CALL OK(x%local_n .EQ. 4, "SetLocal_n: local_n = 4")
! CALL OK(x%SIZE() .EQ. 4, "SetLocal_n: SIZE = 4")
! CALL OK(x%global_n .EQ. 6, "SetLocal_n: global_n unchanged")
!
! CALL x%SetGlobal_n(global_n=10)
! CALL OK(x%global_n .EQ. 10, "SetGlobal_n: global_n = 10")
! CALL OK(x%local_n .EQ. 4, "SetGlobal_n: local_n unchanged")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_SetGlobal_n]]

INTERFACE
  MODULE SUBROUTINE obj_SetLocal_n(obj, local_n)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: local_n
    !! local size
  END SUBROUTINE obj_SetLocal_n
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetGlobal_n@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-11-11
! summary: Set the global size of a node field
!
! global_n is the number of entries of obj on all processes.
! [[obj_Initiate2]] sets it to tSize when it is zero.
!
! Full example: src/modules/AbstractNodeField/examples/SetLocal_n_test_1.F90
!
! See also: [[obj_SetLocal_n]]

INTERFACE
  MODULE SUBROUTINE obj_SetGlobal_n(obj, global_n)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: global_n
    !! global size
  END SUBROUTINE obj_SetGlobal_n
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetFromHDF5@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-11
! summary: Set the values from an HDF5 file
!
! The vector of values is read from the dataset
! group/RealVectorR0/realVec (written by [[obj_Export]]). An error is
! raised (in debug mode) if hdf5 is not initiated, not open, not
! readable, or if the dataset does not exist. The other data of obj
! are not changed.
!
! See also: [[obj_Import]], [[obj_Export]]

INTERFACE
  MODULE SUBROUTINE obj_SetFromHDF5(obj, hdf5, group)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_SetFromHDF5
END INTERFACE

!----------------------------------------------------------------------------
!                                                        SetSingle@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set a single value
!
! The value with index indx becomes scale * VALUE, or scale * VALUE is
! added to it if addContribution is true.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
! REAL(DFP) :: val
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! CALL x%SetAll(VALUE=1.0_DFP)
! CALL x%GetSingle(indx=6, VALUE=val)
! CALL OK(ABS(val - 1.0_DFP) .LT. tol, "SetAll")
!
! CALL x%SetAll(VALUE=2.0_DFP, scale=0.5_DFP, addContribution=math%yes)
! CALL x%GetSingle(indx=1, VALUE=val)
! CALL OK(ABS(val - 2.0_DFP) .LT. tol, "SetAll: add 0.5 * 2")
!
! CALL x%SetSingle(indx=3, VALUE=4.0_DFP, scale=2.0_DFP)
! CALL x%GetSingle(indx=3, VALUE=val)
! CALL OK(ABS(val - 8.0_DFP) .LT. tol, "SetSingle: scale")
!
! CALL x%SetSingle(indx=3, VALUE=1.0_DFP, addContribution=math%yes)
! CALL x%GetSingle(indx=3, VALUE=val)
! CALL OK(ABS(val - 9.0_DFP) .LT. tol, "SetSingle: add")
!
! CALL x%GetSingle(indx=4, VALUE=val)
! CALL OK(ABS(val - 2.0_DFP) .LT. tol, "other entries unchanged")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetSingle]], [[obj_SetAll]], [[obj_SetMultiple1]]

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: indx
    !! index of the value
    REAL(DFP), INTENT(IN) :: VALUE
    !! value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_SetSingle
END INTERFACE

INTERFACE AbstractNodeFieldSetSingle
  MODULE PROCEDURE obj_SetSingle
END INTERFACE AbstractNodeFieldSetSingle

!----------------------------------------------------------------------------
!                                                           SetAll@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-03
! summary: Set all the values
!
! All values become scale * VALUE, or scale * VALUE is added to them
! if addContribution is true.
!
! See also: [[obj_SetSingle]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set or add
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set multiple values by indices
!
! The value with index indx(i) is set from VALUE(i). If addContribution
! is true, then scale * VALUE(i) is added.
!
! SetMultiple can also use a range istart:iend:stride
! ([[obj_SetMultiple2]], [[obj_SetMultiple4]]) and a range of VALUE
! ([[obj_SetMultiple3]]); GetMultiple reads values in the same ways.
! The example shows all of them.
!
! @note
!   Without addContribution, the values are set to scale * VALUE.
! @endnote
!
! Changes:
!   - 2026-10-29: without addContribution, scale is applied (obj = scale *
!     VALUE), as in SetSingle and SetAll; before, scale was not used
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
! REAL(DFP) :: found(6), src(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetAll(VALUE=0.0_DFP)
!
! ! indices
! CALL x%SetMultiple(indx=[2, 4], VALUE=[20.0_DFP, 40.0_DFP])
! CALL x%GetMultiple(indx=[4, 2], VALUE=found, tsize=tsize)
! CALL OK(tsize .EQ. 2, "GetMultiple indx: tsize")
! CALL OK(ALL(ABS(found(1:2) - [40.0_DFP, 20.0_DFP]) .LT. tol), &
!         "SetMultiple indx")
!
! CALL x%SetMultiple(indx=[2, 4], VALUE=[1.0_DFP, 1.0_DFP], &
!                    scale=2.0_DFP, addContribution=math%yes)
! CALL x%GetMultiple(indx=[2, 4], VALUE=found, tsize=tsize)
! CALL OK(ALL(ABS(found(1:2) - [22.0_DFP, 42.0_DFP]) .LT. tol), &
!         "SetMultiple indx: add 2 * value")
!
! ! range, vector value
! CALL x%SetMultiple(istart=1, iend=5, stride=2, &
!                    VALUE=[1.0_DFP, 3.0_DFP, 5.0_DFP])
! CALL x%GetMultiple(istart=1, iend=5, stride=2, VALUE=found, &
!                    tsize=tsize)
! CALL OK(tsize .EQ. 3, "GetMultiple range: tsize")
! CALL OK(ALL(ABS(found(1:3) - [1.0_DFP, 3.0_DFP, 5.0_DFP]) .LT. tol), &
!         "SetMultiple range")
!
! ! range, scalar value
! CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=0.0_DFP)
! CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - [1.0_DFP, 0.0_DFP, 3.0_DFP, 0.0_DFP, &
!                          5.0_DFP, 0.0_DFP]) .LT. tol), &
!         "SetMultiple range: one value")
!
! ! range, range of value: x(2:6:2) = src(1:3)
! CALL x%SetMultiple(istart=2, iend=6, stride=2, VALUE=src, &
!                    istart_value=1, iend_value=3, stride_value=1)
! CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - [1.0_DFP, 1.0_DFP, 3.0_DFP, 2.0_DFP, &
!                          5.0_DFP, 3.0_DFP]) .LT. tol), &
!         "SetMultiple range: range of value")
!
! ! range, range of value: found(4:6) = x(1:3)
! found = 0.0_DFP
! CALL x%GetMultiple(istart=1, iend=3, stride=1, VALUE=found, &
!                    istart_value=4, iend_value=6, stride_value=1, &
!                    tsize=tsize)
! CALL OK(tsize .EQ. 3, "GetMultiple range of value: tsize")
! CALL OK(ALL(ABS(found(4:6) - [1.0_DFP, 1.0_DFP, 3.0_DFP]) .LT. tol), &
!         "GetMultiple range of value")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetMultiple1]], [[obj_SetSingle]]

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple1(obj, indx, VALUE, scale, addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! index, size(indx) = size(value) = tsize
    !! SIZE(indx) = SIZE(VALUE)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values which will be use din obj=value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set multiple values in a range
!
! The values with indices istart:iend:stride are set from VALUE (in
! order). If addContribution is true, then scale * VALUE is added;
! otherwise the values are set to scale * VALUE.
!
! Changes:
!   - 2026-10-29: without addContribution, scale is applied (obj = scale *
!     VALUE), as in SetSingle and SetAll; before, scale was not used
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple2(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! obj = value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-02
! summary: Set multiple values in a range from a range of VALUE
!
! obj(istart:iend:stride) is set from VALUE(istart_value::stride_value)
! (the number of values is given by the range of obj). If
! addContribution is true, then scale times these values is added;
! otherwise the values are set to scale times these values.
!
! Changes:
!   - 2026-10-29: without addContribution, scale is applied (obj = scale *
!     VALUE), as in SetSingle and SetAll; before, scale was not used
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     scale, addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of values
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetMultiple@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Set multiple values in a range to one value
!
! The values with indices istart:iend:stride are set to scale * VALUE.
! If addContribution is true, then scale * VALUE is added.
!
! Changes:
!   - 2026-10-29: without addContribution, scale is applied (obj = scale *
!     VALUE), as in SetSingle and SetAll; before, scale was not used
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple4(obj, istart, iend, stride, VALUE, &
                                     scale, addContribution)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(IN) :: VALUE
    !! obj = value
    REAL(DFP), INTENT(IN), OPTIONAL :: scale
    !! scale
    LOGICAL(LGT), INTENT(IN), OPTIONAL :: addContribution
    !! add or set
  END SUBROUTINE obj_SetMultiple4
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get multiple values by indices
!
! VALUE(i) is the value with index indx(i); tsize = SIZE(indx).
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple1(obj, indx, VALUE, tsize)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    !! SIZE(VALUE) >= SIZE(indx)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get multiple values in a range
!
! VALUE(1:tsize) = obj(istart:iend:stride), where tsize is the number
! of indices of the range.
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple2(obj, istart, iend, stride, VALUE, tsize)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetMultiple@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2024-06-02
! summary: Get multiple values in a range into a range of VALUE
!
! VALUE(istart_value::stride_value) = obj(istart:iend:stride); tsize
! is the number of indices of the range of obj.
!
! See also: [[obj_SetMultiple1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple3(obj, istart, iend, stride, VALUE, &
                                     istart_value, iend_value, stride_value, &
                                     tsize)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(IN) :: istart_value, iend_value, stride_value
    !! range of values
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple3
END INTERFACE

!----------------------------------------------------------------------------
!                                                        GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get a single value
!
! See also: [[obj_SetSingle]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: indx
    !! index of the value
    REAL(DFP), INTENT(OUT) :: VALUE
    !! value
  END SUBROUTINE obj_GetSingle
END INTERFACE

INTERFACE AbstractNodeFieldGetSingle
  MODULE PROCEDURE obj_GetSingle
END INTERFACE AbstractNodeFieldGetSingle

!----------------------------------------------------------------------------
!                                                    GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-03-28
! summary: Get the values of nodes in an FEVariable
!
! It should be implemented by the child class; this routine raises an
! error.
!
! See also: [[ScalarField_:obj_GetFeVariable]]

INTERFACE
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! abstract node field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! value to be returned
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivar
    !! physical variable nubmer
  END SUBROUTINE obj_GetFeVariable
END INTERFACE

INTERFACE AbstractNodeFieldGetFEVariable
  MODULE PROCEDURE obj_GetFeVariable
END INTERFACE AbstractNodeFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                 GetPhysicalNames@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Returns the names of the physical variables
!
! ans(i) is the one character name of physical variable i. SIZE(ans)
! must be the number of physical variables (GetTotalPhysicalVars); an
! error is raised (in debug mode) otherwise, or if the names are not
! allocated.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE FieldOpt_Class, ONLY: TypeFieldOpt
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
! CHARACTER(1), ALLOCATABLE :: names(:)
! INTEGER(I4B) :: tvar
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! tvar = x%GetTotalPhysicalVars()
! CALL OK(tvar .EQ. 1, "GetTotalPhysicalVars")
! ALLOCATE (names(tvar))
! CALL x%GetPhysicalNames(names)
! CALL OK(names(1) .EQ. "u", "GetPhysicalNames")
! CALL OK(ALL(x%GetSpaceCompo(tvar) .EQ. [1]), "GetSpaceCompo")
! CALL OK(ALL(x%GetTimeCompo(tvar) .EQ. [1]), "GetTimeCompo")
! CALL OK(x%GetStorageFMT() .EQ. TypeFieldOpt%storageFormatDOF, &
!         "GetStorageFMT")
! CALL OK(x%Size() .EQ. 6, "Size")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_GetTotalPhysicalVars]], [[obj_GetSpaceCompo]],
! [[obj_GetTimeCompo]]

INTERFACE
  MODULE SUBROUTINE obj_GetPhysicalNames(obj, ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    CHARACTER(1), INTENT(INOUT) :: ans(:)
    !! names of physical variables
  END SUBROUTINE obj_GetPhysicalNames
END INTERFACE

!----------------------------------------------------------------------------
!                                             GetTotalPhysicalVars@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-10-03
! summary: Returns the number of physical variables
!
! An error is raised (in debug mode) if the names of the physical
! variables are not allocated, or if their number differs.
!
! Returns:
!   ans : INTEGER(I4B) - number of physical variables
!
! See also: [[obj_GetPhysicalNames]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_GetTotalPhysicalVars(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B) :: ans
    !! number of physical variables
  END FUNCTION obj_GetTotalPhysicalVars
END INTERFACE

!----------------------------------------------------------------------------
!                                                    GetSpaceCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Returns the space components of the physical variables
!
! Returns the space components of the physical variables, one entry for each
! variable (SIZE(ans) = tPhysicalVars).
!
! Returns:
!   ans : INTEGER(I4B) - space components, SIZE(ans) = tPhysicalVars
!
! See also: [[obj_GetPhysicalNames]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_GetSpaceCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    !! Total number of physical variables
    !! This can be obtained from GetTotalPhysicalVars method
    INTEGER(I4B) :: ans(tPhysicalVars)
    !! space components
  END FUNCTION obj_GetSpaceCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetTimeCompo@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-09-22
! summary: Returns the time components of the physical variables
!
! Returns the time components of the physical variables, one entry for each
! variable (SIZE(ans) = tPhysicalVars).
!
! Returns:
!   ans : INTEGER(I4B) - time components, SIZE(ans) = tPhysicalVars
!
! See also: [[obj_GetPhysicalNames]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_GetTimeCompo(obj, tPhysicalVars) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: tPhysicalVars
    !! number of physical variables
    INTEGER(I4B) :: ans(tPhysicalVars)
    !! time components
  END FUNCTION obj_GetTimeCompo
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Size@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2021-09-25
! summary: Returns the number of values of a node field
!
! Returns tSize, the total number of values.
!
! Returns:
!   ans : INTEGER(I4B) - number of values
!
! See also: [[obj_GetPhysicalNames]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN), OPTIONAL :: dims
    !! not used
    INTEGER(I4B) :: ans
    !! number of values
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                            AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: obj = obj + scale * x
!
! obj and x must have the same size.
!
! AXPY can also add two or three fields ([[obj_AXPY2]],
! [[obj_AXPY3]]); the example shows all of them.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x, y, z, w
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL y%Initiate(name="y", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
! CALL z%Initiate(name="z", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
! CALL w%Initiate(name="w", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL y%SetAll(VALUE=1.0_DFP)
! CALL z%SetAll(VALUE=10.0_DFP)
!
! ! w = 0 + 2 * x
! CALL w%SetAll(VALUE=0.0_DFP)
! CALL w%AXPY(x=x, scale=2.0_DFP)
! CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - 2.0_DFP * src) .LT. tol), "AXPY: w + s * x")
!
! ! w = 2 x + x - y
! CALL w%AXPY(x1=x, x2=y, a1=1.0_DFP, a2=-1.0_DFP)
! CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - (3.0_DFP * src - 1.0_DFP)) .LT. tol), &
!         "AXPY: w + a1 * x1 + a2 * x2")
!
! ! w = 3 x - y + y + 0.1 z - x
! CALL w%AXPY(x1=y, x2=z, x3=x, a1=1.0_DFP, a2=0.1_DFP, a3=-1.0_DFP)
! CALL w%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - (2.0_DFP * src + 1.0_DFP)) .LT. tol), &
!         "AXPY: w + a1 * x1 + a2 * x2 + a3 * x3")
!
! CALL w%DEALLOCATE()
! CALL z%DEALLOCATE()
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_AXPY2]], [[obj_AXPY3]], [[obj_SCAL]]

INTERFACE
  MODULE SUBROUTINE obj_AXPY1(obj, x, scale)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x
    !! node field x
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_AXPY1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-29
! summary: obj = obj + a1 * x1 + a2 * x2
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_AXPY2(obj, x1, x2, a1, a2)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    !! node field x1
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    !! node field x2
    REAL(DFP), INTENT(IN) :: a1
    !! scale of x1
    REAL(DFP), INTENT(IN) :: a2
    !! scale of x2
  END SUBROUTINE obj_AXPY2
END INTERFACE

!----------------------------------------------------------------------------
!                                                            AXPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-29
! summary: obj = obj + a1 * x1 + a2 * x2 + a3 * x3
!
! See also: [[obj_AXPY1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_AXPY3(obj, x1, x2, x3, a1, a2, a3)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x1
    !! node field x1
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x2
    !! node field x2
    CLASS(AbstractNodeField_), INTENT(INOUT) :: x3
    !! node field x3
    REAL(DFP), INTENT(IN) :: a1
    !! scale of x1
    REAL(DFP), INTENT(IN) :: a2
    !! scale of x2
    REAL(DFP), INTENT(IN) :: a3
    !! scale of x3
  END SUBROUTINE obj_AXPY3
END INTERFACE

!----------------------------------------------------------------------------
!                                                            SCAL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: obj = scale * obj
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
! CALL x%SCAL(scale=-0.5_DFP)
! CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found + 0.5_DFP * src) .LT. tol), "SCAL")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_AXPY1]]

INTERFACE
  MODULE SUBROUTINE obj_SCAL(obj, scale)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    REAL(DFP), INTENT(IN) :: scale
    !! scale
  END SUBROUTINE obj_SCAL
END INTERFACE

!----------------------------------------------------------------------------
!                                                            COPY@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-17
! summary: Copy the values: obj = obj2
!
! The values of obj2 are copied into obj by the BLAS1 COPY routine.
! Only the values are copied; both fields must have the same size.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x, y
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL y%Initiate(name="y", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL y%SetAll(VALUE=0.0_DFP)
!
! CALL y%COPY(obj2=x)
! CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "COPY: y = x")
! CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "COPY: x unchanged")
!
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Swap]], [[obj_Initiate1]]

INTERFACE
  MODULE SUBROUTINE obj_Copy(obj, obj2)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! other node field
  END SUBROUTINE obj_Copy
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Swap@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-22
! summary: Swap the values of two node fields, obj <=> obj2
!
! Exchange the nodal values stored in realVec of obj and obj2 by
! calling the BLAS1 SWAP routine of RealVector_. Only the values are
! exchanged; the other components (name, fedof, dof data, ...) of
! obj and obj2 are not changed.
!
! Both fields must be initiated and have the same size. In debug mode
! an error is raised when obj%Size() and obj2%Size() differ.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x, y
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL y%Initiate(name="y", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL y%SetAll(VALUE=-1.0_DFP)
!
! CALL x%Swap(obj2=y)
! CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found + 1.0_DFP) .LT. tol), "Swap: x = old y")
! CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "Swap: y = old x")
!
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Copy]], [[obj_AXPY1]]

INTERFACE
  MODULE SUBROUTINE obj_Swap(obj, obj2)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! first node field, on return it contains the values of obj2
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj2
    !! second node field, on return it contains the values of obj
  END SUBROUTINE obj_Swap
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Norm2@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Returns the Euclidean norm of the values
!
! Returns SQRT(SUM(obj**2)), taken over all the values of the field.
!
! Returns:
!   ans : REAL(DFP) - SQRT(SUM(obj**2))
!
! See also: [[obj_Norm]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Norm2(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    REAL(DFP) :: ans
    !! Euclidean norm
  END FUNCTION obj_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Norm1@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Returns the sum of absolute values
!
! Returns SUM(ABS(obj)), taken over all the values of the field.
!
! Returns:
!   ans : REAL(DFP) - SUM(ABS(obj))
!
! See also: [[obj_Norm]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Norm1(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    REAL(DFP) :: ans
    !! sum of absolute values
  END FUNCTION obj_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Normi@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Returns the infinity norm of the values
!
! Returns MAXVAL(ABS(obj)), taken over all the values of the field.
!
! Returns:
!   ans : REAL(DFP) - MAXVAL(ABS(obj))
!
! See also: [[obj_Norm]] (contains an example)

INTERFACE
  MODULE FUNCTION obj_Normi(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    REAL(DFP) :: ans
    !! largest absolute value
  END FUNCTION obj_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Norm@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2022-01-24
! summary: Returns the L1, L2 or infinity norm of the values
!
! normType selects the norm: TypeConvergenceOpt%normL1 ([[obj_Norm1]]),
! normL2 ([[obj_Norm2]]) or normInfinity ([[obj_Normi]]). An error is
! raised (in debug mode) for other values.
!
! Returns:
!   ans : REAL(DFP) - norm
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeConvergenceOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
! REAL(DFP) :: src(6)
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, -2.0_DFP, 3.0_DFP, -4.0_DFP, 5.0_DFP, -6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
! CALL OK(ABS(x%Norm1() - 21.0_DFP) .LT. tol, "Norm1")
! CALL OK(ABS(x%Norm2() - SQRT(91.0_DFP)) .LT. tol, "Norm2")
! CALL OK(ABS(x%Normi() - 6.0_DFP) .LT. tol, "Normi")
! CALL OK(ABS(x%Norm(TypeConvergenceOpt%normL1) - 21.0_DFP) .LT. tol, &
!         "Norm: L1")
! CALL OK(ABS(x%Norm(TypeConvergenceOpt%normL2) - SQRT(91.0_DFP)) &
!         .LT. tol, "Norm: L2")
! CALL OK(ABS(x%Norm(TypeConvergenceOpt%normInfinity) - 6.0_DFP) &
!         .LT. tol, "Norm: infinity")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Norm1]], [[obj_Norm2]], [[obj_Normi]]

INTERFACE
  MODULE FUNCTION obj_Norm(obj, normType) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    INTEGER(I4B), INTENT(IN) :: normType
    !! normType is defined in TypeConvergenceOpt in BaseType
    REAL(DFP) :: ans
    !! norm
  END FUNCTION obj_Norm
END INTERFACE

!----------------------------------------------------------------------------
!                                                     DOT_PRODUCT@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary: Returns the dot product of two node fields
!
! Returns SUM(obj * obj2), taken over all the values of the field.
!
! Returns:
!   ans : REAL(DFP) - SUM(obj * obj2)
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x, y
! REAL(DFP) :: src(6)
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL y%Initiate(name="y", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL y%SetAll(VALUE=2.0_DFP)
!
! CALL OK(ABS(x%DOT_PRODUCT(y) - 42.0_DFP) .LT. tol, "DOT_PRODUCT")
! CALL OK(ABS(x%DOT_PRODUCT(x) - 91.0_DFP) .LT. tol, &
!         "DOT_PRODUCT: x . x")
!
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE FUNCTION obj_DOT_PRODUCT(obj, obj2) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! node field
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! other node field
    REAL(DFP) :: ans
    !! dot product
  END FUNCTION obj_DOT_PRODUCT
END INTERFACE

!----------------------------------------------------------------------------
!                                                            PMUL@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary: Pointwise product: obj = obj1 * obj2
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x, y, z
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL y%Initiate(name="y", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
! CALL z%Initiate(name="z", engine="NATIVE_SERIAL", fedof=fedof, &
!                 geofedof=geofedof)
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL y%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
! CALL z%SetAll(VALUE=0.0_DFP)
!
! CALL z%PMUL(obj1=x, obj2=y)
! CALL z%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - src**2) .LT. tol), "PMUL: z = x * y")
!
! CALL z%DEALLOCATE()
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_PMUL(obj, obj1, obj2)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field, obj = obj1 * obj2
    CLASS(AbstractNodeField_), INTENT(IN) :: obj1
    !! first node field
    CLASS(AbstractNodeField_), INTENT(IN) :: obj2
    !! second node field
  END SUBROUTINE obj_PMUL
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Reciprocal@BlasMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-12-27
! summary: obj = 1 / obj
!
! Each value is replaced by its reciprocal. The values must not be
! zero.
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! TYPE(ScalarField_) :: x
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
! CALL x%Reciprocal()
! CALL x%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - 1.0_DFP / src) .LT. tol), "Reciprocal")
!
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```

INTERFACE
  MODULE SUBROUTINE obj_Reciprocal(obj)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field, obj = 1 / obj
  END SUBROUTINE obj_Reciprocal
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-24
! summary: Import a node field from an HDF5 file
!
! The data of AbstractField_ are read by Import of AbstractField_ (the
! FEDOFs are given as arguments). Then the DOF data and the values are
! read from the datasets written by [[obj_Export]]: IntR0/
! dof_tPhysicalVars, IntR0/dof_storageFMT, IntR0/tSize,
! IntR1/dof_spaceCompo, IntR1/dof_timeCompo, IntR1/dof_tNodes,
! StringR1/dof_names_char and RealVectorR0/realVec. An error is raised
! (in debug mode) if a dataset is missing.
!
! See also: [[obj_Export]] (contains an example), [[obj_SetFromHDF5]]

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
    CLASS(FEDOF_), TARGET, INTENT(IN), OPTIONAL :: fedof, geofedof
    !! FEDOFs of the field and of the geometry
    TYPE(FEDOFPointer_), INTENT(IN), OPTIONAL :: fedofs(:), geofedofs(:)
    !! FEDOFs of the physical variables
    CLASS(TimeFEDOF_), TARGET, INTENT(IN), OPTIONAL :: timefedof
    !! FEDOF in time
    TYPE(TimeFEDOFPointer_), INTENT(IN), OPTIONAL :: timefedofs(:)
    !! FEDOFs in time of the physical variables
  END SUBROUTINE obj_Import
END INTERFACE

INTERFACE AbstractNodeFieldImport
  MODULE PROCEDURE obj_Import
END INTERFACE AbstractNodeFieldImport

!----------------------------------------------------------------------------
!                                                           Export@HDFMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-24
! summary: Export a node field to an HDF5 file
!
! Nothing is done if obj is not initiated. Otherwise the data of
! AbstractField_ are written by Export of AbstractField_, and the DOF
! data and the values are written in group (see [[obj_Import]] for the
! names of the datasets).
!
! Examples
!
!```fortran
! PROGRAM main
! USE BaseType, ONLY: math => TypeMathOpt
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE HDF5File_Class, ONLY: HDF5File_
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
!
! CHARACTER(*), PARAMETER :: tomlFileName = "AbstractNodeField_test.toml"
! REAL(DFP), PARAMETER :: tol = 1.0E-12_DFP
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_), TARGET :: fedof, geofedof
! CHARACTER(*), PARAMETER :: filename = "Export_test_1.h5"
! TYPE(ScalarField_) :: x, y
! TYPE(HDF5File_) :: h5
! REAL(DFP) :: src(6), found(6)
! INTEGER(I4B) :: tsize
!
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
! CALL x%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
!
! src = [1.0_DFP, 2.0_DFP, 3.0_DFP, 4.0_DFP, 5.0_DFP, 6.0_DFP]
! CALL x%SetMultiple(istart=1, iend=6, stride=1, VALUE=src)
!
! CALL h5%Initiate(filename=filename, mode="NEW")
! CALL h5%OPEN()
! CALL x%Export(hdf5=h5, group="/x")
! CALL h5%DEALLOCATE()
!
! CALL h5%Initiate(filename=filename, mode="READ")
! CALL h5%OPEN()
! CALL y%Import(hdf5=h5, group="/x", fedof=fedof, geofedof=geofedof)
! CALL h5%DEALLOCATE()
!
! CALL OK(y%IsInitiated(), "Import: IsInitiated")
! CALL OK(y%Size() .EQ. 6, "Import: Size")
! CALL OK(y%GetTotalPhysicalVars() .EQ. 1, "Import: physical vars")
! CALL y%GetMultiple(istart=1, iend=6, stride=1, VALUE=found, &
!                    tsize=tsize)
! CALL OK(ALL(ABS(found - src) .LT. tol), "Import: values")
!
! CALL y%DEALLOCATE()
! CALL x%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
!```
!
! See also: [[obj_Import]]

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! node field
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    !! HDF5 file
    CHARACTER(*), INTENT(IN) :: group
    !! group in the HDF5 file
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE AbstractNodeFieldExport
  MODULE PROCEDURE obj_Export
END INTERFACE AbstractNodeFieldExport

!----------------------------------------------------------------------------
!                                       GetVtkUnstructuredGridData@VTKMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu and Vikas Sharma
! date: 2026-10-24
! summary: VTK unstructured grid of Lagrange cells of the field
!
! For each active element of the mesh of obj%fedof:
!
! 1. The cell order \(p\) is obtained from fedof%GetCellOrder.
! 2. The VTK cell type and node ordering for the topology and order
!    are obtained from GetVTKElementTypeFromTopology_ (easifemBase),
!    e.g., VTK_QUADRATIC_TRIANGLE for a triangle with \(p=2\), and
!    VTK_LAGRANGE_QUADRILATERAL for a quadrangle with \(p \ge 3\).
! 3. The equidistance interpolation points of order \(p\) are
!    computed in the reference element and mapped to the physical
!    element by using the shape functions of obj%geofedof.
!
! The points are not shared between cells (discontinuous grid), so the
! number of points is fedof%GetSumElemTotalDOF().
! [[ScalarField_:obj_GetVtkUnstructuredValue1]] uses the same loop, so
! its values match the points computed here. The arrays are
! reallocated; the work arrays of
! [[obj_GetVtkUnstructuredGridData2]] are allocated here. For a block
! node field, ivar selects the FEDOFs obj%fedofs(ivar) and
! obj%geofedofs(ivar).
!
! An error is raised if a VTK cell type is not available for a
! topology and order (e.g., triangle with \(p \ge 3\)).
!
! Examples
!
!```fortran
! PROGRAM main
! USE AbstractMesh_Class, ONLY: AbstractMesh_
! USE ApproxUtility, ONLY: SoftEq
! USE BaseType, ONLY: math => TypeMathOpt
! USE BaseType, ONLY: TypeFEVariableOpt
! USE BaseType, ONLY: InterfaceScalarSubroutine
! USE Display_Method, ONLY: Display
! USE ExceptionHandler_Class, ONLY: e
! USE ExceptionHandler_Class, ONLY: EXCEPTION_DEBUG
! USE ExceptionHandler_Class, ONLY: EXCEPTION_INFORMATION
! USE FEDOF_Class, ONLY: FEDOF_
! USE FEDomain_Class, ONLY: FEDomain_
! USE GlobalData, ONLY: DFP
! USE GlobalData, ONLY: I4B
! USE GlobalData, ONLY: INT8
! USE ScalarField_Class, ONLY: ScalarField_
! USE Test_Method, ONLY: OK
! IMPLICIT NONE
! CHARACTER(*), PARAMETER :: tomlFileName = &
!   "GetVtkUnstructuredGridData_test_1.toml"
! TYPE(FEDomain_) :: dom
! TYPE(FEDOF_) :: fedof, geofedof
! TYPE(ScalarField_) :: u
! CLASS(AbstractMesh_), POINTER :: mesh
! INTEGER(I4B), ALLOCATABLE :: connectivity(:), offsets(:)
! INTEGER(INT8), ALLOCATABLE :: types(:)
! REAL(DFP), ALLOCATABLE :: points(:, :)
!
! CALL e%SetQuietMode(EXCEPTION_DEBUG, math%yes)
! CALL e%SetQuietMode(EXCEPTION_INFORMATION, math%yes)
!
! ! import domain
! CALL dom%ImportFromToml(filename=tomlFileName, tomlName="domain")
!
! ! import u
! CALL u%ImportFromToml(fedof=fedof, geofedof=geofedof, dom=dom, &
!                       filename=tomlFileName, tomlName="u")
! CALL u%Set(2.0_DFP)
!
! ! Get grid connectivity
! CALL u%GetVtkUnstructuredGridData(points=points, &
!                                   connectivity=connectivity, &
!                                   offsets=offsets, types=types)
!
! CALL OK(ALLOCATED(points), "points allocated:")
! CALL OK(ALL(SHAPE(points) .EQ. [3, 32]), "shape of points:")
! CALL OK(ALLOCATED(connectivity), "connectivity allocated:")
! CALL OK(SIZE(connectivity) .EQ. 32, "shape of connectivity:")
! CALL OK(ALLOCATED(offsets), "offsets allocated:")
! CALL OK(SIZE(offsets) .EQ. 3, "size of offsets:")
! CALL OK(ALLOCATED(types), "types allocated:")
! CALL OK(SIZE(types) .EQ. 2, "size of types:")
!
! BLOCK
!   INTEGER(I4B) :: found(16), want(16)
!
!   IF (ALLOCATED(connectivity)) THEN
!     found = connectivity(1:16)
!     want = [1, 2, 3, 4, 5, 6, 7, 8, 10, 9, 12, 11, 13, 14, 16, 15] &
!       - math%one_i
!     CALL OK(ALL(found .EQ. want), "elem=1, connectivity: ")
!
!     found = connectivity(17:32)
!     want = 16 + [1, 2, 3, 4, 5, 6, 7, 8, 10, 9, 12, 11, 13, 14, 16, 15] &
!       - math%one_i
!     CALL OK(ALL(found .EQ. want), "elem=2, connectivity: ")
!   END IF
! END BLOCK
!
! BLOCK
!   REAL(DFP) :: found(3, 16), want(3, 16)
!   REAL(DFP), PARAMETER :: tol = 1.0E-5_DFP
!
!   IF (ALLOCATED(points)) THEN
!     found = points(1:3, 1:16)
!     want(1, :) = [0.0_DFP, 5.0_DFP, 5.0_DFP, 0.0_DFP, &
!                   5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                   5.0_DFP, 5.0_DFP, &
!                   10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP, &
!                   0.0_DFP, 0.0_DFP, &
!                   5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                   10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP]
!
!     want(2, :) = [0.0_DFP, 0.0_DFP, 10.0_DFP, 10.0_DFP, &
!                   0.0_DFP, 0.0_DFP, &
!                   10.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP, &
!                   10.0_DFP, 10.0_DFP, &
!                   20.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                   10.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                   20.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP]
!     want(3, :) = 0.0_DFP
!     CALL OK(ALL(SoftEq(found, want, tol)), "elem=1, points: ")
!
!     found = points(1:3, 17:32)
!     want(1, :) = 5.0_DFP + [0.0_DFP, 5.0_DFP, 5.0_DFP, 0.0_DFP, &
!                             5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                             5.0_DFP, 5.0_DFP, &
!                             10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP, &
!                             0.0_DFP, 0.0_DFP, &
!                             5.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                             10.0_DFP / 3.0_DFP, 5.0_DFP / 3.0_DFP]
!
!     want(2, :) = [0.0_DFP, 0.0_DFP, 10.0_DFP, 10.0_DFP, &
!                   0.0_DFP, 0.0_DFP, &
!                   10.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP, &
!                   10.0_DFP, 10.0_DFP, &
!                   20.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                   10.0_DFP / 3.0_DFP, 10.0_DFP / 3.0_DFP, &
!                   20.0_DFP / 3.0_DFP, 20.0_DFP / 3.0_DFP]
!     want(3, :) = 0.0_DFP
!     CALL OK(ALL(SoftEq(found, want, tol)), "elem=2, points: ")
!   END IF
!
! END BLOCK
!
! ! clean stuff
! CALL u%DEALLOCATE()
! CALL fedof%DEALLOCATE()
! CALL geofedof%DEALLOCATE()
! CALL dom%DEALLOCATE()
! END PROGRAM main
! ! ok 1 - points allocated:
! ! ok 2 - shape of points:
! ! ok 3 - connectivity allocated:
! ! ok 4 - shape of connectivity:
! ! ok 5 - offsets allocated:
! ! ok 6 - size of offsets:
! ! ok 7 - types allocated:
! ! ok 8 - size of types:
! ! ok 9 - elem=1, connectivity:
! ! ok 10 - elem=2, connectivity:
! ! ok 11 - elem=1, points:
! ! ok 12 - elem=2, points:
!```
!
! See also: [[obj_GetVtkUnstructuredGridData2]],
! [[VtkHDF5File_:obj_WriteUnstructuredGrid]]

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredGridData1(obj, points, &
                                                    connectivity, offsets, &
                                                    types, ivar)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! AbstractNodeField obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: points(:, :)
    !! coordinates of points, shape (3, nPoints)
    !! SIZE(points) = (3, nPoints)
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: connectivity(:)
    !! zero based point ids of cells
    INTEGER(I4B), ALLOCATABLE, INTENT(INOUT) :: offsets(:)
    !! offsets of cells in connectivity, size nCells + 1
    !! SIZE(offsets) = nCells + 1, offsets(1) = 0
    INTEGER(INT8), ALLOCATABLE, INTENT(INOUT) :: types(:)
    !! VTK cell types, size nCells
    !! SIZE(types) = nCells
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivar
    !! physical variables, only needed in case of block node field
  END SUBROUTINE obj_GetVtkUnstructuredGridData1
END INTERFACE

!----------------------------------------------------------------------------
!                                       GetVtkUnstructuredGridData@VTKMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu and Vikas Sharma
! date: 2026-10-24
! summary: VTK unstructured grid of Lagrange cells of the field
!   (no allocation)
!
! Same as [[obj_GetVtkUnstructuredGridData1]], but the arrays and the
! work arrays are given by the caller, so that nothing is allocated.
!
! See also: [[obj_GetVtkUnstructuredGridData1]] (contains an example)

INTERFACE
  MODULE SUBROUTINE obj_GetVtkUnstructuredGridData2(obj, points, &
                                                    connectivity, offsets, &
                                                    types, refElemCoord, &
                                                    xij, elemCoord, vtkindx, &
                                                    points_nrow, &
                                                    points_ncol, con_tsize, &
                                                    offsets_tsize, &
                                                    types_tsize, ivar)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    !! AbstractNodeField obj
    REAL(DFP), INTENT(INOUT) :: points(:, :)
    !! coordinates of points, shape (3, nPoints)
    !! nPoints is fedof%GetSumElemTotalDOF()
    !! SIZE(points, 2) >= fedof%GetSumElemTotalDOF()
    INTEGER(I4B), INTENT(INOUT) :: connectivity(:)
    !! zero based point ids of cells, size is obtained by
    !! fedof%GetSumElemTotalDOF()
    INTEGER(I4B), INTENT(INOUT) :: offsets(:)
    !! offsets of cells in connectivity, size nCells + 1
    !! SIZE(offsets) >= nCells + 1
    INTEGER(INT8), INTENT(INOUT) :: types(:)
    !! VTK cell types, size nCells
    !! SIZE(types) >= nCells
    REAL(DFP), INTENT(INOUT) :: refElemCoord(:, :)
    !! temporary storage for nodal coordinate of reference element
    !! size should be at least (3, maxNNE)
    REAL(DFP), INTENT(INOUT) :: xij(:, :)
    !! temporal storage for nodal coordinate of interpolation points
    !! size should be at least (4, maxCon). This variable
    !! is used to construct the quadrature points
    REAL(DFP), INTENT(INOUT) :: elemCoord(:, :)
    !! temporal storage for nodal coordinate of physical element
    !! size should be at least (3, maxNNE)
    INTEGER(I4B), INTENT(INOUT) :: vtkindx(:)
    !! size should be at least maxCon
    INTEGER(I4B), INTENT(OUT) :: points_nrow, points_ncol, &
                                 con_tsize, offsets_tsize, &
                                 types_tsize
    !! points_nrow, points_ncol are shape of points
    !! con_tsize is tsize of connectivity
    !! offsets_tsize is size of offsets
    !! types_tsize is size of types
    INTEGER(I4B), INTENT(IN), OPTIONAL :: ivar
    !! physical variables, only needed in case of block node field
  END SUBROUTINE obj_GetVtkUnstructuredGridData2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
!                                                   ImportFromToml@IOMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data From toml file
!
!# ImportFromToml
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
                                        timefedof, dom)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
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
  END SUBROUTINE obj_ImportFromToml1
END INTERFACE

!----------------------------------------------------------------------------
!                                                     ImportFromToml@Methods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-06-13
! summary:  Import data From toml file
!
!# ImportFromToml
!
! In this method we call obj_ImportFromToml1 method.

INTERFACE
  MODULE SUBROUTINE obj_ImportFromToml2( &
    obj, tomlName, fedof, geofedof, timefedof, dom, afile, &
    filename, printToml)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
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
    obj, table, fedof, geofedof, timefedof, dom)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
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
    obj, tomlName, fedof, geofedof, timefedof, dom, afile, &
    filename, printToml)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    !! name of the key
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:), geofedof(:)
    !! Read the docs of obj_ImportFromToml3
    TYPE(TimeFEDOFPointer_), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: &
      timefedof(:)
    !! Read the docs of obj_ImportFromToml3
    CLASS(AbstractDomain_), OPTIONAL, TARGET, INTENT(IN) :: dom
    !! Read the docs of obj_ImportFromToml3
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
    obj, table, fedof, geofedof, timefedof, dom)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
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
    obj, tomlName, fedof, geofedof, timefedof, dom, afile, &
    filename, printToml)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: tomlName
    TYPE(FEDOFPointer_), ALLOCATABLE, INTENT(INOUT) :: fedof(:), geofedof(:)
    TYPE(AbstractDomainPointer_), INTENT(IN) :: dom(:)
    TYPE(TimeFEDOFPointer_), OPTIONAL, ALLOCATABLE, INTENT(INOUT) :: &
      timefedof(:)
    TYPE(TxtFile_), OPTIONAL, INTENT(INOUT) :: afile
    CHARACTER(*), OPTIONAL, INTENT(IN) :: filename
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: printToml
  END SUBROUTINE obj_ImportFromToml6
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetMaxTotalNodeNumForBC@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Get the maximum total node number for applying boundary conds

INTERFACE
  MODULE SUBROUTINE obj_SetMaxTotalNodeNumForBC1(obj, dbcVec, dbc)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), OPTIONAL, INTENT(INOUT) :: dbcVec(:)
    CLASS(DirichletBC_), OPTIONAL, INTENT(INOUT) :: dbc
    INTEGER(I4B) :: ans
  END SUBROUTINE obj_SetMaxTotalNodeNumForBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                         SetMaxTotalNodeNumForBC@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Get the maximum total node number for applying boundary conds
!
!# Introduction
!   This method if for block node fields

INTERFACE
  MODULE SUBROUTINE obj_SetMaxTotalNodeNumForBC2(obj, ivar, dbcVec, dbc)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    CLASS(DirichletBCPointer_), OPTIONAL, INTENT(INOUT) :: dbcVec(:)
    CLASS(DirichletBC_), OPTIONAL, INTENT(INOUT) :: dbc
    INTEGER(I4B) :: ans
  END SUBROUTINE obj_SetMaxTotalNodeNumForBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetTotalNBC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-12
! summary: Get the size of nbc

INTERFACE
  MODULE FUNCTION obj_GetTotalNBC(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
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
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
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
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
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
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
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
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
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
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
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
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
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
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(1)
    !! This is just a dummy argument to have a unique interface
    TYPE(TimeFEDOFPointer_), ALLOCATABLE :: ans(:)
    !! List of FEDOF pointers
  END FUNCTION obj_GetTimeFEDOFPointer2
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetMaxTotalNodeNumForBC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Get the maximum total node number for applying boundary conds

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalNodeNumForBC1(obj) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTotalNodeNumForBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                         GetMaxTotalNodeNumForBC@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-12-02
! summary: Get the maximum total node number for applying boundary conds

INTERFACE
  MODULE FUNCTION obj_GetMaxTotalNodeNumForBC2(obj, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetMaxTotalNodeNumForBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetTotalNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-26
! summary: Returns total number of nodes for globalNode

INTERFACE
  MODULE FUNCTION obj_GetTotalNodeLoc1( &
    obj, globalNode, ivar, spaceCompo, timeCompo) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical varibale number (this variable is cuurrently not used)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! list of space components
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! list of time components
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalNodeLoc1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetTotalNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-26
! summary:  Get total number of nodes corresponding to nodes in AbstractBC
!
!# Introduction
!
! This method returns the total number of nodes corresponding to
! nodes defined in AbstractBC.
!
! - First, it will extract time components from obj
! - Then it will use ivar (default is 1) to extract the total time compo
!   in ivar
! - Then, it call GetTotalNodeNum on dbc with stored fedof or fedofs
! - The result will be tnode * tspace * ttime

INTERFACE
  MODULE FUNCTION obj_GetTotalNodeLoc2(obj, dbc, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! AbstractField
    CLASS(AbstractBC_), INTENT(INOUT) :: dbc
    !! Abstract boundary condition
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! Physical variable number, default is 1
    INTEGER(I4B) :: ans
    !! Total number of nodes
  END FUNCTION obj_GetTotalNodeLoc2
END INTERFACE

!----------------------------------------------------------------------------
!                                                  GetTotalNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-08-26
! summary: Get total number of nodes corresponding to dbc
!
!# Introduction
! This routine is like GetTotalNodeLoc2, but it is for vector of
! AbstractBCPointer. This routine calls GetTotalNodeLoc2 for each dbc
! In debug mode, if dbc(ii)%ptr is not associated then it will skip that dbc.

INTERFACE
  MODULE FUNCTION obj_GetTotalNodeLoc3(obj, dbc, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! AbstractField
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! A vector of DirichletBCPointer
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical variable number, default is 1
    INTEGER(I4B) :: ans
    !! Total number of Nodes
  END FUNCTION obj_GetTotalNodeLoc3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode

INTERFACE
  MODULE FUNCTION obj_GetNodeLoc1(obj, globalNode, ivar, spaceCompo, &
                                  timeCompo) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical varibale number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! list of space components
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! list of time components
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeLoc1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE FUNCTION obj_GetNodeLoc2(obj, dbc, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    CLASS(AbstractBC_), INTENT(INOUT) :: dbc
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeLoc2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE FUNCTION obj_GetNodeLoc3(obj, dbc, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetNodeLoc3
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc_@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode

INTERFACE
  MODULE SUBROUTINE obj_GetNodeLoc_1(obj, ans, tsize, globalNode, ivar, &
                                     spaceCompo, timeCompo, islocal)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! Abstract node field
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! result
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Total size written in ans
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! Global node number
    !! We get localNodeNumber from the mesh
    !! This routine is designed for getting the nodelocation
    !! in AbstractField for Dirichlet boundary nodes
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical varibale number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo(:)
    !! list of space components
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! list of time components
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: islocal
    !! if true then globalNode are local to obj
  END SUBROUTINE obj_GetNodeLoc_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc_@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE SUBROUTINE obj_GetNodeLoc_2(obj, ans, tsize, dbc, ivar)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    !! Abstract node field
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! result
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Total size written in ans
    CLASS(AbstractBC_), INTENT(INOUT) :: dbc
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetNodeLoc_2
END INTERFACE

!----------------------------------------------------------------------------
!                                                      GetNodeLoc@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2023-11-29
! summary: This function returns the location of globalNode from bc

INTERFACE
  MODULE SUBROUTINE obj_GetNodeLoc_3(obj, ans, tsize, dbc, ivar)
    CLASS(AbstractNodeField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    !! result
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Total size written in ans
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Global node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetNodeLoc_3
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalDirichletBCIndex@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE FUNCTION obj_GetTotalDirichletBCIndex1(obj, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDirichletBCIndex1
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetTotalDirichletBCIndex@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE FUNCTION obj_GetTotalDirichletBCIndex2(obj, dbc, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTotalDirichletBCIndex2
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetDirichletBCIndex@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE FUNCTION obj_GetDirichletBCIndex(obj, ivar) RESULT(ans)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    INTEGER(I4B), ALLOCATABLE :: ans(:)
  END FUNCTION obj_GetDirichletBCIndex
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetDirichletBCIndex@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE SUBROUTINE obj_GetDirichletBCIndex1_(obj, ans, tsize, ivar)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetDirichletBCIndex1_
END INTERFACE

!----------------------------------------------------------------------------
!                                              GetDirichletBCIndex@GeMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-29
! summary:  This function returns the location of globalNode from bc

INTERFACE
  MODULE SUBROUTINE obj_GetDirichletBCIndex2_(obj, dbc, ans, tsize, ivar)
    CLASS(AbstractNodeField_), INTENT(INOUT) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    INTEGER(I4B), INTENT(INOUT) :: ans(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetDirichletBCIndex2_
END INTERFACE

END MODULE AbstractNodeField_Class

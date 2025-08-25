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

!> authors: Vikas Sharma, Ph. D.
! date: 28 June 2021
! summary: Vector field data type is defined

MODULE VectorField_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE BaseType, ONLY: FEVariable_
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE VTKFile_Class, ONLY: VTKFile_
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE TimeOpt_Class, ONLY: TimeOpt_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_
USE FieldOpt_Class, ONLY: TypeFieldOpt

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "VectorField_Class"
CHARACTER(*), PARAMETER :: myprefix = "VectorField"
INTEGER(I4B), PARAMETER :: MYSTORAGEFORMAT = TypeFieldOpt%storageFormatDOF
INTEGER(I4B), PARAMETER :: myconversion = TypeFieldOpt%conversionNodesToDOF

PUBLIC :: VectorField_
PUBLIC :: VectorFieldPointer_
PUBLIC :: SetVectorFieldParam
PUBLIC :: VectorFieldInitiate
PUBLIC :: VectorFieldDeallocate
PUBLIC :: VectorField
PUBLIC :: VectorField_Pointer
PUBLIC :: VectorFieldDisplay
PUBLIC :: VectorFieldExport
PUBLIC :: VectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                              VectorField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Vector field
!
!{!pages/docs-api/VectorField/VectorField_.md!}

TYPE, EXTENDS(AbstractNodeField_) :: VectorField_
  INTEGER(I4B) :: spaceCompo = 0_I4B
  INTEGER(I4B), ALLOCATABLE :: idofs(:)
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    obj_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate4 => obj_Initiate4
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of VectorField
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import data from HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export data to HDF5 file
  PROCEDURE, PUBLIC, PASS(obj) :: ExportToVTK => obj_ExportToVTK
  !! Export data to VTK file

  ! SET:
  ! @SetMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set single entry

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values to a Vector values

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set all values to a given vector

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set selected values to given Vector

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set selected values to given vector

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! obj@spaceCompo = value (if value is a scalar)
  !! obj@spaceCompo = value@spaceCompo (if value is vector )

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set values to a vector by using triplet

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set values to a vector by using triplet

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set nodal values of a space component

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set a single value

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set values by using a FEVariable

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! Set all values to the constant value
  !! WE call SetAll method here

  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
  !! obj@[ivar, idof] = value@[ivar, idof]

  PROCEDURE, PASS(obj) :: Set14 => obj_Set14
  !! obj = value (we call copy method here)

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetByFunction => &
    obj_SetByFunction
  !! Set values from the user function

  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: SetFromSTVectorField => &
    obj_SetFromSTVectorField
  !! Set values from the STVectorField_

  !! Set selected values using FEVariable
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, &
    Set7, Set8, Set9, Set10, Set11, Set12, Set13, Set14

  ! GET:
  ! @GetMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! returns vector values at single node or
  !! get all nodal values of a space-components

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! returns all entries in rank2 array of real

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! returns selected values in rank2 aray of real

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! returns selected values of a space components

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get a single value

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get nodal values of a vector in FEVariable

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get7 => obj_Get7
  !! Get value of vector field in the ScalarField or another VectorField
  !! value = obj@spaceCompo (if value is ScalarField_)
  !! value@spaceCompo = obj@spaceCompo (if value is VectorField_)

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get8 => obj_Get8
  !! Copy  value=obj

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get9 => obj_Get9
  !! value@[ivar, idof] = obj@[ivar, idof]

  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, &
    Get5, Get6, Get7, Get8, Get9
  !! Get the entries of Vector field

  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get multiple values in FEVariable

  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  ! SET:
  ! @DirichletBCMethods

  PROCEDURE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: ApplyDirichletBC2 => obj_ApplyDirichletBC2
END TYPE VectorField_

!----------------------------------------------------------------------------
!                                                       VectorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: VectorFieldPointer_
  CLASS(VectorField_), POINTER :: ptr => NULL()
END TYPE VectorFieldPointer_

!----------------------------------------------------------------------------
!                                            SetVectorFieldParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 23 July 2021
! summary: Sets parameters for creating the vector field
!
INTERFACE
  MODULE SUBROUTINE SetVectorFieldParam(param, name, engine, spaceCompo, &
                                        fieldType, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    !! name of the variable
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !!
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  END SUBROUTINE SetVectorFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                            checkEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.
!
!# Introduction
! This routine check the essential parameters required to the initiate the
! [[VectorField_]] data type. We need following parameters
!
! - CHARACTER(  * ) :: name
! - INTEGER( I4B ) :: tdof

INTERFACE
  MODULE SUBROUTINE obj_checkEssentialParam(obj, param)
    CLASS(VectorField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_checkEssentialParam
END INTERFACE

INTERFACE VectorFieldCheckEssentialParam
  MODULE PROCEDURE obj_checkEssentialParam
END INTERFACE VectorFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                        Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the VectorField_ object
!
!# Introduction
! This routine initiate the vector field object.
! `param` contains the information of parameters required to initiate the
! vector. There are essential and optional information.
! Essential information are described below.
! - `name`  character defining the name of vector field
! - `spaceCompo` is the total degree of freedom or components
! - `fieldType` type of field type; FIELD_TYPE_CONSTANT, FIELD_TYPE_NORMAL

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof, timefedof)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
  END SUBROUTINE obj_Initiate1
END INTERFACE

INTERFACE VectorFieldInitiate
  MODULE PROCEDURE obj_Initiate1
END INTERFACE VectorFieldInitiate

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate2

INTERFACE
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
                                  usePointer)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE VectorFieldInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE VectorFieldInitiate

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-07-19
! summary:  Initiates by passing arguments
!
!# Introduction
!  This method is like obj_Initiate1, but it works with arugments
!  instead of parameter list.

INTERFACE
  MODULE SUBROUTINE obj_Initiate4(obj, name, engine, fieldType, storageFMT, &
                                  comm, local_n, global_n, spaceCompo, &
                                  isSpaceCompo, isSpaceCompoScalar, &
                                  timeCompo, isTimeCompo, isTimeCompoScalar, &
                                  tPhysicalVarNames, physicalVarNames, &
                                  isPhysicalVarNames, &
                                  isPhysicalVarNamesScalar, tNodes, &
                                  isTNodes, isTNodesScalar, tSize, &
                                  fedof, timefedof)
    CLASS(VectorField_), INTENT(INOUT) :: obj
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
    !! storage format of the scalar field
    !! Not required.
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
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1)
    !! Not required
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1)
    !! Not required
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true
    !! Not required
    CHARACTER(*), OPTIONAL, INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNamesScalar
    !! if true then physicalVarNames is scalar
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tNodes(:)
    !! total number of nodes in each physical variable
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodes
    !! if true we will try to access tNodes
    !! Not required
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTNodesScalar
    !! is tNodes scalar
    !! Not required
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tSize
    !! total size of node field
    !! not required
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    !! FEDOF object
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate4
END INTERFACE

INTERFACE VectorFieldInitiate
  MODULE PROCEDURE obj_Initiate4
END INTERFACE VectorFieldInitiate

!----------------------------------------------------------------------------
!                                                      Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Deallocates the data stored inside the VectorField_ obj

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(VectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE VectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE VectorFieldDeallocate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(VectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE

INTERFACE VectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_ptr_vector
END INTERFACE VectorFieldDeallocate

!----------------------------------------------------------------------------
!                                VectorFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-25
! summary:  Safely allocate the vector field
!
!# Introduction
!
! This routine will allocate obj if it is not allocated
! It will allocate obj if its current size is less than newsize

INTERFACE
  MODULE SUBROUTINE obj_VectorFieldSafeAllocate1(obj, newsize)
    TYPE(VectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! allocatable Vector field pointer
    INTEGER(I4B), INTENT(IN) :: newsize
    !! new size of obj
  END SUBROUTINE obj_VectorFieldSafeAllocate1
END INTERFACE

INTERFACE VectorFieldSafeAllocate
  MODULE PROCEDURE obj_VectorFieldSafeAllocate1
END INTERFACE VectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                          Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(VectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This function returns an instance of [[VectorField_]]

INTERFACE
  MODULE FUNCTION obj_Constructor1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    TYPE(VectorField_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE

INTERFACE VectorField
  MODULE PROCEDURE obj_Constructor1
END INTERFACE VectorField

!----------------------------------------------------------------------------
!                                           VectorField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This function returns an instance of [[VectorField_]]

INTERFACE
  MODULE FUNCTION obj_Constructor_1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(VectorField_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE

INTERFACE VectorField_Pointer
  MODULE PROCEDURE obj_Constructor_1
END INTERFACE VectorField_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[VectorField_]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE VectorFieldDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE VectorFieldDisplay

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                  Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE VectorFieldExport
  MODULE PROCEDURE obj_Export
END INTERFACE VectorFieldExport

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_ExportToVTK(obj, vtk)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! node field object
    TYPE(VTKFile_), INTENT(INOUT) :: vtk
    !! vtkfile object
  END SUBROUTINE obj_ExportToVTK
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the Vector field
!
!# Introduction
! This routine Sets the single entry of the vector field. Here, val should
! be a vector representing the components of a vector. The size of `value`
! should be same as `obj%spaceCompo`. In simple words it does following.
!
! vector( :, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call obj%Set( globalNode = 10, value= 100.0_DFP*[1,1,1] )
! call obj%display( "test-1: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, &
                             scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field object
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value, size of value should be obj%spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a Vector field
!
!# Introduction
! This routine work as follows. The size of value should be same as
!  obj%spaceCompo, then this value is Set for all the nodal values
!
! vector( :, i ) = value( : ), for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%Set( value= 10.0_DFP*[1,1,1] )
! call obj%display( "test-2: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value, size of value should be obj%spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a Vector field
!
!# Introduction
! This routine Sets all values of `spaceCompo` component of the vector field
! to given scalar value `value`
!
! vector( spaceCompo, i ) = value, for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%Set( value= -10.0_DFP, spaceCompo=1 )
! call obj%Set( value= -20.0_DFP, spaceCompo=2 )
! call obj%Set( value= -30.0_DFP, spaceCompo=3 )
! call obj%display( "test-3: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    !! vector field object
    REAL(DFP), INTENT(IN) :: VALUE
    !! value (all values are set to value)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using given Vector field
!
!# Introduction
! This routine Set all entries of vector field to given vector
! Here shape of should be value(1:spaceCompo, tNodes).
!
! vector( :, : ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, dom%GetTotalNodes() )
! real2 = 1.0_DFP
! call obj%Set( value=real2 )
! call obj%display( "test-4: vector field = " )
!```

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, VALUE, storageFMT, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! size(value, 1) should be obj%spaceCompo
    !! size(value, 2) should be tNodes
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! if storageFMT is NODES_FMT then:
    !! size(value, 1) should be obj%spaceCompo
    !! size(value, 2) should be tNodes
    !!
    !! if storageFMT is NODES_FMT then:
    !! size(value, 2) should be obj%spaceCompo
    !! size(value, 1) should be tNodes
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using given Vector field
!
!# Introduction
! This routine Set all entries of the component `spaceCompo` vector
! field  to given fortran vector `value`
!
! vector( spaceCompo, : ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, dom%GetTotalNodes() )
! real1 = 3.0_DFP
! call obj%Set( value=real1, spaceCompo=3 )
! call obj%display( "test-5: vector field = " )
!```

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! size of value should be tNodes
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using given Vector field
!
!# Introduction
! This routine Set all entries of the component `spaceCompo` vector
! field  to given scalar field `value`
!
! vector( spaceCompo, : ) = value
!
!
!### Usage
!
!```fortran
! call scalarObj%initiate( param, dom )
! call scalarObj%Set( value = 2.0_DFP )
! call obj%Set( value=scalarObj, spaceCompo=2 )
! call obj%display( "test-6: vector field = ")
! ierr = param%Set( key="fieldType", value=FIELD_TYPE_CONSTANT)
! call scalarObj%Deallocate()
! call scalarObj%initiate( param, dom )
! call scalarObj%Set( value=10.0_DFP )
! call obj%Set( value=scalarObj, spaceCompo=1 )
! call obj%display( "test-7: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! abstract node field
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This soubroutine Sets the selected enties to a vector entry value( : )
! Effectively it does the following:
!
! vector( :, globalNode ) = value( : ), for entries in global nodes
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, 4)
! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
! real2( :, 4 ) = -4.0
! call obj%Set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, VALUE, globalNode, islocal, scale, &
                             addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! vector value, this value will be assigned to all global nodes
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This routine Sets all selected entries.
! vector( :, globalNode ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, 4)
! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
! real2( :, 4 ) = -4.0
! call obj%Set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, globalNode, islocal, VALUE, scale, &
                             addContribution, storageFMT)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! value which will be used for obj=value
    !! if storageFMT is NODES_FMT then
    !!   size(value, 1) should be obj%spaceCompo
    !!   size(value, 2) should be size(globalNode)
    !! if storageFMT is DOF_FMT then
    !!   size(value, 2) should be obj%spaceCompo
    !!   size(value, 1) should be size(globalNode)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format can be NODES_FMT or DOF_FMT
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This routine Sets the selected components of selected nodes to given value
!
! vector( spaceCompo, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, 4)
! real1 = [1,10,100,1000]
! call obj%Set( value=real1, globalNode=[1,3,5,7], spaceCompo=1 )
! call obj%display( "test-9: vector field = " )
!```

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, VALUE, globalNode, islocal, spaceCompo, &
                             scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be used in obj = value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! vector( spaceCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, VALUE, globalNode, islocal, &
                              spaceCompo, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the values using FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, globalNode, islocal, scale, &
                              addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! FEVariable, space-nodal values of vector
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if tree then globalNode is local node number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the values using FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, ivar, idof, VALUE, ivar_value, &
                              idof_value, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, VALUE)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(VectorField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_SetFromSTVectorField(obj, VALUE, timeCompo, &
                                             scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetFromSTVectorField
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-19
! summary:  Set by user function

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times, ivar, idof, &
                                      spaceCompo, timeCompo)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: func
      !! User function
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    !! If present then its size should be 1
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! ivar (not used)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: idof
    !! idof (not used)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space component, not used
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! time component, not used
  END SUBROUTINE obj_SetByFunction
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the single entry of the Vector field
!
!# Introduction
!
! If globalnode is present then this routine returns all spatial components
! at the globalnode
!
! If spacecompo is present then `globalnode` should not be present
! In this case this routine returns the entire vector of spacecompo.

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, tsize, globalNode, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! Size of data written in value
    !! if globalNode is present then tsize = obj%spaceCompo
    !! if spaceCompo is present then tsize = tNodes
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    !! This should be a local node
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space component
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Get all the entries by using given Vector field

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, nrow, ncol, storageFMT, force3D)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of columns written in value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! NODES_FMT:: nrow is obj%spaceCompo or 3, ncol is tNodes
    !! DOF_FMT:: nrow is tNodes, ncol is obj%spaceCompo or 3
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, nrow, ncol, storageFMT, &
                             globalNode, islocal, force3D)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! The number of columns in value is same as the
    !! the size of globalNode
    !! The number of rows in columns is equal to the
    !! spaceCompo
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! The number of columns in value is same as the
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! NODES_FMT:: nrow is obj%spaceCompo or 3, ncol is size(globalNode)
    !! DOF_FMT:: nrow is size(globalNode), ncol is obj%spaceCompo or 3
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
    !! if true then minimum 3 components are returned in value
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
MODULE SUBROUTINE obj_Get4(obj, VALUE, tsize, globalNode, islocal, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! the size of value should be same as globalNode
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, islocal, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, globalNode, islocal)
    CLASS(VectorField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! space nodal values of vector
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! globla or local node numbe
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then globalNode is local node number
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    !! vector field object
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    !! abstract node field
    !! It can be ScalarField_, VectorField_, ScalarFieldLis_, VectorFieldLis_
    !! in case it is a vector field, then obj@spacecompo = value@spacecompo
    !! otherwise obj@spacecompo = value
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get8(obj, VALUE)
    CLASS(VectorField_), INTENT(IN) :: obj
    CLASS(VectorField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Get value

INTERFACE
  MODULE SUBROUTINE obj_Get9(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(VectorField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable in obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom in obj
    INTEGER(I4B), INTENT(IN) :: ivar_value
    !! physical variable in val
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE obj_Get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(VectorField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! is true then globalNode is local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetFeVariable
END INTERFACE

INTERFACE VectorFieldGetFEVariable
  MODULE PROCEDURE obj_GetFeVariable
END INTERFACE VectorFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(VectorField_), INTENT(IN) :: obj
    CHARACTER(:), ALLOCATABLE :: ans
  END FUNCTION obj_GetPrefix
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times, ivar, extField)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times, ivar, extField)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VectorField_Class

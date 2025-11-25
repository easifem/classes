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
! summary: STScalar field data type is defined

MODULE STScalarField_Class
USE GlobalData, ONLY: DFP, I4B, LGT, &
                      DOF_FMT, NodesToDOF, NODES_FMT
USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ScalarField_Class, ONLY: ScalarField_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE BaseType, ONLY: FEVariable_
USE tomlf, ONLY: toml_table
USE AbstractMesh_Class, ONLY: AbstractMesh_
USE TimeOpt_Class, ONLY: TimeOpt_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STScalarField_Class"
INTEGER(I4B), PARAMETER :: mystorageformat = DOF_FMT
INTEGER(I4B), PARAMETER :: myconversion = NodesToDOF

PUBLIC :: STScalarField_
PUBLIC :: STScalarFieldPointer_
PUBLIC :: STScalarFieldInitiate
PUBLIC :: STScalarFieldDeallocate
PUBLIC :: STScalarFieldDisplay
PUBLIC :: STScalarFieldImport
PUBLIC :: STScalarFieldExport
PUBLIC :: STScalarFieldGetTimeCompo
PUBLIC :: STScalarFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                              STScalarField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: STScalar field
!
!{!pages/docs-api/STScalarField/STScalarField_.md}

TYPE, EXTENDS(AbstractNodeField_) :: STScalarField_
  INTEGER(I4B), PUBLIC :: timeCompo = 0_I4B
  INTEGER(I4B), ALLOCATABLE :: idofs(:)
  !! global idofs 1 to timeCompo
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copy
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate4 => obj_Initiate4
  !! Initiate an instance of ScalarField_ by passing arguments
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored inside the STScalarField_ object
  FINAL :: obj_Final
  !! Finalizer

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content of STScalarField_
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the content of STScalarField_
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the content of STScalarField_

  ! SET:
  ! @SetMethods

  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set single entry
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values to a constant time node values
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set all values to a given STScalar
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set selected values to given STScalar
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set selected values to given STScalar
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! obj@timeCompo=obj@timeCompo+scale*value
  !! (value is an instance of abstract noe field)
  !! if value is space-time field, then
  !! value@timeCompo is used
  !! This method calls Set13
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set values to a STScalar by using triplet
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set values to a STScalar by using triplet
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set values to a STScalar by using triplet
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set values to a STScalar by using triplet
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set values using FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! Set all the value to a constant
  !! WE call setall method here
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
  !! obj@[ivar, idof] = value@[ivar, idof]
  PROCEDURE, PASS(obj) :: Set14 => obj_Set14
  !! Copy
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, &
    Set7, Set8, Set9, Set10, Set11, Set12, Set13, &
    Set14
  !! Generic Set Method
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: &
    SetByFunction => obj_SetByFunction
  !! Set by user function

  ! GET:
  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get all components at a given node
  !! Get all values of a given component
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all values in a rank  array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get selected many values in a rank-2 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get selected many values in a rank-1 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get single entry; call GetSingle method
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get value in FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get7 => obj_Get7
  !! Get by copy, value is an instance of AbstractNodeField_
  !! We call Get8
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get8 => obj_Get8
  !! Get a single value of time component at a global/local node
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, Get6, &
    Get7, Get8
  !! Generic Get method
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get Finite Element variable

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: ApplyDirichletBC2 => obj_ApplyDirichletBC2

  ! SET:
  ! @BodySourceMethods
  PROCEDURE, PASS(obj) :: ApplyBodySource1 => obj_ApplyBodySource1
  !! Add contribution of body source to the scalar field
  !! body source is given as user function
  PROCEDURE, PASS(obj) :: ApplyBodySource2 => obj_ApplyBodySource2
  !! Add contribution of body source to the scalar field
  !! body source is given external scalar field
  PROCEDURE, PASS(obj) :: ApplyBodySource3 => obj_ApplyBodySource3
  !! Add contribution of body source to the scalar field
  !! body source is given as user function
  GENERIC, PUBLIC :: ApplyBodySource => ApplyBodySource1, &
    ApplyBodySource2, ApplyBodySource3
  !! Generic method for setting body source

  ! SET:
  ! @SurfaceNBCMethods
  PROCEDURE, PUBLIC, PASS(obj) :: ApplySurfaceNeumannBC => &
    obj_ApplySurfaceNeumannBC
  !! Apply Surface neumann boundary condition

  ! SET:
  ! @PointNBCMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyPointNeumannBC1 => &
    obj_ApplyPointNeumannBC1
  !! Apply point neumann boundary condition
END TYPE STScalarField_

!----------------------------------------------------------------------------
!                                                     STScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldPointer_
  CLASS(STScalarField_), POINTER :: ptr => NULL()
END TYPE STScalarFieldPointer_

!----------------------------------------------------------------------------
!                                  SetSTScalarFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This routine is used to for Setting space time scalar field

! INTERFACE
!   MODULE SUBROUTINE SetSTScalarFieldParam(param, name, timeCompo, &
!                                    engine, fieldType, comm, global_n, local_n)
!     TYPE(ParameterList_), INTENT(INOUT) :: param
!     CHARACTER(*), INTENT(IN) :: name
!     !! name of the variable
!     INTEGER(I4B), INTENT(IN) :: timeCompo
!     !! time component
!     CHARACTER(*), INTENT(IN) :: engine
!     !! engine name
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
!     !! field type
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
!     !! communicator
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
!     !! global size
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
!     !! local size
!   END SUBROUTINE SetSTScalarFieldParam
! END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate2

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, obj2, copyFull, copyStructure, usePointer)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE STScalarFieldInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE STScalarFieldInitiate

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
  MODULE SUBROUTINE obj_Initiate4( &
    obj, name, engine, fieldType, storageFMT, comm, local_n, global_n, &
    spaceCompo, isSpaceCompo, isSpaceCompoScalar, timeCompo, isTimeCompo, &
    isTimeCompoScalar, tPhysicalVarNames, physicalVarNames, &
    isPhysicalVarNames, isPhysicalVarNamesScalar, tNodes, isTNodes, &
    isTNodesScalar, tSize, fedof, geofedof, timefedof)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: name
    !! name of the field, needed
    CHARACTER(*), INTENT(IN) :: engine
    !! name of the engine, needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    !! field type, default is FIELD_TYPE_NORMAL, needed
    !! following options are available FIELD_TYPE_NORMAL
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
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompo
    !! if true we will try to access spaceCompo, NOT REQUIRED
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isSpaceCompoScalar
    !! is space component scalar,
    !! in this case we only access spaceCompo(1), NOT REQUIRED
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo(:)
    !! Time components, needed for space-time field
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompo
    !! if true we will try to access TimeCompo
    !! Not required, Not needed
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isTimeCompoScalar
    !! is Time component scalar,
    !! in this case we only access TimeCompo(1), Not needed
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: tPhysicalVarNames
    !! total physical variable names
    !! if it is zero, then physicalVarNames will not be written
    !! evenif physicalVarNames is present, and isPhysicalVarNames
    !! is true, Not required
    CHARACTER(*), OPTIONAL, INTENT(IN) :: physicalVarNames(:)
    !! Names of the physical variables, NOT REQUIRED
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: isPhysicalVarNames
    !! logical variable to check if physicalVarNames is present or not
    !! if it is false then physicalVarNames will not be written
    !! NOT REQUIRED
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
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof, geofedof
    !! FEDOF object
    CLASS(TimeFEDOF_), OPTIONAL, TARGET, INTENT(IN) :: timefedof
    !! TimeFEDOF object
  END SUBROUTINE obj_Initiate4
END INTERFACE

INTERFACE STScalarFieldInitiate
  MODULE PROCEDURE obj_Initiate4
END INTERFACE STScalarFieldInitiate

!----------------------------------------------------------------------------
!                                               Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: It deallocates the data stored inside the STScalarField_ obj

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE STScalarFieldDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE STScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                STScalarFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-25
! summary:  Safely allocate the scalar field
!
!# Introduction
!
! This routine will allocate obj if it is not allocated
! It will allocate obj if its current size is less than newsize

INTERFACE
  MODULE SUBROUTINE obj_STScalarFieldSafeAllocate1(obj, newsize)
    TYPE(STScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! allocatable scalar field pointer
    INTEGER(I4B), INTENT(IN) :: newsize
    !! new size of obj
  END SUBROUTINE obj_STScalarFieldSafeAllocate1
END INTERFACE

INTERFACE STScalarFieldSafeAllocate
  MODULE PROCEDURE obj_STScalarFieldSafeAllocate1
END INTERFACE STScalarFieldSafeAllocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(STScalarFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE

INTERFACE STScalarFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_ptr_vector
END INTERFACE STScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STScalarField_]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitno)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE STScalarFieldDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE STScalarFieldDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

INTERFACE STScalarFieldImport
  MODULE PROCEDURE obj_Import
END INTERFACE STScalarFieldImport

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE STScalarFieldExport
  MODULE PROCEDURE obj_Export
END INTERFACE STScalarFieldExport

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the STScalar field
!
!# Introduction
! This routine Sets the single entry of the STScalar field. Here, val should
! be a STScalar representing the components of a STScalar. The size of `value`
! should be same as `obj%timeCompo`. In simple words it does following.
!
! STScalar( :, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call obj%Set( globalNode = 10, value= 100.0_DFP*[1,1,1] )
! call obj%display( "test-1: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set1(obj, globalNode, islocal, VALUE, scale, &
                             addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set @globalNode, obj = value
    !! value(a) denotes the value at time node `a`
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
! summary: This routine Sets all the entries of a STScalar field
!
!# Introduction
! This routine work as follows. The size of value should be same as
! obj%timeCompo, then this value is Set for all the nodal values
!
! STScalar( :, i ) = value( : ), for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%Set( value= 10.0_DFP*[1,1,1] )
! call obj%display( "test-2: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set, obj = value
    !! the size of value should be equal to obj%timeCompo
    !! value(i) is value at ith time node
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
! summary: This routine Sets all the entries of a STScalar field
!
!# Introduction
! This routine Sets all values of `timeCompo` component of the STScalar field
! to given scalar value `value`
!
! STScalar( timeCompo, i ) = value, for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%Set( value= -10.0_DFP, timeCompo=1 )
! call obj%Set( value= -20.0_DFP, timeCompo=2 )
! call obj%Set( value= -30.0_DFP, timeCompo=3 )
! call obj%display( "test-3: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set3(obj, VALUE, timeCompo, scale, &
                             addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! obj@timeCompo = value
    REAL(DFP), INTENT(IN) :: VALUE
    !! value to be set, obj=value
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    !! timecompo should be less than or equal to obj%timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using given STScalar field
!
!# Introduction
! This routine Set all entries of STScalar field to given STScalar
! Here shape of should be value(1:tNodes, 1:timeCompo).
!
! STScalar( :, : ) = value( :, : )

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, VALUE, storageFMT, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! obj = value
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! values to be set obj = value
    !! number of cols in value should be equal to obj%timeCompo
    !! number of rows in value should be equal to fedof%tdof
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! Storage format of value,
    !! How the dta is stored in value, NODES_FMT or DOF_FMT
    !! For NODES_FMT, nrow in value = timeCompo
    !! For DOF_FMT, nrow in value = nodes
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using given STScalar field
!
!# Introduction
! This routine Set all entries of the component `timeCompo` STScalar
! field  to given fortran STScalar `value`
!
! STScalar( timeCompo, : ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, dom%GetTotalNodes() )
! real1 = 3.0_DFP
! call obj%Set( value=real1, timeCompo=3 )
! call obj%display( "test-5: STScalar field = " )
!```

INTERFACE
  MODULE SUBROUTINE obj_Set5(obj, VALUE, timeCompo, scale, &
                             addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set obj = value
    !! size of values should be equal to the total number of nodes
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all the entries by using given STScalar field
!
!# Introduction
! This routine Set all entries of the component `timeCompo` STScalar
! field  to given scalar field `value`
!
! STScalar( timeCompo, : ) = value
!
!
!### Usage
!
!```fortran
! call scalarObj%initiate( param, dom )
! call scalarObj%Set( value = 2.0_DFP )
! call obj%Set( value=scalarObj, timeCompo=2 )
! call obj%display( "test-6: STScalar field = ")
! ierr = param%Set( key="fieldType", value=FIELD_TYPE_CONSTANT)
! call scalarObj%Deallocate()
! call scalarObj%initiate( param, dom )
! call scalarObj%Set( value=10.0_DFP )
! call obj%Set( value=scalarObj, timeCompo=1 )
! call obj%display( "test-7: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set6(obj, VALUE, timeCompo, scale, &
                             addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! obj@timeCompo = value
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    !! value in abstract node field
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
!
! This soubroutine Sets the selected enties to a STScalar entry value( : )
!
! Effectively it does the following:
!
! STScalar( :, globalNode ) = value( : ), for entries in global nodes
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, 4)
! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
! real2( :, 4 ) = -4.0
! call obj%Set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set7(obj, VALUE, globalNode, islocal, scale, &
                             addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set, i.e., obj(:, globalNode)=value
    !! The size of the value should be equal to obj%timeCompo
    !! value denotes the time component value at globalNodes
    !! note that all space nodes have same value.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This routine Sets all selected entries.
! STScalar( :, globalNode ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, 4)
! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
! real2( :, 4 ) = -4.0
! call obj%Set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE obj_Set8(obj, globalNode, islocal, VALUE, &
                             storageFMT, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! space-time nodal values at globalNode
    !! The values should be stored in NODES_FMT, that is:
    !!   - number of rows in value denotes the time nodes
    !!   - number of columns in value denotes the space nodes
    !!   - size(value,1) should be obj%timeCompo
    !!   - size(value,2) should be size(globalNode)
    !! The values can be stored in DOF_FMT, that is:
    !!   - number of cols in value denotes the time nodes
    !!   - number of rows in value denotes the space nodes
    !!   - size(value,2) should be obj%timeCompo
    !!   - size(value,1) should be size(globalNode)
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! The storage format of value,
    !! It can be either NODES_FMT or DOF_FMT
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! This routine Sets the selected components of selected nodes to given value
!
! STScalar( timeCompo, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, 4)
! real1 = [1,10,100,1000]
! call obj%Set( value=real1, globalNode=[1,3,5,7], timeCompo=1 )
! call obj%display( "test-9: STScalar field = " )
!```

INTERFACE
  MODULE SUBROUTINE obj_Set9(obj, VALUE, globalNode, islocal, timeCompo, scale, &
                             addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! values to be set, i.e., obj(:, globalNode)=value
    !! these are values at space nodes
    !! the size of value should be equal to sie of globalnode
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! STScalar( timeCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, VALUE, globalNode, islocal, timeCompo, &
                              scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE
    !! values to be set, i.e., obj(:, globalNode)=value
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STScalar values
!
!# Introduction
! Set entries using FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, globalNode, islocal, scale, &
                              addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! value in FEVariable
    !! It should be nodal scalar space-time variable
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: Set the STScalar values
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(IN) :: VALUE
    !! scalar value to be set
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Set the STScalarField

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, ivar, idof, VALUE, ivar_value, &
                              idof_value, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: Set the STScalar values
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, VALUE)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(STScalarField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-12-18
! summary:   Set By function

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times, ivar, idof, &
                                      spaceCompo, timeCompo)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
! summary: This routine returns the single entry of the STScalar field
!
!# Introduction
!
! If globalnode is present then
! It returns all the timecomponent at globalnode
!
! If timecompo is present then
! It returns the scalar field at time timecompo.
!
!@note
!Both globalnode and timecompo should not be present
!@endnote

INTERFACE
  MODULE SUBROUTINE obj_Get1(obj, VALUE, tsize, globalNode, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! Value to be returned
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in value
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    !! global node number (it is local node number)
    !! In this case all the time nodal values of scalar at globalNode
    !! will be returned
    !! in this case tsize will be timeCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! In this case all the space nodal values of obj at timeCompo
    !! will be returned.
    !! In this case tsize will be tnodes
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Get all the entries by using given STScalar field

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, nrow, ncol, storageFMT)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! Number of rows in value equals to the timeCompo
    !! Number of columns in value equals to the total number of nodes
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of cols written in value
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! if storageFMT is NODES_FMT then
    !! nrow = obj%timeCompo, ncol = size(globalNode)
    !! if stroageFMT is DOF_FMT then
    !! nrow = size(globalNode), ncol = obj%timeCompo
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, nrow, ncol, globalNode, islocal, &
                             storageFMT)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(OUT) :: nrow
    !! number of rows written in value
    INTEGER(I4B), INTENT(OUT) :: ncol
    !! number of cols written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! if storageFMT is NODES_FMT then
    !! nrow = obj%timeCompo, ncol = size(globalNode)
    !! if storageFMT is DOF_FMT then
    !! nrow = size(globalNode), ncol = obj%timeCompo
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, tsize, islocal, &
                             timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! values to be returned
    !! size should be equal to the size of globalNode
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size of data written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component, it should be less than obj%timeCompo
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, islocal, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! value to be returned
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component, it should be less than obj%timeCompo
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, globalNode, islocal)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! returned value in FEVariable format
    !! Space-time nodal values of scalar field
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node numbers
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    !! space-time scalar field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    !! returned value in AbstractNodeField format
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component, it should be less than obj%timeCompo
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Get values

INTERFACE
  MODULE SUBROUTINE obj_Get8(obj, ivar, idof, VALUE, ivar_value, &
                             idof_value)
    CLASS(STScalarField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    !! physical variable in obj
    INTEGER(I4B), INTENT(IN) :: idof
    !! local degree of freedom in obj
    INTEGER(I4B), INTENT(IN) :: ivar_value
    !! physical variable in VALUE
    INTEGER(I4B), INTENT(IN) :: idof_value
    !! local degree of freedom in VALUE
  END SUBROUTINE obj_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                               GetTimeComponent@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-10-29
! summary:  Get time components

INTERFACE STScalarFieldGetTimeCompo
  MODULE FUNCTION obj_GetTimeCompo(obj) RESULT(ans)
    CLASS(STScalarField_), INTENT(IN) :: obj
    INTEGER(I4B) :: ans
  END FUNCTION obj_GetTimeCompo
END INTERFACE STScalarFieldGetTimeCompo

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE STScalarFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(STScalarField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number is local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! returned value in FEVariable format (nodal, space, time, scalar)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! This argument is not used
    !! Physical component
  END SUBROUTINE obj_GetFeVariable
END INTERFACE STScalarFieldGetFEVariable

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times, ivar, extField)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times, ivar, extField)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyBodySource@NBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-19
! summary: Add Contribution of body source to scalar field

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource1(obj, bodySource, scale, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a scalar function with
    !! total arguments 4 (x, y, z, time)
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
    REAL(DFP), INTENT(IN) :: times(:)
    !! time, which will be passed to the body source function
  END SUBROUTINE obj_ApplyBodySource1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyBodySource@NBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-19
! summary: Add Contribution of body source to scalar field

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource2(obj, bodySource, scale)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    !! The test function will be corresponding to the obj
    CLASS(STScalarField_), INTENT(INOUT) :: bodySource
    !! Body source in terms of scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
  END SUBROUTINE obj_ApplyBodySource2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 ApplyBodySource@NBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-11-19
! summary: Add Contribution of body source to scalar field

INTERFACE
  MODULE SUBROUTINE obj_ApplyBodySource3(obj, bodySource, scale, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(UserFunction_), INTENT(INOUT) :: bodySource
    !! Body source user function
    !! It should be a scalar function with
    !! total arguments 4 (x, y, z, time)
    REAL(DFP), INTENT(IN) :: scale
    !! scale for body source
    !! obj = obj + scale * bodySource integral
    REAL(DFP), INTENT(IN) :: times
    !! time, which will be passed to the body source function
    !! This time can also represent a quadrature point time
  END SUBROUTINE obj_ApplyBodySource3
END INTERFACE

!----------------------------------------------------------------------------
!                                                   ApplyNeumannBC@NBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add Contribution of neumann boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplySurfaceNeumannBC(obj, nbcField, scale, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    CLASS(STScalarField_), INTENT(INOUT) :: nbcField
    !! Scalar field where we will keep the neumann boundary condition
    !! extension to the entire domain
    REAL(DFP), INTENT(IN) :: scale
    !! Scale for neumann boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times
  END SUBROUTINE obj_ApplySurfaceNeumannBC
END INTERFACE

!----------------------------------------------------------------------------
!                                             ApplyPointNeumannBC@NBCMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 2025-09-06
! summary: Add Contribution of point neumann boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyPointNeumannBC1(obj, scale, times, ivar, &
                                             extField)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    !! Scalar field
    REAL(DFP), INTENT(IN) :: scale
    !! scale for neumann boundary condition
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyPointNeumannBC1
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STScalarField_Class

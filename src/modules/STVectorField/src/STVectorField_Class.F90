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
! summary: STVector field data type is defined

MODULE STVectorField_Class
USE GlobalData, ONLY: DFP, I4B, LGT, DOF_FMT, NODES_FMT, NodesToDOF
USE BaseType, ONLY: FEVariable_
USE AbstractField_Class, ONLY: AbstractField_
USE VectorField_Class, ONLY: VectorField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE ExceptionHandler_Class, ONLY: e
USE HDF5File_Class, ONLY: HDF5File_
USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_
USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_
USE UserFunction_Class, ONLY: UserFunction_
USE TimeFEDOF_Class, ONLY: TimeFEDOF_, TimeFEDOFPointer_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "STVectorField_Class"
INTEGER(I4B), PARAMETER :: mystorageformat = DOF_FMT
INTEGER(I4B), PARAMETER :: myconversion = NodesToDOF

PUBLIC :: STVectorField_
PUBLIC :: STVectorFieldPointer_
PUBLIC :: STVectorFieldInitiate
PUBLIC :: STVectorFieldDeallocate
PUBLIC :: STVectorFieldDisplay
PUBLIC :: STVectorFieldImport
PUBLIC :: STVectorFieldExport
PUBLIC :: STVectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                             STVectorField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-06
! summary: STVector field
!
!{!pages/docs-api/STVectorField/STVectorField_.md!}

TYPE, EXTENDS(AbstractNodeField_) :: STVectorField_
  INTEGER(I4B), PUBLIC :: spaceCompo = 0_I4B
  INTEGER(I4B), PUBLIC :: timeCompo = 0_I4B
  INTEGER(I4B), ALLOCATABLE :: space_idofs(:)
  INTEGER(I4B), ALLOCATABLE :: time_idofs(:)
  INTEGER(I4B), ALLOCATABLE :: idofs(:)
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  !! Initiate by copy
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  !! Deallocate the data stored
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  !! Display the content
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  !! Import the data from HDFFile
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export
  !! Export the data to HDFfile

  ! SET:
  ! @SetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set1 => obj_Set1
  !! Set single entry (Space-time nodal values)
  !! Space-time nodal values are in 2D array (spaceCompo, timeCompo)
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values to a contaant space-time nodal values
  !! Space-time nodal values are given in a 2D array (spaceCompo, timeCompo)
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set3 => obj_Set3
  !! Set all values os space-time component to a given constant value
  !! obj@[spaceCompo, timeCompo] = value
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set4 => obj_Set4
  !! Set all values by using a rank-3 fortran array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set5 => obj_Set5
  !! Set all nodal values of a space-time component by using a rank-1 vector
  !! obj@[spaceCompo, timeCompo] = value(:)
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set6 => obj_Set6
  !! Set space-time nodal values of selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set7 => obj_Set7
  !! Set space-time nodal values of selected nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set8 => obj_Set8
  !! Set values to a STvector by using triplet
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set9 => obj_Set9
  !! Set a single node value of  space-time component
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set10 => obj_Set10
  !! Set selected nodal values by using FEVariable
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set11 => obj_Set11
  !! Set values to a STvector by using triplet
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set12 => obj_Set12
  !! obj@[ivar, idof] = value@[ivar_value, idof_value]
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set13 => obj_Set13
  !! Set all nodal values of many space components and a given time component
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Set14 => obj_Set14
  !! Set values using an instance of AbstractNodeField_
  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, &
    Set7, Set8, Set9, Set10, Set11, Set12, Set13, Set14 

  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SetByFunction => &
    obj_SetByFunction
  PROCEDURE, PUBLIC, NON_OVERRIDABLE, PASS(obj) :: SetFromVectorField => &
    obj_SetFromVectorField
  !! Set by function

  ! GET:
  ! @GetMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get1 => obj_Get1
  !! Get space-time nodal values of a single node in 1D array
  !! Get all the nodal values of space-time component in 1D array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get2 => obj_Get2
  !! Get all the values in a rank3 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get3 => obj_Get3
  !! Get space-time nodal value of many nodes in rank-3 array
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get4 => obj_Get4
  !! Get nodal values of a space-time component at many nodes
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get5 => obj_Get5
  !! Get a single nodal value of space-time component
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get6 => obj_Get6
  !! Get space-time nodal values of a single node
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: Get7 => obj_Get7
  !! Get space-time nodal value in fevariable
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, Get5, &
    Get6, Get7
  PROCEDURE, NON_OVERRIDABLE, PUBLIC, PASS(obj) :: GetFEVariable => &
    obj_GetFeVariable
  !! Get multiple values in FEVariable

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC1 => &
    obj_ApplyDirichletBC1
  !! Appply dirichlet boundary condition
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC2 => &
    obj_ApplyDirichletBC2
  !! Apply dirichlet boundary condition
  PROCEDURE, NON_OVERRIDABLE, PASS(obj) :: ApplyDirichletBC3 => &
    obj_ApplyDirichletBC3
  !! Apply dirichlet boundary condition
  GENERIC, PUBLIC :: ApplyDirichletBC => ApplyDirichletBC1, &
    ApplyDirichletBC2, ApplyDirichletBC3

END TYPE STVectorField_

!---------------------------------------------------------------------------
!                                                     STVectorFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STVectorFieldPointer_
  CLASS(STVectorField_), POINTER :: ptr => NULL()
END TYPE STVectorFieldPointer_

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate2

INTERFACE
  MODULE SUBROUTINE obj_Initiate2( &
    obj, obj2, copyFull, copyStructure, usePointer)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE

INTERFACE STVectorFieldInitiate
  MODULE PROCEDURE obj_Initiate2
END INTERFACE STVectorFieldInitiate

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Deallocates the data stored inside the STVectorField_ obj

INTERFACE
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE

INTERFACE STVectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate
END INTERFACE STVectorFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(STVectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE

INTERFACE STVectorFieldDeallocate
  MODULE PROCEDURE obj_Deallocate_ptr_vector
END INTERFACE STVectorFieldDeallocate

!----------------------------------------------------------------------------
!                                STVectorFieldSafeAllocate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-09-25
! summary:  Safely allocate the STVector field
!
!# Introduction
!
! This routine will allocate obj if it is not allocated
! It will allocate obj if its current size is less than newsize

INTERFACE
  MODULE SUBROUTINE obj_STVectorFieldSafeAllocate1(obj, newsize)
    TYPE(STVectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
    !! allocatable STVector field pointer
    INTEGER(I4B), INTENT(IN) :: newsize
    !! new size of obj
  END SUBROUTINE obj_STVectorFieldSafeAllocate1
END INTERFACE

INTERFACE STVectorFieldSafeAllocate
  MODULE PROCEDURE obj_STVectorFieldSafeAllocate1
END INTERFACE STVectorFieldSafeAllocate

!----------------------------------------------------------------------------
!                                                   Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STVectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STVectorField_]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

INTERFACE STVectorFieldDisplay
  MODULE PROCEDURE obj_Display
END INTERFACE STVectorFieldDisplay

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs, timefedof, &
                               timefedofs, geofedof, geofedofs)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof, geofedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:), geofedofs(:)
    CLASS(TimeFEDOF_), TARGET, OPTIONAL, INTENT(IN) :: timefedof
    TYPE(TimeFEDOFPointer_), OPTIONAL, INTENT(IN) :: timefedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

INTERFACE STVectorFieldImport
  MODULE PROCEDURE obj_Import
END INTERFACE STVectorFieldImport

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

INTERFACE STVectorFieldExport
  MODULE PROCEDURE obj_Export
END INTERFACE STVectorFieldExport

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the single entry of the STVector field
!
!# Introduction
! This routine Sets the single entry of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time
! components of the vector. The first index denotes the space components,
! second index denotes the time-components. As a result, total number of rows
! and columns in `value` are equal to the total number of spaceCompo and
! timeCompo.
!
! STvector( :, :, globalNode ) = value( :, : )

INTERFACE
  MODULE SUBROUTINE obj_Set1( &
    obj, globalNode, islocal, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! space-time nodal values at global node
    !! number of rows in value is equal to obj%spaceCompo
    !! numberof columns in value is equal to obj%spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! add or set
  END SUBROUTINE obj_Set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a STVector field
!
!# Introduction
! This routine Sets all entries of the STvector field. Here,
! `value` is a two dimensional array of real numbers, denoting the space-time
! components of the vector. The first index denotes the space components,
! second index denotes the time-components. As a result, total number of rows
! and columns in `value` are equal to the total number of spaceCompo and
! timeCompo.
!
! STvector( :, :, i ) = value( :, : ), for i = 1, tNodes

INTERFACE
  MODULE SUBROUTINE obj_Set2(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets all the entries of a STVector field
!
!# Introduction
! This routine Sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a scalar value `value`.
!
! STvector( spaceCompo, timeCompo, i ) = value, for i = 1, tNodes

INTERFACE
  MODULE SUBROUTINE obj_Set3( &
    obj, VALUE, spaceCompo, timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! obj = value
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
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
! summary: This routine Set all the entries by using given STVector field
!
!# Introduction
! This routine Set all entries of the space-time vector.
! The first index of `value` denotes the spatial components
! The second index of `value` denotes the temporal components
! The thrid index of `value` denotes the node number
!
! STvector( :, :, : ) = value( :, :, : )

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    !! number of first dim in value is equal to obj%spaceCompo
    !! number of second dim in value is equal to obj%timeCompo
    !! number of 3rd dimension is equal to total number of nodes
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all nodal values of a given space-time component
!
!# Introduction
!
! This routine Sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a vector `value`. Note that the size of `value` should
! be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, timeCompo, : ) = value( : )

INTERFACE
  MODULE SUBROUTINE obj_Set5( &
    obj, VALUE, spaceCompo, timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! nodal values of space-time component
    !! the size should be equal to the total number of nodes
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
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
! summary: This routine Sets the selected entries
!
!# Introduction
! This soubroutine Sets the selected enties in space-time vector to a
! constant space-time nodal values. Here globalNode is the list of global
! node number. `value` is a rank2 array of real numbers. Its first index
! denotes the space component and second component denotes time component.
!
!Effectively it does the following:
!
! STvector( :, :, globalNode ) = value( :, : ), for entries in global nodes

INTERFACE
  MODULE SUBROUTINE obj_Set6( &
    obj, VALUE, globalNode, islocal, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! space-time nodal value which will be assigned to globalNode
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
! This routine Sets selected entries of space-time vector field. Here
! globalNode contains the list of global nodes here values will be changed.
!
! - `value` is a rank-3 array.
! - Its first index denotes the space component,
! - second index denotes the time components
! - third component denotes the node number.
!
!@note
!The size of dimension should be equal to the size of globalNode.
!@endnote
!
! STvector( :, :, globalNode ) = value( :, :, : )

INTERFACE
  MODULE SUBROUTINE obj_Set7( &
    obj, globalNode, islocal, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    REAL(DFP), INTENT(IN) :: VALUE(:, :, :)
    !! space-time nodal values
    !! number of rows in value is equal to obj%spaceCompo
    !! number of cols in value is equal to obj%timeCompo
    !! number of 3rd dimension is equal to size of globalNode
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
! This routine Sets the selected components of selected nodes to given value
!
! STvector( spaceCompo, globalNode ) = value( : )

INTERFACE
  MODULE SUBROUTINE obj_Set8( &
    obj, VALUE, globalNode, islocal, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    !! nodal values of space-time component
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component of obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
! selected components, selected nodes
!
! STvector( spaceCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE obj_Set9( &
    obj, VALUE, globalNode, islocal, spaceCompo, timeCompo, scale, &
    addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! obj = value
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global opr local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component of obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component in obj
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set10( &
    obj, VALUE, globalNode, islocal, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    !! FE variable
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local nodes
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
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
! summary: Set the STvector values using triplet

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    !! Set all value to a constant value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-10
! summary: Set values for several space components for a given timecompo
!
!# Introduction
!
! This routine sets the values of several space components of
! of space-time vector field.
! Note that this routine sets all the nodal values.
!
! The number of columns in `Value` should be equal to the total
! number of nodes in the domain.
!
! The total number of rows in the value should be equal to the size
! of `spaceCompo`

INTERFACE
  MODULE SUBROUTINE obj_Set12( &
    obj, VALUE, spaceCompo, timeCompo, storageFMT, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! If storage is NODES_FMT
    !! The number of rows equal to the size of spaceCompo
    !! The number of columns equal to total number of nodes in domain
    !! each row represents a space components
    !! If storage is DOF_FMT then transpose of the above
    INTEGER(I4B), INTENT(IN) :: spaceCompo(:)
    !! Several space components
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! Several time components
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! Storage format of value
    !! NODES_FMT: row is space component and column is nodes
    !! DOF_FMT: row is nodes and column is space component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !! Scale, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! if true,then we add instead of set
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                              Set@SetMethods
!----------------------------------------------------------------------------

!> author: Shion Shimizu
! date: 2023-12-10
! summary: Set values for several time components for a given spaceCompo

INTERFACE
  MODULE SUBROUTINE obj_Set13( &
    obj, VALUE, spaceCompo, timeCompo, storageFMT, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! The numbe rows in value should be equal to the size of timeCompo
    !! The number of columns in value should be equal to the
    !! total number of  nodes in domain.
    !! The ith row of value denotes the nodal value of
    !! timeCompo(i) and spaceCompo.
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo(:)
    !! several time components
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of value
    !! NODES_FMT: row is time component and column is nodes
    !! DOF_FMT: row is nodes and column is time component
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    !!  Scalae, default is 1
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
    !! Add contribution
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, VALUE)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(STVectorField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Set all nodal values of a given space-time component
!
!# Introduction
!
! This routine Sets all entries of the space-time vector field. Here
! `spaceCompo` and `timeCompo` are the spatial temporal components, which we
! want to replace by a vector of scalars. These vectors of scalar are stored
! inside a scalar field called `value`. Note that the size of `value` should
! be equal to the total number of nodes in the mesh.
!
! STvector( spaceCompo, : ) = value

! INTERFACE
!   MODULE SUBROUTINE obj_Set14( &
!     obj, VALUE, spaceCompo, timeCompo, scale, addContribution)
!     CLASS(STVectorField_), INTENT(INOUT) :: obj
!     !! obj@[spaceCompo, timeCompo] = value (value is scalar field)
!     !! obj@[spaceCompo, timeCompo] = value@timeCompo (value is st scalar)
!     !! obj@[spaceCompo, timeCompo] = value@[spaceCompo, timeCompo]
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     !! Instance of abstract node field which contains
!     !! the nodal values of space-time component
!     INTEGER(I4B), INTENT(IN) :: spaceCompo
!     !! space component of obj
!     INTEGER(I4B), INTENT(IN) :: timeCompo
!     !! time component in obj
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     !! scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!     !! add or set
!   END SUBROUTINE obj_Set14
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-06-08
! summary: Set values
!
!# Introduction
!
! obj@[ivar, idof] = value@[ivar_value, idof_value]
!
! value can be any field. The conversion rule are defined as follows:
!
! If value is scalr, vector stscalr or stvector field then
! We first get the pointer to the value by calling GetPointer method
! Then we call setMultiple method on obj to set the multiple values
! using the range.

! INTERFACE
!   MODULE SUBROUTINE obj_Set16( &
!     obj, ivar, idof, VALUE, ivar_value, idof_value, scale, addContribution)
!     CLASS(STVectorField_), INTENT(INOUT) :: obj
!     INTEGER(I4B), INTENT(IN) :: ivar
!     !! physical variable in obj
!     INTEGER(I4B), INTENT(IN) :: idof
!     !! local degree of freedom in obj
!     CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
!     !! obj = value
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     !! physical variable in value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!     !! local degree of freedom in value
!     REAL(DFP), OPTIONAL, INTENT(IN) :: scale
!     LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
!   END SUBROUTINE obj_Set16
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-12-24
! summary: obj@timeCompo = value

INTERFACE
  MODULE SUBROUTINE obj_SetFromVectorField( &
    obj, VALUE, timeCompo, scale, addContribution)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(VectorField_), INTENT(IN) :: VALUE
    !! It should be a child of VectorField_
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component of vector field
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetFromVectorField
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetByFunction( &
    obj, func, times, ivar, idof, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
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
! date: 2024-06-06
! summary: Get nodal values
!
!# Introduction
!
! if globalNode present then it returns the space-time nodal values
! at that node.
!
! if spaceCompo and timeCompo is present then it returns the
! nodal values of that space-time component
!
! Node Space Time
! Node No-Space No-Time
! Node Space No-Time
! Node No-Space Time
! No-Node Space Time
! No-Node Space No-Time
! No-Node No-Space Time

INTERFACE
  MODULE SUBROUTINE obj_Get1( &
    obj, VALUE, tsize, globalNode, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! value = obj
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! size of data written in value
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    !! local node number
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
    !! time component
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Get all the entries of the space-time vector field
!
!# Introduction
! This routine returns all the nodal values of space-time nodal vector field.
! Here value is a rank3 array of reals.
!
! - Its first index denotes the spatial component
! - the second index denotes the temporal component
! - the third index denotes the node number.

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, storageFMT, dim1, dim2, dim3)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :, :)
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of value
    !! NODES_FMT: dim1, dim2, dim3 => obj%spaceCompo, obj%timeCompo, tNodes
    !! DOF_FMT: dim1, dim2, dim3 => tNodes, obj%spaceCompo, obj%timeCompo
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time nodal values of selected nodes
!
!# Introduction
!         This routine returns the space-time nodal values of selected nodes.
! The values will be returned in rank3 array value.
! - The first index corresponds to the spatial components
! - The second index corresponds to the temporal components,
! - The third index corresponds to the node number.
!
!@note
! The size of third dimension of value should be equal to size of globalNode.
!@endnote

INTERFACE
  MODULE SUBROUTINE obj_Get3( &
    obj, VALUE, storageFMT, globalNode, islocal, dim1, dim2, dim3)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :, :)
    !! returned value
    !! dim1 is space compoennt
    !! dim2 is time component
    !! dim3 is size of globalNode
    INTEGER(I4B), INTENT(IN) :: storageFMT
    !! storage format of value
    !! NODES_FMT: dim1, dim2, dim3 => spaceCompo, timeCompo, size(globalNode)
    !! DOF_FMT: dim1, dim2, dim3 => size(globalNode), spaceCompo, timeCompo
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! local or global node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(OUT) :: dim1, dim2, dim3
    !! dim1 = obj%spaceCompo
    !! dim2 = obj%timeCompo
    !! dim3 = size of globalNode
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the nodal value of a space-time vector field
!
!# Introduction
! This routine returns the nodal values of a space-time nodal vector.
! In this routine we can specify the spatial and temporal component using
! spaceCompo and timeCompo. globalNode contains the list of global node
! number. Also, the values are returned in the a vector scalar `values`. Note
! that the length of value should be equal to the size of globalNode vector.

INTERFACE
  MODULE SUBROUTINE obj_Get4( &
    obj, VALUE, tsize, globalNode, islocal, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! nodal values of space-time component
    !! size of value is same as size of global node
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total size written in value
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    ! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true the global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get5( &
    obj, VALUE, globalNode, islocal, spaceCompo, timeCompo)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    !! nodal value of space-time component
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    !! space component
    INTEGER(I4B), INTENT(IN) :: timeCompo
    !! time component
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, nrow, ncol, globalNode, islocal)
    CLASS(STVectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:, :)
    !! space-time nodal values at global node
    INTEGER(I4B), INTENT(OUT) :: nrow, ncol
    !! number of rows and columns written in value
    !! nrow will be obj%spaceCompo
    !! ncol will be obj%timeCompo
    INTEGER(I4B), INTENT(IN) :: globalNode
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the space-time value at given node number

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, globalNode, islocal)
    CLASS(STVectorField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Get value in an instance of AbstractNodeField_
!
!# Introduction
!
! If spaceCompo and timeCompo are present then:
! - value = obj@[spaceCompo, timeCompo] (if value is scalar field)
! - value@timeCompo = obj@[spaceCompo, timeCompo] (if value is st scalar field)
! - value@spaceCompo = obj@[spaceCompo, timeCompo] (if value is vector field)
! - value@[spaceCompo, timeCompo] = obj@[spaceCompo, timeCompo]
!    (if value is st vector field)
!
! If spacecompo is present and timecompo is not present then:
!   value should be an instance of STScalarField or STVectorField
!   If value is ST scalar field:
!     value@timeCompo = obj@[spaceCompo, timeCompo] for all timeCompo
!   If value is ST vector field:
!     value@[spaceCompo, timeCompo]= obj@[spaceCompo, timeCompo]
!       for all timeCompo
!
! If spacecompo is not present and timecompo is present then:
!   value should an instance of VectorField_ and STVectorField_
!   If value is VectorField_:
!     value@spaceCompo = obj@[spaceCompo, timeCompo] for all spaceCompo
!   If value is STVectorField_:
!     value@[spaceCompom timeCompo] = obj@[spaceCompo, timeCompo]
!       for all spaceCompo
!
! If spacecompo and timecompo are not present then:
!   value should be an instance of STVectorField_
!   In this case; value = obj

! INTERFACE
!   MODULE SUBROUTINE obj_Get8(obj, VALUE, spaceCompo, timeCompo)
!     CLASS(STVectorField_), INTENT(IN) :: obj
!     !! value=obj
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
!     !! value=obj
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
!     !!  space component
!     INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
!     !! time component
!   END SUBROUTINE obj_Get8
! END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 06 Jan 2022
! summary: REturns the value

! INTERFACE
!   MODULE SUBROUTINE obj_Get9( &
!     obj, ivar, idof, VALUE, ivar_value, idof_value)
!     CLASS(STVectorField_), INTENT(IN) :: obj
!     CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
!     INTEGER(I4B), INTENT(IN) :: ivar
!     INTEGER(I4B), INTENT(IN) :: idof
!     INTEGER(I4B), INTENT(IN) :: ivar_value
!     INTEGER(I4B), INTENT(IN) :: idof_value
!   END SUBROUTINE obj_Get9
! END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, islocal, VALUE, ivar)
    CLASS(STVectorField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    !! global or local node number
    LOGICAL(LGT), INTENT(IN) :: islocal
    !! if true then global node number are local node number
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    !! FEvariable
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! physical variable
  END SUBROUTINE obj_GetFeVariable
END INTERFACE

INTERFACE STVectorFieldGetFEVariable
  MODULE PROCEDURE obj_GetFeVariable
END INTERFACE STVectorFieldGetFEVariable

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(INOUT) :: dbc
    REAL(DFP), INTENT(IN) :: times(:)
  END SUBROUTINE obj_ApplyDirichletBC1
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC2(obj, dbc, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    TYPE(DirichletBCPointer_), INTENT(INOUT) :: dbc(:)
    !! Dirichlet boundary condition
    REAL(DFP), INTENT(IN) :: times(:)
    !! times
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                               ApplyDirichletBC@DBCMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 22 Jan 2021
! summary: Apply Dirichlet boundary condition

INTERFACE
  MODULE SUBROUTINE obj_ApplyDirichletBC3(obj, times)
    CLASS(STVectorField_), INTENT(INOUT) :: obj
    !! space-time vector field
    REAL(DFP), INTENT(IN) :: times(:)
    !! times
  END SUBROUTINE obj_ApplyDirichletBC3
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STVectorField_Class

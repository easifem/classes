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
USE GlobalData
USE BaSetype
USE String_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
USE FiniteElement_Class
USE UserFunction_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "VectorField_Class"
CHARACTER(*), PARAMETER :: myprefix = "VectorField"
PUBLIC :: VectorField_
PUBLIC :: VectorFieldPointer_
PUBLIC :: SetVectorFieldParam
PUBLIC :: VectorFieldInitiate1
PUBLIC :: VectorFieldInitiate2
PUBLIC :: VectorFieldDeallocate
PUBLIC :: VectorField
PUBLIC :: VectorField_Pointer
PUBLIC :: VectorFieldDisplay
PUBLIC :: VectorFieldExport
PUBLIC :: TypeVectorField

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
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & obj_checkEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final

  PROCEDURE, PUBLIC, PASS(obj) :: GetPointerOfComponent => &
    & obj_GetPointerOfComponent

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export

  ! SET:
  ! @SetMethods
  PROCEDURE, PASS(obj) :: Set1 => obj_Set1
  !! Set single entry
  PROCEDURE, PASS(obj) :: Set2 => obj_Set2
  !! Set all values to a Vector values
  PROCEDURE, PASS(obj) :: Set3 => obj_Set3
  !! Set all values to a given vector
  PROCEDURE, PASS(obj) :: Set4 => obj_Set4
  !! Set selected values to given Vector
  PROCEDURE, PASS(obj) :: Set5 => obj_Set5
  !! Set selected values to given vector
  PROCEDURE, PASS(obj) :: Set6 => obj_Set6
  !! Set values to a Vector by using triplet
  PROCEDURE, PASS(obj) :: Set7 => obj_Set7
  !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
  !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
  !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set10 => obj_Set10
  !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set11 => obj_Set11
  !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set12 => obj_Set12
  !! Set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
  PROCEDURE, PASS(obj) :: Set14 => obj_Set14
  PROCEDURE, PASS(obj) :: Set15 => obj_Set15
  PROCEDURE, PASS(obj) :: Set16 => obj_Set16
  PROCEDURE, PUBLIC, PASS(obj) :: SetByFunction => obj_SetByFunction
  PROCEDURE, PUBLIC, PASS(obj) :: SetFromSTVectorField =>  &
    & obj_SetFromSTVectorField
  !! Set selected values using FEVariable
  GENERIC, PUBLIC :: Set => &
    & Set1, Set2, Set3, Set4, Set5, Set6, &
    & Set7, Set8, Set9, Set10, Set11, Set12, &
    & Set13, Set14, Set15, Set16

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj) :: Get1 => obj_Get1
  !! returns the single entry
  PROCEDURE, PASS(obj) :: Get2 => obj_Get2
  !! returns all entries in rank2 array of real
  PROCEDURE, PASS(obj) :: Get3 => obj_Get3
  !! returns selected values in XiJ format
  PROCEDURE, PASS(obj) :: Get4 => obj_Get4
  PROCEDURE, PASS(obj) :: Get5 => obj_Get5
  PROCEDURE, PASS(obj) :: Get6 => obj_Get6
  PROCEDURE, PASS(obj) :: Get7 => obj_Get7
  PROCEDURE, PASS(obj) :: Get8 => obj_Get8
  PROCEDURE, PASS(obj) :: Get9 => obj_Get9
  PROCEDURE, PASS(obj) :: Get10 => obj_Get10
  PROCEDURE, PASS(obj) :: Get11 => obj_Get11
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, &
    & Get5, Get6, Get7, Get8, Get9, Get10, Get11
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
!                                                         TypeVectorField
!----------------------------------------------------------------------------

TYPE(VectorField_), PARAMETER :: TypeVectorField =  &
  & VectorField_(domains=NULL())

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
  MODULE SUBROUTINE SetVectorFieldParam(param, name, engine, &
    & spaceCompo, fieldType, comm, local_n, global_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
  END SUBROUTINE SetVectorFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           checkEssentialParam@Constructor
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

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
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

INTERFACE VectorFieldInitiate1
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE VectorFieldInitiate1

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate2

INTERFACE VectorFieldInitiate2
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE VectorFieldInitiate2

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the VectorField_ obj

INTERFACE VectorFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(VectorField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE VectorFieldDeallocate

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE VectorFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate_ptr_vector(obj)
    TYPE(VectorFieldPointer_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
  END SUBROUTINE obj_Deallocate_ptr_vector
END INTERFACE VectorFieldDeallocate

!----------------------------------------------------------------------------
!                                                         Final@Constructor
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
! summary:         This function returns an instance of [[VectorField_]]

INTERFACE VectorField
  MODULE FUNCTION obj_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(VectorField_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE VectorField

!----------------------------------------------------------------------------
!                                           VectorField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[VectorField_]]

INTERFACE VectorField_Pointer
  MODULE FUNCTION obj_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(VectorField_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE VectorField_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[VectorField_]]

INTERFACE VectorFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE VectorFieldDisplay

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom, domains)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE VectorFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE VectorFieldExport

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
  MODULE SUBROUTINE obj_Set1(obj, globalNode, VALUE, &
    & scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
    CLASS(VectorField_), TARGET, INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
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
  MODULE SUBROUTINE obj_Set4(obj, VALUE, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
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
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
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
    INTEGER(I4B), INTENT(IN) :: spaceCompo
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
  MODULE SUBROUTINE obj_Set7(obj, VALUE, globalNode, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
  MODULE SUBROUTINE obj_Set8(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! value is in value(i,J) format.
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
  MODULE SUBROUTINE obj_Set9(obj, VALUE, globalNode, spaceCompo, scale, &
    & addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
! vector( spaceCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, VALUE, globalNode, spaceCompo, scale, &
    & addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries
!
!# Introduction
! Set entries using the selected nodes using triplet.
!

INTERFACE
  MODULE SUBROUTINE obj_Set11(obj, VALUE, istart, iend, stride, scale, &
    & addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the vector values using triplet
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, istart, iend, stride, scale, &
    & addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the values using FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Set the values using FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, VALUE, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_Set15(obj, ivar, idof, VALUE, ivar_value, &
    & idof_value, scale, addContribution)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set15
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_Set16(obj, VALUE)
    CLASS(VectorField_), INTENT(INOUT) :: obj
    CLASS(VectorField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set16
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Set values

INTERFACE
  MODULE SUBROUTINE obj_SetFromSTVectorField(obj, VALUE, timeCompo,  &
    & scale, addContribution)
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
  MODULE SUBROUTINE obj_SetByFunction(obj, func, times, ivar, idof,  &
    & spaceCompo, timeCompo)
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
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Get all the entries by using given Vector field

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE, force3D)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode, force3D)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    !! The number of columns in value is same as the
    !! the size of globalNode
    !! The number of rows in columns is equal to the
    !! spaceCompo
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE obj_Get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get6(obj, VALUE, istart, iend, stride)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE obj_Get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Sets the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get7(obj, VALUE, istart, iend, stride, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get8(obj, VALUE, globalNode)
    CLASS(VectorField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE obj_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get9(obj, VALUE, spaceCompo)
    CLASS(VectorField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE obj_Get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get10(obj, VALUE)
    CLASS(VectorField_), INTENT(IN) :: obj
    CLASS(VectorField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE obj_Get10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2023-03-29
! summary: Get value

INTERFACE
  MODULE SUBROUTINE obj_Get11(obj, ivar, idof, VALUE, ivar_value, idof_value)
    CLASS(VectorField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE obj_Get11
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE VectorFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, VALUE, ivar)
    CLASS(VectorField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
  END SUBROUTINE obj_GetFeVariable
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
    CLASS(DirichletBC_), INTENT(IN) :: dbc
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
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    CLASS(AbstractNodeField_), OPTIONAL, INTENT(INOUT) :: extField
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                          GetPointerOfComponent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
  MODULE FUNCTION obj_GetPointerOfComponent(obj, spaceCompo) RESULT(ans)
    CLASS(VectorField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE VectorField_Class

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
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STScalarField_Class"
CHARACTER(*), PARAMETER :: myprefix = "STScalarField"
PUBLIC :: STScalarField_
PUBLIC :: STScalarFieldPointer_
PUBLIC :: SetSTScalarFieldParam
PUBLIC :: STScalarFieldCheckEssentialParam
PUBLIC :: STScalarFieldInitiate1
PUBLIC :: STScalarFieldInitiate2
PUBLIC :: STScalarFieldDeallocate
PUBLIC :: STScalarField
PUBLIC :: STScalarField_Pointer
PUBLIC :: STScalarFieldDisplay
PUBLIC :: STScalarFieldImport
PUBLIC :: STScalarFieldExport
PUBLIC :: STScalarFieldGetTimeCompo

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
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods

  PROCEDURE, PUBLIC, PASS(obj) :: CheckEssentialParam => &
    & obj_CheckEssentialParam
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate2 => obj_Initiate2
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final

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
    !! Set all values to a STScalar values
  PROCEDURE, PASS(obj) :: Set3 => obj_Set3
    !! Set all values to a given STScalar
  PROCEDURE, PASS(obj) :: Set4 => obj_Set4
    !! Set selected values to given STScalar
  PROCEDURE, PASS(obj) :: Set5 => obj_Set5
    !! Set selected values to given STScalar
  PROCEDURE, PASS(obj) :: Set6 => obj_Set6
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set7 => obj_Set7
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set10 => obj_Set10
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set11 => obj_Set11
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set12 => obj_Set12
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
    !! Set values using FEVariable
  PROCEDURE, PASS(obj) :: Set14 => obj_Set14
    !! Set values using FEVariable
  PROCEDURE, PASS(obj) :: Set15 => obj_Set15
  PROCEDURE, PASS(obj) :: Set16 => obj_Set16

  GENERIC, PUBLIC :: Set => Set1, Set2, Set3, Set4, Set5, Set6, &
    & Set7, Set8, Set9, Set10, Set11, Set12, Set13, Set14, Set15,  &
    & Set16

  ! GET:
  ! @GetMethods
  PROCEDURE, PASS(obj) :: Get1 => obj_Get1
  PROCEDURE, PASS(obj) :: Get2 => obj_Get2
  PROCEDURE, PASS(obj) :: Get3 => obj_Get3
  PROCEDURE, PASS(obj) :: Get4 => obj_Get4
  PROCEDURE, PASS(obj) :: Get5 => obj_Get5
  PROCEDURE, PASS(obj) :: Get6 => obj_Get6
  PROCEDURE, PASS(obj) :: Get7 => obj_Get7
  PROCEDURE, PASS(obj) :: Get8 => obj_Get8
  PROCEDURE, PASS(obj) :: Get9 => obj_Get9
  PROCEDURE, PASS(obj) :: Get10 => obj_Get10
  GENERIC, PUBLIC :: Get => Get1, Get2, Get3, Get4, &
    & Get5, Get6, Get7, Get8, Get9
  PROCEDURE, PASS(obj) :: GetPointerOfComponent => &
    & obj_GetPointerOfComponent
  PROCEDURE, PUBLIC, PASS(obj) :: GetFEVariable => obj_GetFeVariable
  !! Get Finite Element variable
  PROCEDURE, PUBLIC, PASS(obj) :: GetPrefix => obj_GetPrefix

  ! SET:
  ! @DirichletBCMethods
  PROCEDURE, PASS(obj) :: ApplyDirichletBC1 => obj_ApplyDirichletBC1
  PROCEDURE, PASS(obj) :: ApplyDirichletBC2 => obj_ApplyDirichletBC2

END TYPE STScalarField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE(STScalarField_), PARAMETER, PUBLIC :: TypeScalarField = &
  & STScalarField_(domains=NULL())

!----------------------------------------------------------------------------
!                                                     STScalarFieldPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldPointer_
  CLASS(STScalarField_), POINTER :: ptr => NULL()
END TYPE STScalarFieldPointer_

!----------------------------------------------------------------------------
!                                         SetSTScalarFieldParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This routine is used to for Setting space time scalar field

INTERFACE
  MODULE SUBROUTINE SetSTScalarFieldParam(param, name, timeCompo, &
    & engine, fieldType, comm, global_n, local_n)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: timeCompo
    CHARACTER(*), INTENT(IN) :: engine
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: fieldType
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: comm
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: global_n
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: local_n
  END SUBROUTINE SetSTScalarFieldParam
END INTERFACE

!----------------------------------------------------------------------------
!                                           CheckEssentialParam@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Check the essential parameters in param.
!
!# Introduction
! This routine Check the essential parameters required to the initiate the
! [[STScalarField_]] data type. We need following parameters
!
! - CHARACTER(  * ) :: name
! - INTEGER( I4B ) :: tdof

INTERFACE STScalarFieldCheckEssentialParam
  MODULE SUBROUTINE obj_CheckEssentialParam(obj, param)
    CLASS(STScalarField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE obj_CheckEssentialParam
END INTERFACE STScalarFieldCheckEssentialParam

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the STScalarField_ object
!
!# Introduction
! This routine initiate the STScalar field object.
! `param` contains the information of parameters required to initiate the
! STScalar. There are essential and optional information.
! Essential information are described below.
! - `name`  character defining the name of STScalar field
! - `timeCompo` is the total degree of freedom or components
! - `fieldType` type of field type; FIELD_TYPE_CONSTANT, FIELD_TYPE_NORMAL

INTERFACE STScalarFieldInitiate1
  MODULE SUBROUTINE obj_Initiate1(obj, param, dom)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE obj_Initiate1
END INTERFACE STScalarFieldInitiate1

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Initiate2

INTERFACE STScalarFieldInitiate2
  MODULE SUBROUTINE obj_Initiate2(obj, obj2, copyFull, copyStructure, &
    & usePointer)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(AbstractField_), INTENT(INOUT) :: obj2
    !! It should be a child of AbstractNodeField_
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyFull
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: copyStructure
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: usePointer
  END SUBROUTINE obj_Initiate2
END INTERFACE STScalarFieldInitiate2

!----------------------------------------------------------------------------
!                                                     Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the STScalarField_ obj

INTERFACE STScalarFieldDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE STScalarFieldDeallocate

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STScalarField_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                       STScalar@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[STScalarField_]]

INTERFACE STScalarField
  MODULE FUNCTION obj_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(STScalarField_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE STScalarField

!----------------------------------------------------------------------------
!                                         STScalarField_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[STScalarField_]]

INTERFACE STScalarField_Pointer
  MODULE FUNCTION obj_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(STScalarField_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE STScalarField_Pointer

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STScalarField_]]

INTERFACE STScalarFieldDisplay
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE STScalarFieldDisplay

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE STScalarFieldImport
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, dom, domains)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE obj_Import
END INTERFACE STScalarFieldImport

!----------------------------------------------------------------------------
!                                                                Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE STScalarFieldExport
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
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
  MODULE SUBROUTINE obj_Set1(obj, globalNode, VALUE, scale, &
      & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
    CLASS(STScalarField_), TARGET, INTENT(INOUT) :: obj
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
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
! Here shape of should be value(1:timeCompo, tNodes).
!
! STScalar( :, : ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, dom%GetTotalNodes() )
! real2 = 1.0_DFP
! call obj%Set( value=real2 )
! call obj%display( "test-4: STScalar field = " )
!```

INTERFACE
  MODULE SUBROUTINE obj_Set4(obj, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
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
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: timeCompo
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
! This soubroutine Sets the selected enties to a STScalar entry value( : )
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
  MODULE SUBROUTINE obj_Set7(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
  MODULE SUBROUTINE obj_Set8(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
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
  MODULE SUBROUTINE obj_Set9(obj, VALUE, globalNode, timeCompo, scale, &
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
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
! STScalar( timeCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE obj_Set10(obj, VALUE, globalNode, timeCompo, scale, &
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: timeCompo
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
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
! summary: Set the STScalar values using triplet
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, istart, iend, stride, scale, &
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
! summary: Set the STScalar values
!
!# Introduction
! Set entries using FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Set13(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
! date: 24 Jan 2022
! summary: Set the STScalar values
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE obj_Set14(obj, VALUE, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Set the STScalarField

INTERFACE
  MODULE SUBROUTINE obj_Set15(obj, ivar, idof, VALUE, ivar_value, &
    & idof_value, scale, addContribution)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
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
! date: 24 Jan 2022
! summary: Set the STScalar values
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE obj_Set16(obj, VALUE)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(STScalarField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set16
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
  MODULE SUBROUTINE obj_Get1(obj, VALUE, globalNode, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine Get all the entries by using given STScalar field

INTERFACE
  MODULE SUBROUTINE obj_Get2(obj, VALUE)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  END SUBROUTINE obj_Get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get3(obj, VALUE, globalNode)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE obj_Get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get4(obj, VALUE, globalNode, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE obj_Get5(obj, VALUE, globalNode, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: timeCompo
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
    CLASS(STScalarField_), INTENT(IN) :: obj
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
  MODULE SUBROUTINE obj_Get7(obj, VALUE, istart, iend, stride, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE obj_Get8(obj, VALUE, globalNode)
    CLASS(STScalarField_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
  !! Nodal, Vector, SpaceTime
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
  MODULE SUBROUTINE obj_Get9(obj, VALUE, timeCompo)
    CLASS(STScalarField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE obj_Get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-29
! summary: Get values

INTERFACE
  MODULE SUBROUTINE obj_Get10(obj, ivar, idof, VALUE, ivar_value,  &
    & idof_value)
    CLASS(STScalarField_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE obj_Get10
END INTERFACE

!----------------------------------------------------------------------------
!                                                   GetFEVariable@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Set single entry

INTERFACE STScalarFieldGetFEVariable
  MODULE SUBROUTINE obj_GetFeVariable(obj, globalNode, VALUE, ivar)
    CLASS(STScalarField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: ivar
    !! This argument is not used
  END SUBROUTINE obj_GetFeVariable
END INTERFACE STScalarFieldGetFEVariable

!----------------------------------------------------------------------------
!                                                   GetPrefix@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-11-26
! summary:  Get prefix

INTERFACE
  MODULE FUNCTION obj_GetPrefix(obj) RESULT(ans)
    CLASS(STScalarField_), INTENT(IN) :: obj
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
  MODULE SUBROUTINE obj_ApplyDirichletBC1(obj, dbc, times)
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(DirichletBC_), INTENT(IN) :: dbc
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
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
    CLASS(STScalarField_), INTENT(INOUT) :: obj
    CLASS(DirichletBCPointer_), INTENT(IN) :: dbc(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: times(:)
  END SUBROUTINE obj_ApplyDirichletBC2
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
  MODULE FUNCTION obj_GetPointerOfComponent(obj, timeCompo) RESULT(ans)
    CLASS(STScalarField_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointerOfComponent
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
!
!----------------------------------------------------------------------------

END MODULE STScalarField_Class

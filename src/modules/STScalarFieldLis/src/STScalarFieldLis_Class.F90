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

MODULE STScalarFieldLis_Class
USE GlobalData, ONLY: DFP, I4B, LGT

USE AbstractField_Class, ONLY: AbstractField_
USE AbstractNodeField_Class, ONLY: AbstractNodeField_
USE STScalarField_Class, ONLY: STScalarField_
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class, ONLY: HDF5File_

USE FEDOF_Class, ONLY: FEDOF_, FEDOFPointer_

USE DirichletBC_Class, ONLY: DirichletBC_, DirichletBCPointer_

USE BaseType, ONLY: FEVariable_

IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "STScalarFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "STScalarField"

PUBLIC :: STScalarFieldLis_
PUBLIC :: STScalarFieldLisPointer_
PUBLIC :: STScalarFieldLis
PUBLIC :: STScalarFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                         STScalarFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-23
! summary: STScalar field for LIS_OMP engine
!
!{!pages/docs-api/STScalarFieldLis/STScalarFieldLis_.md}

TYPE, EXTENDS(STScalarField_) :: STScalarFieldLis_
CONTAINS
  PRIVATE

  ! CONSTRUCTOR:
  ! @ConstructorMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => obj_Initiate1
  !! Initiate an instance of STScalarFieldLis_
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => obj_Deallocate
  FINAL :: obj_Final

  ! IO:
  ! @IOMethods
  PROCEDURE, PUBLIC, PASS(obj) :: Display => obj_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => obj_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => obj_Export

  ! SET:
  ! @SetMethods
  PROCEDURE, PUBLIC, PASS(obj) :: SetSingle => obj_SetSingle
  PROCEDURE, PASS(obj) :: SetAll => obj_SetAll
  PROCEDURE, PASS(obj) :: SetMultiple => obj_SetMultiple

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
  PROCEDURE, PASS(obj) :: Set8 => obj_Set8
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set9 => obj_Set9
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set10 => obj_Set10
    !! Set values to a STScalar by using triplet
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set12 => obj_Set12
    !! Set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: Set13 => obj_Set13
    !! Set values using FEVariable
  PROCEDURE, PASS(obj) :: Set14 => obj_Set14
    !! Set values using FEVariable

  ! GET:
  ! @GetMethods

  PROCEDURE, PUBLIC, PASS(obj) :: Size => obj_Size
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => obj_GetSingle

  PROCEDURE, PASS(obj) :: GetMultiple1 => obj_GetMultiple1
  !! get many values from indices
  PROCEDURE, PASS(obj) :: GetMultiple2 => obj_GetMultiple2
  !! get many values from trides
  PROCEDURE, PASS(obj) :: GetMultiple3 => obj_GetMultiple3
  !! get many values from trides

  GENERIC, PUBLIC :: GetMultiple => GetMultiple1, GetMultiple2, &
    GetMultiple3

  PROCEDURE, PASS(obj) :: Get1 => obj_Get1
  PROCEDURE, PASS(obj) :: Get2 => obj_Get2
  PROCEDURE, PASS(obj) :: Get3 => obj_Get3
  PROCEDURE, PASS(obj) :: Get4 => obj_Get4
  PROCEDURE, PASS(obj) :: Get5 => obj_Get5
  PROCEDURE, PASS(obj) :: Get7 => obj_Get7
  PROCEDURE, PASS(obj) :: Get8 => obj_Get8
  !! Get the entries of STScalar field

  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => obj_GetPointer
  !! Get pointer
END TYPE STScalarFieldLis_

!----------------------------------------------------------------------------
!                                                     STScalarFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldLisPointer_
  CLASS(STScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE STScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                       STScalar@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-31
! summary:  function returns an instance of [[STScalarFieldLis_]]

INTERFACE STScalarFieldLis
  MODULE FUNCTION obj_Constructor1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    TYPE(STScalarFieldLis_) :: ans
  END FUNCTION obj_Constructor1
END INTERFACE STScalarFieldLis

!----------------------------------------------------------------------------
!                                        STScalarFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 2024-05-31
! summary: This function returns an instance of [[STScalarFieldLis_]]

INTERFACE STScalarFieldLis_Pointer
  MODULE FUNCTION obj_Constructor_1(param, fedof) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
    CLASS(STScalarFieldLis_), POINTER :: ans
  END FUNCTION obj_Constructor_1
END INTERFACE STScalarFieldLis_Pointer

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_Final(obj)
    TYPE(STScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the STScalarFieldLis_ object
!
!# Introduction
! This routine initiate the STScalar field object.
! `param` contains the information of parameters required to initiate the
! STScalar. There are essential and optional information.
! Essential information are described below.
! - `name`  character defining the name of STScalar field
! - `timeCompo` is the total degree of freedom or components
! - `fieldType` type of field type; FIELD_TYPE_CONSTANT, FIELD_TYPE_NORMAL

INTERFACE
  MODULE SUBROUTINE obj_Initiate1(obj, param, fedof)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    CLASS(FEDOF_), TARGET, INTENT(IN) :: fedof
  END SUBROUTINE obj_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                             Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the
! STScalarFieldLis_ obj

INTERFACE STScalarFieldLisDeallocate
  MODULE SUBROUTINE obj_Deallocate(obj)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE obj_Deallocate
END INTERFACE STScalarFieldLisDeallocate

!----------------------------------------------------------------------------
!                                                         Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STScalarFieldLis_]]

INTERFACE
  MODULE SUBROUTINE obj_Display(obj, msg, unitNo)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE obj_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                         Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE obj_Import(obj, hdf5, group, fedof, fedofs)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    CLASS(FEDOF_), TARGET, OPTIONAL, INTENT(IN) :: fedof
    TYPE(FEDOFPointer_), OPTIONAL, INTENT(IN) :: fedofs(:)
  END SUBROUTINE obj_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE obj_Export(obj, hdf5, group)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE obj_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_Size(obj, dims) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION obj_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetSingle(obj, indx, VALUE, scale, &
                                  addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetAll(obj, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE obj_SetMultiple(obj, indx, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE obj_SetMultiple
END INTERFACE

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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    !! obj = value
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    !! values to be set obj = value
    !! number of cols in value should be equal to obj%timeCompo
    !! number of rows in value should be equal to fedof%tdof
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
! date: 24 Jan 2022
! summary: Set the STScalar values
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE obj_Set12(obj, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(STScalarField_), INTENT(IN) :: VALUE
  END SUBROUTINE obj_Set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_GetSingle(obj, indx, VALUE)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE obj_GetSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple1(obj, indx, VALUE, tsize)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple2(obj, istart, iend, stride, VALUE, tsize)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: istart, iend, stride
    !! index, size(indx) = size(value) = tsize
    REAL(DFP), INTENT(OUT) :: VALUE(:)
    !! returned vlaue
    INTEGER(I4B), INTENT(OUT) :: tsize
    !! total number of data written in value
  END SUBROUTINE obj_GetMultiple2
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2024-06-02
! summary: Get multiple entries using trides

INTERFACE
  MODULE SUBROUTINE obj_GetMultiple3(obj, istart, iend, stride, VALUE, &
                                istart_value, iend_value, stride_value, tsize)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE(:)
    !! Value to be returned
    !! The size should be obj%timeCompo
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
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
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
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
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
  MODULE SUBROUTINE obj_Get7(obj, VALUE, timeCompo)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    !! space-time scalar field
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    !! returned value in AbstractNodeField format
    !! value can be an instance of
    !! ScalarField_, ScalarFieldLis_
    !!   In this case obj@timeCompo will be returned
    !! value can be an instance of
    !! STScalarField_, STScalarFieldLis_
    !!   In this case obj@timeCompo will be returned
    !!  in value@timeCompo
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
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: ivar
    INTEGER(I4B), INTENT(IN) :: idof
    INTEGER(I4B), INTENT(IN) :: ivar_value
    INTEGER(I4B), INTENT(IN) :: idof_value
  END SUBROUTINE obj_Get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION obj_GetPointer(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION obj_GetPointer
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE STScalarFieldLis_Class

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
USE GlobalData
USE BaseType
USE String_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE ScalarField_Class, ONLY: ScalarField_
USE STScalarField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "STScalarFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "STScalarField"

!----------------------------------------------------------------------------
!                                                              STScalarFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-23
! summary: STScalar field for LIS_OMP engine
!
!{!pages/STScalarFieldLis_.md}

TYPE, EXTENDS(STScalarField_) :: STScalarFieldLis_
#ifdef USE_LIS
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => stsField_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: Display => stsField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => stsField_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => stsField_Export
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => stsField_Deallocate
  FINAL :: stsField_Final
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => stsField_Norm2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => stsField_Norm1
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => stsField_Normi
  PROCEDURE, PUBLIC, PASS(obj) :: Size => stsField_Size
  !
  ! @SetMethods
  !
  PROCEDURE, PASS(obj) :: SetSingle => stsField_SetSingle
  PROCEDURE, PASS(obj) :: SetAll => stsField_SetAll
  PROCEDURE, PASS(obj) :: SetMultiple => stsField_SetMultiple
  PROCEDURE, PASS(obj) :: set1 => stsField_set1
    !! set single entry
  PROCEDURE, PASS(obj) :: set2 => stsField_set2
    !! set all values to a STScalar values
  PROCEDURE, PASS(obj) :: set3 => stsField_set3
    !! set all values to a given STScalar
  PROCEDURE, PASS(obj) :: set4 => stsField_set4
    !! set selected values to given STScalar
  PROCEDURE, PASS(obj) :: set5 => stsField_set5
    !! set selected values to given STScalar
  PROCEDURE, PASS(obj) :: set6 => stsField_set6
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: set7 => stsField_set7
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: set8 => stsField_set8
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: set9 => stsField_set9
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: set10 => stsField_set10
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: set11 => stsField_set11
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: set12 => stsField_set12
    !! set values to a STScalar by using triplet
  PROCEDURE, PASS(obj) :: set13 => stsField_set13
    !! set values using FEVariable
  PROCEDURE, PASS(obj) :: set14 => stsField_set14
    !! set values using FEVariable
  PROCEDURE, PASS(obj) :: get1 => stsField_get1
  PROCEDURE, PASS(obj) :: get2 => stsField_get2
  PROCEDURE, PASS(obj) :: get3 => stsField_get3
  PROCEDURE, PASS(obj) :: get4 => stsField_get4
  PROCEDURE, PASS(obj) :: get5 => stsField_get5
  PROCEDURE, PASS(obj) :: get6 => stsField_get6
  PROCEDURE, PASS(obj) :: get7 => stsField_get7
  PROCEDURE, PASS(obj) :: get8 => stsField_get8
  PROCEDURE, PASS(obj) :: get9 => stsField_get9
  !! get the entries of STScalar field
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointerOfComponent => &
    & stsField_GetPointerOfComponent
  !! Get pointer of component
  PROCEDURE, PUBLIC, PASS(obj) :: GetPointer => &
    & stsField_GetPointer
  !! Get pointer
#endif
END TYPE STScalarFieldLis_

PUBLIC :: STScalarFieldLis_
TYPE(STScalarFieldLis_), PARAMETER, PUBLIC :: TypeSTScalarField = &
  & STScalarFieldLis_(domains=NULL())

!----------------------------------------------------------------------------
!                                                     STScalarFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: STScalarFieldLisPointer_
  CLASS(STScalarFieldLis_), POINTER :: ptr => NULL()
END TYPE STScalarFieldLisPointer_

PUBLIC :: STScalarFieldLisPointer_

!----------------------------------------------------------------------------
!                                                       STScalar@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[STScalarFieldLis_]]

INTERFACE
  MODULE FUNCTION stsField_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(STScalarFieldLis_) :: ans
  END FUNCTION stsField_Constructor1
END INTERFACE

INTERFACE STScalarFieldLis
  MODULE PROCEDURE stsField_Constructor1
END INTERFACE STScalarFieldLis

PUBLIC :: STScalarFieldLis

!----------------------------------------------------------------------------
!                                         STScalarFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[STScalarFieldLis_]]

INTERFACE
  MODULE FUNCTION stsField_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(STScalarFieldLis_), POINTER :: ans
  END FUNCTION stsField_Constructor_1
END INTERFACE

INTERFACE STScalarFieldLis_Pointer
  MODULE PROCEDURE stsField_Constructor_1
END INTERFACE STScalarFieldLis_Pointer

PUBLIC :: STScalarFieldLis_Pointer

#ifdef USE_LIS

!----------------------------------------------------------------------------
!                                                         Final@Constructor
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE stsField_Final(obj)
    TYPE(STScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE stsField_Final
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
  MODULE SUBROUTINE stsField_Initiate1(obj, param, dom)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE stsField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the STScalarFieldLis_ obj

INTERFACE
  MODULE SUBROUTINE stsField_Deallocate(obj)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE stsField_Deallocate
END INTERFACE

INTERFACE STScalarFieldLisDeallocate
  MODULE PROCEDURE stsField_Deallocate
END INTERFACE STScalarFieldLisDeallocate

PUBLIC :: STScalarFieldLisDeallocate

!----------------------------------------------------------------------------
!                                                                 Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[STScalarFieldLis_]]

INTERFACE
  MODULE SUBROUTINE stsField_Display(obj, msg, unitNo)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE stsField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Import@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE stsField_Import(obj, hdf5, group, dom, domains)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE stsField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                                Export@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE stsField_Export(obj, hdf5, group)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE stsField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION stsField_Norm2(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION stsField_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION stsField_Norm1(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION stsField_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION stsField_Normi(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION stsField_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                   Size@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION stsField_Size(obj, dims) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION stsField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE stsField_setSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_setSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE stsField_setAll(obj, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_setAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE stsField_setMultiple(obj, indx, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_setMultiple
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the single entry of the STScalar field
!
!# Introduction
! This routine sets the single entry of the STScalar field. Here, val should
! be a STScalar representing the components of a STScalar. The size of `value`
! should be same as `obj%timeCompo`. In simple words it does following.
!
! STScalar( :, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call obj%set( globalNode = 10, value= 100.0_DFP*[1,1,1] )
! call obj%display( "test-1: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE stsField_set1(obj, globalNode, VALUE, scale, &
      & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STScalar field
!
!# Introduction
! This routine work as follows. The size of value should be same as
! obj%timeCompo, then this value is set for all the nodal values
!
! STScalar( :, i ) = value( : ), for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%set( value= 10.0_DFP*[1,1,1] )
! call obj%display( "test-2: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE stsField_set2(obj, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), TARGET, INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets all the entries of a STScalar field
!
!# Introduction
! This routine sets all values of `timeCompo` component of the STScalar field
! to given scalar value `value`
!
! STScalar( timeCompo, i ) = value, for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%set( value= -10.0_DFP, timeCompo=1 )
! call obj%set( value= -20.0_DFP, timeCompo=2 )
! call obj%set( value= -30.0_DFP, timeCompo=3 )
! call obj%display( "test-3: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE stsField_set3(obj, VALUE, timeCompo, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STScalar field
!
!# Introduction
! This routine set all entries of STScalar field to given STScalar
! Here shape of should be value(1:timeCompo, tNodes).
!
! STScalar( :, : ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, dom%getTotalNodes() )
! real2 = 1.0_DFP
! call obj%set( value=real2 )
! call obj%display( "test-4: STScalar field = " )
!```

INTERFACE
  MODULE SUBROUTINE stsField_set4(obj, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STScalar field
!
!# Introduction
! This routine set all entries of the component `timeCompo` STScalar
! field  to given fortran STScalar `value`
!
! STScalar( timeCompo, : ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, dom%getTotalNodes() )
! real1 = 3.0_DFP
! call obj%set( value=real1, timeCompo=3 )
! call obj%display( "test-5: STScalar field = " )
!```

INTERFACE
  MODULE SUBROUTINE stsField_set5(obj, VALUE, timeCompo, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine set all the entries by using given STScalar field
!
!# Introduction
! This routine set all entries of the component `timeCompo` STScalar
! field  to given scalar field `value`
!
! STScalar( timeCompo, : ) = value
!
!
!### Usage
!
!```fortran
! call scalarObj%initiate( param, dom )
! call scalarObj%set( value = 2.0_DFP )
! call obj%set( value=scalarObj, timeCompo=2 )
! call obj%display( "test-6: STScalar field = ")
! ierr = param%set( key="fieldType", value=FIELD_TYPE_CONSTANT)
! call scalarObj%Deallocate()
! call scalarObj%initiate( param, dom )
! call scalarObj%set( value=10.0_DFP )
! call obj%set( value=scalarObj, timeCompo=1 )
! call obj%display( "test-7: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE stsField_set6(obj, VALUE, timeCompo, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    CLASS(ScalarField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This soubroutine sets the selected enties to a STScalar entry value( : )
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
! call obj%set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE stsField_set7(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets all selected entries.
! STScalar( :, globalNode ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, 4)
! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
! real2( :, 4 ) = -4.0
! call obj%set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: STScalar field = ")
!```

INTERFACE
  MODULE SUBROUTINE stsField_set8(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets the selected components of selected nodes to given value
!
! STScalar( timeCompo, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, 4)
! real1 = [1,10,100,1000]
! call obj%set( value=real1, globalNode=[1,3,5,7], timeCompo=1 )
! call obj%display( "test-9: STScalar field = " )
!```

INTERFACE
  MODULE SUBROUTINE stsField_set9(obj, VALUE, globalNode, timeCompo, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! STScalar( timeCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE stsField_set10(obj, VALUE, globalNode, timeCompo, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries
!
!# Introduction
! Set entries using the selected nodes using triplet.
!

INTERFACE
  MODULE SUBROUTINE stsField_set11(obj, VALUE, istart, iend, stride, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: set the STScalar values using triplet
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE stsField_set12(obj, VALUE, istart, iend, stride, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set12
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
  MODULE SUBROUTINE stsField_set13(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 Jan 2022
! summary: set the STScalar values
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE stsField_set14(obj, VALUE, scale, addContribution)
    CLASS(STScalarFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE stsField_set14
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
  MODULE SUBROUTINE stsField_get1(obj, VALUE, globalNode, timeCompo)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: timeCompo
  END SUBROUTINE stsField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine get all the entries by using given STScalar field

INTERFACE
  MODULE SUBROUTINE stsField_get2(obj, VALUE)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
  END SUBROUTINE stsField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stsField_get3(obj, VALUE, globalNode)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE stsField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stsField_get4(obj, VALUE, globalNode, timeCompo)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stsField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE stsField_get5(obj, VALUE, globalNode, timeCompo)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stsField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
  MODULE SUBROUTINE stsField_get6(obj, VALUE, istart, iend, stride)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE stsField_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine sets the selected entries

INTERFACE
  MODULE SUBROUTINE stsField_get7(obj, VALUE, istart, iend, stride, timeCompo)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stsField_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE stsField_get8(obj, VALUE, globalNode)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
  !! Nodal, Vector, SpaceTime
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE stsField_get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE stsField_get9(obj, VALUE, timeCompo)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    CLASS(ScalarField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: timeCompo
  END SUBROUTINE stsField_get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine returns pointer to a specific component

INTERFACE
  MODULE FUNCTION stsField_getPointerOfComponent(obj, timeCompo) RESULT(ans)
    CLASS(STScalarFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: timeCompo
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION stsField_getPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!                                                     GetPointer@GetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION stsField_getPointer(obj) RESULT(ans)
    CLASS(STScalarFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION stsField_getPointer
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#endif
END MODULE STScalarFieldLis_Class

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

MODULE VectorFieldLis_Class
USE GlobalData
USE BaseType
USE String_Class
USE AbstractField_Class
USE AbstractNodeField_Class
USE VectorField_Class
USE ExceptionHandler_Class, ONLY: e
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
USE DirichletBC_Class
IMPLICIT NONE
PRIVATE
CHARACTER(*), PARAMETER :: modName = "VectorFieldLis_Class"
CHARACTER(*), PARAMETER :: myprefix = "VectorField"

!----------------------------------------------------------------------------
!                                                              VectorFieldLis_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: Vector field
!
!{!pages/VectorFieldLis_.md}

TYPE, EXTENDS(VectorField_) :: VectorFieldLis_
#ifdef USE_LIS
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: Size => vField_Size
  PROCEDURE, PUBLIC, PASS(obj) :: Norm2 => vField_Norm2
  PROCEDURE, PUBLIC, PASS(obj) :: Norm1 => vField_Norm1
  PROCEDURE, PUBLIC, PASS(obj) :: Normi => vField_Normi
  PROCEDURE, PUBLIC, PASS(obj) :: Initiate1 => vField_Initiate1
  PROCEDURE, PUBLIC, PASS(obj) :: DEALLOCATE => vField_Deallocate
  PROCEDURE, PUBLIC, PASS(obj) :: getPointer => &
    & vField_getPointer
  PROCEDURE, PUBLIC, PASS(obj) :: getPointerOfComponent => &
    & vField_getPointerOfComponent
  PROCEDURE, PUBLIC, PASS(obj) :: Display => vField_Display
  PROCEDURE, PUBLIC, PASS(obj) :: IMPORT => vField_Import
  PROCEDURE, PUBLIC, PASS(obj) :: Export => vField_Export
  FINAL :: vField_Final
  !
  ! @SetMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: setSingle => vField_setSingle
  PROCEDURE, PASS(obj) :: setAll => vField_setAll
  PROCEDURE, PASS(obj) :: setMultiple => vField_setMultiple
  PROCEDURE, PASS(obj) :: set1 => vField_set1
    !! set single entry
  PROCEDURE, PASS(obj) :: set2 => vField_set2
    !! set all values to a Vector values
  PROCEDURE, PASS(obj) :: set3 => vField_set3
    !! set all values to a given vector
  PROCEDURE, PASS(obj) :: set4 => vField_set4
    !! set selected values to given Vector
  PROCEDURE, PASS(obj) :: set5 => vField_set5
    !! set selected values to given vector
  PROCEDURE, PASS(obj) :: set6 => vField_set6
    !! set values to a Vector by using triplet
  PROCEDURE, PASS(obj) :: set7 => vField_set7
    !! set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: set8 => vField_set8
    !! set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: set9 => vField_set9
    !! set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: set10 => vField_set10
    !! set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: set11 => vField_set11
    !! set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: set12 => vField_set12
    !! set values to a vector by using triplet
  PROCEDURE, PASS(obj) :: set13 => vField_set13
  PROCEDURE, PASS(obj) :: set14 => vField_set14
    !! set selected values using FEVariable
  !
  ! @GetMethods
  !
  PROCEDURE, PUBLIC, PASS(obj) :: GetSingle => vField_getSingle
  PROCEDURE, PASS(obj) :: get1 => vField_get1
    !! returns the single entry
  PROCEDURE, PASS(obj) :: get2 => vField_get2
    !! returns all entries in rank2 array of real
  PROCEDURE, PASS(obj) :: get3 => vField_get3
    !! returns selected values in XiJ format
  PROCEDURE, PASS(obj) :: get4 => vField_get4
  PROCEDURE, PASS(obj) :: get5 => vField_get5
  PROCEDURE, PASS(obj) :: get6 => vField_get6
  PROCEDURE, PASS(obj) :: get7 => vField_get7
  PROCEDURE, PASS(obj) :: get8 => vField_get8
  PROCEDURE, PASS(obj) :: get9 => vField_get9
  PROCEDURE, PASS(obj) :: get10 => vField_get10
    !! get the entries of Vector field
#endif
END TYPE VectorFieldLis_

PUBLIC :: VectorFieldLis_
TYPE(VectorFieldLis_), PARAMETER, PUBLIC :: TypeVectorFieldLis =  &
  & VectorFieldLis_(domains=NULL())

!----------------------------------------------------------------------------
!                                                       VectorFieldLisPointer_
!----------------------------------------------------------------------------

TYPE :: VectorFieldLisPointer_
  CLASS(VectorFieldLis_), POINTER :: ptr => NULL()
END TYPE VectorFieldLisPointer_

PUBLIC :: VectorFieldLisPointer_

!----------------------------------------------------------------------------
!                                                         Vector@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[VectorFieldLis_]]

INTERFACE
  MODULE FUNCTION vField_Constructor1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    TYPE(VectorFieldLis_) :: ans
  END FUNCTION vField_Constructor1
END INTERFACE

INTERFACE VectorFieldLis
  MODULE PROCEDURE vField_Constructor1
END INTERFACE VectorFieldLis

PUBLIC :: VectorFieldLis

!----------------------------------------------------------------------------
!                                           VectorFieldLis_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary:         This function returns an instance of [[VectorFieldLis_]]

INTERFACE
  MODULE FUNCTION vField_Constructor_1(param, dom) RESULT(Ans)
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
    CLASS(VectorFieldLis_), POINTER :: ans
  END FUNCTION vField_Constructor_1
END INTERFACE

INTERFACE VectorFieldLis_Pointer
  MODULE PROCEDURE vField_Constructor_1
END INTERFACE VectorFieldLis_Pointer

PUBLIC :: VectorFieldLis_Pointer

#ifdef USE_LIS

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION vField_Norm2(obj) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION vField_Norm2
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION vField_Norm1(obj) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION vField_Norm1
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Norm2@BlasMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE FUNCTION vField_Normi(obj) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP) :: ans
  END FUNCTION vField_Normi
END INTERFACE

!----------------------------------------------------------------------------
!                                                    Size@ConstructorMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: Returns the size

INTERFACE
  MODULE FUNCTION vField_Size(obj, dims) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), OPTIONAL :: dims
    INTEGER(I4B) :: ans
  END FUNCTION vField_Size
END INTERFACE

!----------------------------------------------------------------------------
!                                                Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This subroutine initiates the VectorFieldLis_ object
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
  MODULE SUBROUTINE vField_Initiate1(obj, param, dom)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Domain_), TARGET, INTENT(IN) :: dom
  END SUBROUTINE vField_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                              Deallocate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary:  Deallocate the data stored inside the VectorFieldLis_ obj

INTERFACE
  MODULE SUBROUTINE vField_Deallocate(obj)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE vField_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  Final@ConstructorMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vField_Final(obj)
    TYPE(VectorFieldLis_), INTENT(INOUT) :: obj
  END SUBROUTINE vField_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: Display the content of [[VectorFieldLis_]]

INTERFACE
  MODULE SUBROUTINE vField_Display(obj, msg, unitNo)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    CHARACTER(*), INTENT(IN) :: msg
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitNo
  END SUBROUTINE vField_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine Imports the content

INTERFACE
  MODULE SUBROUTINE vField_Import(obj, hdf5, group, dom, domains)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
    TYPE(Domain_), TARGET, OPTIONAL, INTENT(IN) :: dom
    TYPE(DomainPointer_), TARGET, OPTIONAL, INTENT(IN) :: domains(:)
  END SUBROUTINE vField_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine Exports the content

INTERFACE
  MODULE SUBROUTINE vField_Export(obj, hdf5, group)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(HDF5File_), INTENT(INOUT) :: hdf5
    CHARACTER(*), INTENT(IN) :: group
  END SUBROUTINE vField_Export
END INTERFACE

!----------------------------------------------------------------------------
!                                                      SetSingle@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vField_setSingle(obj, indx, VALUE, scale, &
    & addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_setSingle
END INTERFACE

!----------------------------------------------------------------------------
!                                                          SetAll@SetMethods
!----------------------------------------------------------------------------

INTERFACE
  MODULE SUBROUTINE vField_setAll(obj, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_setAll
END INTERFACE

!----------------------------------------------------------------------------
!                                                     SetMultiple@SetMethods
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE vField_setMultiple(obj, indx, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: indx(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_setMultiple
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the single entry of the Vector field
!
!# Introduction
! This routine sets the single entry of the vector field. Here, val should
! be a vector representing the components of a vector. The size of `value`
! should be same as `obj%spaceCompo`. In simple words it does following.
!
! vector( :, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call obj%set( globalNode = 10, value= 100.0_DFP*[1,1,1] )
! call obj%display( "test-1: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE vField_set1(obj, globalNode, VALUE, &
    & scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets all the entries of a Vector field
!
!# Introduction
! This routine work as follows. The size of value should be same as
!  obj%spaceCompo, then this value is set for all the nodal values
!
! vector( :, i ) = value( : ), for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%set( value= 10.0_DFP*[1,1,1] )
! call obj%display( "test-2: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE vField_set2(obj, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), TARGET, INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set2
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets all the entries of a Vector field
!
!# Introduction
! This routine sets all values of `spaceCompo` component of the vector field
! to given scalar value `value`
!
! vector( spaceCompo, i ) = value, for i = 1, tNodes
!
!
!### Usage
!
!```fortran
! call obj%set( value= -10.0_DFP, spaceCompo=1 )
! call obj%set( value= -20.0_DFP, spaceCompo=2 )
! call obj%set( value= -30.0_DFP, spaceCompo=3 )
! call obj%display( "test-3: vector field = ")
!```

INTERFACE
 MODULE SUBROUTINE vField_set3(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine set all the entries by using given Vector field
!
!# Introduction
! This routine set all entries of vector field to given vector
! Here shape of should be value(1:spaceCompo, tNodes).
!
! vector( :, : ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, dom%getTotalNodes() )
! real2 = 1.0_DFP
! call obj%set( value=real2 )
! call obj%display( "test-4: vector field = " )
!```

INTERFACE
  MODULE SUBROUTINE vField_set4(obj, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set4
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine set all the entries by using given Vector field
!
!# Introduction
! This routine set all entries of the component `spaceCompo` vector
! field  to given fortran vector `value`
!
! vector( spaceCompo, : ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, dom%getTotalNodes() )
! real1 = 3.0_DFP
! call obj%set( value=real1, spaceCompo=3 )
! call obj%display( "test-5: vector field = " )
!```

INTERFACE
 MODULE SUBROUTINE vField_set5(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set5
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine set all the entries by using given Vector field
!
!# Introduction
! This routine set all entries of the component `spaceCompo` vector
! field  to given scalar field `value`
!
! vector( spaceCompo, : ) = value
!
!
!### Usage
!
!```fortran
! call scalarObj%initiate( param, dom )
! call scalarObj%set( value = 2.0_DFP )
! call obj%set( value=scalarObj, spaceCompo=2 )
! call obj%display( "test-6: vector field = ")
! ierr = param%set( key="fieldType", value=FIELD_TYPE_CONSTANT)
! call scalarObj%Deallocate()
! call scalarObj%initiate( param, dom )
! call scalarObj%set( value=10.0_DFP )
! call obj%set( value=scalarObj, spaceCompo=1 )
! call obj%display( "test-7: vector field = ")
!```

INTERFACE
 MODULE SUBROUTINE vField_set6(obj, VALUE, spaceCompo, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    CLASS(AbstractNodeField_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set6
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the selected entries
!
!# Introduction
! This soubroutine sets the selected enties to a vector entry value( : )
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
! call obj%set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: vector field = ")
!```

INTERFACE
 MODULE SUBROUTINE vField_set7(obj, VALUE, globalNode, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set7
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets all selected entries.
! vector( :, globalNode ) = value( :, : )
!
!
!### Usage
!
!```fortran
! call reallocate( real2, 3, 4)
! real2( :, 1 ) = -1.0; real2( :, 2 ) = -2.0; real2( :, 3 ) = -3.0
! real2( :, 4 ) = -4.0
! call obj%set( value=real2, globalNode=[1,3,5,7] )
! call obj%display( "test-8: vector field = ")
!```

INTERFACE
  MODULE SUBROUTINE vField_set8(obj, globalNode, VALUE, scale, &
    & addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
  !! value is in value(i,J) format.
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set8
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the selected entries
!
!# Introduction
! This routine sets the selected components of selected nodes to given value
!
! vector( spaceCompo, globalNode ) = value( : )
!
!
!### Usage
!
!```fortran
! call reallocate( real1, 4)
! real1 = [1,10,100,1000]
! call obj%set( value=real1, globalNode=[1,3,5,7], spaceCompo=1 )
! call obj%display( "test-9: vector field = " )
!```

INTERFACE
  MODULE SUBROUTINE vField_set9(obj, VALUE, globalNode, spaceCompo, scale, &
    & addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set9
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the selected entries
!
!# Introduction
! selected components, selected nodes
!
! vector( spaceCompo, globalNode ) = value

INTERFACE
  MODULE SUBROUTINE vField_set10(obj, VALUE, globalNode, spaceCompo, scale, &
    & addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set10
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the selected entries
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE vField_set11(obj, VALUE, istart, iend, stride, scale, &
    & addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), INTENT(IN) :: VALUE(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set11
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: set the vector values using triplet
!
!# Introduction
! Set entries using the selected nodes using triplet.

INTERFACE
  MODULE SUBROUTINE vField_set12(obj, VALUE, istart, iend, stride, scale, &
    & addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set12
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: set the values using FEVariable

INTERFACE
  MODULE SUBROUTINE vField_set13(obj, VALUE, globalNode, scale, &
    & addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    TYPE(FEVariable_), INTENT(IN) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set13
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Set@SetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: set the values using FEVariable

INTERFACE
  MODULE SUBROUTINE vField_set14(obj, VALUE, scale, addContribution)
    CLASS(VectorFieldLis_), INTENT(INOUT) :: obj
    REAL(DFP), INTENT(IN) :: VALUE
    REAL(DFP), OPTIONAL, INTENT(IN) :: scale
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: addContribution
  END SUBROUTINE vField_set14
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
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
  MODULE SUBROUTINE vField_get1(obj, VALUE, globalNode, spaceCompo)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: globalNode
    INTEGER(I4B), OPTIONAL, INTENT(IN) :: spaceCompo
  END SUBROUTINE vField_get1
END INTERFACE

!----------------------------------------------------------------------------
!                                                            Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine get all the entries by using given Vector field

INTERFACE
  MODULE SUBROUTINE vField_get2(obj, VALUE, force3D)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
  END SUBROUTINE vField_get2
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE vField_get3(obj, VALUE, globalNode, force3D)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    LOGICAL(LGT), OPTIONAL, INTENT(IN) :: force3D
  END SUBROUTINE vField_get3
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE vField_get4(obj, VALUE, globalNode, spaceCompo)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE vField_get4
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine returns the selected entries

INTERFACE
  MODULE SUBROUTINE vField_get5(obj, VALUE, globalNode, spaceCompo)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE vField_get5
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the selected entries

INTERFACE
  MODULE SUBROUTINE vField_get6(obj, VALUE, istart, iend, stride)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:, :)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
  END SUBROUTINE vField_get6
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine sets the selected entries

INTERFACE
  MODULE SUBROUTINE vField_get7(obj, VALUE, istart, iend, stride, spaceCompo)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    REAL(DFP), ALLOCATABLE, INTENT(INOUT) :: VALUE(:)
    INTEGER(I4B), INTENT(IN) :: istart
    INTEGER(I4B), INTENT(IN) :: iend
    INTEGER(I4B), INTENT(IN) :: stride
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE vField_get7
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine returns the selected entries in FEVariable

INTERFACE
  MODULE SUBROUTINE vField_get8(obj, VALUE, globalNode)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    TYPE(FEVariable_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: globalNode(:)
  END SUBROUTINE vField_get8
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE vField_get9(obj, VALUE, spaceCompo)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    CLASS(AbstractNodeField_), INTENT(INOUT) :: VALUE
    INTEGER(I4B), INTENT(IN) :: spaceCompo
  END SUBROUTINE vField_get9
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Get@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine return value in FEVariable

INTERFACE
  MODULE SUBROUTINE vField_get10(obj, VALUE)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    CLASS(VectorField_), INTENT(INOUT) :: VALUE
  END SUBROUTINE vField_get10
END INTERFACE

!----------------------------------------------------------------------------
!                                          getPointerOfComponent@GetMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine cannot be called

INTERFACE
  MODULE FUNCTION vField_getPointerOfComponent(obj, spaceCompo) RESULT(ans)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: spaceCompo
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION vField_getPointerOfComponent
END INTERFACE

!----------------------------------------------------------------------------
!                                                      getPointer@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-25
! summary: This routine cannot be called

INTERFACE
  MODULE FUNCTION vField_getPointer(obj) RESULT(ans)
    CLASS(VectorFieldLis_), TARGET, INTENT(IN) :: obj
    REAL(DFP), POINTER :: ans(:)
  END FUNCTION vField_getPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                       GetSingle@GetMethods
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date:  2023-03-28
! summary: Get single entry

INTERFACE
  MODULE SUBROUTINE vField_getSingle(obj, indx, VALUE)
    CLASS(VectorFieldLis_), INTENT(IN) :: obj
    INTEGER(I4B), INTENT(IN) :: indx
    REAL(DFP), INTENT(OUT) :: VALUE
  END SUBROUTINE vField_getSingle
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

#endif
END MODULE VectorFieldLis_Class

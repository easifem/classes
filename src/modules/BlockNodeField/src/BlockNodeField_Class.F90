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
! summary: Scalar field data type is defined

MODULE BlockNodeField_Class
USE GlobalData
USE BaseType
USE AbstractField_Class
USE AbstractNodeField_Class
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
USE HDF5File_Class
USE Domain_Class
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "BlockNodeField_Class"
TYPE( ExceptionHandler_ ) :: e

!----------------------------------------------------------------------------
!                                                           BlockNodeField_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This nodal field is designed for the multiphysics applications
!
!# Introduction
!
! [[BlockBlockNodeField_]] is a child of [[AbstractNodeField_]] class. This
! nodal field considers each component of the field as a nodal degree of
! freedom.

TYPE, EXTENDS( AbstractNodeField_ ) :: BlockNodeField_
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( obj ) :: addSurrogate => Block_addSurrogate
  PROCEDURE, PUBLIC, PASS( obj ) :: checkEssentialParam => Block_checkEssentialParam
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate1 => Block_initiate1
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate2 => Block_initiate2
  PROCEDURE, PUBLIC, PASS( obj ) :: initiate3 => Block_initiate3
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => Block_Display
  PROCEDURE, PUBLIC, PASS( obj ) :: DeallocateData => Block_DeallocateData
  FINAL :: Block_Final
  PROCEDURE, PUBLIC, PASS( obj ) :: Import => Block_Import
  PROCEDURE, PUBLIC, PASS( obj ) :: Export => Block_Export
END TYPE BlockNodeField_

PUBLIC :: BlockNodeField_

TYPE( BlockNodeField_ ), PARAMETER, PUBLIC :: TypeBlockNodeField = &
  & BlockNodeField_( domains=NULL() )

!----------------------------------------------------------------------------
!                                                    BlockNodeFieldPointer_
!----------------------------------------------------------------------------

TYPE :: BlockNodeFieldPointer_
  CLASS( BlockNodeField_ ), POINTER :: ptr => NULL()
END TYPE BlockNodeFieldPointer_

PUBLIC :: BlockNodeFieldPointer_

!----------------------------------------------------------------------------
!                                 setBlockNodeFieldParam@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 29 Sept 2021
! summary: Sets the essential parameters to construct an instance of block
! node field
!
!# Introduction
!
! This routine sets the essential parameters required for constructing the
! BlockNodeField_
!
! - `param` contains the parameters
! - the size of `name`, `spaceCompo`, `timeCompo` `fieldType` should be same
! - the size of `name` actually equal to the total number of physical var
! - `name` name of each physical variable
! - `spaceCompo` space components of each physical variable
! - `timeCompo` time components of each physical variable
! - `fieldType` of each physical variable

INTERFACE
MODULE SUBROUTINE setBlockNodeFieldParam( param, name, &
  & spaceCompo, timeCompo, fieldType )
  TYPE( ParameterList_ ), INTENT( INOUT ) :: param
  TYPE( String ), INTENT( IN ) :: name( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: spaceCompo( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: timeCompo( : )
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: fieldType( : )
END SUBROUTINE setBlockNodeFieldParam
END INTERFACE

PUBLIC :: setBlockNodeFieldParam

!----------------------------------------------------------------------------
!                                      addSurrogate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE Block_addSurrogate( obj, Userobj )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
  TYPE( ExceptionHandler_ ), INTENT( IN ) :: Userobj
END SUBROUTINE Block_addSurrogate
END INTERFACE

!----------------------------------------------------------------------------
!                                      checkEssentialParam@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine check the essential parameters in param.

INTERFACE
MODULE SUBROUTINE Block_checkEssentialParam( obj, param )
  CLASS( BlockNodeField_ ), INTENT( IN ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
END SUBROUTINE Block_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the BlockNodeField_ object
!
!# Introduction
! This routine initiate the [[BlockNodeField_]] object.
! `param` contains the information of parameters required to initiate the
! Block node field. There are essential and optional information inside the
! `param`.
!
!@note
! `param` should be constructed by calling [[BlockNodeField_Class::setBloclNodeFieldParam]] routine.
!@endnote

INTERFACE
MODULE SUBROUTINE Block_Initiate1( obj, param, dom )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( Domain_ ), TARGET, INTENT( IN ) :: dom
END SUBROUTINE Block_Initiate1
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the BlockNodeField_ object by copying
!
!# Introduction
! This routine initiate the [[BlockNodeField_]] object by copying

INTERFACE
MODULE SUBROUTINE Block_Initiate2( obj, obj2, copyFull, copyStructure, usePointer )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
  CLASS( AbstractField_ ), INTENT( INOUT ) :: obj2
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyFull
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: copyStructure
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: usePointer
END SUBROUTINE Block_Initiate2
END INTERFACE

!----------------------------------------------------------------------------
!                                                 Initiate@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This subroutine initiates the BlockNodeField_ object
!
!# Introduction
!
! This routine initiate the [[BlockNodeField_]] object.
! `param` contains the information of parameters required to initiate the
! instance of BlockNodeField_ .
!
! - It is better to make `param` by calling
! [[BlockNodeField_::setBlockNodeFieldParam]]
! - The size of `dom` should be equal to the number of physical variables
! present in the block node field.
! - `dom` contains the pointer to [[Domain_]] class.

INTERFACE
MODULE SUBROUTINE Block_Initiate3( obj, param, dom )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( DomainPointer_ ), TARGET, INTENT( IN ) :: dom(:)
END SUBROUTINE Block_Initiate3
END INTERFACE

!----------------------------------------------------------------------------
!                                           DeallocateData@ConstructorMethod
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 25 June 2021
! summary: This routine deallocates the data stored inside the BlockNodeField_ obj

INTERFACE
MODULE SUBROUTINE Block_DeallocateData( obj )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Block_DeallocateData
END INTERFACE

INTERFACE DeallocateData
  MODULE PROCEDURE Block_DeallocateData
END INTERFACE DeallocateData

PUBLIC :: DeallocateData

!----------------------------------------------------------------------------
!                                                    Final@ConstructorMethod
!----------------------------------------------------------------------------

INTERFACE
MODULE SUBROUTINE Block_Final( obj )
  TYPE( BlockNodeField_ ), INTENT( INOUT ) :: obj
END SUBROUTINE Block_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Display@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 26 June 2021
! summary: Display the content of [[BlockNodeField_]]

INTERFACE
MODULE SUBROUTINE Block_Display( obj, msg, unitNo )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: msg
  INTEGER( I4B ), OPTIONAL, INTENT( IN ) :: unitNo
END SUBROUTINE Block_Display
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Import@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Imports the content

INTERFACE
MODULE SUBROUTINE Block_Import( obj, hdf5, group, dom, domains )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
  TYPE( Domain_ ), TARGET, OPTIONAL, INTENT( IN ) :: dom
  TYPE( DomainPointer_ ), TARGET, OPTIONAL, INTENT( IN ) :: domains( : )
END SUBROUTINE Block_Import
END INTERFACE

!----------------------------------------------------------------------------
!                                                           Export@IOMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 16 July 2021
! summary: This routine Exports the content

INTERFACE
MODULE SUBROUTINE Block_Export( obj, hdf5, group )
  CLASS( BlockNodeField_ ), INTENT( INOUT ) :: obj
  TYPE( HDF5File_ ), INTENT( INOUT ) :: hdf5
  CHARACTER( LEN = * ), INTENT( IN ) :: group
END SUBROUTINE Block_Export
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE BlockNodeField_Class
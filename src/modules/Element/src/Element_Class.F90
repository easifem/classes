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
!

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	This module defines finite element class
MODULE Element_Class
USE GlobalData
USE BaseType
USE String_Class, ONLY:String
USE ExceptionHandler_Class, ONLY: ExceptionHandler_
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE

CHARACTER( LEN=* ), PARAMETER :: modName="ELEMENT_CLASS"
TYPE( ExceptionHandler_ ), SAVE, PUBLIC :: eElement
!$OMP THREADPRIVATE(eElement)

!----------------------------------------------------------------------------
!                                                                  Element_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	FiniteElement Datat type
!
!{!pages/Element.md}

TYPE :: Element_
  PRIVATE
  INTEGER( I4B ), ALLOCATABLE :: Nptrs( : )
    !! Node pointers for an element
  INTEGER( I4B ) :: MAT_Type = 0_I4B
    !! Material type of an element
  CLASS( ReferenceElement_ ), POINTER :: refelem => NULL( )
    !! Pointer to the ReferenceElement

  CONTAINS
  ! Constructor
  GENERIC, PUBLIC :: Initiate => elem_init_from_fpl, elem_init_from_elem
    !! Generic method for initiating the instance of [[Element_]]
  GENERIC, PUBLIC :: OPERATOR( .Nptrs. ) => getNptrs
    !! Returns Node numbers
  GENERIC, PUBLIC :: ASSIGNMENT( = ) => elem_init_from_elem

  PROCEDURE, PUBLIC, PASS( obj ) :: elem_init_from_fpl
    !! Initiate element from FPL
  PROCEDURE, PUBLIC, PASS( obj ) :: elem_init_from_elem
    !! Initiate element from another [[Element_]]
  PROCEDURE, PUBLIC, PASS( obj ) :: setMaterialType => elem_setMaterialType
    !! Set Material Type
  PROCEDURE, PUBLIC, PASS( obj ) :: getMaterialType => elem_getMaterialType
    !! Set Material Type
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => elem_Deallocate
  PROCEDURE, PUBLIC, PASS( Obj ) :: DeallocateElement => elem_Deallocate
    !! Deallocate
  FINAL :: elem_final
    !! Finalize for element
  PROCEDURE, PUBLIC, PASS( obj ) :: isBoundaryElement => elem_isBoundaryElement
    !! Returns true if [[Element_]] is a boundary element
  PROCEDURE, PUBLIC, PASS( obj ) :: getNptrs => elem_getNptrs
    !! Returns Nptrs of [[Element_]]
  PROCEDURE, PUBLIC, PASS( obj ) :: setNptrs => elem_setNptrs
    !! Set the Nptrs of [[Element_]]
  PROCEDURE, PUBLIC, PASS( obj ) :: getRefElemPointer => elem_getRefElemPointer
    !! get the pointer to reference element
  PROCEDURE, PUBLIC, PASS( obj ) :: setRefElemPointer => elem_setRefElemPointer
    !! set the pointer to reference element
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => elem_display
    !! Displays the content of [[Element_]]
END TYPE Element_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: Element_
TYPE( Element_ ), PARAMETER, PUBLIC :: &
  & TypeElement = Element_( Nptrs = NULL( ), refelem = NULL( ) )

!----------------------------------------------------------------------------
!                                                           ElementPointer_
!----------------------------------------------------------------------------

TYPE :: ElementPointer_
  CLASS( Element_ ), POINTER :: Ptr => NULL( )
END TYPE ElementPointer_

PUBLIC :: ElementPointer_

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Initiate an instance of [[Element_]] from FPL
!
!# Introduction
! Initiate an instance of [[Element_]] from [[FPL_]]
!
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! type( Element_ ) :: obj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! call obj%initiate( param=param, refelem = refelem )
! call obj%display( "test-1: obj = " )
!```

INTERFACE
MODULE SUBROUTINE elem_init_from_fpl( obj, param, refelem )
  CLASS( Element_ ), INTENT( INOUT ) :: obj
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: refelem
END SUBROUTINE elem_init_from_fpl
END INTERFACE

!----------------------------------------------------------------------------
!                                                      Initiate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Initiate an instance of [[Element_]] from another instance
!
!# Introduction
! This subroutine initiate an instance of [[Element_]] with other another instance.
!
! This subroutine is used for defining assignment operation.
!
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! type( Element_ ) :: obj, anotherobj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! call obj%initiate( param=param, refelem = refelem )
! call obj%display( "test-1: obj = " )
! call pass( 'elem_init_from_fpl()' )
! call anotherobj%initiate(obj) !<-----------
! call anotherobj%display("test-1: anotherobj = ")
! call pass( 'elem_init_from_elem' )
!```

INTERFACE
MODULE SUBROUTINE elem_init_from_elem( obj, anotherobj  )
  CLASS( Element_ ), INTENT( INOUT ) :: obj
  CLASS( Element_ ), INTENT( IN ) :: anotherobj
END SUBROUTINE elem_init_from_elem
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Element@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Construct an instance of [[Element_]] from FPL
!
!# Introduction
! 	Returns an instance of [[Element_]] from [[ParameterList_]]
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj, anotherobj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! allocate( Element_::obj )
! obj = Element( param=param, refelem = refelem ) !<-----------
! call obj%display( "test-3:" )
! call pass('elem_init_from_fpl()')
! allocate( Element_::anotherobj )
! anotherobj = Element(obj)
! call anotherobj%display("test-3")
! call pass( "elem_init_from_elem()" )
!```

INTERFACE
MODULE FUNCTION Constructor1( param, refelem ) RESULT( ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: refelem
  TYPE( Element_ ) :: ans
END FUNCTION Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                       Element@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Construct an instance of [[Element_]]
!
!# Introduction
! 	Constructing an instance of [[Element_]] from another instance of [[Element_]] or any of its child
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj, anotherobj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! allocate( Element_::obj )
! obj = Element( param=param, refelem = refelem )
! call obj%display( "test-3:" )
! call pass('elem_init_from_fpl()')
! allocate( Element_::anotherobj )
! anotherobj = Element(obj) !<---------
! call anotherobj%display("test-3")
! call pass( "elem_init_from_elem()" )
!```

INTERFACE
MODULE FUNCTION Constructor2( anotherobj ) RESULT( ans )
  CLASS( Element_ ), TARGET, INTENT( IN ) :: anotherobj
  TYPE( Element_ ) :: ans
END FUNCTION Constructor2
END INTERFACE

INTERFACE Element
  MODULE PROCEDURE Constructor1, Constructor2
END INTERFACE Element

PUBLIC :: Element

!----------------------------------------------------------------------------
!                                                            Element_Pointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[Element_]]
!
!# Introduction
! 	Returns a pointer to an instance of [[Element_]]
!
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj, anotherobj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! obj => Element_Pointer( param=param, refelem = refelem ) !<----------
! call obj%display( "test-4:" )
! call pass('elem_init_from_fpl()')
! allocate( Element_::anotherobj )
! anotherobj => Element_Pointer(obj)
! call anotherobj%display("test-4")
! call pass( "elem_init_from_elem()" )
!```

INTERFACE
MODULE FUNCTION Constructor_1( param, refelem ) RESULT( ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: refelem
  CLASS( Element_ ), POINTER :: ans
END FUNCTION Constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                                             Element_Pointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[Element_]]
!
!# Introduction
! 	Returns a pointer to a newly constructed instance of [[Element_]] from another instance of [[Element_]] or any of its child
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj, anotherobj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! obj => Element_Pointer( param=param, refelem = refelem )
! call obj%display( "test-4:" )
! call pass('elem_init_from_fpl()')
! allocate( Element_::anotherobj )
! anotherobj => Element_Pointer(obj) !<--------
! call anotherobj%display("test-4")
! call pass( "elem_init_from_elem()" )
!```

INTERFACE
MODULE FUNCTION Constructor_2( anotherobj ) RESULT( ans )
  CLASS( Element_ ), TARGET, INTENT( IN ) :: anotherobj
  CLASS( Element_ ), POINTER :: ans
END FUNCTION Constructor_2
END INTERFACE

!----------------------------------------------------------------------------
!                                               Element_Pointer@Constructor
!----------------------------------------------------------------------------

INTERFACE Element_Pointer
  MODULE PROCEDURE Constructor_1, Constructor_2
END INTERFACE Element_Pointer

PUBLIC :: Element_Pointer

!----------------------------------------------------------------------------
!                                                 setMaterialType@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	set the material property of an element
!
!# Introduction
! 	Set the material property
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! obj => Element_Pointer( param=param, refelem = refelem )
! call obj%setMaterialType(2) !<----------
! call obj%setNptrs([3,4])
! call ok( ALL(obj%getNptrs() == [3,4]), 'getNptrs')
! call ok( obj%getMaterialType() == 2 , 'getNptrs')
! call obj%display( "test-5:" )
!```

INTERFACE
MODULE PURE SUBROUTINE elem_setMaterialType( obj, MatType  )
  CLASS( Element_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: MatType
END SUBROUTINE elem_setMaterialType
END INTERFACE

!----------------------------------------------------------------------------
!                                                 setMaterialType@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	get the material property of an element
!
!# Introduction
! 	get The material property
!
!### Usage
!
!```fortran

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	set the material property of an element
!
!# Introduction
! 	Set the material property
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! obj => Element_Pointer( param=param, refelem = refelem )
! call obj%setMaterialType(2)
! call obj%setNptrs([3,4])
! call ok( ALL(obj%getNptrs() == [3,4]), 'getNptrs')
! call ok( obj%getMaterialType() == 2 , 'getNptrs') !<------
! call obj%display( "test-5:" )
!```

INTERFACE
MODULE PURE FUNCTION elem_getMaterialType( obj ) RESULT(ans)
  CLASS( Element_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION elem_getMaterialType
END INTERFACE

!----------------------------------------------------------------------------
!                                                Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Deallocate the data stored inside [[Element_]]
!
!# Introduction
! 	Deallocate Data

INTERFACE
MODULE PURE SUBROUTINE elem_Deallocate( obj )
  CLASS( Element_ ), INTENT( INOUT ) :: obj
END SUBROUTINE elem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                     Elem_Final@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Finalize method for [[Element_]]

INTERFACE
MODULE SUBROUTINE elem_final( obj )
  TYPE( Element_ ), INTENT( INOUT ) :: obj
END SUBROUTINE elem_Final
END INTERFACE

!----------------------------------------------------------------------------
!                                              isBoundaryElement@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns true if the element is a boundary element
!
!# Introduction
! 	Returns true if an element is a boundary element.

INTERFACE
MODULE PURE FUNCTION elem_isBoundaryElement( obj, NSD ) RESULT( ans )
  CLASS( Element_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: NSD
  LOGICAL( LGT ) :: ans
END FUNCTION elem_isBoundaryElement
END INTERFACE

!----------------------------------------------------------------------------
!                                                        getNptrs@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the node numbers and connectivity
!
!# Introduction
! 	Returns the Nptrs
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! obj => Element_Pointer( param=param, refelem = refelem )
! call obj%setMaterialType(2)
! call obj%setNptrs([3,4])
! call ok( ALL(obj%getNptrs() == [3,4]), 'getNptrs')
! call ok( obj%getMaterialType() == 2 , 'getNptrs')
! call obj%display( "test-5:" )
!```

INTERFACE
MODULE PURE FUNCTION elem_getNptrs( obj ) RESULT( Nptrs )
  CLASS( Element_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE ::  Nptrs( : )
END FUNCTION elem_getNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                      setNptrs@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Set the node number and connnectivity
!
!# Introduction
! 	Set the Nptrs
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! obj => Element_Pointer( param=param, refelem = refelem )
! call obj%setMaterialType(2)
! call obj%setNptrs([3,4])
! call ok( ALL(obj%getNptrs() == [3,4]), 'getNptrs')
! call ok( obj%getMaterialType() == 2 , 'getNptrs')
! call obj%display( "test-5:" )
!```
INTERFACE
MODULE PURE SUBROUTINE elem_setNptrs( obj, Nptrs )
  CLASS( Element_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: Nptrs( : )
END SUBROUTINE elem_setNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                         getRefElemPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 April 2021
! summary: 	Returns the pointer to refelem [[ReferenceElement_]]

INTERFACE
MODULE FUNCTION elem_getRefElemPointer( obj ) RESULT( ans )
  CLASS( Element_ ), INTENT( IN ) :: obj
  CLASS( ReferenceElement_ ), POINTER :: ans
END FUNCTION elem_getRefElemPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                          setRefElemPointer
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 April 2021
! summary: 	set the pointer to refelem [[ReferenceElement_]]

INTERFACE
MODULE SUBROUTINE elem_setRefElemPointer( obj, refelem )
  CLASS( Element_ ), INTENT( INOUT ) :: obj
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: refelem
END SUBROUTINE elem_setRefElemPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	display the content of [[Element_]]
!
!# Introduction
! 	Displays the content of [[Element_]]
!
!### Usage
!
!```fortran
! class( ReferenceElement_ ), pointer :: refelem
! class( Element_ ), pointer :: obj
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! refelem => ReferenceLine_Pointer( NSD = 1 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! obj => Element_Pointer( param=param, refelem = refelem )
! call obj%setMaterialType(2)
! call obj%setNptrs([3,4])
! call ok( ALL(obj%getNptrs() == [3,4]), 'getNptrs')
! call ok( obj%getMaterialType() == 2 , 'getNptrs')
! call obj%display( "test-5:" )
!```

INTERFACE
MODULE SUBROUTINE elem_display( obj, msg, UnitNo, FullDisp )
  CLASS( Element_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: FullDisp
END SUBROUTINE elem_display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE Element_Class
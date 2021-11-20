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
! summary: 	[[FacetElement_]] is defined

MODULE FacetElement_Class
USE GlobalData
USE BaseType
USE Element_Class
USE FPL, ONLY: ParameterList_
IMPLICIT NONE
PRIVATE

CHARACTER( LEN=* ), PARAMETER :: modName="FACETELEMENT_CLASS"

!----------------------------------------------------------------------------
!                                                              FacetElement_
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Facet Element is defined
!
!{!pages/FacetElement.md}

TYPE, EXTENDS( Element_ ) :: FacetElement_
  PRIVATE
  INTEGER( I4B ) :: LocalID = 0_I4B
  CLASS( Element_ ), POINTER :: Cell => NULL( )
  CLASS( Element_ ), POINTER :: OuterCell => NULL( )

  CONTAINS
  ! Constructor
  PROCEDURE, PUBLIC, PASS( obj ) :: getCellNptrs => faceElem_getCellNptrs
  PROCEDURE, PUBLIC, PASS( obj ) :: setCellNptrs => faceElem_setCellNptrs
  PROCEDURE, PUBLIC, PASS( obj ) :: getCellPointer => faceElem_getCellPointer
  PROCEDURE, PUBLIC, PASS( obj ) :: setCellPointer => faceElem_setCellPointer
  PROCEDURE, PUBLIC, PASS( obj ) :: FreeCellPointer => faceElem_freeCellPointer
  PROCEDURE, PUBLIC, PASS( obj ) :: getFacetLocalID => faceElem_getFacetLocalID
  PROCEDURE, PUBLIC, PASS( obj ) :: setFacetLocalID => faceElem_setFacetLocalID
  PROCEDURE, PUBLIC, PASS( obj ) :: getFacetLocalNptrs => faceElem_getFacetLocalNptrs
  PROCEDURE, PUBLIC, PASS( obj ) :: Display => faceElem_display
  FINAL :: faceElem_Deallocate
  PROCEDURE, PUBLIC, PASS( obj ) :: Deallocate => elem_Deallocate
END TYPE FacetElement_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

PUBLIC :: FacetElement_
TYPE( FacetElement_ ), PARAMETER, PUBLIC :: &
  & TypeFacetElement = FacetElement_( LocalID = 0_I4B )

!----------------------------------------------------------------------------
!                                                        FacetElementPointer_
!----------------------------------------------------------------------------

TYPE :: FacetElementPointer_
  CLASS( FacetElementPointer_ ), POINTER :: Ptr => NULL( )
END TYPE FacetElementPointer_

PUBLIC :: FacetElementPointer_

!----------------------------------------------------------------------------
!                                                   FacetElement@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns an instance of [[FacetElement_]]
!
!# Introduction
! 	Returns an instance of [[FacetElement_]] from [[ParameterList_]]
!
!
!### Usage
!
!```fortran
! class( element_ ), pointer :: cell, obj
! class( ReferenceElement_ ), pointer :: refelem
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! ! Reference Element
! refelem => ReferenceTriangle_Pointer( NSD = 2 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! ! Cell Element
! cell => Element_Pointer( param=param, refelem=refelem )
! call cell%display('test-1: cell elem:')
! ! Facet element
! nptrs = [2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! refelem => ReferenceLine_Pointer( NSD = 2 )
! obj => FacetElement_Pointer(param=param, refelem=refelem)
! select type( obj )
! type is (FacetElement_)
!   call obj%setCellPointer(cell)
!   call obj%setFacetLocalID(2)
! end select
! call obj%display('test-1: facet elem:')
! call param%free()
! call FPL_FINALIZE()
!```

INTERFACE
MODULE FUNCTION Constructor1( param, refelem ) RESULT( ans )
  CLASS( ReferenceElement_ ), TARGET, INTENT( INOUT ) :: refelem
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  TYPE( FacetElement_ ) :: ans
END FUNCTION Constructor1
END INTERFACE

!----------------------------------------------------------------------------
!                                                   FacetElement@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns an instance of [[FacetElement_]]
!
!# Introduction
! 	Constructing an instance of [[FacetElement_]] from another instance of [[Element_]] or any of its child
!
!### Usage
!
!```fortran
! class( element_ ), pointer :: cell, obj
! class( ReferenceElement_ ), pointer :: refelem
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! ! Reference Element
! refelem => ReferenceTriangle_Pointer( NSD = 2 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! ! Cell Element
! cell => Element_Pointer( param=param, refelem=refelem )
! call cell%display('test-1: cell elem:')
! ! Facet element
! nptrs = [2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! refelem => ReferenceLine_Pointer( NSD = 2 )
! obj => FacetElement_Pointer(param=param, refelem=refelem)
! select type( obj )
! type is (FacetElement_)
!   call obj%setCellPointer(cell)
!   call obj%setFacetLocalID(2)
! end select
! call obj%display('test-1: facet elem:')
! call param%free()
! call FPL_FINALIZE()
!```

INTERFACE
MODULE FUNCTION Constructor2( anotherobj ) RESULT( ans )
  CLASS( Element_ ), TARGET, INTENT( IN ) :: anotherobj
  TYPE( FacetElement_ ) :: ans
END FUNCTION Constructor2
END INTERFACE

INTERFACE FacetElement
  MODULE PROCEDURE Constructor1, Constructor2
END INTERFACE

PUBLIC :: FacetElement

!----------------------------------------------------------------------------
!                                           FacetElement_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[FacetElement_]]
!
!### Usage
!
!```fortran
! class( element_ ), pointer :: cell, obj
! class( ReferenceElement_ ), pointer :: refelem
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! ! Reference Element
! refelem => ReferenceTriangle_Pointer( NSD = 2 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! ! Cell Element
! cell => Element_Pointer( param=param, refelem=refelem )
! call cell%display('test-1: cell elem:')
! ! Facet element
! nptrs = [2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! refelem => ReferenceLine_Pointer( NSD = 2 )
! obj => FacetElement_Pointer(param=param, refelem=refelem)
! select type( obj )
! type is (FacetElement_)
!   call obj%setCellPointer(cell)
!   call obj%setFacetLocalID(2)
! end select
! call obj%display('test-1: facet elem:')
! call param%free()
! call FPL_FINALIZE()
!```

INTERFACE
MODULE FUNCTION Constructor_1( param, refelem ) RESULT( ans )
  TYPE( ParameterList_ ), INTENT( IN ) :: param
  CLASS( ReferenceElement_ ), TARGET, INTENT( IN ) :: refelem
  CLASS( FacetElement_ ), POINTER :: ans
END FUNCTION Constructor_1
END INTERFACE

!----------------------------------------------------------------------------
!                                           FacetElement_Pointer@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[FacetElement_]]
!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns a pointer to an instance of [[FacetElement_]]
!
!### Usage
!
!```fortran
! class( element_ ), pointer :: cell, obj
! class( ReferenceElement_ ), pointer :: refelem
! type( ParameterList_ ) :: param
! integer( I4B ), ALLOCATABLE :: nptrs(:)
! integer( I4B ) :: ierr
! ! Reference Element
! refelem => ReferenceTriangle_Pointer( NSD = 2 )
! call FPL_INIT()
! call param%init()
! nptrs = [1,2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! ! Cell Element
! cell => Element_Pointer( param=param, refelem=refelem )
! call cell%display('test-1: cell elem:')
! ! Facet element
! nptrs = [2,3]
! ierr = param%set(key='nptrs', value=nptrs)
! ierr = param%set(key="mat_type", value=1)
! refelem => ReferenceLine_Pointer( NSD = 2 )
! obj => FacetElement_Pointer(param=param, refelem=refelem)
! select type( obj )
! type is (FacetElement_)
!   call obj%setCellPointer(cell)
!   call obj%setFacetLocalID(2)
! end select
! call obj%display('test-1: facet elem:')
! call param%free()
! call FPL_FINALIZE()
!```

INTERFACE
MODULE FUNCTION Constructor_2( anotherobj ) RESULT( ans )
  CLASS( FacetElement_ ), TARGET, INTENT( IN ) :: anotherobj
  CLASS( FacetElement_ ), POINTER :: ans
END FUNCTION Constructor_2
END INTERFACE

INTERFACE FacetElement_Pointer
  MODULE PROCEDURE Constructor_1, Constructor_2
END INTERFACE

PUBLIC :: FacetElement_Pointer

!----------------------------------------------------------------------------
!                                                 Deallocate@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Deallocate the memeory occupied by [[FacetElement_]]

INTERFACE
MODULE PURE SUBROUTINE elem_Deallocate( obj )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: obj
END SUBROUTINE elem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                          Final@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	17 April 2021
! summary: 	finalizer for [[FacetElement_]]

INTERFACE
MODULE SUBROUTINE faceElem_Deallocate( obj )
  TYPE( FacetElement_ ), INTENT( INOUT ) :: obj
END SUBROUTINE faceElem_Deallocate
END INTERFACE

!----------------------------------------------------------------------------
!                                                  getCellNptrs@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the node number of cell element

INTERFACE
MODULE PURE FUNCTION faceElem_getCellNptrs( obj ) RESULT( ans )
  CLASS( FacetElement_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: ans( : )
END FUNCTION faceElem_getCellNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                  setCellNptrs@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the node number of cell element

INTERFACE
MODULE PURE SUBROUTINE faceElem_setCellNptrs( obj, nptrs )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: nptrs( : )
END SUBROUTINE faceElem_setCellNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                               getPointerToCell@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the pointer to cell

INTERFACE
MODULE FUNCTION faceElem_getCellPointer( obj ) RESULT( ans )
  CLASS( FacetElement_ ), INTENT( IN ), TARGET :: obj
  CLASS( Element_ ), POINTER :: ans
END FUNCTION faceElem_getCellPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                               SetPointerToCell@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Sets the pointer to cell element

INTERFACE
MODULE PURE SUBROUTINE faceElem_setCellPointer( obj, cell )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: obj
  CLASS( Element_ ), INTENT( INOUT ), TARGET :: cell
END SUBROUTINE faceElem_setCellPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                              FreePointerToCell@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Free the pointer to cell

INTERFACE
MODULE PURE SUBROUTINE faceElem_freeCellPointer( obj )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: obj
END SUBROUTINE faceElem_freeCellPointer
END INTERFACE

!----------------------------------------------------------------------------
!                                                getFacetLocalID@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the facet local ID

INTERFACE
MODULE PURE FUNCTION faceElem_getFacetLocalID( obj ) RESULT( ans )
  CLASS( FacetElement_ ), INTENT( IN ) :: obj
  INTEGER( I4B ) :: ans
END FUNCTION faceElem_getFacetLocalID
END INTERFACE

!----------------------------------------------------------------------------
!                                                getFacetLocalID@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: 	Returns the facet local ID

INTERFACE
MODULE PURE SUBROUTINE faceElem_setFacetLocalID( obj, id )
  CLASS( FacetElement_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: id
END SUBROUTINE faceElem_setFacetLocalID
END INTERFACE

!----------------------------------------------------------------------------
!                                             getFacetLocalNptrs@Constructor
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 24 March 2021
! summary: Returns the Local node number of facet element

INTERFACE
MODULE FUNCTION faceElem_getFacetLocalNptrs( obj ) RESULT( nptrs )
  CLASS( FacetElement_ ), INTENT( IN ) :: obj
  INTEGER( I4B ), ALLOCATABLE :: nptrs( : )
END FUNCTION faceElem_getFacetLocalNptrs
END INTERFACE

!----------------------------------------------------------------------------
!                                                               Display@IO
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 	24 March 2021
! summary: Displays content of [[FacetElement_]]

INTERFACE
MODULE SUBROUTINE faceElem_display( obj, msg, UnitNo, FullDisp )
  CLASS( FacetElement_ ), INTENT( IN ) :: obj
  CHARACTER( LEN = * ), INTENT( IN ) :: Msg
  INTEGER( I4B ), INTENT( IN ), OPTIONAL :: UnitNo
  LOGICAL( LGT ), OPTIONAL, INTENT( IN ) :: FullDisp
END SUBROUTINE faceElem_display
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE FacetElement_Class

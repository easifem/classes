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
! summary: 	Constructor methods for [[Element_]]


SUBMODULE( Element_Class ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_init_from_fpl
  INTEGER( I4B ), allocatable :: s( : )
  INTEGER( I4B ) :: ierr
  CHARACTER( LEN=* ), PARAMETER :: myName="elem_init_from_fpl()"

  IF( .NOT. param%ispresent(key="nptrs") ) THEN
    CALL eElement%raiseError(modName//"::"//myName//" - "// &
        "nptrs key should be present in param")
  ELSE
    ierr = param%getShape(key="nptrs", shape=s)
    CALL Reallocate(obj%nptrs, s(1))
    ierr = param%get(key="nptrs", value=obj%nptrs)
  END IF

  IF( .NOT. param%ispresent(key="mat_type") ) THEN
    CALL eElement%raiseError(modName//"::"//myName//" - "// &
        "mat_type should be present in param")
  ELSE
    ierr = param%get(key="mat_type", value=obj%mat_type)
  END IF

  obj%refelem => refelem
END PROCEDURE elem_init_from_fpl

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_init_from_elem
  obj%nptrs = anotherobj%nptrs
  obj%MAT_Type = anotherobj%Mat_Type
  obj%refelem => anotherobj%refelem
END PROCEDURE elem_init_from_elem

!----------------------------------------------------------------------------
!                                                                 Element
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL ans%Initiate( param=param, refelem = refelem )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                                 Element
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor2
  CALL ans%Initiate( anotherobj )
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                            Element_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( ans )
  CALL ans%Initiate( param = param, refelem = refelem )
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                            Element_Pointer
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
  ALLOCATE( ans )
  CALL ans%Initiate( anotherobj )
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                            setMaterialType
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_setMaterialType
  obj%Mat_Type = MatType
END PROCEDURE elem_setMaterialType

!----------------------------------------------------------------------------
!                                                           getMaterialType
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_getMaterialType
  ans = obj%Mat_Type
END PROCEDURE elem_getMaterialType

!----------------------------------------------------------------------------
!                                                           DeallocatetDatat
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_deallocateData
  IF( ALLOCATED( obj%nptrs ) ) DEALLOCATE( obj%nptrs )
  obj%MAT_Type = 0
  obj%refelem => NULL( )
END PROCEDURE elem_deallocateData

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_final
  CALL obj%DeallocateData()
END PROCEDURE elem_final

!----------------------------------------------------------------------------
!                                                         isBoundaryElement
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_isBoundaryElement
  IF( NSD .NE. obj%refelem%XiDimension ) THEN
    ans = .TRUE.
  ELSE
    ans = .FALSE.
  END IF
END PROCEDURE elem_isBoundaryElement

!----------------------------------------------------------------------------
!                                                                 getNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_getNptrs
  IF( ALLOCATED( obj%nptrs ) ) THEN
    nptrs = obj%nptrs
  ELSE
    ALLOCATE( nptrs( 0 ) )
  END IF
END PROCEDURE elem_getNptrs

!----------------------------------------------------------------------------
!                                                                 setNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_setNptrs
  obj%nptrs = nptrs
END PROCEDURE elem_setNptrs

!----------------------------------------------------------------------------
!                                                     elem_getRefElemPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_getRefElemPointer
  IF( ASSOCIATED(obj%refelem) ) THEN
    ans => obj%refelem
  ELSE
    ans => NULL()
  END IF
END PROCEDURE elem_getRefElemPointer

!----------------------------------------------------------------------------
!                                                     elem_setRefElemPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_setRefElemPointer
  obj%refelem => refelem
END PROCEDURE elem_setRefElemPointer

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------
END SUBMODULE Constructor

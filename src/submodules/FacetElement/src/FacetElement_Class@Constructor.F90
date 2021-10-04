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

SUBMODULE(FacetElement_Class) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL ans%Initiate( param=param, refelem=refelem )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor2
  CALL ans%Initiate( anotherobj )
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( FacetElement_::ans )
  CALL ans%Initiate( param=param, refelem=refelem)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
  ALLOCATE( FacetElement_::ans )
  CALL ans%Initiate( anotherobj )
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE elem_deallocateData
  CALL obj%DeallocateElement()
  obj%LocalId = 0
  obj%Cell => NULL( )
  obj%OuterCell => NULL( )
END PROCEDURE elem_deallocateData

!----------------------------------------------------------------------------
!                                                             Final
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_deallocateData
  CALL obj%DeallocateData()
END PROCEDURE faceElem_deallocateData

!----------------------------------------------------------------------------
!                                                               getCellNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_getCellNptrs
  ans = obj%Cell%getNptrs()
END PROCEDURE faceElem_getCellNptrs

!----------------------------------------------------------------------------
!                                                               setCellNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_setCellNptrs
  call obj%Cell%setNptrs(nptrs)
END PROCEDURE faceElem_setCellNptrs

!----------------------------------------------------------------------------
!                                                           getPointerToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_getCellPointer
  ans => obj%Cell
END PROCEDURE faceElem_getCellPointer

!----------------------------------------------------------------------------
!                                                          SetPointerToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_setCellPointer
  obj%Cell => cell
END PROCEDURE faceElem_setCellPointer

!----------------------------------------------------------------------------
!                                                          FreePointerToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_freeCellPointer
  obj%Cell => NULL( )
END PROCEDURE faceElem_freeCellPointer

!----------------------------------------------------------------------------
!                                                           getFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_getFacetLocalID
  ans = obj%LocalID
END PROCEDURE faceElem_getFacetLocalID

!----------------------------------------------------------------------------
!                                                            setFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_setFacetLocalID
  obj%LocalID = id
END PROCEDURE faceElem_setFacetLocalID

!----------------------------------------------------------------------------
!                                                        getFacetLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE faceElem_getFacetLocalNptrs
  ! Define internal variables
  INTEGER( I4B ) :: i, j, b
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : ), CellNptrs(:), FacetNptrs(:), &
    & DummyNptrs( : )
  CLASS( ReferenceElement_ ), POINTER :: RefCellElem

  RefCellElem => obj%Cell%getRefElemPointer()
  j = obj%getFacetLocalID( )
  FM = FacetMatrix( RefCellElem )
  b = FM( j, 3 ) + 3
  Nptrs = FM( j, 4 : b )
  CellNptrs = obj%Cell%getNptrs( )
  FacetNptrs = obj%getNptrs( )
  !! Not that these nptrs are such that area points outerward
  !! however we need to fix the ordering so that
  !! NptrsOfCell( LocalNptrs( 1 ) ) .eq. NptrsOfFacet( 1 )
  IF( ANY( FacetNptrs .NE. CellNptrs( Nptrs ) ) ) THEN
    b = SIZE( Nptrs ) ! tnodes in facet
    DummyNptrs = Nptrs ! copy of facet nptrs
    DO i = 1, b
      DO j = 1, b
        IF( FacetNptrs( i ) .EQ. CellNptrs( DummyNptrs( j ) )  ) THEN
          Nptrs( i ) = DummyNptrs( j )
        END IF
      END DO
    END DO
    DEALLOCATE( DummyNptrs )
  END IF
  DEALLOCATE( FM, CellNptrs, FacetNptrs )
END PROCEDURE faceElem_getFacetLocalNptrs

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Constructor

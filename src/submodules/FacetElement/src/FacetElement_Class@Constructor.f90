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

SUBMODULE( FacetElement_Class ) Constructor
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor1
  CALL Obj%Initiate( Nptrs = Nptrs, Mat_Type = Mat_Type, RefElem = RefElem )
END PROCEDURE Constructor1

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor2
  Obj%Mat_Type = -1
  Obj%Nptrs = [-1]
  Obj%RefElem => NULL( )
END PROCEDURE Constructor2

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor3
  CALL Obj%Initiate( AnotherObj )
END PROCEDURE Constructor3

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_1
  ALLOCATE( Obj )
  CALL Obj%Initiate( Nptrs=Nptrs, Mat_Type=Mat_Type, RefElem=RefElem)
END PROCEDURE Constructor_1

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_2
  ALLOCATE( Obj )
  Obj%Mat_Type = -1
  Obj%Nptrs = [-1]
  Obj%RefElem => NULL( )
END PROCEDURE Constructor_2

!----------------------------------------------------------------------------
!                                                               FacetElement
!----------------------------------------------------------------------------

MODULE PROCEDURE Constructor_3
  ALLOCATE( Obj )
  CALL Obj%Initiate( AnotherObj )
END PROCEDURE Constructor_3

!----------------------------------------------------------------------------
!                                                             DeallocateData
!----------------------------------------------------------------------------

MODULE PROCEDURE Deallocate_Data
  IF( ALLOCATED( Obj%Nptrs ) ) DEALLOCATE( Obj%Nptrs )
  Obj%MAT_Type = 0
  Obj%RefElem => NULL( )
  Obj%LocalId = 0
  Obj%Cell => NULL( )
  Obj%OuterCell => NULL( )
END PROCEDURE Deallocate_Data

!----------------------------------------------------------------------------
!                                                               getCellNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE getCellNptrs
  Ans = Obj%Cell%Nptrs
END PROCEDURE getCellNptrs

!----------------------------------------------------------------------------
!                                                          SetPointerToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE SetPointerToCell
  Obj%Cell => CellObj
END PROCEDURE SetPointerToCell

!----------------------------------------------------------------------------
!                                                           getPointerToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE getPointerToCell
  CellObj => Obj%Cell
END PROCEDURE getPointerToCell

!----------------------------------------------------------------------------
!                                                          FreePointerToCell
!----------------------------------------------------------------------------

MODULE PROCEDURE FreePointerToCell
  Obj%Cell => NULL( )
END PROCEDURE FreePointerToCell

!----------------------------------------------------------------------------
!                                                           getFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE getFacetLocalID
  Ans = Obj%LocalID
END PROCEDURE getFacetLocalID

!----------------------------------------------------------------------------
!                                                            setFacetLocalID
!----------------------------------------------------------------------------

MODULE PROCEDURE setFacetLocalID
  Obj%LocalID = Id
END PROCEDURE setFacetLocalID

!----------------------------------------------------------------------------
!                                                        getFacetLocalNptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE getFacetLocalNptrs
  ! Define internal variables
  INTEGER( I4B ) :: i, j, b
  INTEGER( I4B ), ALLOCATABLE :: FM( :, : ), CellNptrs(:), FacetNptrs(:), &
    & DummyNptrs( : )

  ASSOCIATE( RefCellElem => Obj%Cell%RefElem )
    j = Obj%FacetLocalID( )
    FM = FacetMatrix( RefCellElem )
    b = FM( j, 3 ) + 3
    Nptrs = FM( j, 4 : b )
    CellNptrs = Obj%Cell%getNptrs( )
    FacetNptrs = Obj%getNptrs( )
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
  END ASSOCIATE

END PROCEDURE getFacetLocalNptrs

END SUBMODULE Constructor

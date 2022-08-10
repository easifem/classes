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

SUBMODULE(Test_Topology_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE topo_Initiate
  obj%name=name
  obj%nptrs=nptrs
  obj%xiDimension=xiDimension
END PROCEDURE topo_Initiate

!----------------------------------------------------------------------------
!                                                                Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
  obj%name=0
  obj%xiDimension=0
  IF(ALLOCATED(obj%nptrs)) DEALLOCATE( obj%nptrs )
END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                   Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
  CALL Display( msg, unitno=unitno)
  CALL Display( "Element type : " // TRIM( ElementName( obj%name ) ), &
    & unitno=unitno )
  CALL Display( "xidimension : " // TRIM( ToString( obj%xiDimension ) ), &
    & unitno=unitno )
  CALL Display( obj%nptrs, "nptrs : ", unitno=unitno, orient="ROW" )
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                   Getnptrs
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Getnptrs
  IF( ALLOCATED( obj%nptrs ) ) THEN
    ans = obj%nptrs
  ELSE
    ALLOCATE( ans(0) )
  END IF
END PROCEDURE obj_Getnptrs

!----------------------------------------------------------------------------
!                                                             Getxidimension
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Getxidimension
  ans = obj%xiDimension
END PROCEDURE obj_Getxidimension

!----------------------------------------------------------------------------
!                                                                    GetName
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetName
  ans = obj%name
END PROCEDURE obj_GetName

!----------------------------------------------------------------------------
!                                                                     GetNNE
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetNNE
  IF( ALLOCATED( obj%nptrs ) ) THEN
    ans = SIZE( obj%nptrs )
  ELSE
    ans = 0_I4B
  END IF
END PROCEDURE obj_GetNNE

!----------------------------------------------------------------------------
!                                                           GetFacetTopology
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_GetFacetTopology
  !!
  SELECT CASE (elemType)
  !!
  CASE (Line2)
    ALLOCATE (ans(2))
    ans(1)%nptrs = nptrs(1:1)
    ans(1)%name = point
    ans(1)%xidimension = 0

    ans(2)%nptrs = nptrs(2:2)
    ans(2)%name = point
    ans(2)%xidimension = 0
  !!
  CASE (Triangle3)
    ALLOCATE (ans(3))
    ans(1)%nptrs = nptrs([1, 2])
    ans(2)%nptrs = nptrs([2, 3])
    ans(3)%nptrs = nptrs([3, 1])
    ans(1:3)%xidimension = 1
    ans(1:3)%name = line2
  !!
  CASE (Quadrangle4)
    ALLOCATE (ans(4))
    ans(1)%nptrs = nptrs([1, 2])
    ans(2)%nptrs = nptrs([2, 3])
    ans(3)%nptrs = nptrs([3, 4])
    ans(4)%nptrs = nptrs([4, 1])
    ans(1:)%xidimension = 1
    ans(1:)%name = line2
  !!
  CASE (Tetrahedron4)
    ALLOCATE (ans(4))
    ans(1)%nptrs = nptrs([1, 2, 3])
    ans(2)%nptrs = nptrs([3, 1, 4])
    ans(3)%nptrs = nptrs([4, 2, 3])
    ans(4)%nptrs = nptrs([1, 2, 4])
    ans(:)%xidimension = 2
    ans(:)%name = Triangle3
  !!
  CASE (Hexahedron8)
    ALLOCATE (ans(6))
    ans(1)%nptrs = nptrs([1, 4, 3, 2])
    ans(2)%nptrs = nptrs([1, 5, 8, 4])
    ans(3)%nptrs = nptrs([5, 6, 7, 8])
    ans(4)%nptrs = nptrs([2, 3, 7, 6])
    ans(5)%nptrs = nptrs([3, 4, 8, 7])
    ans(6)%nptrs = nptrs([1, 2, 6, 5])
    ans(:)%xidimension = 2
    ans(:)%name = Quadrangle4
  !!
  CASE (Prism6)
    ALLOCATE (ans(5))
    ans(1)%nptrs = nptrs([5, 4, 1, 2])
    ans(2)%nptrs = nptrs([4, 6, 3, 1])
    ans(3)%nptrs = nptrs([2, 3, 6, 5])
    ans(4)%nptrs = nptrs([1, 3, 2])
    ans(5)%nptrs = nptrs([4, 5, 6])
    ans(:)%xidimension = 2
    ans(1:3)%name = Quadrangle4
    ans(4:5)%name = Triangle3
  !!
  CASE (Pyramid5)
    ALLOCATE (ans(5))
    ans(1)%nptrs = nptrs([1, 2, 5])
    ans(2)%nptrs = nptrs([2, 3, 5])
    ans(3)%nptrs = nptrs([3, 4, 5])
    ans(4)%nptrs = nptrs([1, 5, 4])
    ans(5)%nptrs = nptrs([4, 3, 2, 1])
    ans(:)%xidimension = 2
    ans(1:4)%name = Triangle3
    ans(5)%name = Quadrangle4
    !! Order=2 elements
  CASE (Line3)
    ALLOCATE (ans(2))
    ans(1)%nptrs = nptrs([1])
    ans(1)%name = point
    ans(1)%xidimension = 0
    ans(2)%nptrs = nptrs([2])
    ans(2)%name = point
    ans(2)%xidimension = 0
  !!
  CASE (Triangle6)
    ALLOCATE (ans(3))
    ans(1)%nptrs = nptrs([1, 2, 4])
    ans(2)%nptrs = nptrs([2, 3, 5])
    ans(3)%nptrs = nptrs([3, 1, 6])
    ans(1:3)%xidimension = 1
    ans(1:3)%name = line3
  !!
  CASE (Quadrangle9)
    ALLOCATE (ans(4))
    ans(1)%nptrs = nptrs([1, 2, 5])
    ans(2)%nptrs = nptrs([2, 3, 6])
    ans(3)%nptrs = nptrs([3, 4, 7])
    ans(4)%nptrs = nptrs([4, 1, 8])
    ans(1:)%xidimension = 1
    ans(1:)%name = line3
  !!
  CASE (Quadrangle8)
    ALLOCATE (ans(4))
    ans(1)%nptrs = nptrs([1, 2, 5])
    ans(2)%nptrs = nptrs([2, 3, 6])
    ans(3)%nptrs = nptrs([3, 4, 7])
    ans(4)%nptrs = nptrs([4, 1, 8])
    ans(1:)%xidimension = 1
    ans(1:)%name = line3
  !!
  CASE (Tetrahedron10)
    ALLOCATE (ans(4))
    ans(1)%nptrs = nptrs([1, 2, 3, 5, 6, 7])
    ans(2)%nptrs = nptrs([3, 1, 4, 7, 8, 10])
    ans(3)%nptrs = nptrs([4, 2, 3, 9, 6, 10])
    ans(4)%nptrs = nptrs([1, 2, 4, 5, 9, 8])
    ans(:)%xidimension = 2
    ans(:)%name = Triangle6
  !!
  CASE (Hexahedron20)
    ALLOCATE (ans(6))
    ans(1)%nptrs = nptrs([1, 4, 3, 2, 10, 14, 12, 9])
    ans(2)%nptrs = nptrs([1, 5, 8, 4, 11, 18, 16, 10])
    ans(3)%nptrs = nptrs([5, 6, 7, 8, 17, 19, 20, 18])
    ans(4)%nptrs = nptrs([2, 3, 7, 6, 12, 15, 19, 13])
    ans(5)%nptrs = nptrs([3, 4, 8, 7, 14, 16, 20, 15])
    ans(6)%nptrs = nptrs([1, 2, 6, 5, 9, 13, 17, 11])
    ans(:)%xidimension = 2
    ans(:)%name = Quadrangle8
  !!
  CASE (Hexahedron27)
    ALLOCATE (ans(6))
    ans(1)%nptrs = nptrs([1, 4, 3, 2, 10, 14, 12, 9, 21])
    ans(2)%nptrs = nptrs([1, 5, 8, 4, 11, 18, 16, 10, 23])
    ans(3)%nptrs = nptrs([5, 6, 7, 8, 17, 19, 20, 18, 26])
    ans(4)%nptrs = nptrs([2, 3, 7, 6, 12, 15, 19, 13, 24])
    ans(5)%nptrs = nptrs([3, 4, 8, 7, 14, 16, 20, 15, 25])
    ans(6)%nptrs = nptrs([1, 2, 6, 5, 9, 13, 17, 11, 22])
    ans(:)%xidimension = 2
    ans(:)%name = Quadrangle9
  !!
  CASE (Prism15)
    ALLOCATE (ans(5))
    ans(1)%nptrs = nptrs([5, 4, 1, 2, 13, 9, 7, 11])
    ans(2)%nptrs = nptrs([4, 6, 3, 1, 14, 12, 8, 9])
    ans(3)%nptrs = nptrs([2, 3, 6, 5, 10, 12, 15, 11])
    ans(4)%nptrs = nptrs([1, 3, 2, 8, 10, 7])
    ans(5)%nptrs = nptrs([4, 5, 6, 13, 15, 14])
    ans(:)%xidimension = 2
    ans(1:3)%name = Quadrangle8
    ans(4:5)%name = Triangle6
  !!
  CASE (Prism18)
    ALLOCATE (ans(5))
    ans(1)%nptrs = nptrs([5, 4, 1, 2, 13, 9, 7, 11, 16])
    ans(2)%nptrs = nptrs([4, 6, 3, 1, 14, 12, 8, 9, 17])
    ans(3)%nptrs = nptrs([2, 3, 6, 5, 10, 12, 15, 11, 18])
    ans(4)%nptrs = nptrs([1, 3, 2, 8, 10, 7])
    ans(5)%nptrs = nptrs([4, 5, 6, 13, 15, 14])
    ans(:)%xidimension = 2
    ans(1:3)%name = Quadrangle9
    ans(4:5)%name = Triangle6
  !!
  CASE (Pyramid13)
    ALLOCATE (ans(5))
    ans(1)%nptrs = nptrs([1, 2, 5, 6, 10, 8])
    ans(2)%nptrs = nptrs([2, 3, 5, 9, 12, 10])
    ans(3)%nptrs = nptrs([3, 4, 5, 11, 13, 12])
    ans(4)%nptrs = nptrs([1, 5, 4, 8, 13, 7])
    ans(5)%nptrs = nptrs([4, 3, 2, 1, 11, 9, 6, 7])
    ans(:)%xidimension = 2
    ans(1:4)%name = Triangle6
    ans(5)%name = Quadrangle8
  !!
  CASE (Pyramid14)
    ALLOCATE (ans(5))
    ans(1)%nptrs = nptrs([1, 2, 5, 6, 10, 8])
    ans(2)%nptrs = nptrs([2, 3, 5, 9, 12, 10])
    ans(3)%nptrs = nptrs([3, 4, 5, 11, 13, 12])
    ans(4)%nptrs = nptrs([1, 5, 4, 8, 13, 7])
    ans(5)%nptrs = nptrs([4, 3, 2, 1, 11, 9, 6, 7, 13])
    ans(:)%xidimension = 2
    ans(1:4)%name = Triangle6
    ans(5)%name = Quadrangle9
  !!
  CASE (Triangle9)
    ALLOCATE (ans(3))
    ans(1)%nptrs = nptrs([1, 2, 4, 5])
    ans(2)%nptrs = nptrs([2, 3, 6, 7])
    ans(3)%nptrs = nptrs([3, 1, 8, 9])
    ans(1:3)%xidimension = 1
    ans(1:3)%name = line4
  !!
  CASE (Triangle10)
    ALLOCATE (ans(3))
    ans(1)%nptrs = nptrs([1, 2, 4, 5])
    ans(2)%nptrs = nptrs([2, 3, 6, 7])
    ans(3)%nptrs = nptrs([3, 1, 8, 9])

    ans(1:3)%xidimension = 1
    ans(1:3)%name = line4

  CASE (Triangle12)
    ALLOCATE (ans(3))
    ans(1)%nptrs = nptrs([1, 2, 4, 5, 6])
    ans(2)%nptrs = nptrs([2, 3, 7, 8, 9])
    ans(3)%nptrs = nptrs([3, 1, 10, 11, 12])

    ans(1:3)%xidimension = 1
    ans(1:3)%name = line5

  CASE (Triangle15a)
    ALLOCATE (ans(3))
    ans(1)%nptrs = nptrs([1, 2, 4, 5, 6])
    ans(2)%nptrs = nptrs([2, 3, 7, 8, 9])
    ans(3)%nptrs = nptrs([3, 1, 10, 11, 12])

    ans(1:3)%xidimension = 1
    ans(1:3)%name = line5

  CASE (Line4)
    ALLOCATE (ans(2))
    ans(1)%nptrs = nptrs([1])
    ans(1)%name = point
    ans(1)%xidimension = 0

    ans(2)%nptrs = nptrs([2])
    ans(2)%name = point
    ans(2)%xidimension = 0

  CASE (Line5)
    ALLOCATE (ans(2))
    ans(1)%nptrs = nptrs([1])
    ans(1)%name = point
    ans(1)%xidimension = 0

    ans(2)%nptrs = nptrs([2])
    ans(2)%name = point
    ans(2)%xidimension = 0

  CASE (Line6)
    ALLOCATE (ans(2))
    ans(1)%nptrs = nptrs([1])
    ans(1)%name = point
    ans(1)%xidimension = 0

    ans(2)%nptrs = nptrs([2])
    ans(2)%name = point
    ans(2)%xidimension = 0

  CASE (Triangle15b, Triangle21, Tetrahedron20, Tetrahedron35, &
    & Tetrahedron56, Hexahedron64, Hexahedron125)
    !!
  END SELECT
  !!
END PROCEDURE obj_GetFacetTopology

END SUBMODULE Methods
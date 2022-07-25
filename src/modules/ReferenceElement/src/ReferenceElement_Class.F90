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

MODULE Test_ReferenceElement_Class
USE GlobalData
USE Test_Topology_Class
IMPLICIT NONE
PRIVATE

INTEGER( I4B ), PARAMETER :: MAX_NAME_LENGTH=25

!----------------------------------------------------------------------------
!                                                   Test_ReferenceElement_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceElement_
  ! REAL(DFP), ALLOCATABLE :: xiJ(:, :)
    !! Node coord
    !! I think reference element should not contain the nodecoord
    !! It is a purely topological quantity
  INTEGER(I4B) :: entityCounts(4) = 0
    !!
    !! Number of 0D, 1D, 2D, 3D subentities in the reference element
    !!
  INTEGER(I4B) :: xiDimension = 0
    !! Xidimension  elemType
    !! 0 -> point
    !! 1 -> line
    !! 2 -> surface
    !! 3 -> volume
  INTEGER(I4B) :: name = 0
    !! name of the element
  CHARACTER( LEN = MAX_NAME_LENGTH ) :: name_str=""
    !! name of the element
  INTEGER(I4B) :: dim = 0
    !! Number of spatial dimensions
  TYPE(Test_Topology_), ALLOCATABLE :: topology(:)
    !! Topology information of 0D, 1, 2, 3D entities
END TYPE Test_ReferenceElement_

PUBLIC :: Test_ReferenceElement_

!----------------------------------------------------------------------------
!                                            Test_ReferenceElementPointer_
!----------------------------------------------------------------------------

TYPE :: Test_ReferenceElementPointer_
  CLASS(Test_ReferenceElement_), POINTER :: ptr => NULL()
END TYPE Test_ReferenceElementPointer_

PUBLIC :: Test_ReferenceElementPointer_

END MODULE Test_ReferenceElement_Class
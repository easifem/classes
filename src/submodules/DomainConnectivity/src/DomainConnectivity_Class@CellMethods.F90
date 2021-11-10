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

SUBMODULE(DomainConnectivity_Class) CellMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE dc_initiateCellToCellData1
CHARACTER(LEN=*), PARAMETER :: myName = "dc_initiateCellToCellData1"
CLASS(Mesh_), POINTER :: mesh1 => NULL()
CLASS(Mesh_), POINTER :: mesh2 => NULL()
TYPE(BoundingBox_) :: Box
CLASS(ReferenceElement_), POINTER :: refelem1 => null()
CLASS(ReferenceElement_), POINTER :: refelem2 => null()
INTEGER(I4B), ALLOCATABLE :: nptrs1(:), nptrs2(:)
INTEGER(I4B) :: ii, jj, nsd, order1, order2
REAL(DFP) :: X(3)
REAL(DFP), POINTER :: node1(:, :)
REAL(DFP), POINTER :: node2(:, :)
!> main
!> check
if (.not. obj%isNodeToNode) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'NodeToNode data is not initiated!')
!> check
IF (obj%isCellToCell) THEN
  CALL e%raiseWarning(modName//"::"//myName//" - "// &
    & "It seems, obj%cellToCell data is already initiated")
END IF
!> get mesh pointer
mesh1 => domain1%GetMeshPointer(dim=dim1, entityNum=entityNum1)
mesh2 => domain2%GetMeshPointer(dim=dim2, entityNum=entityNum2)
!! TODO
!! is it possible to have bounds of obj%cellToCell from
!! mesh1%minElemNum to mesh1%maxElemNum, it will save the space
CALL Reallocate(obj%cellToCell, mesh1%maxElemNum)
obj%isCellToCell = .TRUE.
refelem1 => mesh1%getRefElemPointer()
refelem2 => mesh2%getRefElemPointer()
IF (ElementTopology(refelem1) .ne. ElementTopology(refelem2)) &
     & CALL e%raiseError(modName//"::"//myName//" - "// &
     & 'Topology of mesh element is not the same.')

order1 = elementOrder(refelem1)
order2 = elementOrder(refelem2)

IF (order1 .LE. order2) then
else
end if
END PROCEDURE dc_initiateCellToCellData1

END SUBMODULE CellMethods

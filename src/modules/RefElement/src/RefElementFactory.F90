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

MODULE RefElementFactory
USE GlobalData
USE Topology_Class
USE AbstractRefElement_Class
USE RefPoint_Class
USE RefLine_Class
USE RefTriangle_Class
USE RefQuadrangle_Class
USE RefTetrahedron_Class
USE RefHexahedron_Class
USE RefPyramid_Class
USE RefPrism_Class
IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                         RefElement_Pointer
!----------------------------------------------------------------------------

!> author: Vikas Sharma, Ph. D.
! date: 27 Aug 2022
! summary: Reference element pointer

FUNCTION RefElement_Pointer(elemType) RESULT(ans)
  INTEGER(I4B), INTENT(IN) :: elemType
  CLASS(AbstractRefElement_), POINTER :: ans
  ans => NULL()
  SELECT CASE (elemType)
  CASE (Point)
    ALLOCATE (RefPoint_ :: ans)
  CASE (Line)
    ALLOCATE (RefLine_ :: ans)
  CASE (Triangle)
    ALLOCATE (RefTriangle_ :: ans)
  CASE (Quadrangle)
    ALLOCATE (RefQuadrangle_ :: ans)
  CASE (Tetrahedron)
    ALLOCATE (RefTetrahedron_ :: ans)
  CASE (Hexahedron)
    ALLOCATE (RefHexahedron_ :: ans)
  CASE (Prism)
    ALLOCATE (RefPrism_ :: ans)
  CASE (Pyramid)
    ALLOCATE (RefPyramid_ :: ans)
  END SELECT
END FUNCTION RefElement_Pointer

END MODULE RefElementFactory

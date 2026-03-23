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

MODULE GmshModelOccMeshInterface
USE ISO_C_BINDING, ONLY: C_INT
USE ISO_C_BINDING, ONLY: C_DOUBLE
USE ISO_C_BINDING, ONLY: C_SIZE_T
IMPLICIT NONE
PRIVATE

PUBLIC :: gmshModelOccMeshSetSize

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

! Set a mesh size constraint on the entities `dimTags' in the OpenCASCADE CAD
! representation. Currently only entities of dimension 0 (points) are
! handled.
!
! GMSH_API void gmshModelOccMeshSetSize(int *dimTags, size_t dimTags_n,
! const double size,
! int *ierr);

INTERFACE
  SUBROUTINE gmshModelOccMeshSetSize( &
    dimTags, dimTags_n, size, ierr) &
    BIND(C, NAME="gmshModelOccMeshSetSize")
    IMPORT
    INTEGER(C_SIZE_T), VALUE, INTENT(in) :: dimTags_n
    INTEGER(C_INT), INTENT(IN) :: dimTags(dimTags_n)
    REAL(C_DOUBLE), VALUE, INTENT(IN) :: size
    INTEGER(C_INT), INTENT(OUT) :: ierr
  END SUBROUTINE gmshModelOccMeshSetSize
END INTERFACE

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE GmshModelOccMeshInterface

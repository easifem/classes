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

MODULE GmshModelOccMesh_Class
USE GlobalData, ONLY: DFP, I4B, LGT
USE GmshInterface
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE
CHARACTER( LEN = * ), PARAMETER :: modName = "GMSHMODELOCCMESH_CLASS"
INTEGER( C_INT ) :: ierr
!$OMP THREADPRIVATE(ierr)
INTEGER( I4B ), PARAMETER :: maxStrLen = 256

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelOccMesh_
  CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS( Obj ) :: SetSize=>mesh_SetSize
END TYPE GmshModelOccMesh_

PUBLIC :: GmshModelOccMesh_
TYPE( GmshModelOccMesh_ ), PUBLIC, PARAMETER :: TypeGmshModelOccMesh = GmshModelOccMesh_()

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: GmshModelOccMeshPointer_
  CLASS( GmshModelOccMesh_ ), POINTER :: Ptr => NULL()
END TYPE

PUBLIC :: GmshModelOccMeshPointer_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

CONTAINS

FUNCTION mesh_SetSize(obj, dimTags, meshSize) &
  & RESULT( ans )
  CLASS( GmshModelOccMesh_ ), INTENT( INOUT ) :: obj
  INTEGER( I4B ), INTENT( IN ) :: dimTags( : )
  REAL( DFP ), INTENT( IN ) :: meshSize
  INTEGER( I4B ) :: ans

  ! Internal
  CALL gmshModelOccMeshSetSize(dimTags, size(dimTags, kind=c_size_t), &
    & meshSize, ierr)
  ans = int(ierr, i4b)
END FUNCTION mesh_SetSize

END MODULE GmshModelOccMesh_Class
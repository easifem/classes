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

#define I4B C_INT
#define DFP C_DOUBLE
#define _I_OUT_ INTEGER( C_INT ), INTENT( OUT )
#define _I_IN_ INTEGER( C_INT ), INTENT( IN )
#define _I_V_IN_ INTEGER( C_INT ), VALUE, INTENT( IN )
#define _ST_V_IN_ INTEGER( C_SIZE_T ), VALUE, INTENT( IN )
#define _ST_OUT_ INTEGER( C_SIZE_T ), INTENT( OUT )
#define _ST_IN_ INTEGER( C_SIZE_T ), INTENT( IN )
#define _R_V_IN_ REAL( C_DOUBLE ), VALUE, INTENT( IN )
#define _R_IN_ REAL( C_DOUBLE ), INTENT( IN )
#define _R_OUT_ REAL( C_DOUBLE ), INTENT( OUT )
#define _CPTR_V_IN_ TYPE(C_PTR), VALUE, INTENT( IN )
#define _CPTR_IN_ TYPE(C_PTR), INTENT( IN )
#define _CPTR_OUT_ TYPE(C_PTR), INTENT( OUT )

MODULE GmshInterface
USE ISO_C_BINDING
IMPLICIT NONE
PRIVATE

INTEGER(I4B), PARAMETER :: maxStrLen = 256
INTEGER, PARAMETER, PUBLIC :: GMSH_API_VERSION_MAJOR = 4
INTEGER, PARAMETER, PUBLIC :: GMSH_API_VERSION_MINOR = 11
INTEGER, PARAMETER, PUBLIC :: GMSH_API_VERSION_PATCH = 0
INTEGER, PARAMETER, PUBLIC :: GMSH_API_MAX_STR_LEN = maxStrLen
CHARACTER(100), PARAMETER, PUBLIC :: GMSH_API_VERSION = "4.11.0"

#include "./Gmsh.inc"
#include "./GmshOption.inc"
#include "./GmshModel.inc"
! 🏁
#include "./GmshGraphics.inc"
#include "./GmshFLTK.inc"
#include "./GmshModelGeo.inc"
#include "./GmshModelGeoMesh.inc"
#include "./GmshModelOcc.inc"
#include "./GmshModelOccMesh.inc"
#include "./GmshModelMesh.inc"
#include "./GmshOnelab.inc"

END MODULE GmshInterface

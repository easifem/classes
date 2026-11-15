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

#define FTL_TEMPLATE_TYPE AbstractMeshPointer_
#define FTL_TEMPLATE_TYPE_IS_DERIVED
#define FTL_TEMPLATE_TYPE_NAME Mesh
#define FTL_INSTANTIATE_TEMPLATE

!> author: Vikas Sharma, Ph. D.
! date: 2026-10-23
! summary: Vector of mesh pointers
!
! MeshPointerVector_ is a dynamic array of AbstractMeshPointer_. It is
! made from the FTL template ftlPointerVector.inc.
!
! Changes:
!   - 2026-10-23: Stores AbstractMeshPointer_ (Mesh_Class was removed)

MODULE MeshPointerVector_Class
USE AbstractMesh_Class, ONLY: AbstractMeshPointer_
#include "../../ftlMacros/ftlPointerVector.inc"

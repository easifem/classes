# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME}
  PRIVATE ${src_path}/AbstractMeshUtility.F90
          ${src_path}/AbstractMesh_Class@ConstructorMethods.F90
          ${src_path}/AbstractMesh_Class@IOMethods.F90
          ${src_path}/AbstractMesh_Class@HDFMethods.F90
          ${src_path}/AbstractMesh_Class@VTKMethods.F90
          ${src_path}/AbstractMesh_Class@GetMethods.F90
          ${src_path}/AbstractMesh_Class@NodeDataMethods.F90
          ${src_path}/AbstractMesh_Class@ElementDataMethods.F90
          ${src_path}/AbstractMesh_Class@BoundaryDataMethods.F90
          ${src_path}/AbstractMesh_Class@FacetDataMethods.F90
          ${src_path}/AbstractMesh_Class@SetMethods.F90
          ${src_path}/AbstractMesh_Class@FaceDataMethods.F90
          ${src_path}/AbstractMesh_Class@EdgeDataMethods.F90)

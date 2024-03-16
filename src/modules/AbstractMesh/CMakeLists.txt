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
  PRIVATE ${src_path}/AbstractMesh_Class.F90
          ${src_path}/NodeData_Class.F90
          ${src_path}/NodeDataList_Class.F90
          ${src_path}/NodeDataBinaryTree_Class.F90
          ${src_path}/EdgeData_Class.F90
          ${src_path}/EdgeDataList_Class.F90
          ${src_path}/EdgeDataBinaryTree_Class.F90
          ${src_path}/EdgeDataBinaryTreeUtility.F90
          ${src_path}/ElemData_Class.F90
          ${src_path}/ElemDataList_Class.F90
          ${src_path}/ElemDataBinaryTree_Class.F90
          ${src_path}/FacetData_Class.F90
          # ${src_path}/EdgeTreeData_Class.F90
          # ${src_path}/EdgeBinaryTree_Class.F90
          # ${src_path}/EdgeBinaryTreeUtility.F90
          ${src_path}/ElementShapeFunctionData_Class.F90)

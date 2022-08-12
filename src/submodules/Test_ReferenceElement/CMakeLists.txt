# This program is a part of EASIFEM library
# Copyright (C) 2020-2021  Vikas Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https: //www.gnu.org/licenses/>
#

SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Test_Topology_Class@Methods.F90
  ${src_path}/Test_ReferenceElement_Class@Methods.F90
  ${src_path}/Test_ReferencePoint_Class@Methods.F90
  ${src_path}/Test_ReferenceLine_Class@Methods.F90
  ${src_path}/Test_ReferenceTriangle_Class@Methods.F90
  ${src_path}/Test_ReferenceQuadrangle_Class@Methods.F90
  ${src_path}/Test_ReferenceTetrahedron_Class@Methods.F90
  ${src_path}/Test_ReferenceHexahedron_Class@Methods.F90
  ${src_path}/Test_ReferencePrism_Class@Methods.F90
  ${src_path}/Test_ReferencePyramid_Class@Methods.F90
)
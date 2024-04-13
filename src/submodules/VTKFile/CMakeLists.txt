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
  ${src_path}/VTKDataArrayEncoder@Rank1Methods.F90
  ${src_path}/VTKDataArrayEncoder@Rank2Methods.F90
  ${src_path}/VTKDataArrayEncoder@Rank3Methods.F90
  ${src_path}/VTKDataArrayEncoder@Rank4Methods.F90
  ${src_path}/VTKDataArrayEncoder@XYZRank1Methods.F90
  ${src_path}/VTKDataArrayEncoder@XYZRank2Methods.F90
  ${src_path}/VTKDataArrayEncoder@XYZRank3Methods.F90
  ${src_path}/VTKFile_Class@ConstructorMethods.F90
  ${src_path}/VTKFile_Class@TagsMethods.F90
  ${src_path}/VTKFile_Class@CellMethods.F90
  ${src_path}/VTKFile_Class@VertMethods.F90
  ${src_path}/VTKFile_Class@DataArrayMethods.F90
  ${src_path}/VTKFile_Class@DataArrayAppendedMethods.F90
  ${src_path}/VTKFile_Class@FieldDataMethods.F90
  ${src_path}/VTKFile_Class@PointsMethods.F90
  ${src_path}/VTKFile_Class@PieceMethods.F90
)
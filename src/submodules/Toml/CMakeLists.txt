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

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
    ${PROJECT_NAME}
    PRIVATE
        ${src_path}/TomlUtility@BasicMethods.F90
        ${src_path}/TomlUtility@StringMethods.F90
        ${src_path}/TomlUtility@IntMethods.F90
        ${src_path}/TomlUtility@Int8VectorMethods.F90
        ${src_path}/TomlUtility@Int8StaticVectorMethods.F90
        ${src_path}/TomlUtility@Int8MatrixMethods.F90
        ${src_path}/TomlUtility@Int8StaticMatrixMethods.F90
        ${src_path}/TomlUtility@Int16VectorMethods.F90
        ${src_path}/TomlUtility@Int16StaticVectorMethods.F90
        ${src_path}/TomlUtility@Int16MatrixMethods.F90
        ${src_path}/TomlUtility@Int16StaticMatrixMethods.F90
        ${src_path}/TomlUtility@Int32VectorMethods.F90
        ${src_path}/TomlUtility@Int32StaticVectorMethods.F90
        ${src_path}/TomlUtility@Int32MatrixMethods.F90
        ${src_path}/TomlUtility@Int32StaticMatrixMethods.F90
        ${src_path}/TomlUtility@Int64VectorMethods.F90
        ${src_path}/TomlUtility@Int64StaticVectorMethods.F90
        ${src_path}/TomlUtility@Int64MatrixMethods.F90
        ${src_path}/TomlUtility@Int64StaticMatrixMethods.F90
        ${src_path}/TomlUtility@RealMethods.F90
        ${src_path}/TomlUtility@Real32VectorMethods.F90
        ${src_path}/TomlUtility@Real32StaticVectorMethods.F90
        ${src_path}/TomlUtility@Real32MatrixMethods.F90
        ${src_path}/TomlUtility@Real32StaticMatrixMethods.F90
        ${src_path}/TomlUtility@Real64VectorMethods.F90
        ${src_path}/TomlUtility@Real64StaticVectorMethods.F90
        ${src_path}/TomlUtility@Real64MatrixMethods.F90
        ${src_path}/TomlUtility@Real64StaticMatrixMethods.F90
        ${src_path}/TomlUtility@BoolMethods.F90
)

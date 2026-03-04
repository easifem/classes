# This program is a part of EASIFEM library
# Expandable And Scalable Infrastructure for Finite Element Methods
# htttps://www.easifem.com
# Vikas Sharma, Ph.D., vickysharma0812@gmail.com
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

set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
    ${PROJECT_NAME}
    PRIVATE
        ${src_path}/HDF5File_Class@ConstructorMethods.F90
        ${src_path}/HDF5File_Class@WriteReal64.F90
        ${src_path}/HDF5File_Class@WriteReal32.F90
        ${src_path}/HDF5File_Class@WriteInt32.F90
        ${src_path}/HDF5File_Class@WriteBool.F90
        ${src_path}/HDF5File_Class@WriteString.F90
        ${src_path}/HDF5File_Class@ReadReal64.F90
        ${src_path}/HDF5File_Class@ReadReal32.F90
        ${src_path}/HDF5File_Class@ReadInt32.F90
        ${src_path}/HDF5File_Class@ReadString.F90
        ${src_path}/HDF5File_Class@ReadBool.F90
        ${src_path}/HDF5File_Class@WriteAttribute.F90
        ${src_path}/HDF5File_Class@ReadAttribute.F90
        ${src_path}/HDF5FileUtility@Methods.F90
)

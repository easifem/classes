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

# SET(HDF5_NO_FIND_PACKAGE_CONFIG_FILE true CACHE BOOL "Set true to skip trying
# to find hdf5-config.cmake" FORCE)
find_package(HDF5 REQUIRED COMPONENTS Fortran HL)
if(HDF5_VERSION VERSION_LESS 1.8.7)
  message(WARNING "HDF5 VERSION SHOULD BE >= 1.8.7")
endif()
if(HDF5_FOUND)
  message(STATUS "HDF5 FOUND: ")
  list(APPEND TARGET_COMPILE_DEF "-DUSE_HDF5")
  list(APPEND TARGET_COMPILE_DEF "${HDF5_Fortran_DEFINITIONS}")
  message(STATUS "HDF5 fortran lib :: ${HDF5_Fortran_LIBRARIES}")
else()
  message(ERROR "HDF5 NOT FOUND")
endif()
target_link_libraries(${PROJECT_NAME} PUBLIC ${HDF5_Fortran_LIBRARIES})
target_include_directories(${PROJECT_NAME} PUBLIC ${HDF5_Fortran_INCLUDE_DIRS})

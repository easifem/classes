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
# Find HDF5
IF( ${PROJECT_NAME} MATCHES "easifemClasses" )
  # SET(HDF5_NO_FIND_PACKAGE_CONFIG_FILE true CACHE BOOL "Set true to skip trying to find hdf5-config.cmake" FORCE)
  FIND_PACKAGE(
    HDF5 
    REQUIRED 
    COMPONENTS Fortran HL
    )
  IF(HDF5_VERSION VERSION_LESS 1.8.7)
    MESSAGE(WARNING "HDF5 VERSION SHOULD BE >= 1.8.7")
  ENDIF()
  IF(HDF5_FOUND)
    MESSAGE(STATUS "HDF5 FOUND: ")
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_HDF5" )
    LIST( APPEND TARGET_COMPILE_DEF "${HDF5_Fortran_DEFINITIONS}" )
    MESSAGE( STATUS "HDF5 fortran lib :: ${HDF5_Fortran_LIBRARIES}")
  ELSE()
    MESSAGE( ERROR "HDF5 NOT FOUND" )
  ENDIF()
  TARGET_LINK_LIBRARIES(${PROJECT_NAME} PUBLIC ${HDF5_Fortran_LIBRARIES} )
  TARGET_INCLUDE_DIRECTORIES(${PROJECT_NAME} PUBLIC ${HDF5_Fortran_INCLUDE_DIRS})
ENDIF()

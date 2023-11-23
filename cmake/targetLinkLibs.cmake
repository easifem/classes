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
#

# Link libs to the project

#....................................................................
#
#....................................................................

FUNCTION(FIND_EASIFEM_DEPENDENCY EXT_PKG_LIST)
  FOREACH(p ${EXT_PKG_LIST})
    FIND_PACKAGE( ${p} REQUIRED )
    IF( ${p}_FOUND )
      MESSAGE(STATUS "FOUND ${p}")
    ELSE()
      MESSAGE(ERROR "NOT FOUND ${p}")
    ENDIF()
  ENDFOREACH()
ENDFUNCTION(FIND_EASIFEM_DEPENDENCY)

#....................................................................
#
#....................................................................

FUNCTION(LINK_EASIFEM_DEPENDENCY EXT_PKG_LIST PROJECT_NAME)
  FOREACH(p ${EXT_PKG_LIST})
    TARGET_LINK_LIBRARIES( ${PROJECT_NAME} PUBLIC ${p}::${p} )
  ENDFOREACH()
ENDFUNCTION(LINK_EASIFEM_DEPENDENCY)

#....................................................................
#
#....................................................................

IF( ${PROJECT_NAME} MATCHES "easifemBase" )
  IF( USE_LAPACK95 )
    LIST(APPEND EXT_PKGS LAPACK95)
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_LAPACK95" )
  ENDIF()

  LIST(APPEND EXT_PKGS Sparsekit)

  FIND_EASIFEM_DEPENDENCY( "${EXT_PKGS}" )
  LINK_EASIFEM_DEPENDENCY( "${EXT_PKGS}" "${PROJECT_NAME}" )
ENDIF()

#....................................................................
#
#....................................................................

FIND_PACKAGE( easifemBase REQUIRED )
IF( easifemBase_FOUND )
  MESSAGE(STATUS "FOUND easifemBase")
ELSE()
  MESSAGE(ERROR "NOT FOUND easifemBase")
ENDIF()
TARGET_LINK_LIBRARIES( ${PROJECT_NAME} PUBLIC easifemBase::easifemBase )


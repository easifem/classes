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
#COMPILE DEF
#
#Single precision or double precision
OPTION(USE_Real32 OFF)
OPTION(USE_Real64 OFF)
IF(USE_Real32)
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_Real32" )
ELSEIF(USE_Real64)
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_Real64" )
ELSE()
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_Real64" )
ENDIF()
OPTION(USE_Int32 OFF)
OPTION(USE_Int64 OFF)
IF(USE_Int32)
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_Int32" )
ELSEIF(USE_Real64)
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_Int64" )
ELSE()
  LIST( APPEND TARGET_COMPILE_DEF "-DUSE_Int32" )
ENDIF()
LIST( APPEND TARGET_COMPILE_DEF "-D${CMAKE_HOST_SYSTEM_NAME}_SYSTEM" )

#DEFINE DEBUG
IF( $<CONFIG:DEBUG> )
  LIST( APPEND TARGET_COMPILE_DEF "-DDEBUG_VER" )
ENDIF()

#ADD TO PROJECT
TARGET_COMPILE_DEFINITIONS( ${PROJECT_NAME} PUBLIC ${TARGET_COMPILE_DEF} )
MESSAGE( STATUS "COMPILE DEFINITIONS USED ARE ${TARGET_COMPILE_DEF}")
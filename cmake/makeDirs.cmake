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
INCLUDE( GNUInstallDirs )
SET(
  CMAKE_Fortran_MODULE_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR} )
SET(
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR} )
SET(
  CMAKE_ARCHIVE_OUTPUT_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR} )
SET(
  CMAKE_RUNTIME_OUTPUT_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR} )
SET(
  INSTALL_LIBDIR
  ${CMAKE_INSTALL_LIBDIR}
  CACHE PATH "install location of lib" )
SET(
  INSTALL_INCLUDEDIR
  ${CMAKE_INSTALL_INCLUDEDIR}
  CACHE PATH "install location of include" )
SET(
  INSTALL_BINDIR
  ${CMAKE_INSTALL_BINDIR}
  CACHE PATH "install location of bin" )
IF(WIN32 AND NOT CYGWIN)
  SET(DEF_INSTALL_CMAKEDIR CMake)
ELSE()
  SET(DEF_INSTALL_CMAKEDIR share/cmake/${PROJECT_NAME})
ENDIF()
SET(INSTALL_CMAKEDIR ${DEF_INSTALL_CMAKEDIR} CACHE PATH "Installation directory for CMake files")
FOREACH(p LIB BIN INCLUDE CMAKE)
  FILE(TO_NATIVE_PATH ${CMAKE_INSTALL_PREFIX}/${INSTALL_${p}DIR} _path)
  MESSAGE(STATUS "Installing ${p} componenets to ${_path}")
ENDFOREACH()
OPTION(BUILD_SHARED_LIBS "Build shared library" ON)
IF(BUILD_SHARED_LIBS)
  MESSAGE(STATUS "${PROJECT_NAME} will be built as a shared library.")
ELSE()
  MESSAGE(STATUS "${PROJECT_NAME} will be built as a static library.")
ENDIF()
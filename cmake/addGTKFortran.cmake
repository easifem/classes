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
#gtk-4-fortran
IF( ${PROJECT_NAME} MATCHES "easifemBase" )
  OPTION( USE_GTK OFF )
  IF( USE_GTK )
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_GTK" )

    FIND_PACKAGE(PkgConfig)
    pkg_check_modules(GTKFORTRAN QUIET gtk-4-fortran)
    FIND_PATH(GTKFORTRAN_MODULE_DIR NAMES gtk.mod
    PATHS ${GTKFORTRAN_INCLUDE_DIRS})
    SET(GTKFORTRAN_LIBRARY ${GTKFORTRAN_LIBRARIES})
    SET(GTKFORTRAN_LIBRARY_DIRS ${GTKFORTRAN_LIBRARY_DIRS})
    SET(GTKFORTRAN_INCLUDE_DIR ${GTKFORTRAN_INCLUDE_DIRS})
    SET(GTKFORTRAN_MODULE_DIR ${GTKFORTRAN_MODULE_DIR})

    TARGET_LINK_LIBRARIES(
      ${PROJECT_NAME}
      PUBLIC
      ${GTKFORTRAN_LIBRARY_DIRS}
      ${GTKFORTRAN_LIBRARY}
      )
    TARGET_INCLUDE_DIRECTORIES(
      ${PROJECT_NAME}
      PUBLIC
      ${GTKFORTRAN_INCLUDE_DIR}
      ${PLPLOT_FORTRAN_INCLUDE_DIR}
      )
    MESSAGE( STATUS "PLPLOT_LIBRARY : ${PLPLOT_LIBRARY}" )
    MESSAGE( STATUS "PLPLOT_FORTRAN_LIBRARY : ${PLPLOT_FORTRAN_LIBRARY}" )
    MESSAGE( STATUS "PLPLOT_INCLUDE_DIR : ${PLPLOT_INCLUDE_DIR}" )
    MESSAGE( STATUS "PLPLOT_FORTRAN_INCLUDE_DIR : ${PLPLOT_FORTRAN_INCLUDE_DIR}" )
  ELSE()
    MESSAGE( STATUS "NOT USING PLPLOT LIBRARIES" )
  ENDIF()
ENDIF()
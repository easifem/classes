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
    pkg_check_modules(GTKFORTRAN REQUIRED gtk-4-fortran)
    FIND_PATH(
      GTKFORTRAN_MODULE_DIRS
      NAMES gtk.mod
      PATHS ${GTKFORTRAN_INCLUDE_DIRS})
    FIND_LIBRARY(GTKFORTRAN_LIBRARY NAMES gtk-4-fortran)


    MESSAGE( STATUS "GTKFORTRAN_CFLAGS : ${GTKFORTRAN_CFLAGS}" )
    MESSAGE( STATUS "GTKFORTRAN_LIBRARY : ${GTKFORTRAN_LIBRARY}" )
    MESSAGE( STATUS "GTKFORTRAN_LIBRARIES : ${GTKFORTRAN_LIBRARIES}" )
    MESSAGE( STATUS "GTKFORTRAN_LIBRARY_DIRS : ${GTKFORTRAN_LIBRARY_DIRS}" )
    MESSAGE( STATUS "GTKFORTRAN_INCLUDE_DIRS : ${GTKFORTRAN_INCLUDE_DIRS}" )
    MESSAGE( STATUS "GTKFORTRAN_MODULE_DIRS : ${GTKFORTRAN_MODULE_DIRS}" )


    TARGET_LINK_LIBRARIES(
      ${PROJECT_NAME}
      PUBLIC
      ${GTKFORTRAN_LIBRARY}
      ${GTKFORTRAN_LIBRARIES}
      )
    TARGET_INCLUDE_DIRECTORIES(
      ${PROJECT_NAME}
      PUBLIC
      ${GTKFORTRAN_INCLUDE_DIRS}
      ${GTKFORTRAN_MODULE_DIRS}
      )


  ELSE()
    MESSAGE( STATUS "NOT USING PLPLOT LIBRARIES" )
  ENDIF()
ENDIF()
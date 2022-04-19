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
#PLPLOT
IF( ${PROJECT_NAME} MATCHES "easifemBase" )
  OPTION( USE_PLPLOT OFF )
  IF( USE_PLPLOT )
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_PLPLOT" )
    IF( UNIX )
      IF(APPLE)
        SET(PLPLOT_INCLUDE_DIR
          "/usr/local/lib/fortran/modules/plplot" )
        SET(PLPLOT_LIBRARY
          "/usr/local/lib/libplplot.dylib" )
        SET(PLPLOT_FORTRAN_LIBRARY
          "/usr/local/lib/libplplotfortran.dylib" )
      ELSE()
        SET(PLPLOT_INCLUDE_DIR
          "/usr/lib/x86_64-linux-gnu/fortran/modules/plplot" )
        SET(PLPLOT_LIBRARY
          "/usr/lib/x86_64-linux-gnu/libplplot.so" )
        SET(PLPLOT_FORTRAN_LIBRARY
          "/usr/lib/x86_64-linux-gnu/libplplotfortran.so" )
      ENDIF()
    ENDIF()
    TARGET_LINK_LIBRARIES(
      ${PROJECT_NAME}
      PUBLIC
      ${PLPLOT_LIBRARY}
      ${PLPLOT_FORTRAN_LIBRARY}
      )
    TARGET_INCLUDE_DIRECTORIES(
      ${PROJECT_NAME}
      PUBLIC
      ${PLPLOT_INCLUDE_DIR}
      )
    MESSAGE( STATUS "PLPLOT_LIBRARY : ${PLPLOT_LIBRARY}" )
    MESSAGE( STATUS "PLPLOT_FORTRAN_LIBRARY : ${PLPLOT_FORTRAN_LIBRARY}" )
    MESSAGE( STATUS "PLPLOT_INCLUDE_DIR : ${PLPLOT_INCLUDE_DIR}" )
  ELSE()
    MESSAGE( STATUS "NOT USING PLPLOT LIBRARIES" )
  ENDIF()
ENDIF()
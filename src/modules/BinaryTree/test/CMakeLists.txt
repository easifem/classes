# This program is a part of
# EASIFEM
# version 2023.1.0
# Copyright (C) 2020-2023 Vikas Sharma
# Website: www.easifem.com
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
CMAKE_MINIMUM_REQUIRED(VERSION 3.20.0 FATAL_ERROR)
SET(PROJECT_NAME "easifemApp")
SET(PROJECT_SRC_PATH "/home/easifem/Dropbox/easifem/easifem-classes/src/modules/BinaryTree/test")
PROJECT(${PROJECT_NAME})
ENABLE_LANGUAGE(Fortran)
SET(TARGET_NAME "test")

IF(NOT CMAKE_BUILD_TYPE)
  SET(CMAKE_BUILD_TYPE Debug CACHE STRING "Build type" FORCE)
ENDIF()

IF (
${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU"
OR Fortran_COMPILER_NAME MATCHES "gfortran*")
LIST(
APPEND
FORTRAN_FLAGS
"-ffree-form"
"-ffree-line-length-none"
"-std=f2008"
"-fimplicit-none"
)
LIST(APPEND FORTRAN_FLAGS_RELEASE "-O3" )
LIST(
APPEND
FORTRAN_FLAGS_DEBUG
"-fbounds-check"
"-g" "-fbacktrace" "-Wextra" "-Wall" "-fprofile-arcs"
"-ftest-coverage" "-Wimplicit-interface"
)

ELSEIF(
${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel"
OR Fortran_COMPILER_NAME MATCHES "ifort*")
LIST(APPEND FORTRAN_FLAGS
"-r8" "-W1"
)
LIST(APPEND FORTRAN_FLAGS_RELEASE "-O3")
LIST(APPEND FORTRAN_FLAGS_DEBUG
"-O0" "-traceback" "-g" "-debug all"
"-check all" "-ftrapuv" "-warn" "nointerfaces"
)

ELSEIF (
${CMAKE_Fortran_COMPILER_ID} STREQUAL "XL"
OR Fortran_COMPILER_NAME MATCHES "xlf*")
LIST(APPEND FORTRAN_FLAGS
"-q64" "-qrealsize=8"
"-qsuffix=f=f90:cpp=f90")
LIST(APPEND FORTRAN_FLAGS_RELEASE
"-O3" "-qstrict")
LIST(APPEND FORTRAN_FLAGS_DEBUG
"-O0" "-g" "-qfullpath" "-qkeepparm")

ELSE ()
MESSAGE(ERROR "No optimized Fortran compiler flags are known")
ENDIF ()

LIST(
APPEND
CMAKE_PREFIX_PATH
"$ENV{EASIFEM_EXTPKGS}"
"$ENV{EASIFEM_BASE}"
"$ENV{EASIFEM_CLASSES}"
"$ENV{EASIFEM_MATERIALS}"
"$ENV{EASIFEM_KERNELS}"
)


LIST( 
  APPEND 
  EASIFEM_LIBS 
easifemClasses
) 

FOREACH(LIB ${EASIFEM_LIBS})
  FIND_PACKAGE(${LIB})
ENDFOREACH()

OPTION( TESTID, "Test number" )
OPTION( CLASSID, "Testing component" )

IF(NOT CLASSID)
  ADD_EXECUTABLE(${TARGET_NAME} ${FILE})
ELSE( )
  ADD_EXECUTABLE(${TARGET_NAME}
    ${PROJECT_SRC_PATH}/${CLASSID}${PROJECT_NAME}${TESTID}.F90)
ENDIF()

TARGET_LINK_LIBRARIES( ${TARGET_NAME}
easifemClasses::easifemClasses )


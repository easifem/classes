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

cmake_minimum_required(VERSION 3.20.0 FATAL_ERROR)
set(PROJECT_NAME "easifemClasses")
project(${PROJECT_NAME})

enable_language(Fortran C CXX)

set(VERSION_MAJOR "24")
set(VERSION_MINOR "10")
set(VERSION_BugFix "3")
set(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})

set(CMAKE_PROJECT_DESCRIPTION
    "${PROJECT_NAME} is  part of EASIFEM platform.
    EASIFEM: Expandable and Scalable Infrastructure for Finite Element Methods.
    ")

set(CMAKE_PROJECT_HOMEPAGE_URL "https://www.easifem.com")

set(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")
set(namespace "${PROJECT_NAME}")

include(CMakePrintHelpers)
include(FortranCInterface)

FortranCInterface_VERIFY()

list(APPEND TARGET_COMPILE_DEF "-DUSE_CMAKE")

# find my cmake modules here...
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)

# check error
if(" ${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL " ${CMAKE_CURRENT_BINARY_DIR}")
  message(
    FATAL_ERROR
      "[ERROR] :: Build directory and Source directory cannot be same.")
endif()

# make directories include(${PROJECT_SOURCE_DIR}/cmake/makeDirs.cmake)

include(GNUInstallDirs)

set(CMAKE_Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_INCLUDEDIR})

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_LIBDIR})

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_INSTALL_BINDIR})

set(INSTALL_LIBDIR
    ${CMAKE_INSTALL_LIBDIR}
    CACHE PATH "Installation location of lib")

set(INSTALL_INCLUDEDIR
    ${CMAKE_INSTALL_INCLUDEDIR}
    CACHE PATH "Installation location of module files")

set(INSTALL_BINDIR
    ${CMAKE_INSTALL_BINDIR}
    CACHE PATH "Installation location of binary files")

if(WIN32 AND NOT CYGWIN)
  set(DEF_INSTALL_CMAKEDIR CMake)
else()
  set(DEF_INSTALL_CMAKEDIR share/cmake/${PROJECT_NAME})
endif()

set(INSTALL_CMAKEDIR
    ${DEF_INSTALL_CMAKEDIR}
    CACHE PATH "Installation directory for CMake files")

foreach(p LIB BIN INCLUDE CMAKE)
  file(TO_NATIVE_PATH ${CMAKE_INSTALL_PREFIX}/${INSTALL_${p}DIR} _path)
  message(STATUS "Installing ${p} componenets to ${_path}")
endforeach()

option(BUILD_SHARED_LIBS "Build shared library" ON)

if(BUILD_SHARED_LIBS)
  message(STATUS "${PROJECT_NAME} will be built as a shared library.")
  add_library(${PROJECT_NAME} SHARED "")
  set_property(TARGET ${PROJECT_NAME} PROPERTY POSITION_INDEPENDENT_CODE TRUE)
else()
  message(STATUS "${PROJECT_NAME} will be built as a static library.")
  add_library(${PROJECT_NAME} STATIC "")
endif()

message(
  STATUS
    "[INFO] :: Is the Fortran compiler loaded? ${CMAKE_Fortran_COMPILER_LOADED}"
)

if(CMAKE_Fortran_COMPILER_LOADED)
  message(STATUS "[INFO] :: Fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
  message(
    STATUS
      "[INFO] :: Fortran compiler version is: ${CMAKE_Fortran_COMPILER_VERSION}"
  )
endif()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE
      Release
      CACHE STRING "Build type" FORCE)
endif()

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU" OR Fortran_COMPILER_NAME MATCHES
                                                  "gfortran*")

  list(
    APPEND
    FORTRAN_FLAGS
    "-ffree-form"
    "-ffree-line-length-none"
    "-std=f2018"
    "-fimplicit-none"
    "-fno-range-check")

  list(APPEND FORTRAN_FLAGS_RELEASE "-O3")

  if(APPLE)
    list(
      APPEND
      FORTRAN_FLAGS_DEBUG
      "-fbounds-check"
      "-g"
      "-fbacktrace"
      "-Wextra"
      "-Wall"
      # "-fprofile-arcs"
      "-ftest-coverage"
      "-Wimplicit-interface")

  else()
    list(
      APPEND
      FORTRAN_FLAGS_DEBUG
      "-fbounds-check"
      "-g"
      "-fbacktrace"
      "-Wextra"
      "-Wall"
      # "-fprofile-arcs"
      "-ftest-coverage"
      "-Wimplicit-interface")
  endif()

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel" OR Fortran_COMPILER_NAME
                                                        MATCHES "ifort*")
  list(APPEND FORTRAN_FLAGS "-r8" "-W1")
  list(APPEND FORTRAN_FLAGS_RELEASE "-O3")
  list(
    APPEND
    FORTRAN_FLAGS_DEBUG
    "-O0"
    "-traceback"
    "-g"
    "-debug all"
    "-check all"
    "-ftrapuv"
    "-warn"
    "nointerfaces")

elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "XL" OR Fortran_COMPILER_NAME
                                                     MATCHES "xlf*")

  list(APPEND FORTRAN_FLAGS "-q64" "-qrealsize=8" "-qsuffix=f=f90:cpp=f90")
  list(APPEND FORTRAN_FLAGS_RELEASE "-O3" "-qstrict")
  list(APPEND FORTRAN_FLAGS_DEBUG "-O0" "-g" "-qfullpath" "-qkeepparm")

else()
  message(ERROR "[ERROR] :: No optimized Fortran compiler flags are known")
endif()

cmake_print_variables(FORTRAN_FLAGS)
cmake_print_variables(FORTRAN_FLAGS_RELEASE)
cmake_print_variables(FORTRAN_FLAGS_DEBUG)

# include(${PROJECT_SOURCE_DIR}/cmake/targetCompileOpts.cmake)
target_compile_options(
  ${PROJECT_NAME}
  PRIVATE ${TARGET_COMPILE_OPT} ${FORTRAN_FLAGS}
          "$<$<CONFIG:Debug>:${FORTRAN_FLAGS_DEBUG}>"
          "$<$<CONFIG:Release>:${FORTRAN_FLAGS_RELEASE}>")

# include(${PROJECT_SOURCE_DIR}/cmake/targetIncludeDirs.cmake)
target_include_directories(
  ${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
                         $<INSTALL_INTERFACE:${INSTALL_INCLUDEDIR}>)

# include(${PROJECT_SOURCE_DIR}/cmake/targetProperties.cmake) target properties
set_target_properties(
  ${PROJECT_NAME}
  PROPERTIES POSITION_INDEPENDENT_CODE 1
             SOVERSION ${VERSION_MAJOR}
             OUTPUT_NAME ${PROJECT_NAME}
             LIBRARY_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
             RUNTIME_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
             MACOSX_RPATH ON
             WINDOWS_EXPORT_ALL_SYMBOLS ON)

# include(${PROJECT_SOURCE_DIR}/cmake/targetCompileDefs.cmake)
option(USE_REAL32 OFF)
option(USE_REAL64 ON)

if(USE_REAL32)
  list(APPEND TARGET_COMPILE_DEF "-DUSE_Real32")
endif()

if(USE_REAL64)
  list(APPEND TARGET_COMPILE_DEF "-DUSE_Real64")
endif()

option(USE_INT32 ON)
if(USE_INT32)
  list(APPEND TARGET_COMPILE_DEF "-DUSE_Int32")
endif()

option(USE_INT64 OFF)
if(USE_INT64)
  list(APPEND TARGET_COMPILE_DEF "-DUSE_Int64")
endif()

list(APPEND TARGET_COMPILE_DEF "-D${CMAKE_HOST_SYSTEM_NAME}_SYSTEM")

# DEFINE DEBUG
if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
  list(APPEND TARGET_COMPILE_DEF "-DDEBUG_VER")
endif()

# include(${PROJECT_SOURCE_DIR}/cmake/install.cmake) Installation
install(
  DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  DESTINATION "./"
  COMPONENT "${PROJECT_NAME}")

install(
  EXPORT ${TARGETS_EXPORT_NAME}
  FILE "${TARGETS_EXPORT_NAME}.cmake"
  NAMESPACE ${namespace}::
  DESTINATION ${INSTALL_CMAKEDIR}
  COMPONENT "${PROJECT_NAME}")

include(CMakePackageConfigHelpers)

write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY AnyNewerVersion)

configure_package_config_file(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  INSTALL_DESTINATION ${INSTALL_CMAKEDIR}
  PATH_VARS INSTALL_INCLUDEDIR)

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  DESTINATION ${INSTALL_CMAKEDIR}
  COMPONENT "${PROJECT_NAME}-dev")

# # Find external dependency of the project FIXME: # needed for python-baesd cli
# # # will deprecate in future if(NOT CMAKE_PREFIX_PATH) list(APPEND
# CMAKE_PREFIX_PATH "$ENV{EASIFEM_EXTPKGS}" "$ENV{EASIFEM_BASE}") endif()

# linking include(${PROJECT_SOURCE_DIR}/cmake/targetLinkLibs.cmake)
find_package(easifemBase REQUIRED)
if(easifemBase_FOUND)
  message(STATUS "FOUND easifemBase")
else()
  message(ERROR "NOT FOUND easifemBase")
endif()
target_link_libraries(${PROJECT_NAME} PUBLIC easifemBase::easifemBase)

# Add source files
include(src/modules/CMakeLists.txt)
include(src/submodules/CMakeLists.txt)

# this should be in the end.
target_compile_definitions(${PROJECT_NAME} PUBLIC ${TARGET_COMPILE_DEF})
message(STATUS "[INFO] :: Compiler definition : ${TARGET_COMPILE_DEF}")

install(
  TARGETS ${PROJECT_NAME} ${C_PROJECTS}
  EXPORT ${TARGETS_EXPORT_NAME}
  COMPONENT "${PROJECT_NAME}"
  ARCHIVE DESTINATION ${INSTALL_LIBDIR}
  RUNTIME DESTINATION ${INSTALL_BINDIR}
  LIBRARY DESTINATION ${INSTALL_LIBDIR})

include(${PROJECT_SOURCE_DIR}/cmake/packaging.cmake)

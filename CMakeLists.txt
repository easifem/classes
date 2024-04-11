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
enable_language(C)
enable_language(Fortran)
enable_language(CXX)
set(VERSION_MAJOR "23")
set(VERSION_MINOR "6")
set(VERSION_BugFix "0")
set(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})
set(CMAKE_PROJECT_DESCRIPTION
    "Expandable and Scalable Infrastructure for Finite Element Methods")
set(CMAKE_PROJECT_HOMEPAGE_URL
    "https://github.com/vickysharma0812/easifem-classes")
set(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")
set(namespace "${PROJECT_NAME}")
include(CMakePrintHelpers)
include(FortranCInterface)
FortranCInterface_VERIFY()
list(APPEND TARGET_COMPILE_DEF "-DUSE_CMAKE")
# find my cmake modules here...
list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
include(${PROJECT_SOURCE_DIR}/cmake/checkError.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/makeDirs.cmake)
if(BUILD_SHARED_LIBS)
  add_library(${PROJECT_NAME} SHARED "")
else()
  add_library(${PROJECT_NAME} STATIC "")
endif()
include(${PROJECT_SOURCE_DIR}/cmake/Compiler.cmake)
# Add source files
include(src/modules/CMakeLists.txt)
include(src/submodules/CMakeLists.txt)
include(${PROJECT_SOURCE_DIR}/cmake/prefixPaths.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addOpenBLAS.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addOpenMP.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addSuperLU.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addLIS.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addMetis.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addARPACK.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addHDF5.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addPLPLOT.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addGmsh.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addFFTW.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/addGTKFortran.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/targetLinkLibs.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/targetIncludeDirs.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/targetCompileOpts.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/targetCompileDefs.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/targetProperties.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/install.cmake)
include(${PROJECT_SOURCE_DIR}/cmake/packaging.cmake)

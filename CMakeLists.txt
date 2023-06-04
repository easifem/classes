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

CMAKE_MINIMUM_REQUIRED(VERSION 3.20.0 FATAL_ERROR)
SET(PROJECT_NAME "easifemClasses")
PROJECT(${PROJECT_NAME})
ENABLE_LANGUAGE(C)
ENABLE_LANGUAGE(Fortran)
ENABLE_LANGUAGE(CXX)
SET(VERSION_MAJOR "23")
SET(VERSION_MINOR "1")
SET(VERSION_BugFix "0")
SET(PROJECT_VERSION ${VERSION_MAJOR}.${VERSION_MINOR}.${VERSION_BugFix})
SET(CMAKE_PROJECT_DESCRIPTION "Expandable and Scalable Infrastructure for Finite Element Methods")
SET(CMAKE_PROJECT_HOMEPAGE_URL "https://github.com/vickysharma0812/easifem-classes")
SET(TARGETS_EXPORT_NAME "${PROJECT_NAME}Targets")
SET(namespace "${PROJECT_NAME}")
INCLUDE(CMakePrintHelpers)
INCLUDE(FortranCInterface)
FortranCInterface_VERIFY()
LIST( APPEND TARGET_COMPILE_DEF "-DUSE_CMAKE" )
# find my cmake modules here...
LIST(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Modules)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/checkError.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/makeDirs.cmake)
IF(BUILD_SHARED_LIBS)
  ADD_LIBRARY(${PROJECT_NAME} SHARED "" )
ELSE()
  ADD_LIBRARY(${PROJECT_NAME} STATIC "")
ENDIF()
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/Compiler.cmake)
# Add source files
INCLUDE(src/modules/CMakeLists.txt)
INCLUDE(src/submodules/CMakeLists.txt)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/prefixPaths.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addOpenBLAS.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addOpenMP.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addSuperLU.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addLIS.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addMetis.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addARPACK.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addHDF5.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addPLPLOT.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addGmsh.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addFFTW.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/addGTKFortran.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetLinkLibs.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetIncludeDirs.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetCompileOpts.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetCompileDefs.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/targetProperties.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/install.cmake)
INCLUDE(${PROJECT_SOURCE_DIR}/cmake/packaging.cmake)

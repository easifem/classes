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
#GMSH SDK
IF( ${PROJECT_NAME} MATCHES "easifemClasses" )
  OPTION( USE_GMSH_SDK OFF )
  IF( USE_GMSH_SDK )
    LIST( APPEND TARGET_COMPILE_DEF "-DUSE_GMSH_SDK" )
    FIND_PACKAGE(PkgConfig REQUIRED)
    FIND_LIBRARY(GMSH_LIBRARY
      NAMES gmsh gmsh.4.12
      PATHS "$ENV{CONDA_PREFIX}/lib" "/opt/homebrew/lib" )
      # PATHS "/opt/homebrew/lib" )
    SET(GMSH_LIBRARIES ${GMSH_LIBRARY})

    INCLUDE(FindPackageHandleStandardArgs)
    FIND_PACKAGE_HANDLE_STANDARD_ARGS(
      GMSH DEFAULT_MSG
      GMSH_LIBRARIES
    )

    # SET(GMSH_LIBRARIES "$ENV{CONDA_PREFIX}/lib/libgmsh.so")

    TARGET_LINK_LIBRARIES(
      ${PROJECT_NAME}
      PUBLIC
      ${GMSH_LIBRARIES}
      )

    MESSAGE( STATUS "GMSH_LIBRARIES : ${GMSH_LIBRARIES}" )
  ELSE()
    MESSAGE( STATUS "NOT USING GMSH SDK LIBRARIES" )
  ENDIF()
ENDIF()

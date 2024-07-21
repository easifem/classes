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


###############################
###### I N S T A L L ##########
###############################

INSTALL(
  TARGETS ${PROJECT_NAME}
  EXPORT ${TARGETS_EXPORT_NAME}
  COMPONENT "${PROJECT_NAME}"
  ARCHIVE
    DESTINATION ${INSTALL_LIBDIR}
  RUNTIME
    DESTINATION ${INSTALL_BINDIR}
  LIBRARY
    DESTINATION ${INSTALL_LIBDIR}
)

INSTALL(
  DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  DESTINATION "./"
  COMPONENT "${PROJECT_NAME}"
  # COMPONENT lib 
)

INSTALL(
  EXPORT ${TARGETS_EXPORT_NAME}
  FILE "${TARGETS_EXPORT_NAME}.cmake"
  NAMESPACE ${namespace}::
  DESTINATION ${INSTALL_CMAKEDIR}
  COMPONENT "${PROJECT_NAME}"
  # COMPONENT dev
)

INCLUDE(CMakePackageConfigHelpers)

WRITE_BASIC_PACKAGE_VERSION_FILE(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY AnyNewerVersion
)

CONFIGURE_PACKAGE_CONFIG_FILE(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  INSTALL_DESTINATION ${INSTALL_CMAKEDIR}
  PATH_VARS INSTALL_INCLUDEDIR
)

INSTALL(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  DESTINATION ${INSTALL_CMAKEDIR}
  COMPONENT "${PROJECT_NAME}-dev"
)

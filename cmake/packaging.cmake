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

SET(CPACK_PACKAGE_NAME ${PROJECT_NAME})
SET(CPACK_PACKAGE_VENDOR "easifem.com")
SET(CPACK_PACKAGE_HOMEPAGE_URL "https://www.easifem.com")
set(CPACK_PACKAGE_CONTACT "vickysharma0812@gmail.com")



SET(CPACK_VERBATIM_VARIABLES YES)
# If set to TRUE, values of variables prefixed with CPACK_ will be escaped
# before being written to the configuration files, so that the cpack
# program receives them exactly as they were specified. If not, characters
# like quotes and backslashes can cause parsing errors or alter the value
# received by the cpack program. Defaults to FALSE for
# backwards compatibility.

SET(CPACK_PACKAGE_DESCRIPTION
"
Expandable And Scalable Infrastructure for Finite Element Methods, 
EASIFEM, is a computational framework for FEM written in Modern-Fortran. 
easifemClasses is a critical part of EASIFEM framework. It contains many
useful components which are necessary for building higher level classes
of finite element code.
")

# CPACK_PACKAGE_DESCRIPTION_FILE
# A text file used to describe the project when CPACK_PACKAGE_DESCRIPTION is not explicitly set. The default value for CPACK_PACKAGE_DESCRIPTION_FILE points to a built-in template file Templates/CPack.GenericDescription.txt.

SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY
"
Expandable And Scalable Infrastructure for Finite Element Methods, 
EASIFEM, is a computational framework for FEM written in Modern-Fortran. 
easifemClasses is a critical part of EASIFEM framework.
=======================================================================
")

# A description of the project, used in places such as the introduction
# screen of CPack-generated Windows installers. If not set, the value of this
# variable is populated from the file named by CPACK_PACKAGE_DESCRIPTION_FILE.



SET(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")

SET(CPACK_PACKAGE_VERSION_MAJOR "${VERSION_MAJOR}")
# Package major version. This variable will always be set, but its default
# value depends on whether or not version details were given to the project()
# command in the top level CMakeLists.txt file. If version details were given,
# the default value will be CMAKE_PROJECT_VERSION_MAJOR. If no version details
# were given, a default version of 0.1.1 will be assumed, leading to
# CPACK_PACKAGE_VERSION_MAJOR having a default value of 0.

SET(CPACK_PACKAGE_VERSION_MINOR "${VERSION_MINOR}")
# Package minor version. The default value is determined based on whether or
# not version details were given to the project() command in the top level
# CMakeLists.txt file. If version details were given, the default value willbe
# CMAKE_PROJECT_VERSION_MINOR, but if no minor version component was specified
# then CPACK_PACKAGE_VERSION_MINOR will be left unset. If no project version
# was given at all, a default version of 0.1.1 will be assumed, leading to
# CPACK_PACKAGE_VERSION_MINOR having a default value of 1.

SET(CPACK_PACKAGE_VERSION_PATCH "${VERSION_BugFix}")
# Package patch version. The default value is determined based on whether
# or not version details were given to the project() command in the top
# level CMakeLists.txt file. If version details were given, the default
# value will be CMAKE_PROJECT_VERSION_PATCH, but if no patch version
# component was specified then CPACK_PACKAGE_VERSION_PATCH will be left
# unset. If no project version was given at all, a default version of
# 0.1.1 will be assumed, leading to CPACK_PACKAGE_VERSION_PATCH having
# a default value of 1.


# CPACK_PACKAGE_ICON
# A branding image that will be displayed inside the installer (used by GUI installers).

SET(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/LICENSE")
# License to be embedded in the installer. It will typically be displayed to the user by the produced installer (often with an explicit "Accept" button, for graphical installers) prior to installation. This license file is NOT added to the installed files but is used by some CPack generators like NSIS. If you want to use UTF-8 characters, the file needs to be encoded in UTF-8 BOM. If you want to install a license file (may be the same as this one) along with your project, you must add an appropriate CMake install() command in your CMakeLists.txt.

# CPACK_RESOURCE_FILE_README
set(CPACK_RESOURCE_FILE_README "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
# ReadMe file to be embedded in the installer. It typically describes in some detail the purpose of the project during the installation. Not all CPack generators use this file.

# CPACK_RESOURCE_FILE_WELCOME
# Welcome file to be embedded in the installer. It welcomes users to this installer. Typically used in the graphical installers on Windows and Mac OS X.

# CPACK_MONOLITHIC_INSTALL¶
# Disables the component-based installation mechanism. When set, the component specification is ignored and all installed items are put in a single "MONOLITHIC" package. Some CPack generators do monolithic packaging by default and may be asked to do component packaging by setting CPACK_<GENNAME>_COMPONENT_INSTALL to TRUE.

# CPACK_PACKAGE_CHECKSUM¶
# An algorithm that will be used to generate an additional file with the checksum of the package. The output file name will be:

SET(CPACK_PACKAGE_DIRECTORY ${PROJECT_BINARY_DIR}/package)
# The directory in which CPack is doing its packaging. If it is not set then
# this will default (internally) to the build dir.
# This variable may be defined in a CPack config file or from the cpack
# command line option -B.
# If set, the command line option overrides the value found in the
# config file.

SET(CPACK_PACKAGE_INSTALL_DIRECTORY ${CPACK_PACKAGE_NAME} )
# Installation directory on the target system. This may be used by
# some CPack generators like NSIS to create an installation directory
#  e.g., "CMake 2.5" below the installation prefix.
# All installed elements will be put inside this directory.


LIST(
  APPEND
  CpackGen
  DEB
)
# TGZ
SET(CPACK_GENERATOR "${CpackGen}")
# SET(CPACK_SOURCE_GENERATOR "TGZ DEB")

# List of CPack generators to use. If not specified, CPack will create a set of options following the naming pattern CPACK_BINARY_<GENNAME> (e.g. CPACK_BINARY_NSIS) allowing the user to enable/disable individual generators. If the -G option is given on the cpack command line, it will override this variable and any CPACK_BINARY_<GENNAME> options.

SET(CPACK_SOURCE_IGNORE_FILES .git/ .github/ .vscode/ .mypy_cache/ _CPack_Packages/
${CMAKE_BINARY_DIR}/ ${PROJECT_BINARY_DIR}/  ".*~$"
)

SET(CPACK_STRIP_FILES YES)
# List of files to be stripped. Starting with CMake 2.6.0, CPACK_STRIP_FILES will be a boolean variable which enables stripping of all files (a list of files evaluates to TRUE in CMake, so this change is compatible).


SET(CPACK_OUTPUT_FILE_PREFIX "${CMAKE_SOURCE_DIR}/_packages")

SET(CPACK_PACKAGING_INSTALL_PREFIX "/opt/easifem/classes/")
#/${CMAKE_PROJECT_VERSION}")

SET(
    CPACK_INSTALL_DEFAULT_DIRECTORY_PERMISSIONS
    OWNER_READ OWNER_WRITE OWNER_EXECUTE
    GROUP_READ GROUP_EXECUTE
    WORLD_READ WORLD_EXECUTE
)

# CPACK_OUTPUT_CONFIG_FILE
# The name of the CPack binary configuration file. This file is the CPack configuration generated by the CPack module for binary installers. Defaults to CPackConfig.cmake.

# CPACK_SOURCE_OUTPUT_CONFIG_FILE¶
# The name of the CPack source configuration file. This file is the CPack configuration generated by the CPack module for source installers. Defaults to CPackSourceConfig.cmake.


# CPACK_PACKAGE_EXECUTABLES¶
# Lists each of the executables and associated text label to be used to create Start Menu shortcuts. For example, setting this to the list ccmake;CMake will create a shortcut named "CMake" that will execute the installed executable ccmake. Not all CPack generators use it (at least NSIS, and WIX do).

SET(CPACK_DEBIAN_FILE_NAME DEB-DEFAULT)
SET(CPACK_COMPONENTS_GROUPING ALL_COMPONENTS_IN_ONE)
SET(CPACK_DEB_COMPONENT_INSTALL YES)
SET(CPACK_DEBIAN_PACKAGE_NAME, "${CPACK_PACKAGE_NAME}")
# SET(CPACK_DEBIAN_PACKAGE_ARCHITECTURE,  "i386")
SET(CPACK_DEBIAN_PACKAGE_SHLIBDEPS YES)
SET(CPACK_DEBIAN_PACKAGE_MAINTAINER "Vikas Sharma <${CPACK_PACKAGE_CONTACT}>")
SET(CPACK_DEBIAN_PACKAGE_SECTION, "devl")
SET(CPACK_DEBIAN_PACKAGE_PRIORITY, "optional")

INCLUDE(CPack)
MESSAGE(STATUS "Components to pack: ${CPACK_COMPONENTS_ALL}")

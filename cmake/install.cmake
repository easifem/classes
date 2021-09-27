###############################
###### I N S T A L L ##########
###############################
INSTALL(
  TARGETS
    ${PROJECT_NAME}
  EXPORT
    ${TARGETS_EXPORT_NAME}
  ARCHIVE
    DESTINATION ${INSTALL_LIBDIR}
    COMPONENT lib
  RUNTIME
    DESTINATION ${INSTALL_BINDIR}
    COMPONENT bin
  LIBRARY
    DESTINATION ${INSTALL_LIBDIR}
    COMPONENT lib )

INSTALL(
  DIRECTORY
    ${CMAKE_Fortran_MODULE_DIRECTORY}
    DESTINATION "./"
    COMPONENT lib )

INSTALL(
  EXPORT
    ${TARGETS_EXPORT_NAME}
    NAMESPACE ${namespace}::
    DESTINATION ${INSTALL_CMAKEDIR}
    COMPONENT dev)

INCLUDE(CMakePackageConfigHelpers)

CONFIGURE_PACKAGE_CONFIG_FILE(
  ${CMAKE_CURRENT_SOURCE_DIR}/cmake/Config.cmake.in
  ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
  INSTALL_DESTINATION ${INSTALL_CMAKEDIR})

IF(BUILD_SHARED_LIBS OR PROJECT_VERSION_MAJOR EQUAL 0)
  # Due to the uncertain ABI compatibility of Fortran shared libraries
  # limit compatibility for dynamic linking to same minor version.
  SET(COMPATIBILITY AnyNewerVersion)
  # set(COMPATIBILITY SameMinorVersion)
ELSE()
  # Require API compatibility via semantic versioning for static linking.
  SET(COMPATIBILITY AnyNewerVersion)
  # set(COMPATIBILITY SameMajorVersion)
ENDIF()

WRITE_BASIC_PACKAGE_VERSION_FILE(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ${COMPATIBILITY})

INSTALL(
  FILES
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake
  DESTINATION
    ${INSTALL_CMAKEDIR})
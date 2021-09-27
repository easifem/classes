# SYSTEM INFORMATION
# Disable in-source builds to prevent source tree corruption.
IF(" ${CMAKE_CURRENT_SOURCE_DIR}" STREQUAL " ${CMAKE_CURRENT_BINARY_DIR}")
  MESSAGE(FATAL_ERROR "
    FATAL: In-source builds are not allowed.
    You should create a separate directory for build files.")
ENDIF()
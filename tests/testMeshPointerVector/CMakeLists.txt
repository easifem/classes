# This file is a part of easifem-base
# (c) 2021, all right reserved, Dr Vikas Sharma
#
# log
# 22/02/2021 this file was created
#-----------------------------------------------------------------------

CMAKE_MINIMUM_REQUIRED(VERSION 3.10.0 FATAL_ERROR)
SET(PROJECT_NAME "MeshPointerVector")
PROJECT(${PROJECT_NAME})
ENABLE_LANGUAGE(Fortran)
SET(TARGET_NAME "test")

LIST(APPEND CMAKE_Fortran_FLAGS "-cpp")

ADD_EXECUTABLE(${TARGET_NAME} test_${PROJECT_NAME}.f90)
LIST(APPEND CMAKE_PREFIX_PATH "$ENV{EASIFEM_EXTPKGS}" "$ENV{EASIFEM_BASE}" "$ENV{EASIFEM_CLASSES}")
LIST(APPEND LIBS "LAPACK95" "PENF" "BeFoR64" "FACE" "StringiFor" "FPL" "easifemBase" "easifemClasses")
FOREACH(LIB ${LIBS})
  FIND_PACKAGE(${LIB})
  # INTERFACE_LINK_LIBRARIES
  IF(${LIB}_FOUND)
    GET_TARGET_PROPERTY(
      INCLUDEDIR
      ${LIB}::${LIB}
      INTERFACE_INCLUDE_DIRECTORIES)
    TARGET_INCLUDE_DIRECTORIES(
      ${TARGET_NAME}
      PRIVATE
      ${INCLUDEDIR} )
    TARGET_LINK_LIBRARIES(
      ${TARGET_NAME}
      PUBLIC
      ${LIB}::${LIB} )
    MESSAGE(STATUS "${LIB}::${LIB}")
  ELSE()
    MESSAGE(ERROR " ${LIB} not found")
  ENDIF()
ENDFOREACH()

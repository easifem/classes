SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ScalarField_Class.F90
  ${src_path}/STScalarField_Class.F90
)
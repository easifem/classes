set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ScalarField_Class@Constructor.F90
  ${src_path}/ScalarField_Class@IOMethods.F90
  ${src_path}/ScalarField_Class@SetMethods.F90
  ${src_path}/ScalarField_Class@GetMethods.F90
  ${src_path}/STScalarField_Class@Constructor.F90
  ${src_path}/STScalarField_Class@IOMethods.F90
  ${src_path}/STScalarField_Class@SetMethods.F90
  ${src_path}/STScalarField_Class@GetMethods.F90
)
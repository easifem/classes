set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ScalarField_Class@Constructor.f90
  ${src_path}/ScalarField_Class@IOMethods.f90
  ${src_path}/ScalarField_Class@SetMethods.f90
  ${src_path}/ScalarField_Class@GetMethods.f90
  ${src_path}/STScalarField_Class@Constructor.f90
  ${src_path}/STScalarField_Class@IOMethods.f90
  ${src_path}/STScalarField_Class@SetMethods.f90
  ${src_path}/STScalarField_Class@GetMethods.f90
)
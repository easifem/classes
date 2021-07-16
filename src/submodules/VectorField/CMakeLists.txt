set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/VectorField_Class@Constructor.f90
  ${src_path}/VectorField_Class@IO.f90
  ${src_path}/VectorField_Class@SetMethods.f90
  ${src_path}/VectorField_Class@GetMethods.f90
  ${src_path}/STVectorField_Class@Constructor.f90
  ${src_path}/STVectorField_Class@IO.f90
  ${src_path}/STVectorField_Class@SetMethods.f90
  ${src_path}/STVectorField_Class@GetMethods.f90
)
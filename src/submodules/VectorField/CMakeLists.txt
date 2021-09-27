set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/VectorField_Class@Constructor.F90
  ${src_path}/VectorField_Class@IOMethods.F90
  ${src_path}/VectorField_Class@SetMethods.F90
  ${src_path}/VectorField_Class@GetMethods.F90
  ${src_path}/STVectorField_Class@Constructor.F90
  ${src_path}/STVectorField_Class@IOMethods.F90
  ${src_path}/STVectorField_Class@SetMethods.F90
  ${src_path}/STVectorField_Class@GetMethods.F90
)
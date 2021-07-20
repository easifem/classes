set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/MatrixField_Class@Constructor.f90
  ${src_path}/MatrixField_Class@IO.f90
  ${src_path}/MatrixField_Class@GetMethods.f90
  ${src_path}/MatrixField_Class@MatVecMethods.f90
  ${src_path}/MatrixField_Class@LUsolveMethods.f90
  ${src_path}/MatrixField_Class@PreconditionMethods.f90
)
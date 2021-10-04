SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/MatrixField_Class@Constructor.F90
  ${src_path}/MatrixField_Class@IOMethods.F90
  ${src_path}/MatrixField_Class@GetMethods.F90
  ${src_path}/MatrixField_Class@SetMethods.F90
  ${src_path}/MatrixField_Class@MatVecMethods.F90
  ${src_path}/MatrixField_Class@LUsolveMethods.F90
  ${src_path}/MatrixField_Class@PreconditionMethods.F90
)
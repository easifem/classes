SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/BlockMatrixField_Class@ConstructorMethods.F90
  ${src_path}/BlockMatrixField_Class@IOMethods.F90
  ${src_path}/BlockMatrixField_Class@GetMethods.F90
  ${src_path}/BlockMatrixField_Class@SetMethods.F90
  ${src_path}/BlockMatrixField_Class@MatVecMethods.F90
  ${src_path}/BlockMatrixField_Class@LUsolveMethods.F90
  ${src_path}/BlockMatrixField_Class@PreconditionMethods.F90
)
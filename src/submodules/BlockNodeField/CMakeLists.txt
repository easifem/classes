set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/BlockNodeField_Class@ConstructorMethods.F90
  ${src_path}/BlockNodeField_Class@IOMethods.F90
)
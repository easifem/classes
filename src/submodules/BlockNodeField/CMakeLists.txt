set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/BlockNodeField_Class@ConstructorMethods.f90
  ${src_path}/BlockNodeField_Class@IOMethods.f90
)
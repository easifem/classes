SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/BlockNodeField_Class@ConstructorMethods.F90
  ${src_path}/BlockNodeField_Class@IOMethods.F90
)
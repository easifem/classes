SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/UserFunction_Class@ConstructorMethods.F90
  ${src_path}/UserFunction_Class@IOMethods.F90
  ${src_path}/UserFunction_Class@GetMethods.F90
  ${src_path}/UserFunction_Class@SetMethods.F90
)
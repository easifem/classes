set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/XMLTag_Class@ConstructorMethods.f90
  ${src_path}/XMLTag_Class@GetMethods.f90
  ${src_path}/XMLTag_Class@SetMethods.f90
  ${src_path}/XMLTag_Class@IOMethods.f90
)
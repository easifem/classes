set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Vector_Class@Constructor.f90
  ${src_path}/Vector_Class@IO.f90
  ${src_path}/Vector_Class@SetMethods.f90
  ${src_path}/Vector_Class@GetMethods.f90
)
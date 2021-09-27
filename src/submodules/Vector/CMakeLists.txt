set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Vector_Class@Constructor.F90
  ${src_path}/Vector_Class@IO.F90
  ${src_path}/Vector_Class@SetMethods.F90
  ${src_path}/Vector_Class@GetMethods.F90
)
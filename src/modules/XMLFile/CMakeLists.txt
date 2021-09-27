set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/XMLTag_Class.F90
  ${src_path}/XMLFile_Class.F90
)
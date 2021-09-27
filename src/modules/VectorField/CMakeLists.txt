set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/VectorField_Class.F90
  ${src_path}/STVectorField_Class.F90
)
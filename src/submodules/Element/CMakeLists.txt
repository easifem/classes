set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Element_Class@Constructor.f90
  ${src_path}/Element_Class@IO.f90
  ${src_path}/Element_Class@ShapeData.f90
  ${src_path}/Element_Class@Virtual.f90
)
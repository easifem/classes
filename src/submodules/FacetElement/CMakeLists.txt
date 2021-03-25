set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/FacetElement_Class@Constructor.f90
  ${src_path}/FacetElement_Class@IO.f90
  ${src_path}/FacetElement_Class@ShapeData.f90
)
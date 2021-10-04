SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/FacetElement_Class@Constructor.F90
  ${src_path}/FacetElement_Class@IO.F90)
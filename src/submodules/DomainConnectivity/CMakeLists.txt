SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/DomainConnectivity_Class@ConstructorMethods.F90
  # ${src_path}/DomainConnectivity_Class@IOMethods.F90
  ${src_path}/DomainConnectivity_Class@FacetMethods.F90
  ${src_path}/DomainConnectivity_Class@CellMethods.F90
  ${src_path}/DomainConnectivity_Class@NodeMethods.F90
)

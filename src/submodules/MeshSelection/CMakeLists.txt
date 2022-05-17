SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/MeshSelection_Class@ConstructorMethods.F90
  ${src_path}/MeshSelection_Class@IOMethods.F90
  ${src_path}/MeshSelection_Class@SetMethods.F90
  ${src_path}/MeshSelection_Class@GetMethods.F90
  ${src_path}/MeshSelection_Class@GetElemNumMethods.F90
  ${src_path}/MeshSelection_Class@GetNodeNumMethods.F90
)
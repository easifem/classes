set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/MeshSelection_Class@ConstructorMethods.f90
  ${src_path}/MeshSelection_Class@IOMethods.f90
  ${src_path}/MeshSelection_Class@SetMethods.f90
  ${src_path}/MeshSelection_Class@GetMethods.f90
)
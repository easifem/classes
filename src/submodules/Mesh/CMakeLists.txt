SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Mesh_Class@ConstructorMethods.F90
  ${src_path}/Mesh_Class@IOMethods.F90
  ${src_path}/Mesh_Class@getMethod.F90
  ${src_path}/Mesh_Class@setMethod.F90
  ${src_path}/Mesh_Class@MeshDataMethods.F90
  )
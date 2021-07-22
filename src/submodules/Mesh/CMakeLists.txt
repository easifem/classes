SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Mesh_Class@Constructor.f90
  ${src_path}/Mesh_Class@IO.f90
  ${src_path}/Mesh_Class@getMethod.f90
  ${src_path}/Mesh_Class@setMethod.f90
  ${src_path}/Mesh_Class@MeshDataMethods.f90
  )
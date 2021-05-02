SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/GmshInterface/C_interface_module.f90
  ${src_path}/GmshInterface/GmshInterface.f90
  ${src_path}/Gmsh_Class.f90
  ${src_path}/GmshGraphics_Class.f90
  ${src_path}/GmshOption_Class.f90
  ${src_path}/GmshFLTK_Class.f90
  ${src_path}/GmshModel_Class.f90
  ${src_path}/GmshModelGeo_Class.f90
  ${src_path}/GmshModelGeoMesh_Class.f90
  ${src_path}/GmshModelOcc_Class.f90
  ${src_path}/GmshModelOccMesh_Class.f90
  ${src_path}/GmshModelMesh_Class.f90 )
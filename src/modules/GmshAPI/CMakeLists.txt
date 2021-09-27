SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Gmsh_Class.F90
  ${src_path}/GmshInterface/GmshInterface.F90
  ${src_path}/GmshFLTK_Class.F90
  ${src_path}/GmshGraphics_Class.F90
  ${src_path}/GmshModel_Class.F90
  ${src_path}/GmshModelGeo_Class.F90
  ${src_path}/GmshModelGeoMesh_Class.F90
  ${src_path}/GmshModelMesh_Class.F90
  ${src_path}/GmshModelOcc_Class.F90
  ${src_path}/GmshModelOccMesh_Class.F90
  ${src_path}/GmshOnelab_Class.F90
  ${src_path}/GmshOption_Class.F90
)
SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/mshFormat_Class.F90
  ${src_path}/mshPhysicalNames_Class.F90
  ${src_path}/mshEntity_Class.F90
  ${src_path}/mshNodes_Class.F90
  ${src_path}/mshElements_Class.F90
  ${src_path}/MSH_Class.F90
  )
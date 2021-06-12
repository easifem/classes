SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/mshFormat_Class@Methods.f90
  ${src_path}/mshPhysicalNames_Class@Methods.f90
  ${src_path}/mshEntity_Class@Methods.f90
  ${src_path}/mshNodes_Class@Methods.f90
  ${src_path}/mshElements_Class@Methods.f90
  ${src_path}/MSH_Class@Methods.f90
  ${src_path}/MSH_Class@ExportMethods.f90
  )
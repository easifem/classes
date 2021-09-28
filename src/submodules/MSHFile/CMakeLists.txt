SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/mshFormat_Class@Methods.F90
  ${src_path}/mshPhysicalNames_Class@Methods.F90
  ${src_path}/mshEntity_Class@Methods.F90
  ${src_path}/mshNodes_Class@Methods.F90
  ${src_path}/mshElements_Class@Methods.F90
  ${src_path}/MSHFile_Class@ConstructorMethods.F90
  ${src_path}/MSHFile_Class@IOMethods.F90
  )
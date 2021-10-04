SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/XMLTag_Class@ConstructorMethods.F90
  ${src_path}/XMLTag_Class@GetMethods.F90
  ${src_path}/XMLTag_Class@SetMethods.F90
  ${src_path}/XMLTag_Class@IOMethods.F90
  ${src_path}/XMLFile_Class@ConstructorMethods.F90
  ${src_path}/XMLFile_Class@IOMethods.F90
  ${src_path}/XMLFile_Class@SetMethods.F90
  ${src_path}/XMLFile_Class@GetMethods.F90
)
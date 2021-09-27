SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Domain_Class@Constructor.F90
  ${src_path}/Domain_Class@IOMethods.F90
  ${src_path}/Domain_Class@getMethods.F90
  ${src_path}/Domain_Class@setMethods.F90
  )
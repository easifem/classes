SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/Domain_Class@Constructor.f90
  ${src_path}/Domain_Class@IOMethods.f90
  ${src_path}/Domain_Class@getMethods.f90
  ${src_path}/Domain_Class@setMethods.f90
  )
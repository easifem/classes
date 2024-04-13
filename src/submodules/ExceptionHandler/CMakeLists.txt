SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ExceptionHandler_Class@Constructor.F90
  ${src_path}/ExceptionHandler_Class@getMethod.F90
  ${src_path}/ExceptionHandler_Class@setMethod.F90
  ${src_path}/ExceptionHandler_Class@raiseMethod.F90
  ${src_path}/ExceptionHandler_Class@Enquire.F90
)
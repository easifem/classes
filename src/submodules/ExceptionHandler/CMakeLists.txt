set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/ExceptionHandler_Class@Constructor.f90
  ${src_path}/ExceptionHandler_Class@getMethod.f90
  ${src_path}/ExceptionHandler_Class@setMethod.f90
  ${src_path}/ExceptionHandler_Class@raiseMethod.f90
  ${src_path}/ExceptionHandler_Class@Enquire.f90
)
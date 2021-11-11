SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/AbstractCommandLineArg_Class.F90
  ${src_path}/CommandLineArg_Utils.F90
  ${src_path}/CommandLineArg_Class.F90
  ${src_path}/CommandLineGroupArg_Class.F90
  ${src_path}/CommandLineInterface_Class.F90
  ${src_path}/CommandLine_Method.F90
)
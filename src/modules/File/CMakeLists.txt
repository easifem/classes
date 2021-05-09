SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/AbstractFile_Class.f90
  ${src_path}/FortranFile_Class.f90
  ${src_path}/InputFile_Class.f90
  ${src_path}/HDF5File_Class.f90
  )
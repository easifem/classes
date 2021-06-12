SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/AbstractFile_Class@Methods.f90
  ${src_path}/FortranFile_Class@Methods.f90
  ${src_path}/TxtFile_Class@Methods.f90
  ${src_path}/InputFile_Class@Methods.f90
  ${src_path}/HDF5File_Class@Methods.f90
  ${src_path}/HDF5File_Class@WriteReal64.f90
  ${src_path}/HDF5File_Class@WriteReal32.f90
  ${src_path}/HDF5File_Class@WriteInt32.f90
  ${src_path}/HDF5File_Class@WriteBool.f90
  ${src_path}/HDF5File_Class@WriteString.f90
  ${src_path}/HDF5File_Class@ReadReal64.f90
  ${src_path}/HDF5File_Class@ReadReal32.f90
  ${src_path}/HDF5File_Class@ReadInt32.f90
  ${src_path}/HDF5File_Class@ReadString.f90
  ${src_path}/HDF5File_Class@ReadBool.f90
  ${src_path}/HDF5File_Class@WriteAttribute.f90
  ${src_path}/HDF5File_Class@ReadAttribute.f90
  )
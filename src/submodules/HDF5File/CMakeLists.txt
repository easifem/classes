SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/HDF5File_Class@ConstructorMethods.F90
  ${src_path}/HDF5File_Class@WriteReal64.F90
  ${src_path}/HDF5File_Class@WriteReal32.F90
  ${src_path}/HDF5File_Class@WriteInt32.F90
  ${src_path}/HDF5File_Class@WriteBool.F90
  ${src_path}/HDF5File_Class@WriteString.F90
  ${src_path}/HDF5File_Class@ReadReal64.F90
  ${src_path}/HDF5File_Class@ReadReal32.F90
  ${src_path}/HDF5File_Class@ReadInt32.F90
  ${src_path}/HDF5File_Class@ReadString.F90
  ${src_path}/HDF5File_Class@ReadBool.F90
  ${src_path}/HDF5File_Class@WriteAttribute.F90
  ${src_path}/HDF5File_Class@ReadAttribute.F90
)
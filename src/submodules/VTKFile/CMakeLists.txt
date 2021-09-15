set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/VTKDataArrayEncoder@Rank1Methods.f90
  ${src_path}/VTKDataArrayEncoder@Rank2Methods.f90
  ${src_path}/VTKDataArrayEncoder@Rank3Methods.f90
  ${src_path}/VTKDataArrayEncoder@Rank4Methods.f90
  ${src_path}/VTKFile_Class@ConstructorMethods.f90
  ${src_path}/VTKFile_Class@IOMethods.f90
  ${src_path}/VTKFile_Class@DataArrayMethods.f90
  ${src_path}/VTKFile_Class@DataArrayAppendedMethods.f90
)
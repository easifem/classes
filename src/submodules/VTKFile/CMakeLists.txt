SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/VTKDataArrayEncoder@Rank1Methods.F90
  ${src_path}/VTKDataArrayEncoder@Rank2Methods.F90
  ${src_path}/VTKDataArrayEncoder@Rank3Methods.F90
  ${src_path}/VTKDataArrayEncoder@Rank4Methods.F90
  ${src_path}/VTKDataArrayEncoder@XYZRank1Methods.F90
  ${src_path}/VTKDataArrayEncoder@XYZRank2Methods.F90
  ${src_path}/VTKDataArrayEncoder@XYZRank3Methods.F90
  ${src_path}/VTKFile_Class@ConstructorMethods.F90
  ${src_path}/VTKFile_Class@IOMethods.F90
  ${src_path}/VTKFile_Class@DataArrayMethods.F90
  ${src_path}/VTKFile_Class@DataArrayAppendedMethods.F90
  ${src_path}/VTKFile_Class@FieldDataMethods.F90
  ${src_path}/VTKFile_Class@PointsMethods.F90
  ${src_path}/VTKFile_Class@PieceMethods.F90
)
SET(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
TARGET_SOURCES(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/DirichletBC_Class@ConstructorMethods.F90
  ${src_path}/DirichletBC_Class@IOMethods.F90
  ${src_path}/DirichletBC_Class@SetMethods.F90
  ${src_path}/DirichletBC_Class@GetMethods.F90
)
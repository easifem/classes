set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/DirichletBC_Class@ConstructorMethods.f90
  ${src_path}/DirichletBC_Class@IOMethods.f90
  ${src_path}/DirichletBC_Class@SetMethods.f90
  ${src_path}/DirichletBC_Class@GetMethods.f90
)
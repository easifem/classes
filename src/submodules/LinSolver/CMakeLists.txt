set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/LinSolver_Class@Constructor.f90
  ${src_path}/LinSolver_Class@IOMethods.f90
  ${src_path}/LinSolver_Class@SetMethods.f90
  ${src_path}/LinSolver_Class@SolveMethods.f90
)
set(src_path "${CMAKE_CURRENT_LIST_DIR}/src/")
target_sources(
  ${PROJECT_NAME} PRIVATE
  ${src_path}/LinSolver_Class@Constructor.F90
  ${src_path}/LinSolver_Class@IOMethods.F90
  ${src_path}/LinSolver_Class@SetMethods.F90
  ${src_path}/LinSolver_Class@SolveMethods.F90
)
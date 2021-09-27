#INCLDE DIR
TARGET_INCLUDE_DIRECTORIES(
  ${PROJECT_NAME}
  PUBLIC
  $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
  $<INSTALL_INTERFACE:${INSTALL_INCLUDEDIR}>
  "$ENV{EASIFEM_EXTPKGS}/include"
  "$ENV{EASIFEM_BASE}/include"
  "$ENV{EASIFEM_CLASSES}/include"
  "$ENV{EASIFEM_MATERIALS}/include"
  "$ENV{EASIFEM_KERNELS}/include"
)
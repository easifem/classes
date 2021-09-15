#ExceptionHandler
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ExceptionHandler/CMakeLists.txt)
#AbstractFile
include(${CMAKE_CURRENT_LIST_DIR}/AbstractFile/CMakeLists.txt)
#FortranFile
include(${CMAKE_CURRENT_LIST_DIR}/FortranFile/CMakeLists.txt)
#TxtFile
include(${CMAKE_CURRENT_LIST_DIR}/TxtFile/CMakeLists.txt)
#InputFile
include(${CMAKE_CURRENT_LIST_DIR}/InputFile/CMakeLists.txt)
#HDF5File
include(${CMAKE_CURRENT_LIST_DIR}/HDF5File/CMakeLists.txt)
#XMLFile
include(${CMAKE_CURRENT_LIST_DIR}/XMLFile/CMakeLists.txt)
#VTKFile
include(${CMAKE_CURRENT_LIST_DIR}/VTKFile/CMakeLists.txt)
#Element
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Element/CMakeLists.txt)
# FacetElement
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FacetElement/CMakeLists.txt)
# MSH
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MSH/CMakeLists.txt)
# Mesh
include(${CMAKE_CURRENT_LIST_DIR}/Mesh/CMakeLists.txt)
# Domain
include(${CMAKE_CURRENT_LIST_DIR}/Domain/CMakeLists.txt)
# MeshSelection
include(${CMAKE_CURRENT_LIST_DIR}/MeshSelection/CMakeLists.txt)
# Vector
include(${CMAKE_CURRENT_LIST_DIR}/Vector/CMakeLists.txt)
# ScalarField
include(${CMAKE_CURRENT_LIST_DIR}/ScalarField/CMakeLists.txt)
# VectorField
include(${CMAKE_CURRENT_LIST_DIR}/VectorField/CMakeLists.txt)
# AbstractMatrixField
include(${CMAKE_CURRENT_LIST_DIR}/AbstractMatrixField/CMakeLists.txt)
# MatrixField
include(${CMAKE_CURRENT_LIST_DIR}/MatrixField/CMakeLists.txt)

#AbstractLinSolver
include(${CMAKE_CURRENT_LIST_DIR}/AbstractLinSolver/CMakeLists.txt)
#LinSolver
include(${CMAKE_CURRENT_LIST_DIR}/LinSolver/CMakeLists.txt)

#FieldFactory
include(${CMAKE_CURRENT_LIST_DIR}/FieldFactory/CMakeLists.txt)

#DirichletBC
include(${CMAKE_CURRENT_LIST_DIR}/DirichletBC/CMakeLists.txt)

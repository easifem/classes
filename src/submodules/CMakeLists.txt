# This program is a part of EASIFEM library
# Copyright (C) 2020-2021  Vikas Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https: //www.gnu.org/licenses/>
#

# ExceptionHandler
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ExceptionHandler/CMakeLists.txt)

# AbstractFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractFile/CMakeLists.txt)

# FortranFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FortranFile/CMakeLists.txt)

# TxtFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/TxtFile/CMakeLists.txt)

# InputFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/InputFile/CMakeLists.txt)

# HDF5File
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/HDF5File/CMakeLists.txt)

# XMLFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/XMLFile/CMakeLists.txt)

# VTKFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VTKFile/CMakeLists.txt)

# Element
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Element/CMakeLists.txt)

# FacetElement
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FacetElement/CMakeLists.txt)

# MSHFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MSHFile/CMakeLists.txt)

# Mesh
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Mesh/CMakeLists.txt)

# Domain
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Domain/CMakeLists.txt)

# DomainConnectivity
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/DomainConnectivity/CMakeLists.txt)

# MeshSelection
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MeshSelection/CMakeLists.txt)

# Vector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Vector/CMakeLists.txt)

# AbstractField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractField/CMakeLists.txt)

# AbstractNodeField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractNodeField/CMakeLists.txt)

# AbstractMeshField
include(${CMAKE_CURRENT_LIST_DIR}/AbstractMeshField/CMakeLists.txt)

# ScalarMeshField
include(${CMAKE_CURRENT_LIST_DIR}/ScalarMeshField/CMakeLists.txt)

# STScalarMeshField
include(${CMAKE_CURRENT_LIST_DIR}/STScalarMeshField/CMakeLists.txt)

# VectorMeshField
include(${CMAKE_CURRENT_LIST_DIR}/VectorMeshField/CMakeLists.txt)

# STVectorMeshField
include(${CMAKE_CURRENT_LIST_DIR}/STVectorMeshField/CMakeLists.txt)

# TensorMeshField
include(${CMAKE_CURRENT_LIST_DIR}/TensorMeshField/CMakeLists.txt)

# STTensorMeshField
include(${CMAKE_CURRENT_LIST_DIR}/STTensorMeshField/CMakeLists.txt)

# ScalarField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ScalarField/CMakeLists.txt)

# STScalarField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STScalarField/CMakeLists.txt)

# VectorField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VectorField/CMakeLists.txt)

# STVectorField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STVectorField/CMakeLists.txt)

# BlockNodeField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BlockNodeField/CMakeLists.txt)

# AbstractMatrixField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractMatrixField/CMakeLists.txt)

# MatrixField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MatrixField/CMakeLists.txt)

# BlockMatrixField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BlockMatrixField/CMakeLists.txt)

# FieldFactory
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FieldFactory/CMakeLists.txt)

# AbstractBC
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractBC/CMakeLists.txt)

# DirichletBC
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/DirichletBC/CMakeLists.txt)

# NeumannBC
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/NeumannBC/CMakeLists.txt)

# AbstractLinSolver
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractLinSolver/CMakeLists.txt)

# LinSolver
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/LinSolver/CMakeLists.txt)

# LinSolverFactory
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/LinSolverFactory/CMakeLists.txt)

# AbstractFunction
include(${CMAKE_CURRENT_LIST_DIR}/AbstractFunction/CMakeLists.txt)

# Polynomial
include(${CMAKE_CURRENT_LIST_DIR}/Polynomial/CMakeLists.txt)

# UserFunction
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/UserFunction/CMakeLists.txt)
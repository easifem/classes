# This program is a part of EASIFEM library Copyright (C) 2020-2021  Vikas
# Sharma, Ph.D
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program.  If not, see <https: //www.gnu.org/licenses/>
#

# ExceptionHandler
include(${CMAKE_CURRENT_LIST_DIR}/ExceptionHandler/CMakeLists.txt)

# CPUTime
include(${CMAKE_CURRENT_LIST_DIR}/CPUTime/CMakeLists.txt)

# AbstractFile
include(${CMAKE_CURRENT_LIST_DIR}/AbstractFile/CMakeLists.txt)

# FortranFile
include(${CMAKE_CURRENT_LIST_DIR}/FortranFile/CMakeLists.txt)

# TxtFile
include(${CMAKE_CURRENT_LIST_DIR}/TxtFile/CMakeLists.txt)

# CSVFile
include(${CMAKE_CURRENT_LIST_DIR}/CSVFile/CMakeLists.txt)

# InputFile
include(${CMAKE_CURRENT_LIST_DIR}/InputFile/CMakeLists.txt)

# HDF5File
include(${CMAKE_CURRENT_LIST_DIR}/HDF5File/CMakeLists.txt)

# XMLFile
include(${CMAKE_CURRENT_LIST_DIR}/XMLFile/CMakeLists.txt)

# VTKFile
include(${CMAKE_CURRENT_LIST_DIR}/VTKFile/CMakeLists.txt)

# PVDFile
include(${CMAKE_CURRENT_LIST_DIR}/PVDFile/CMakeLists.txt)

# Toml
include(${CMAKE_CURRENT_LIST_DIR}/Toml/CMakeLists.txt)

# Element
include(${CMAKE_CURRENT_LIST_DIR}/Element/CMakeLists.txt)

# FacetElement
include(${CMAKE_CURRENT_LIST_DIR}/FacetElement/CMakeLists.txt)

# MSHFile
include(${CMAKE_CURRENT_LIST_DIR}/MSHFile/CMakeLists.txt)

# AbstractMesh
include(${CMAKE_CURRENT_LIST_DIR}/AbstractMesh/CMakeLists.txt)

# FEMesh
include(${CMAKE_CURRENT_LIST_DIR}/FEMesh/CMakeLists.txt)

# Mesh
include(${CMAKE_CURRENT_LIST_DIR}/Mesh/CMakeLists.txt)

# MeshFacetData
include(${CMAKE_CURRENT_LIST_DIR}/MeshFacetData/CMakeLists.txt)

# AbstractDomain
include(${CMAKE_CURRENT_LIST_DIR}/AbstractDomain/CMakeLists.txt)

# FEDomain
include(${CMAKE_CURRENT_LIST_DIR}/FEDomain/CMakeLists.txt)

# Domain
include(${CMAKE_CURRENT_LIST_DIR}/Domain/CMakeLists.txt)

# DomainConnectivity
include(${CMAKE_CURRENT_LIST_DIR}/DomainConnectivity/CMakeLists.txt)

# MeshSelection
include(${CMAKE_CURRENT_LIST_DIR}/MeshSelection/CMakeLists.txt)

# Vector
include(${CMAKE_CURRENT_LIST_DIR}/Vector/CMakeLists.txt)

# AbstractField
include(${CMAKE_CURRENT_LIST_DIR}/AbstractField/CMakeLists.txt)

# AbstractNodeField
include(${CMAKE_CURRENT_LIST_DIR}/AbstractNodeField/CMakeLists.txt)

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
include(${CMAKE_CURRENT_LIST_DIR}/ScalarField/CMakeLists.txt)

# ScalarFieldLis
include(${CMAKE_CURRENT_LIST_DIR}/ScalarFieldLis/CMakeLists.txt)

# STScalarField
include(${CMAKE_CURRENT_LIST_DIR}/STScalarField/CMakeLists.txt)

# STScalarFieldLis
include(${CMAKE_CURRENT_LIST_DIR}/STScalarFieldLis/CMakeLists.txt)

# VectorField
include(${CMAKE_CURRENT_LIST_DIR}/VectorField/CMakeLists.txt)

# VectorFieldLis
include(${CMAKE_CURRENT_LIST_DIR}/VectorFieldLis/CMakeLists.txt)

# STVectorField
include(${CMAKE_CURRENT_LIST_DIR}/STVectorField/CMakeLists.txt)

# STVectorFieldLis
include(${CMAKE_CURRENT_LIST_DIR}/STVectorFieldLis/CMakeLists.txt)

# BlockNodeField
include(${CMAKE_CURRENT_LIST_DIR}/BlockNodeField/CMakeLists.txt)

# BlockNodeFieldLis
include(${CMAKE_CURRENT_LIST_DIR}/BlockNodeFieldLis/CMakeLists.txt)

# AbstractMatrixField
include(${CMAKE_CURRENT_LIST_DIR}/AbstractMatrixField/CMakeLists.txt)

# MatrixField
include(${CMAKE_CURRENT_LIST_DIR}/MatrixField/CMakeLists.txt)

# MatrixFieldLis
include(${CMAKE_CURRENT_LIST_DIR}/MatrixFieldLis/CMakeLists.txt)

# BlockMatrixField
include(${CMAKE_CURRENT_LIST_DIR}/BlockMatrixField/CMakeLists.txt)

# BlockMatrixFieldLis
include(${CMAKE_CURRENT_LIST_DIR}/BlockMatrixFieldLis/CMakeLists.txt)

# FieldFactory
include(${CMAKE_CURRENT_LIST_DIR}/FieldFactory/CMakeLists.txt)

# AbstractBC
include(${CMAKE_CURRENT_LIST_DIR}/AbstractBC/CMakeLists.txt)

# DirichletBC
include(${CMAKE_CURRENT_LIST_DIR}/DirichletBC/CMakeLists.txt)

# NeumannBC
include(${CMAKE_CURRENT_LIST_DIR}/NeumannBC/CMakeLists.txt)

# NitscheBC
include(${CMAKE_CURRENT_LIST_DIR}/NitscheBC/CMakeLists.txt)

# AbstractLinSolver
include(${CMAKE_CURRENT_LIST_DIR}/AbstractLinSolver/CMakeLists.txt)

# LinSolver
include(${CMAKE_CURRENT_LIST_DIR}/LinSolver/CMakeLists.txt)

# LinSolverLis
include(${CMAKE_CURRENT_LIST_DIR}/LinSolverLis/CMakeLists.txt)

# LinSolverFactory
include(${CMAKE_CURRENT_LIST_DIR}/LinSolverFactory/CMakeLists.txt)

# AbstractFunction
include(${CMAKE_CURRENT_LIST_DIR}/AbstractFunction/CMakeLists.txt)

# AbstractBasis
include(${CMAKE_CURRENT_LIST_DIR}/AbstractBasis/CMakeLists.txt)

# Monomial1D
include(${CMAKE_CURRENT_LIST_DIR}/Monomial1D/CMakeLists.txt)

# Monomial2D
include(${CMAKE_CURRENT_LIST_DIR}/Monomial2D/CMakeLists.txt)

# Monomial3D
include(${CMAKE_CURRENT_LIST_DIR}/Monomial3D/CMakeLists.txt)

# MonomialND
include(${CMAKE_CURRENT_LIST_DIR}/MonomialND/CMakeLists.txt)

# AbstractPolynomial
include(${CMAKE_CURRENT_LIST_DIR}/AbstractPolynomial/CMakeLists.txt)

# Polynomial1D
include(${CMAKE_CURRENT_LIST_DIR}/Polynomial1D/CMakeLists.txt)

# Polynomial2D
include(${CMAKE_CURRENT_LIST_DIR}/Polynomial2D/CMakeLists.txt)

# Polynomial3D
include(${CMAKE_CURRENT_LIST_DIR}/Polynomial3D/CMakeLists.txt)

# PolynomialND
include(${CMAKE_CURRENT_LIST_DIR}/PolynomialND/CMakeLists.txt)

# Lagrange1D
include(${CMAKE_CURRENT_LIST_DIR}/Lagrange1D/CMakeLists.txt)

# Lagrange2D
include(${CMAKE_CURRENT_LIST_DIR}/Lagrange2D/CMakeLists.txt)

# Lagrange3D
include(${CMAKE_CURRENT_LIST_DIR}/Lagrange3D/CMakeLists.txt)

# Lagrange3D
include(${CMAKE_CURRENT_LIST_DIR}/Lagrange3D/CMakeLists.txt)

# AbstractOrthopol1D
include(${CMAKE_CURRENT_LIST_DIR}/AbstractOrthopol1D/CMakeLists.txt)

# Jacobi1D
include(${CMAKE_CURRENT_LIST_DIR}/Jacobi1D/CMakeLists.txt)

# Ultraspherical1D
include(${CMAKE_CURRENT_LIST_DIR}/Ultraspherical1D/CMakeLists.txt)

# Legendre1D
include(${CMAKE_CURRENT_LIST_DIR}/Legendre1D/CMakeLists.txt)

# ChebyshevFirst1D
include(${CMAKE_CURRENT_LIST_DIR}/ChebyshevFirst1D/CMakeLists.txt)

# PolynomialFactory
include(${CMAKE_CURRENT_LIST_DIR}/PolynomialFactory/CMakeLists.txt)

# Tree3R
include(${CMAKE_CURRENT_LIST_DIR}/Tree3R/CMakeLists.txt)

# UserFunction
include(${CMAKE_CURRENT_LIST_DIR}/UserFunction/CMakeLists.txt)

# RefElement
include(${CMAKE_CURRENT_LIST_DIR}/RefElement/CMakeLists.txt)

# AbstractFE
include(${CMAKE_CURRENT_LIST_DIR}/AbstractFE/CMakeLists.txt)

# FiniteElement
include(${CMAKE_CURRENT_LIST_DIR}/FiniteElement/CMakeLists.txt)

# MixedFiniteElement
include(${CMAKE_CURRENT_LIST_DIR}/MixedFiniteElement/CMakeLists.txt)

# AbstractPlot
include(${CMAKE_CURRENT_LIST_DIR}/AbstractPlot/CMakeLists.txt)

# VTKPlot
include(${CMAKE_CURRENT_LIST_DIR}/VTKPlot/CMakeLists.txt)

# PLPlot
include(${CMAKE_CURRENT_LIST_DIR}/PLPlot/CMakeLists.txt)

# materials here

# AbstractMaterialModel
include(${CMAKE_CURRENT_LIST_DIR}/AbstractMaterialModel/CMakeLists.txt)

# AbstractSolidMechanicsModel
include(${CMAKE_CURRENT_LIST_DIR}/AbstractSolidMechanicsModel/CMakeLists.txt)

# LinearElasticModel
include(${CMAKE_CURRENT_LIST_DIR}/LinearElasticModel/CMakeLists.txt)

# LinearPoroElasticModel
include(${CMAKE_CURRENT_LIST_DIR}/LinearPoroElasticModel/CMakeLists.txt)

# NewtonianFluidModel
include(${CMAKE_CURRENT_LIST_DIR}/NewtonianFluidModel/CMakeLists.txt)

# MaterialFactory
include(${CMAKE_CURRENT_LIST_DIR}/MaterialFactory/CMakeLists.txt)

# AbstractMaterial
include(${CMAKE_CURRENT_LIST_DIR}/AbstractMaterial/CMakeLists.txt)

# SolidMaterial
include(${CMAKE_CURRENT_LIST_DIR}/SolidMaterial/CMakeLists.txt)

# fluidMaterial
include(${CMAKE_CURRENT_LIST_DIR}/FluidMaterial/CMakeLists.txt)

# porousMaterial
include(${CMAKE_CURRENT_LIST_DIR}/PorousMaterial/CMakeLists.txt)

# ----------------------------------------------------------------------------
# KERNELS
# ----------------------------------------------------------------------------

# AbstractKernel
include(${CMAKE_CURRENT_LIST_DIR}/AbstractKernel/CMakeLists.txt)

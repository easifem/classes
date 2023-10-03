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

# CommandLineArg
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/CommandLineArg/CMakeLists.txt)

# AbstractFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractFile/CMakeLists.txt)

# FortranFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FortranFile/CMakeLists.txt)

# TxtFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/TxtFile/CMakeLists.txt)

# CSVFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/CSVFile/CMakeLists.txt)

# InputFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/InputFile/CMakeLists.txt)

# HDF5File
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/HDF5File/CMakeLists.txt)

# XMLFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/XMLFile/CMakeLists.txt)

# VTKFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VTKFile/CMakeLists.txt)

# Files
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Files/CMakeLists.txt)

# HDF5File
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/HDF5File/CMakeLists.txt)

# FPL
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FPL/CMakeLists.txt)

# Element
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Element/CMakeLists.txt)

# FacetElement
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FacetElement/CMakeLists.txt)

# FE
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ElementFactory/CMakeLists.txt)

# IntList
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/IntList/CMakeLists.txt)

# RealList
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/RealList/CMakeLists.txt)

# StringList
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/StringList/CMakeLists.txt)

# ElementList
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ElementList/CMakeLists.txt)

# ElementPointerVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ElementPointerVector/CMakeLists.txt)

# Mesh
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Mesh/CMakeLists.txt)

# MeshPointerVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MeshPointerVector/CMakeLists.txt)

# MeshConnectivity
# INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MeshConnectivity/CMakeLists.txt)

# Domain
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Domain/CMakeLists.txt)

# DomainConnectivity
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/DomainConnectivity/CMakeLists.txt)

# MeshSelection
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MeshSelection/CMakeLists.txt)

# GmshAPI
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/GmshAPI/CMakeLists.txt)

# MSHFile
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MSHFile/CMakeLists.txt)

# AbstractFunction
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractFunction/CMakeLists.txt)

# AbstractBasis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractBasis/CMakeLists.txt)

# Monomial1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Monomial1D/CMakeLists.txt)

# Monomial2D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Monomial2D/CMakeLists.txt)

# Monomial3D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Monomial3D/CMakeLists.txt)

# MonomialND
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MonomialND/CMakeLists.txt)

# AbstractPolynomial
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractPolynomial/CMakeLists.txt)

# Polynomial1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Polynomial1D/CMakeLists.txt)

# Polynomial2D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Polynomial2D/CMakeLists.txt)

# Polynomial3D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Polynomial3D/CMakeLists.txt)

# PolynomialND
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/PolynomialND/CMakeLists.txt)

# Lagrange1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Lagrange1D/CMakeLists.txt)

# Lagrange2D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Lagrange2D/CMakeLists.txt)

# Lagrange3D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Lagrange3D/CMakeLists.txt)

# AbstractOrthopol1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractOrthopol1D/CMakeLists.txt)

# Jacobi1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Jacobi1D/CMakeLists.txt)

# Ultraspherical1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Ultraspherical1D/CMakeLists.txt)

# Legendre1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Legendre1D/CMakeLists.txt)

# ChebyshevFirst1D
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ChebyshevFirst1D/CMakeLists.txt)

# PolynomialFactory
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/PolynomialFactory/CMakeLists.txt)

# RefElement
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/RefElement/CMakeLists.txt)

# AbstractFE
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractFE/CMakeLists.txt)

# FiniteElement
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FiniteElement/CMakeLists.txt)

# MixedFiniteElement
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MixedFiniteElement/CMakeLists.txt)

# FiniteElementFactory
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FiniteElementFactory/CMakeLists.txt)

# Tree3R
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Tree3R/CMakeLists.txt)

# AbstractBC
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractBC/CMakeLists.txt)

# DirichletBC
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/DirichletBC/CMakeLists.txt)

# NeumannBC
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/NeumannBC/CMakeLists.txt)

# NitscheBC
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/NitscheBC/CMakeLists.txt)

# UserFunction
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/UserFunction/CMakeLists.txt)

# AbstractVector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractVector/CMakeLists.txt)

# Vector
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Vector/CMakeLists.txt)

# AbstractField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractField/CMakeLists.txt)

# AbstractNodeField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractNodeField/CMakeLists.txt)

# AbstractMeshField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractMeshField/CMakeLists.txt)

# ScalarMeshField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ScalarMeshField/CMakeLists.txt)

# STScalarMeshField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STScalarMeshField/CMakeLists.txt)

# VectorMeshField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VectorMeshField/CMakeLists.txt)

# STVectorMeshField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STVectorMeshField/CMakeLists.txt)

# TensorMeshField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/TensorMeshField/CMakeLists.txt)

# STTensorMeshField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STTensorMeshField/CMakeLists.txt)

# AbstractElementField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractElementField/CMakeLists.txt)

# AbstractMatrixField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractMatrixField/CMakeLists.txt)

# ScalarField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ScalarField/CMakeLists.txt)

# ScalarFieldLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/ScalarFieldLis/CMakeLists.txt)

# STScalarField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STScalarField/CMakeLists.txt)

# STScalarFieldLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STScalarFieldLis/CMakeLists.txt)

# VectorField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VectorField/CMakeLists.txt)

# VectorFieldLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VectorFieldLis/CMakeLists.txt)

# STVectorField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STVectorField/CMakeLists.txt)

# STVectorFieldLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/STVectorFieldLis/CMakeLists.txt)

# BlockNodeField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BlockNodeField/CMakeLists.txt)

# BlockNodeFieldLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BlockNodeFieldLis/CMakeLists.txt)

# MatrixField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MatrixField/CMakeLists.txt)

# MatrixFieldLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/MatrixFieldLis/CMakeLists.txt)

# BlockMatrixField
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BlockMatrixField/CMakeLists.txt)

# BlockMatrixFieldLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/BlockMatrixFieldLis/CMakeLists.txt)

# AbstractLinSolver
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractLinSolver/CMakeLists.txt)

# Field
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Field/CMakeLists.txt)

# FieldFactory
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/FieldFactory/CMakeLists.txt)

# LinSolver
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/LinSolver/CMakeLists.txt)

# LinSolverLis
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/LinSolverLis/CMakeLists.txt)

# LinSolverFactory
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/LinSolverFactory/CMakeLists.txt)

# AbstractPlot
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/AbstractPlot/CMakeLists.txt)

# VTKPlot
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/VTKPlot/CMakeLists.txt)

# EasyPlplot
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/EasyPlplot/CMakeLists.txt)

# PLPlot
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/PLPlot/CMakeLists.txt)

# Plot
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/Plot/CMakeLists.txt)

# easifemClasses
INCLUDE(${CMAKE_CURRENT_LIST_DIR}/easifemClasses/CMakeLists.txt)



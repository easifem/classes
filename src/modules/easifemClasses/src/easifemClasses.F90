! This program is a part of EASIFEM library
! Copyright (C) 2020-2021  Vikas Sharma, Ph.D
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https: //www.gnu.org/licenses/>
!

MODULE easifemClasses
USE CPUTime_Class
USE Gmsh_Class
USE GmshStructuredMesh_Class

USE ElementFactory
USE ExceptionHandler_Class
USE Files
USE FPL_Method
!FTL Containers
USE ElementList_Class
USE ElementPointerVector_Class
USE IntList_Class
USE RealList_Class
USE StringList_Class

USE ElemData_Class
USE ElemDataBinaryTree_Class
USE ElemDataList_Class

USE NodeData_Class
USE NodeDataBinaryTree_Class
USE NodeDataList_Class

USE EdgeData_Class
USE EdgeDataBinaryTree_Class
USE EdgeDataList_Class

USE FaceData_Class
USE FaceDataBinaryTree_Class
USE FaceDataList_Class

USE FacetData_Class

USE AbstractMesh_Class
USE FEMesh_Class
USE MeshPointerVector_Class

USE AbstractDomain_Class
USE FEDomain_Class
! USE Domain_Class
! USE DomainUtility
USE DomainConnectivity_Class
USE FEDomainConnectivity_Class
USE MeshSelection_Class
! USE QuadratureVariables_Class

USE AbstractVector_Class
USE mshElements_Class
USE mshEntity_Class
USE MSHFile_Class
USE mshFormat_Class
USE mshNodes_Class
USE mshPhysicalNames_Class
USE Vector_Class

USE AbstractLinSolver_Class
USE Field
USE FieldFactory
USE LinSolver_Class
USE LinSolverFactory

USE BoundaryCondition

USE PolynomialFactory
USE UserFunction_Class

USE FiniteElementFactory
USE RefElementFactory

USE Tree3R_Class

USE Plot_Method
! USE ogpf

USE TomlUtility

END MODULE easifemClasses

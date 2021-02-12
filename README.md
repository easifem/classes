# EASIFEM

## Introduction

### About

EASIFEM stands for Expandable And Scalable infrastructure for Finite Element Method. It is a framework for developing code for finite element method by using modern fortran. The framework has been developed in [Modern-Fortran](https://fortran-lang.org/) programming language. The library is still in the development stage. EASIFEM is written both in OOP and Multiple dispatch approach.

### Capabilities

The goal of EASIFEM is to ease the coding of FEM by using fortran language. The library is developed for developing code for complex problems related to multiphysics and multiphase problems. EASIFEM will facilitate an environment where new FEM can be implemented easily while decreasing the time required for writing the code.

Dr Vikas Sharma, the creator EASIFEM conducts his research in the area of computational geomechanics. Therefore, so far, easifem has been used to develop applications related to dam-reservoir-soil interactions, seismic cracking of dams, nonlinear seismic behavior of earthdams, high-order accurate solvers for transient problems, space-time finite element methods for moving boundary problems, simulation of freezing and thawing in frozen soils, among others.

EASIFEM describes all types of datatype required for coding FEM, such as string, file, vector, array, degrees of freedom, indices, mesh, nodes, finite elements, domain, shape functions, quadrature rules, linear solver, sparse and dense matrix, iterative and non iterative solvers, among others. EASIFEM also have a strong interface with the [GMSH](https://gmsh.info/) for pre- and post- processing. In addition, it provides great interface with [PARAVIEW](https://www.paraview.org/) for visualization of results.

In future, EASIFEM, will also act as a platform to use exisiting finite element libraries in effortless manner. The vision of EASIFEM will always be  to keep  FEM programming easy and ellegant.

### Bug Reporting

### ChangeLog

### License

### ToDo

## Installation

### Installing

## Usage

## Structure

### Base

| | | |
 --- | --- | --- |
[BoundingBox](./pages/BoundingBox.md) | [Buffer](#) | [DOF](#)
[ElemShapeData](#) | [FEMatrix](#) | [FEVariable](#)
[File](#) | [IndexValue](#) | [IterationData](#)
[KeyValue](#) | [QuadraturePoint](#) | [Random](#)
[Tensor](#) | [ReferenceElement](#) | [RealMatrix](#)
[SparseMatrix](#) | [IntVector](#) | [RealVector](#)
[AbstractArray](#) | [AbstractMatrix](#) | [AbstractVector](#)
[BaseMethod](#) | [BaseType](#) | [BlasInterface](#)
[ErrorHandling](#) | [GlobalData](#) | [IO](#)
[Utility](#) | |

## Dependencies

- [String](./pages/String.md)

## Contributing

## Credits
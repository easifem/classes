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

MODULE PolynomialFactory
USE AbstractFunction_Class
USE AbstractBasis_Class
USE Monomial1D_Class
USE Monomial2D_Class
USE Monomial3D_Class
USE AbstractPolynomialSpace1D_Class
USE AbstractPolynomialSpace2D_Class
USE AbstractPolynomialSpace3D_Class
! USE AbstractPolynomial_Class
!
USE Polynomial1D_Class
USE AbstractOrthopol1D_Class
USE AbstractOrthopolSpace1D_Class
USE Jacobi1D_Class
USE ChebyshevFirst1D_Class
USE JacobiSpace1D_Class
USE UltrasphericalSpace1D_Class
USE LegendreSpace1D_Class
USE Chebyshev1Space1D_Class
USE Lagrange1D_Class
USE LagrangeSpace1D_Class
!!
USE Polynomial2D_Class
USE Lagrange2D_Class
USE LagrangeSpace2D_Class
!!
USE Polynomial3D_Class
USE Lagrange3D_Class
USE LagrangeSpace3D_Class
!!
! USE PolynomialND_Class
END MODULE PolynomialFactory

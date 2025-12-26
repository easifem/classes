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

MODULE easifemMaterials
USE AbstractMaterialModel_Class
USE AbstractSolidMechanicsModel_Class
USE AbstractFluidMechanicsModel_Class
USE LinearElasticModel_Class
USE LinearPoroElasticModel_Class
USE NewtonianFluidModel_Class
USE AbstractMaterial_Class
USE SolidMaterial_Class
USE FluidMaterial_Class
USE PorousMaterial_Class
USE MaterialFactory
USE SolidMechanicsModelUtility
END MODULE easifemMaterials

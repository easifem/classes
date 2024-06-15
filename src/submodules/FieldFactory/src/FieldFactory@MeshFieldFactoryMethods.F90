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

!> authors: Vikas Sharma, Ph. D.
! date: 26 Aug 2021
! summary: This modules is a factory for linear solvers

SUBMODULE(FieldFactory) MeshFieldFactoryMethods
USE FPL, ONLY: ParameterList_

USE StringUtility, ONLY: UpperCase

USE ScalarMeshField_Class, ONLY: ScalarMeshField_
USE STScalarMeshField_Class, ONLY: STScalarMeshField_
USE VectorMeshField_Class, ONLY: VectorMeshField_
USE STVectorMeshField_Class, ONLY: STVectorMeshField_
USE TensorMeshField_Class, ONLY: TensorMeshField_
USE STTensorMeshField_Class, ONLY: STTensorMeshField_

IMPLICIT NONE

CONTAINS

!----------------------------------------------------------------------------
!                                                          MeshFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFieldFactory
CHARACTER(*), PARAMETER :: myName = "MatrixFieldFactory()"
! TYPE(String) :: engine0
CHARACTER(:), ALLOCATABLE :: name0

name0 = UpperCase(name)

SELECT CASE (name0)

CASE ("SCALAR")

  ALLOCATE (ScalarMeshField_ :: ans)

CASE ("STSCALAR")

  ALLOCATE (STScalarMeshField_ :: ans)

CASE ("VECTOR")

  ALLOCATE (VectorMeshField_ :: ans)

CASE ("STVECTOR")

  ALLOCATE (STVectorMeshField_ :: ans)

CASE ("TENSOR")

  ALLOCATE (TensorMeshField_ :: ans)

CASE ("STTENSOR")

  ALLOCATE (STTensorMeshField_ :: ans)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[NO CASE FOUND] :: No case found for given name'// &
                    " following values are accepted = "// &
                  "[Scalar, STScalar, Vector, STVector, Tensor, STTensor]"// &
                    " but found "//TRIM(name))
  ALLOCATE (ScalarMeshField_ :: ans)
  RETURN
END SELECT
END PROCEDURE MeshFieldFactory

!----------------------------------------------------------------------------
!                                                   ScalarMeshFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE ScalarMeshFieldFactory
CHARACTER(*), PARAMETER :: myName = "ScalarMatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: name0

name0 = UpperCase(name)

SELECT CASE (name0)

CASE ("SCALAR")

  ALLOCATE (ScalarMeshField_ :: ans)

CASE ("STSCALAR")

  ALLOCATE (STScalarMeshField_ :: ans)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[NO CASE FOUND] :: No case found for given name'// &
                    " following values are accepted = "// &
                    "[Scalar, STScalar]"// &
                    " but found "//name)
  ALLOCATE (ScalarMeshField_ :: ans)
  RETURN
END SELECT

END PROCEDURE ScalarMeshFieldFactory

!----------------------------------------------------------------------------
!                                                     SclaarMeshFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE VectorMeshFieldFactory
CHARACTER(*), PARAMETER :: myName = "VectorMatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: name0

name0 = UpperCase(name)

SELECT CASE (name0)

CASE ("VECTOR")

  ALLOCATE (VectorMeshField_ :: ans)

CASE ("STVECTOR")

  ALLOCATE (STVectorMeshField_ :: ans)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[NO CASE FOUND] :: No case found for given name'// &
                    " following values are accepted = "// &
                    "[Vector, STVector]"// &
                    " but found "//name)
  ALLOCATE (VectorMeshField_ :: ans)
  RETURN
END SELECT

END PROCEDURE VectorMeshFieldFactory

!----------------------------------------------------------------------------
!                                                     TensorMeshFieldFactory
!----------------------------------------------------------------------------

MODULE PROCEDURE TensorMeshFieldFactory
CHARACTER(*), PARAMETER :: myName = "TensorMatrixFieldFactory()"
CHARACTER(:), ALLOCATABLE :: name0

name0 = UpperCase(name)

SELECT CASE (name0)

CASE ("TENSOR")

  ALLOCATE (TensorMeshField_ :: ans)

CASE ("STTENSOR")

  ALLOCATE (STTensorMeshField_ :: ans)

CASE DEFAULT

  CALL e%RaiseError(modName//'::'//myName//' - '// &
                    '[NO CASE FOUND] :: No case found for given name'// &
                    " following values are accepted = "// &
                    "[Tensor, STTensor]"// &
                    " but found "//TRIM(name))
  ALLOCATE (TensorMeshField_ :: ans)
  RETURN

END SELECT

END PROCEDURE TensorMeshFieldFactory

END SUBMODULE MeshFieldFactoryMethods

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

MODULE ScalarMeshField_Class
USE GlobalData
USE BaseType
USE FPL, ONLY: ParameterList_
USE Mesh_Class, ONLY: Mesh_
USE ExceptionHandler_Class, ONLY: e
USE AbstractField_Class
USE AbstractMeshField_Class
IMPLICIT NONE
PRIVATE
CHARACTER(LEN=*), PARAMETER :: modName = "ScalarMeshField_Class"

!----------------------------------------------------------------------------
!                                                     ScalarMeshField_Class
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 20 Feb 2022
! summary: Scalar mesh field

TYPE, EXTENDS(AbstractMeshField_) :: ScalarMeshField_
CONTAINS
  PRIVATE
  PROCEDURE, PUBLIC, PASS(obj) :: checkEssentialParam => &
    & aField_checkEssentialParam
    !! check essential parameters
  PROCEDURE, PASS(obj) :: Initiate1 => aField_Initiate1
    !! Initiate the field by reading param and a given mesh
END TYPE ScalarMeshField_

PUBLIC :: ScalarMeshField_

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

TYPE :: ScalarMeshFieldPointer_
  CLASS(ScalarMeshField_), POINTER :: ptr => NULL()
END TYPE ScalarMeshFieldPointer_

PUBLIC :: ScalarMeshFieldPointer_

!----------------------------------------------------------------------------
!                              setAbstractMeshFieldParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE setScalarMeshFieldParam(param, name, &
    & fieldType, varType, engine, defineOn, nns)
    TYPE(ParameterList_), INTENT(INOUT) :: param
    CHARACTER(LEN=*), INTENT(IN) :: name
    INTEGER(I4B), INTENT(IN) :: fieldType
    INTEGER(I4B), INTENT(IN) :: varType
    CHARACTER(LEN=*), INTENT(IN) :: engine
    INTEGER(I4B), INTENT(IN) :: defineOn
  !! Nodal, Quadrature
    INTEGER(I4B), INTENT(IN) :: nns
  !! Number of node in space
  END SUBROUTINE setScalarMeshFieldParam
END INTERFACE

PUBLIC :: setScalarMeshFieldParam

!----------------------------------------------------------------------------
!                                     checkEssentialParam@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: This routine check the essential parameters in param.

INTERFACE
  MODULE SUBROUTINE aField_checkEssentialParam(obj, param)
    CLASS(ScalarMeshField_), INTENT(IN) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
  END SUBROUTINE aField_checkEssentialParam
END INTERFACE

!----------------------------------------------------------------------------
!                                               Initiate@ConstructorMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 17 Feb 2022
! summary: Initiate the field by reading param and given domain

INTERFACE
  MODULE SUBROUTINE aField_Initiate1(obj, param, mesh)
    CLASS(ScalarMeshField_), INTENT(INOUT) :: obj
    TYPE(ParameterList_), INTENT(IN) :: param
    TYPE(Mesh_), TARGET, INTENT(IN) :: mesh
  END SUBROUTINE aField_Initiate1
END INTERFACE

END MODULE ScalarMeshField_Class

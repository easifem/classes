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

MODULE KernelElasticityProperties_Method
USE GlobalData, ONLY: DFP, I4B, LGT

USE ExceptionHandler_Class, ONLY: e

USE AbstractField_Class, ONLY: TypeField

USE BaseType, ONLY: TypeFEVariableOpt

USE AbstractMeshField_Class, ONLY: AbstractTensorMeshField_, &
                                   AbstractScalarMeshField_

USE ScalarMeshField_Class, ONLY: SetScalarMeshFieldParam
USE STScalarMeshField_Class, ONLY: SetSTScalarMeshFieldParam

USE TensorMeshField_Class, ONLY: SetTensorMeshFieldParam
USE STTensorMeshField_Class, ONLY: SetSTTensorMeshFieldParam

USE FEDomain_Class, ONLY: FEDomain_

USE FPL, ONLY: ParameterList_

USE AbstractMesh_Class, ONLY: AbstractMesh_

IMPLICIT NONE

PRIVATE

CHARACTER(*), PARAMETER :: modName = "KernelElasticityProperties_Method"

PUBLIC :: KernelInitiateConstantElasticityProperties

CONTAINS

!----------------------------------------------------------------------------
!                                       InitiateConstantElasticityProperties
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateConstantElasticityProperties(youngsModulus, &
                                        shearModulus, cijkl, dom, nnt, engine)
  TYPE(AbstractScalarMeshField_), INTENT(INOUT) :: youngsModulus
  TYPE(AbstractScalarMeshField_), INTENT(INOUT) :: shearModulus
  TYPE(AbstractTensorMeshField_), INTENT(INOUT) :: cijkl
  TYPE(FEDomain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: engine

  !! Internal variables
  CHARACTER(*), PARAMETER :: myName = &
                             "KernelInitiateConstantElasticityProperties()"

  INTEGER(I4B), PARAMETER :: default_nns = 1, default_dim1 = 6, &
                             default_dim2 = 6, default_nnt = 1

  TYPE(ParameterList_) :: param1, param2, param3

  CLASS(AbstractMesh_), POINTER :: mesh

  CHARACTER(:), ALLOCATABLE :: errmsg, name

  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[START]')
#endif

  mesh => dom%GetMeshPointer()
  isok = ASSOCIATED(mesh)
  errmsg = 'mesh is not ASSOCIATED.'
  CALL AssertError1(isok, myname, errmsg)

  isok = .NOT. mesh%isEmpty()
  errmsg = "mesh is EMPTY"
  CALL AssertError1(isok, myname, errmsg)

  CALL param1%Initiate()
  CALL param2%Initiate()
  CALL param3%Initiate()

  IF (nnt .EQ. 1) THEN
    CALL SetScalarMeshFieldParam(param=param1, name="youngsModulus", &
                   fieldType=TypeField%constant, varType=TypeField%constant, &
        engine=engine, defineOn=TypeFEVariableOpt%quadrature, nns=default_nns)

    CALL SetScalarMeshFieldParam(param=param2, name="shearModulus", &
                   fieldType=TypeField%constant, varType=TypeField%constant, &
        engine=engine, defineOn=TypeFEVariableOpt%quadrature, nns=default_nns)

    CALL SetTensorMeshFieldParam(param=param3, name="cijkl", &
                   fieldType=TypeField%constant, varType=TypeField%constant, &
                       engine=engine, defineOn=TypeFEVariableOpt%quadrature, &
                        nns=default_nns, dim1=default_dim1, dim2=default_dim2)

  ELSE
    CALL SetSTScalarMeshFieldParam(param=param1, name="youngsModulus", &
                   fieldType=TypeField%constant, varType=TypeField%constant, &
                       engine=engine, defineOn=TypeFEVariableOpt%quadrature, &
                                   nns=default_nns, nnt=default_nnt)

    CALL SetSTScalarMeshFieldParam(param=param2, name="shearModulus", &
                   fieldType=TypeField%constant, varType=TypeField%constant, &
                       engine=engine, defineOn=TypeFEVariableOpt%quadrature, &
                                   nns=default_nns, nnt=default_nnt)

    CALL SetSTTensorMeshFieldParam(param=param3, name="cijkl", &
                   fieldType=TypeField%constant, varType=TypeField%constant, &
                       engine=engine, defineOn=TypeFEVariableOpt%quadrature, &
       nns=default_nns, dim1=default_dim1, dim2=default_dim2, nnt=default_nnt)

  END IF

  CALL youngsModulus%Initiate(param=param1, mesh=mesh)
  CALL shearModulus%Initiate(param=param2, mesh=mesh)
  CALL cijkl%Initiate(param=param3, mesh=mesh)

  CALL param1%DEALLOCATE()
  CALL param2%DEALLOCATE()
  CALL param3%DEALLOCATE()

  NULLIFY (mesh)

#ifdef DEBUG_VER
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
                          '[END]')
#endif

END SUBROUTINE KernelInitiateConstantElasticityProperties

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE KernelElasticityProperties_Method

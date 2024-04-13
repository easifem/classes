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
USE GlobalData
USE Field
USE BaseMethod
USE BaseType
USE FieldFactory
USE Domain_Class
USE Mesh_Class
USE ExceptionHandler_Class, ONLY: e
IMPLICIT NONE
PRIVATE

CHARACTER(*), PARAMETER :: modName = "KernelElasticityProperties_Method"
PUBLIC :: KernelInitiateConstantElasticityProperties

CONTAINS

!----------------------------------------------------------------------------
!                                       InitiateConstantElasticityProperties
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateConstantElasticityProperties(youngsModulus,  &
  & shearModulus, cijkl, dom, nnt, engine)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: youngsModulus(:)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: shearModulus(:)
  TYPE(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: cijkl(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: engine

  ! Internal variables
  CHARACTER(*), PARAMETER :: myName =  &
    & "KernelInitiateConstantElasticityProperties()"
  INTEGER(I4B) :: ii, tmesh, nsd
  TYPE(ParameterList_) :: param1, param2, param3
  CLASS(Mesh_), POINTER :: amesh
  CHARACTER(:), ALLOCATABLE :: name
  LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START]')
#endif

  amesh => NULL()
  CALL param1%Initiate()
  CALL param2%Initiate()
  CALL param3%Initiate()
  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  IF (nnt .EQ. 1) THEN
    name = ""
    CALL SetScalarMeshFieldParam(param=param1, name="youngsModulus", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant,  &
      & engine=engine, defineOn=Nodal, nns=1)

    CALL SetScalarMeshFieldParam(param=param2, name="shearModulus", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant,  &
      & engine=engine, defineOn=Nodal, nns=1)

    CALL SetTensorMeshFieldParam(param=param3, name="cijkl", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, &
      & nns=1, dim1=6, dim2=6)

  ELSE
    name = "ST"

    CALL SetSTScalarMeshFieldParam(param=param1, name="youngsModulus", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, nns=1, nnt=1)

    CALL SetSTScalarMeshFieldParam( &
      & param=param2, name="shearModulus", fieldType=FIELD_TYPE_CONSTANT, &
      & varType=Constant, engine=engine, defineOn=Nodal, &
      & nns=1, nnt=1)

    CALL SetSTTensorMeshFieldParam(param=param3, name="cijkl", &
      & fieldType=FIELD_TYPE_CONSTANT, varType=Constant, &
      & engine=engine, defineOn=Nodal, nns=1, dim1=6, dim2=6,  &
      & nnt=1)
  END IF

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)

    isok = ASSOCIATED(youngsModulus(ii)%ptr)
    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'youngsModulus ('//tostring(ii)//') already ASSOCIATED.')
    END IF

    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Making youngsModulus ('//tostring(ii)//').')
      youngsModulus(ii)%ptr => ScalarMeshFieldFactory(engine=engine, &
        & name=name//"Scalar")
      CALL youngsModulus(ii)%ptr%Initiate(param=param1, mesh=amesh)
    END IF

    isok = ASSOCIATED(shearModulus(ii)%ptr)
    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'shearModulus ('//tostring(ii)//') already ASSOCIATED.')
    END IF

    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Making shearModulus ('//tostring(ii)//').')
      shearModulus(ii)%ptr => ScalarMeshFieldFactory(engine=engine,  &
        & name=name//"Scalar")
      CALL shearModulus(ii)%ptr%Initiate(param=param2, mesh=amesh)
    END IF

    isok = ASSOCIATED(Cijkl(ii)%ptr)
    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Cijkl ('//tostring(ii)//') already ASSOCIATED.')
    END IF

    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'Making Cijkl ('//tostring(ii)//').')
      Cijkl(ii)%ptr => TensorMeshFieldFactory(engine=engine, &
        & name=name//"Tensor")
      CALL Cijkl(ii)%ptr%Initiate(param=param3, mesh=amesh)
    END IF
  END DO

  CALL param1%DEALLOCATE()
  CALL param2%DEALLOCATE()
  CALL param3%DEALLOCATE()
  NULLIFY (amesh)

#ifdef DEBUG_VER
  CALL e%raiseInformation(modName//'::'//myName//' - '// &
    & '[END]')
#endif
END SUBROUTINE KernelInitiateConstantElasticityProperties

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END MODULE KernelElasticityProperties_Method

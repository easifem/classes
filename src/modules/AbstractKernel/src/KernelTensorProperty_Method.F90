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

MODULE KernelTensorProperty_Method
USE GlobalData
USE Field
USE FieldFactory
USE BaseMethod
USE BaseType
USE SolidMaterial_Class
USE Domain_Class
USE Mesh_Class
USE ExceptionHandler_Class, ONLY: e

PRIVATE

PUBLIC :: KernelSetTensorProperty
PUBLIC :: KernelInitiateTensorProperty

CHARACTER(*), PARAMETER :: modName = "KernelTensorProperty_Method"

INTERFACE KernelSetTensorProperty
  MODULE PROCEDURE KernelSetTensorProperty1
END INTERFACE KernelSetTensorProperty

INTERFACE KernelInitiateTensorProperty
  MODULE PROCEDURE KernelInitiateTensorProperty1
END INTERFACE KernelInitiateTensorProperty

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateTensorProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateTensorProperty1(vars, materials, dom, nnt, varname,  &
  & matid, engine)
  TYPE(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  CHARACTER(*), INTENT(IN) :: engine

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateTensorProperty1()"
  LOGICAL(LGT) :: isok
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractTensorMeshField_), POINTER :: var
  INTEGER(I4B) :: nsd, tmesh, ii, id
  CHARACTER(:), ALLOCATABLE :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  amesh => NULL()
  material => NULL()
  var => NULL()

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  name = "STScalar"
  IF (nnt .EQ. 1) name = "Scalar"

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    id = amesh%GetMaterial(matid)

    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: for mesh = '//tostring(ii)//' found  id = 0.')
      CYCLE
    END IF

    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      CYCLE
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (isok) THEN
      vars(ii)%ptr => TensorMeshFieldFactory(engine=engine, name=name)

      var => vars(ii)%ptr
      CALL var%Initiate(material=material, mesh=amesh, name=varname,  &
        & engine=engine)
      var => NULL()
    ELSE
      vars(ii)%ptr => NULL()
    END IF
  END DO

  NULLIFY (amesh, material, var)
  name = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelInitiateTensorProperty1

!----------------------------------------------------------------------------
!                                                         SetScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelSetTensorProperty1(vars, materials, dom, varname,  &
  & matid, times)
  TYPE(AbstractTensorMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  REAL(DFP), INTENT(IN) :: times(:)

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelSetTensorProperty1()"
  LOGICAL(LGT) :: isok
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractTensorMeshField_), POINTER :: var
  INTEGER(I4B) :: nsd, tmesh, ii, id
  CHARACTER(:), ALLOCATABLE :: name

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[START] ')
#endif DEBUG_VER

  amesh => NULL()
  material => NULL()
  var => NULL()

  nsd = dom%GetNSD()
  tmesh = dom%GetTotalMesh(dim=nsd)

  DO ii = 1, tmesh
    amesh => dom%GetMeshPointer(dim=nsd, entityNum=ii)
    id = amesh%GetMaterial(matid)

    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: for mesh = '//tostring(ii)//' found  id = 0.')
      CYCLE
    END IF

    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[SKIPPING] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      CYCLE
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (isok) THEN
      var => vars(ii)%ptr
      CALL var%Set(material=material, dom=dom, name=varname, times=times)
      var => NULL()
    ELSE
      vars(ii)%ptr => NULL()
    END IF
  END DO

  NULLIFY (amesh, material, var)
  name = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER
END SUBROUTINE KernelSetTensorProperty1
END MODULE KernelTensorProperty_Method

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

MODULE KernelScalarProperty_Method
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

PUBLIC :: KernelSetScalarProperty
PUBLIC :: KernelInitiateScalarProperty

CHARACTER(*), PARAMETER :: modName = "KernelScalarProperty_Method"

INTERFACE KernelSetScalarProperty
  MODULE PROCEDURE KernelSetScalarProperty1
END INTERFACE KernelSetScalarProperty

INTERFACE KernelInitiateScalarProperty
  MODULE PROCEDURE KernelInitiateScalarProperty1
END INTERFACE KernelInitiateScalarProperty

CONTAINS

!----------------------------------------------------------------------------
!                                                 InitiateScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelInitiateScalarProperty1(vars, materials, dom, nnt,  &
  & varname, matid, engine)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  INTEGER(I4B), INTENT(IN) :: nnt
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  CHARACTER(*), INTENT(IN) :: engine

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelInitiateScalarProperty1()"
  LOGICAL(LGT) :: isok, problem
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractScalarMeshField_), POINTER :: var
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
    isok = ASSOCIATED(amesh)

    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: mesh('//tostring(ii)//') not ASSOCIATED.')
      RETURN
    END IF

    problem = amesh%isEmpty()
    IF (problem) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'mesh('//tostring(ii)//') is EMPTY.')
      CYCLE
    END IF

    id = amesh%GetMaterial(matid)
    IF (id .EQ. 0_I4B) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: For mesh('//tostring(ii)//') found  id = 0.')
      RETURN
    END IF

    material => materials(id)%ptr

    isok = ASSOCIATED(material)
    IF (.NOT. isok) THEN
      CALL e%RaiseError(modName//'::'//myName//' - '// &
        & '[INTERNAL ERROR] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      RETURN
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (.NOT. isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'material name '//varname//" NOT FOUND.")
      ! vars(ii)%ptr => NULL()
    END IF

    IF (isok) THEN
      CALL e%RaiseInformation(modName//'::'//myName//' - '// &
        & 'material name '//varname//" FOUND.")
      vars(ii)%ptr => ScalarMeshFieldFactory(engine=engine, name=name)
      var => vars(ii)%ptr
      CALL var%Initiate(material=material, mesh=amesh, name=varname,  &
        & engine=engine)
      var => NULL()
    END IF

  END DO

  NULLIFY (amesh, material, var)
  name = ""

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
    & '[END] ')
#endif DEBUG_VER

END SUBROUTINE KernelInitiateScalarProperty1

!----------------------------------------------------------------------------
!                                                         SetScalarProperty
!----------------------------------------------------------------------------

SUBROUTINE KernelSetScalarProperty1(vars, materials, dom, varname,  &
  & matid, timeVec)
  TYPE(AbstractScalarMeshFieldPointer_), INTENT(INOUT) :: vars(:)
  TYPE(SolidMaterialPointer_), INTENT(INOUT) :: materials(:)
  TYPE(Domain_), INTENT(INOUT) :: dom
  CHARACTER(*), INTENT(IN) :: varname
  INTEGER(I4B), INTENT(IN) :: matid
  REAL(DFP), INTENT(IN) :: timeVec(:)

  ! Define internal variables
  CHARACTER(*), PARAMETER :: myName = "KernelSetScalarProperty1()"
  LOGICAL(LGT) :: isok, problem
  CLASS(Mesh_), POINTER :: amesh
  CLASS(SolidMaterial_), POINTER :: material
  CLASS(AbstractScalarMeshField_), POINTER :: var
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
    problem = amesh%isEmpty()
    IF (problem) CYCLE

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
        & '[INTERNAL ERROR] :: AbstractKernel_::solidMaterial('//  &
        & tostring(ii)//') is not ASSOCIATED.')
      CYCLE
    END IF

    isok = material%IsMaterialPresent(name=varname)
    IF (isok) THEN
      var => vars(ii)%ptr
      CALL var%Set(material=material, dom=dom, name=varname, timeVec=timeVec)
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
END SUBROUTINE KernelSetScalarProperty1

END MODULE KernelScalarProperty_Method

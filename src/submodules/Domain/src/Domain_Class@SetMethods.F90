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

SUBMODULE(Domain_Class) SetMethods
USE BaseMethod
USE DomainConnectivity_Class
USE DomainUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setSparsity1
CHARACTER(*), PARAMETER :: myName = "Domain_setSparsity1"
!
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
    & "Domain is not initiated, first initiate")
END IF
!
! Call SetSparsity1 from DomainUtility
!
CALL SetSparsity1(obj=obj, mat=mat)
!
END PROCEDURE Domain_setSparsity1

!----------------------------------------------------------------------------
!                                                               setSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setSparsity2
CHARACTER(*), PARAMETER :: myName = "Domain_setSparsity2"
INTEGER(I4B) :: ivar, nsd(SIZE(domains))
CHARACTER(20) :: matProp

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[START] SetSparsity()')

DO ivar = 1, SIZE(domains)

  IF (.NOT. ASSOCIATED(domains(ivar)%ptr)) THEN
    CALL e%raiseError(modName//"::"//myName//" - "// &
      & 'DOMAINS( '//TOSTRING(ivar)//' ) NOT ASSOCIATED')
  ELSE
    IF (.NOT. domains(ivar)%ptr%isInitiated)  &
      & CALL e%raiseError(modName//"::"//myName//" - "// &
      & 'DOMAINS( '//TOSTRING(ivar)//' )%ptr NOT INITIATED')
  END IF

  nsd(ivar) = domains(ivar)%ptr%getNSD()

END DO

IF (ANY(nsd .NE. nsd(1))) THEN
  CALL e%raiseError(modName//"::"//myName//" - "// &
  & 'It seems that NSD (number of spatial dimensions) of domains are &
  & not identical')
END IF

CALL Display("Calling SetSparsity2 or SetSpartsity3 from DomainUtility")
matProp = GetMatrixProp(mat)

IF (TRIM(matProp) .EQ. "RECTANGLE") THEN
  CALL SetSparsity3(domains=domains, mat=mat)
ELSE
  CALL SetSparsity2(domains=domains, mat=mat)
END IF

CALL e%raiseInformation(modName//'::'//myName//' - '// &
& '[END] SetSparsity()')

END PROCEDURE Domain_setSparsity2

!----------------------------------------------------------------------------
!                                                          setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setTotalMaterial
INTEGER(I4B) :: ii
CLASS(mesh_), POINTER :: meshptr
!
DO ii = 1, obj%getTotalMesh(dim=dim)
  meshptr => obj%getMeshPointer(dim=dim, entityNum=ii)
  CALL meshptr%setTotalMaterial(n)
END DO
meshptr => NULL()
END PROCEDURE Domain_setTotalMaterial

!----------------------------------------------------------------------------
!                                                          setTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_setMaterial
CLASS(mesh_), POINTER :: meshptr
!
meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
CALL meshptr%setMaterial(medium=medium, material=material)
meshptr => NULL()
END PROCEDURE Domain_setMaterial

!----------------------------------------------------------------------------
!                                                           SetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetNodeCoord1
CHARACTER(*), PARAMETER :: myName = "Domain_SetNodeCoord1"
REAL(DFP) :: scale0

IF (.NOT. ALLOCATED(obj%nodeCoord)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Domain_::obj%nodeCoord not allocated')
END IF
IF (SIZE(nodeCoord, 1) .NE. SIZE(obj%nodeCoord, 1) &
  & .OR. SIZE(nodeCoord, 2) .NE. SIZE(obj%nodeCoord, 2)) THEN
  CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'Size of nodeCoord does not match with Domain_::obj%nodeCoord')
END IF

scale0 = input(option=scale, default=1.0_DFP)

IF (PRESENT(addContribution)) THEN
  obj%nodeCoord = obj%nodeCoord + scale * nodeCoord
ELSE
  obj%nodeCoord = nodeCoord
END IF

END PROCEDURE Domain_SetNodeCoord1

!----------------------------------------------------------------------------
!                                                                 SetQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE Domain_SetQuality
CLASS(Mesh_), POINTER :: meshptr
CHARACTER(*), PARAMETER :: myName = "Domain_SetQuality"
REAL(DFP), ALLOCATABLE :: max_(:, :), min_(:, :)
INTEGER(I4B) :: tmesh, imesh

IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
  meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
  IF (meshptr%getTotalElements() .EQ. 0) THEN
    CALL e%raiseWarning(modName//'::'//myName//' - '// &
    & 'mesh if empty')
  ELSE
    CALL meshptr%SetQuality(&
      & measures=measures, &
      & max_measures=max_measures, &
      & min_measures=min_measures, &
      & nodeCoord=obj%nodeCoord, &
      & local_nptrs=obj%local_nptrs &
      & )
  END IF
  NULLIFY (meshptr)
  RETURN
END IF

IF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
  tmesh = obj%getTotalMesh(dim=dim)
  CALL Reallocate(max_, SIZE(measures), tmesh)
  min_ = max_

  DO imesh = 1, tmesh
    meshptr => obj%getMeshPointer(dim=dim, entityNum=imesh)
    IF (meshptr%getTotalElements() .EQ. 0) THEN
      max_(:, imesh) = -1 * MaxDFP
      min_(:, imesh) = MaxDFP
    ELSE
      CALL meshptr%SetQuality(&
        & measures=measures, &
        & max_measures=max_(:, imesh), &
        & min_measures=min_(:, imesh), &
        & nodeCoord=obj%nodeCoord, &
        & local_nptrs=obj%local_nptrs &
        & )
    END IF
  END DO

  max_measures = MAXVAL(max_, dim=2)
  min_measures = MINVAL(min_, dim=2)
  NULLIFY (meshptr)
  DEALLOCATE (max_, min_)
  RETURN
END IF

CALL e%raiseError(modName//'::'//myName//' - '// &
  & 'No case found')

END PROCEDURE Domain_SetQuality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods

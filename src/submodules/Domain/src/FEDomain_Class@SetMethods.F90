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

SUBMODULE(FEDomain_Class) SetMethods
! USE BaseMethod
! USE DomainConnectivity_Class
! USE DomainUtility
USE Display_Method
USE InputUtility
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity1()"

IF (.NOT. obj%isInitiated) THEN
  CALL e%RaiseError(modName//"::"//myName//" - "// &
    & "[INTERNAL ERROR] :: Domain is not initiated, first initiate")
  RETURN
END IF
#endif

SELECT CASE (obj%nsd)
CASE (0)
  CALL obj%meshPoint%SetSparsity(mat=mat)
CASE (1)
  CALL obj%meshCurve%SetSparsity(mat=mat)
CASE (2)
  CALL obj%meshSurface%SetSparsity(mat=mat)
CASE (3)
  CALL obj%meshVolume%SetSparsity(mat=mat)
CASE DEFAULT
  CALL e%RaiseError(modName//'::'//myName//' - '// &
    & '[INTERNAL ERROR] :: No case found for nsd='//tostring(obj%nsd))
  RETURN
END SELECT

END PROCEDURE obj_SetSparsity1

!----------------------------------------------------------------------------
!                                                               SetSparsity
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetSparsity2
CHARACTER(*), PARAMETER :: myName = "obj_SetSparsity2()"
! INTEGER(I4B) :: ivar, nsd(SIZE(domains))
! CHARACTER(20) :: matProp

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif

CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

! DO ivar = 1, SIZE(domains)
!
!   IF (.NOT. ASSOCIATED(domains(ivar)%ptr)) THEN
!     CALL e%RaiseError(modName//"::"//myName//" - "// &
!       & 'DOMAINS( '//TOSTRING(ivar)//' ) NOT ASSOCIATED')
!   ELSE
!     IF (.NOT. domains(ivar)%ptr%isInitiated)  &
!       & CALL e%RaiseError(modName//"::"//myName//" - "// &
!       & 'DOMAINS( '//TOSTRING(ivar)//' )%ptr NOT INITIATED')
!   END IF
!
!   nsd(ivar) = domains(ivar)%ptr%getNSD()
!
! END DO
!
! IF (ANY(nsd .NE. nsd(1))) THEN
!   CALL e%RaiseError(modName//"::"//myName//" - "// &
!   & 'It seems that NSD (number of spatial dimensions) of domains are &
!   & not identical')
! END IF
!
! CALL Display("Calling SetSparsity2 or SetSpartsity3 from DomainUtility")
! matProp = GetMatrixProp(mat)
!
! IF (TRIM(matProp) .EQ. "RECTANGLE") THEN
!   CALL SetSparsity3(domains=domains, mat=mat)
! ELSE
!   CALL SetSparsity2(domains=domains, mat=mat)
! END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif

END PROCEDURE obj_SetSparsity2

!----------------------------------------------------------------------------
!                                                          SetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetTotalMaterial
SELECT CASE (dim)
CASE (0)
  CALL obj%meshPoint%SetTotalMaterial(n)
CASE (1)
  CALL obj%meshCurve%SetTotalMaterial(n)
CASE (2)
  CALL obj%meshSurface%SetTotalMaterial(n)
CASE (3)
  CALL obj%meshVolume%SetTotalMaterial(n)
END SELECT
END PROCEDURE obj_SetTotalMaterial

!----------------------------------------------------------------------------
!                                                          SetTotalMaterial
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetMaterial
CHARACTER(*), PARAMETER :: myName = "obj_SetMaterial()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')

! meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
! CALL meshptr%SetMaterial(medium=medium, material=material)
! meshptr => NULL()
END PROCEDURE obj_SetMaterial

!----------------------------------------------------------------------------
!                                                           SetNodeCoord
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetNodeCoord1
CHARACTER(*), PARAMETER :: myName = "obj_SetNodeCoord1()"
REAL(DFP) :: scale0
LOGICAL(LGT) :: problem

problem = .NOT. ALLOCATED(obj%nodeCoord)
IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[INTERNAL ERROR] :: FEDomain_::obj%nodeCoord not allocated')
  RETURN
END IF

problem = ALL(SHAPE(nodeCoord) .NE. SHAPE(obj%nodeCoord))

IF (problem) THEN
  CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[INTERNAL ERROR] :: Shape of nodeCoord does not match '// &
  & 'with obj_::obj%nodeCoord')
  RETURN
END IF

scale0 = Input(option=scale, default=1.0_DFP)

IF (PRESENT(addContribution)) THEN
  obj%nodeCoord = obj%nodeCoord + scale * nodeCoord
ELSE
  obj%nodeCoord = nodeCoord
END IF

END PROCEDURE obj_SetNodeCoord1

!----------------------------------------------------------------------------
!                                                                 SetQuality
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetQuality
CHARACTER(*), PARAMETER :: myName = "obj_SetQuality()"
CALL e%RaiseError(modName//'::'//myName//' - '// &
  & '[WIP ERROR] :: This routine is under development')
! CLASS(Mesh_), POINTER :: meshptr
! CHARACTER(*), PARAMETER :: myName = "obj_SetQuality"
! REAL(DFP), ALLOCATABLE :: max_(:, :), min_(:, :)
! INTEGER(I4B) :: tmesh, imesh, dim0
!
!
! dim0 = Input(default=obj%nsd, option=dim)
!
! IF (PRESENT(dim) .AND. PRESENT(entityNum)) THEN
!   meshptr => obj%getMeshPointer(dim=dim, entityNum=entityNum)
!   IF (meshptr%getTotalElements() .EQ. 0) THEN
!     CALL e%RaiseWarning(modName//'::'//myName//' - '// &
!     & 'mesh if empty')
!   ELSE
!     CALL meshptr%SetQuality(&
!       & measures=measures, &
!       & max_measures=max_measures, &
!       & min_measures=min_measures, &
!       & nodeCoord=obj%nodeCoord, &
!       & local_nptrs=obj%local_nptrs &
!       & )
!   END IF
!   NULLIFY (meshptr)
!   RETURN
! END IF
!
! IF (PRESENT(dim) .AND. .NOT. PRESENT(entityNum)) THEN
!   tmesh = obj%getTotalMesh(dim=dim)
!   CALL Reallocate(max_, SIZE(measures), tmesh)
!   min_ = max_
!
!   DO imesh = 1, tmesh
!     meshptr => obj%getMeshPointer(dim=dim, entityNum=imesh)
!     IF (meshptr%getTotalElements() .EQ. 0) THEN
!       max_(:, imesh) = -1 * MaxDFP
!       min_(:, imesh) = MaxDFP
!     ELSE
!       CALL meshptr%SetQuality(&
!         & measures=measures, &
!         & max_measures=max_(:, imesh), &
!         & min_measures=min_(:, imesh), &
!         & nodeCoord=obj%nodeCoord, &
!         & local_nptrs=obj%local_nptrs &
!         & )
!     END IF
!   END DO
!
!   max_measures = MAXVAL(max_, dim=2)
!   min_measures = MINVAL(min_, dim=2)
!   NULLIFY (meshptr)
!   DEALLOCATE (max_, min_)
!   RETURN
! END IF
!
! CALL e%RaiseError(modName//'::'//myName//' - '// &
!   & 'No case found')

END PROCEDURE obj_SetQuality

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods

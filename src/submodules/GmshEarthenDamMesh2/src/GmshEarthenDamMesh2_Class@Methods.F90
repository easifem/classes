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

SUBMODULE(GmshEarthenDamMesh2_Class) Methods
USE Display_Method, ONLY: Display
USE Display_Method, ONLY: ToString
IMPLICIT NONE

#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: modName = "GmshEarthenDamMesh2_Class@Methods.F90"
#endif

CONTAINS

!----------------------------------------------------------------------------
!                                                                 Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Deallocate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Deallocate()"
#endif

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

obj%recombineAll = math%yes
obj%filename = ""

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

END PROCEDURE obj_Deallocate

!----------------------------------------------------------------------------
!                                                                  Display
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Display
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Display()"
#endif

LOGICAL(LGT) :: isok
INTEGER(I4B) :: tsize, ii

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

CALL Display(obj%recombineAll, "recombineAll: ", unitno=unitno)
CALL obj%filename%Display("filename: ", unitno=unitno)

isok = ALLOCATED(obj%points)
CALL Display(isok, "points ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  tsize = SIZE(obj%points)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%points(ii)%ptr)
    CALL Display(isok, "points("//ToString(ii)//") ASSOCIATED : ", &
                 unitno=unitno)

    IF (isok) THEN
      CALL obj%points(ii)%ptr%Display( &
        "points("//ToString(ii)//"): ", unitno=unitno)
    END IF
  END DO
END IF

isok = ALLOCATED(obj%lines)
CALL Display(isok, "lines ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  tsize = SIZE(obj%lines)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%lines(ii)%ptr)
    CALL Display(isok, "lines("//ToString(ii)//") ASSOCIATED : ", &
                 unitno=unitno)

    IF (isok) THEN
      CALL obj%lines(ii)%ptr%Display( &
        "lines("//ToString(ii)//"): ", unitno=unitno)
    END IF
  END DO
END IF

isok = ALLOCATED(obj%surfaces)
CALL Display(isok, "surfaces ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  tsize = SIZE(obj%surfaces)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%surfaces(ii)%ptr)
    CALL Display(isok, "surfaces("//ToString(ii)//") ASSOCIATED : ", &
                 unitno=unitno)

    IF (isok) THEN
      CALL obj%surfaces(ii)%ptr%Display( &
        "surfaces("//ToString(ii)//"): ", unitno=unitno)
    END IF
  END DO
END IF

isok = ALLOCATED(obj%physicalGroups)
CALL Display(isok, "physicalGroups ALLOCATED: ", unitno=unitno)
IF (isok) THEN
  tsize = SIZE(obj%physicalGroups)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%physicalGroups(ii)%ptr)
    CALL Display(isok, "physicalGroups("//ToString(ii)//") ASSOCIATED : ", &
                 unitno=unitno)

    IF (isok) THEN
      CALL obj%physicalGroups(ii)%ptr%Display( &
        "physicalGroups("//ToString(ii)//"): ", unitno=unitno)
    END IF
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Display

!----------------------------------------------------------------------------
!                                                                    Generate
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_Generate
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_Generate()"
#endif

INTEGER(I4B), PARAMETER :: nsd = 2
INTEGER(I4B) :: ii, tsize, ierr
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Writing points...')
#endif

isok = ALLOCATED(obj%points)
IF (isok) THEN
  tsize = SIZE(obj%points)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%points(ii)%ptr)
    IF (.NOT. isok) CYCLE
    CALL obj%points(ii)%ptr%CreateGmshModel(gmsh=gmsh)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Writing lines...')
#endif

isok = ALLOCATED(obj%lines)
IF (isok) THEN
  tsize = SIZE(obj%lines)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%lines(ii)%ptr)
    IF (.NOT. isok) CYCLE
    CALL obj%lines(ii)%ptr%CreateGmshModel(gmsh=gmsh)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'Writing surfaces...')
#endif

isok = ALLOCATED(obj%surfaces)
IF (isok) THEN
  tsize = SIZE(obj%surfaces)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%surfaces(ii)%ptr)
    IF (.NOT. isok) CYCLE
    CALL obj%surfaces(ii)%ptr%CreateGmshModel(gmsh=gmsh)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling obj%model%geo%Synchronize()...')
#endif

ierr = gmsh%model%geo%Synchronize()

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'writing physicalGroups...')
#endif

isok = ALLOCATED(obj%physicalGroups)
IF (isok) THEN
  tsize = SIZE(obj%physicalGroups)
  DO ii = 1, tsize
    isok = ASSOCIATED(obj%physicalGroups(ii)%ptr)
    IF (.NOT. isok) CYCLE
    CALL obj%physicalGroups(ii)%ptr%CreateGmshModel(gmsh=gmsh)
  END DO
END IF

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling gmsh%option%SetNumber()...')
#endif

ierr = gmsh%option%SetNumber(name="Mesh.SaveAll", VALUE=math%one_i)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling gmsh%model%mesh%Generate()...')
#endif

ierr = gmsh%model%mesh%generate(nsd)

#ifdef DEBUG_VER
CALL e%RaiseDebug(modName//'::'//myName//' - '// &
                  'calling gmsh%Write()...')
#endif

ierr = gmsh%WRITE(obj%filename%chars())

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_Generate

!----------------------------------------------------------------------------
!                                                              Include Error
!----------------------------------------------------------------------------

#include "../../include/errors.F90"

END SUBMODULE Methods

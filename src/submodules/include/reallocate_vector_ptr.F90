! This program is a part of EASIFEM library
! Expandable And Scalable Infrastructure for Finite Element Methods
! htttps://www.easifem.com
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

! Interface is given below

! INTERFACE MeshSelectionReallocate
!   MODULE SUBROUTINE Deallocate_Ptr_Vector(obj, tsize)
!     TYPE(FOO_), ALLOCATABLE, INTENT(INOUT) :: obj(:)
!   INTEGER( I4B ), INTENT(IN) :: tsize
!   END SUBROUTINE Deallocate_Ptr_Vector
! END INTERFACE MeshSelectionReallocate

INTEGER(I4B) :: ii, tsize1
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = .NOT. ALLOCATED(obj)
IF (isok) THEN
  ALLOCATE (obj(tsize))

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

tsize1 = SIZE(obj)

isok = tsize1 .EQ. tsize

IF (isok) THEN
  DO ii = 1, tsize
    obj(ii)%ptr => NULL()
  END DO

#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

DEALLOCATE (obj)
ALLOCATE (obj(tsize))

DO ii = 1, tsize
  obj(ii)%ptr => NULL()
END DO

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

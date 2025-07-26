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

! INTERFACE MeshSelectionDeallocate
!   MODULE SUBROUTINE Deallocate_Ptr_Vector(obj)
!     TYPE(FOO_), ALLOCATABLE :: obj(:)
!   END SUBROUTINE Deallocate_Ptr_Vector
! END INTERFACE MeshSelectionDeallocate

INTEGER(I4B) :: ii, tsize
LOGICAL(LGT) :: isok

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

isok = ALLOCATED(obj)
IF (.NOT. isok) THEN
#ifdef DEBUG_VER
  CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                          '[END] ')
#endif
  RETURN
END IF

tsize = SIZE(obj)
DO ii = 1, tsize
  isok = ASSOCIATED(obj(ii)%ptr)
  IF (isok) THEN
    CALL obj(ii)%ptr%DEALLOCATE()
    obj(ii)%ptr => NULL()
  END IF
END DO

DEALLOCATE (obj)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif

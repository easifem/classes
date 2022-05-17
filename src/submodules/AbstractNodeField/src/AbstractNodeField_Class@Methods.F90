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

SUBMODULE(AbstractNodeField_Class) Methods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                getPointer
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_getPointer
  ans => getPointer( obj%realVec )
END PROCEDURE anf_getPointer

!----------------------------------------------------------------------------
!                                                                    Size
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_size
  ans = obj%tSize
END PROCEDURE anf_size

!----------------------------------------------------------------------------
!                                                                 Initiate2
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_initiate2
  !!
#ifdef DEBUG_VER
  CHARACTER( LEN = * ), PARAMETER :: myName="anf_initiate2"
#endif
  !!
  INTEGER( I4B ) :: ii, tsize
  !!
#ifdef DEBUG_VER
  !!
  IF( .NOT. obj2%isInitiated ) &
    & CALL e%raiseError(modName//'::'//myName// " - "// &
    & 'Obj2 is not initiated!')
#endif
  !!
  obj%isInitiated = .TRUE.
  obj%fieldType = obj2%fieldType
  obj%domain => obj2%domain
  obj%name = obj2%name
  obj%engine = obj2%engine
  !!
  if( allocated( obj2%domains ) ) then
    tsize = size( obj2%domains )
    allocate( obj%domains( tsize ) )
    do ii = 1, tsize
      obj%domains(ii)%ptr => obj2%domains(ii)%ptr
    end do
  end if
  !!
  SELECT TYPE ( obj2 )
  CLASS IS ( AbstractNodeField_ )
    obj%tSize = obj2%tSize
    obj%realVec = obj2%realVec
    obj%dof = obj2%dof
  END SELECT
  !!
END PROCEDURE anf_initiate2

!----------------------------------------------------------------------------
!                                                            anf_Initiate3
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_initiate3
END PROCEDURE anf_initiate3

!----------------------------------------------------------------------------
!                                                            Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Deallocate
  CALL AbstractFieldDeallocate(obj)
  obj%tSize = 0
  CALL Deallocate(obj%realVec)
  CALL Deallocate(obj%dof)
END PROCEDURE anf_Deallocate

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

MODULE PROCEDURE anf_Norm2
  ans = NORM2( obj=obj%realvec )
END PROCEDURE anf_Norm2

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE Methods
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

SUBMODULE(Vector_Class) SetMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                   set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set1
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "vec_set1"

IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Vector object is not initiated')
END IF

IF (obj%tSize .GE. nodenum) THEN
  CALL set(obj%realVec, nodenum=[nodenum], VALUE=[VALUE])
ELSE
  CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'nodenum :: '//tostring(nodenum)// &
  & " is greater than tSize : "//tostring(obj%tSize))
END IF

#else
CALL set(obj%realVec, nodenum=[nodenum], VALUE=[VALUE])
#endif

END PROCEDURE vec_set1

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set2
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "vec_set2"
IF (.NOT. obj%isInitiated) THEN
  CALL e%raiseError(modName//'::'//myName//" - "// &
    & 'Vector object is not initiated')
END IF
#endif
CALL set(obj%realVec, VALUE=VALUE)
END PROCEDURE vec_set2

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set3
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "vec_set3"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (obj%tSize .NE. SIZE(VALUE)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Size of given value is not same as the size of real vector')
#endif
CALL set(obj%realVec, VALUE=VALUE)
END PROCEDURE vec_set3

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set4
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "vec_set4"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (ANY(nodenum .GT. obj%tSize)) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Some of the nodenum are out of bound, nodenum>tSize')
#endif
CALL set(obj%realVec, nodenum=nodenum, VALUE=VALUE)
END PROCEDURE vec_set4

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set5
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "vec_set5"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (istart .GT. obj%tsize .OR. iend .GT. obj%tsize) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Index is out of bound')
#endif
CALL set(obj%realVec, istart=istart, iend=iend, stride=stride, &
  & VALUE=VALUE)
END PROCEDURE vec_set5

!----------------------------------------------------------------------------
!                                                                       set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set6
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "vec_set6"
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (istart .GT. obj%tsize .OR. iend .GT. obj%tsize &
  & .OR. INT((iend - istart + stride) / stride) .GT. obj%tsize) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Index is out of bound')
#endif
CALL set(obj%realVec, istart=istart, iend=iend, stride=stride, &
  & VALUE=VALUE)
END PROCEDURE vec_set6

!----------------------------------------------------------------------------
!                                                                        set
!----------------------------------------------------------------------------

MODULE PROCEDURE vec_set7
CHARACTER(*), PARAMETER :: myName = "vec_set7"
REAL(DFP), ALLOCATABLE :: localValue(:)
INTEGER(I4B) :: ii, jj, kk
#ifdef DEBUG_VER
IF (.NOT. obj%isInitiated) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'Vector object is not initiated')
IF (storageFMT .NE. FMT_DOF .AND. storageFMT .NE. FMT_NODES) &
  & CALL e%raiseError(modName//'::'//myName//" - "// &
  & 'The value of storageFMT should be either ' &
  & //TRIM(str(FMT_DOF, .TRUE.)) &
  & //" or "//TRIM(str(FMT_NODES, .TRUE.)))
#endif
IF (PRESENT(dofs)) THEN
  ! check the size of val
  ii = SIZE(dofs) * SIZE(nodeNum)
#ifdef DEBUG_VER
  IF (ii .NE. SIZE(VALUE)) &
    & CALL e%raiseError(modName//'::'//myName//" - " &
    & //'The size of val is not correct: needed ' &
    & //TRIM(str(ii, .TRUE.)) &
    & //"but found : "//TRIM(str(SIZE(VALUE), .TRUE.)))
#endif
  ALLOCATE (localValue(SIZE(nodeNum)))
  DO ii = 1, SIZE(dofs)
    IF (storageFMT .EQ. FMT_DOF) THEN
      DO jj = 1, SIZE(nodeNum)
        kk = (ii - 1) * SIZE(nodeNum) + jj
        localValue(jj) = VALUE(kk)
      END DO
    ELSE
      DO jj = 1, SIZE(nodeNum)
        kk = (jj - 1) * SIZE(dofs) + ii
        localValue(jj) = VALUE(kk)
      END DO
    END IF
    CALL set(Vec=obj%realVec%val, &
      & obj=obj%dof, &
      & nodenum=nodeNum, &
      & VALUE=localValue, &
      & idof=dofs(ii))
  END DO
ELSE
    !! check the size of val
  ii = (.tdof.obj%dof) * SIZE(nodeNum)
#ifdef DEBUG_VER
  IF (ii .NE. SIZE(VALUE)) &
    & CALL e%raiseError(modName//'::'//myName//" - " &
    & //'The size of val is not correct: needed ' &
    & //TRIM(str(ii, .TRUE.)) &
    & //"but found : "//TRIM(str(SIZE(VALUE), .TRUE.)))
#endif
  IF (storageFMT .EQ. obj%dof%storageFMT) THEN
    CALL set(Vec=obj%realVec%val, obj=obj%dof, nodenum=nodeNum, &
      & VALUE=VALUE, Conversion=[NONE])
  ELSE
    IF (storageFMT .EQ. FMT_NODES) THEN
      CALL set(Vec=obj%realVec%val, obj=obj%dof, nodenum=nodeNum, &
        & VALUE=VALUE, Conversion=[NodesToDOF])
    ELSE
      CALL set(Vec=obj%realVec%val, obj=obj%dof, nodenum=nodeNum, &
        & VALUE=VALUE, Conversion=[DOFtoNodes])
    END IF
  END IF
END IF
END PROCEDURE vec_set7

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE SetMethods

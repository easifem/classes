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

SUBMODULE( XMLTag_Class ) SetMethods
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                                  setParent
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setParent
  NULLIFY(obj%parent)
  obj%parent => parent
END PROCEDURE xmlTag_setParent

!----------------------------------------------------------------------------
!                                                               setName
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setName
  obj%name=name
END PROCEDURE xmlTag_setName

!----------------------------------------------------------------------------
!                                                                 setChildren
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setChildren
  INTEGER( I4B ) :: i, nChildren
  !if thisXMLE already has children, clear and deallocate them first
  IF(obj%hasChildren()) THEN
    DO i=SIZE(obj%children),1,-1
      CALL obj%children(i)%DeallocateData()
    ENDDO
    DEALLOCATE(obj%children)
  ENDIF
  !> 
  IF( SIZE(children) > 0 ) THEN
    nChildren=SIZE(children)
    obj%children => children
    SELECT TYPE(obj); TYPE IS( xmlTag_ )
      DO i=1,nChildren
        obj%children(i)%parent => obj
      END DO
    END SELECT
  END IF
END PROCEDURE xmlTag_setChildren

!----------------------------------------------------------------------------
!                                                            setAttribute
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setAttribute
  INTEGER( I4B ) :: i
  TYPE( String ), ALLOCATABLE :: tmpNames(:),tmpVals(:)
  !> 
  DO i=1,obj%tAttributes
    IF(name == obj%attrNames(i)) THEN
      obj%attrValues(i)=value
      EXIT
    ENDIF
  ENDDO
  !If not currently an attribute add it
  IF(obj%tAttributes > 0) THEN
    ALLOCATE(tmpNames(obj%tAttributes))
    ALLOCATE(tmpVals(obj%tAttributes))
    tmpNames=obj%attrNames
    tmpVals=obj%attrValues
    IF(ALLOCATED(obj%attrNames)) DEALLOCATE(obj%attrNames)
    IF(ALLOCATED(obj%attrValues)) DEALLOCATE(obj%attrValues)
    obj%tAttributes=obj%tAttributes+1
    ALLOCATE(obj%attrNames(obj%tAttributes))
    ALLOCATE(obj%attrValues(obj%tAttributes))
    DO i=1,obj%tAttributes-1
      obj%attrNames(i)=tmpNames(i)
      obj%attrValues(i)=tmpVals(i)
    ENDDO
    obj%attrNames(obj%tAttributes)=name
    obj%attrValues(obj%tAttributes)=value
    DEALLOCATE(tmpNames)
    DEALLOCATE(tmpVals)
  ELSE
    IF(ALLOCATED(obj%attrNames)) DEALLOCATE(obj%attrNames)
    IF(ALLOCATED(obj%attrValues)) DEALLOCATE(obj%attrValues)
    obj%tAttributes=obj%tAttributes+1
    ALLOCATE(obj%attrNames(obj%tAttributes))
    ALLOCATE(obj%attrValues(obj%tAttributes))
    obj%attrNames=name
    obj%attrValues=value
  ENDIF
END PROCEDURE xmlTag_setAttribute

END SUBMODULE SetMethods
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

SUBMODULE(XMLTag_Class) SetMethods
USE BaseMethod
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
!                                                                    setName
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setName
  obj%name=name
END PROCEDURE xmlTag_setName

!----------------------------------------------------------------------------
!                                                                 setContent
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setContent
  LOGICAL( LGT ) :: isContentIndented0
  isContentIndented0=INPUT( Default=.FALSE., option=isContentIndented )
  IF( isContentIndented0 ) THEN
    obj%content = new_line('a')//REPEAT(CHAR_SPACE, obj%indent+2) &
      & // content // new_line('a')
  else
    obj%content = content
  endif
END PROCEDURE xmlTag_setContent

!----------------------------------------------------------------------------
!                                                                 setChildren
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setChildren
  INTEGER( I4B ) :: i, nChildren
  !if thisXMLE already has children, clear and deallocate them first
  IF(obj%hasChildren()) THEN
    DO i=SIZE(obj%children),1,-1
      CALL obj%children(i)%Deallocate()
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

!----------------------------------------------------------------------------
!                                                             setAttributes
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_setAttributes
  INTEGER( I4B ) :: ii
  DO ii = 1, SIZE( names )
    CALL obj%setAttribute( name=names( ii ), value=values( ii ) )
  END DO
END PROCEDURE xmlTag_setAttributes

!----------------------------------------------------------------------------
!                                                                      set
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_set
  IF( PRESENT( name ) ) CALL obj%setName( name )
  IF( PRESENT( attrName ) .AND. PRESENT( attrValue ) ) THEN
    CALL obj%setAttribute( name=attrName, value=attrValue )
  END IF
  IF( PRESENT( attrNames ) .AND. PRESENT( attrValues ) ) THEN
    CALL obj%setAttributes( names=attrNames, values=attrValues )
  END IF
  IF( PRESENT( parent ) ) CALL obj%setParent( parent )
  IF( PRESENT( children ) ) CALL obj%setChildren( children )
  IF( PRESENT( indent ) ) obj%Indent = indent
  IF( PRESENT( isSelfClosing ) ) obj%isSelfClosing = isSelfClosing
  IF( PRESENT( content ) ) CALL obj%setContent( content, isContentIndented )
END PROCEDURE xmlTag_set

END SUBMODULE SetMethods
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

SUBMODULE(XMLTag_Class) ConstructorMethods
USE BaseMethod
IMPLICIT NONE
CONTAINS

!----------------------------------------------------------------------------
!                                                             addSurrogate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_addSurrogate
  CALL e%addSurrogate( UserObj )
END PROCEDURE xmlTag_addSurrogate

!----------------------------------------------------------------------------
!                                                                 Initiate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_Initiate
  INTEGER( I4B ) :: tChild,ichild,ierr
  INTEGER( I4B ), ALLOCATABLE :: childTags(:,:)
  TYPE( String ) :: startTagName,endTagName,tmpStr
  CHARACTER( LEN = * ), PARAMETER :: myName="xmlTag_Initiate"

  IF(iTag(3,tagStart) .EQ. EMPTY_ELEMENT_TAG) THEN
    !> Empty Element
    IF(tagEnd .EQ. tagStart) THEN
      !Get obj%name
      CALL getTagName( &
        & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
        & ierr=ierr, tagname=obj%name )
      !Process the attributes
      CALL ConvertCharArrayToStr( &
        & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
        & strobj = tmpStr )
      CALL parseTagAttributes( &
        & chars=TRIM(tmpStr%chars()), &
        & tAttributes = obj%tAttributes, &
        & attrNames = obj%attrNames, &
        & attrValues = obj%attrValues, &
        & ierr = ierr)
      IF(ierr .NE. 0) THEN
        CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Some error has occured while parsing tag attributes')
      ENDIF
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Some error has occured while parsing tag attributes')
    ENDIF

  ELSEIF(iTag(3,tagStart) .EQ. START_TAG) THEN
    !Start/End Tagged Element
    IF(tagEnd > tagStart) THEN
      IF(iTag(3,tagEnd) .EQ. END_TAG) THEN
        !Verify matching element names
        CALL getTagName( &
          & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
          & ierr=ierr, tagname= startTagName)
        CALL getTagName( &
          & chars=cachedFile(iTag(1,tagEnd):iTag(2,tagEnd)), &
          ierr=ierr, tagname=endTagName)
        IF(startTagName .EQ. endTagName .AND. ierr .EQ. 0) THEN
          !Store the name
          obj%name=startTagName
          !Process attributes
          CALL ConvertCharArrayToStr( &
            & chars=cachedFile(iTag(1,tagStart):iTag(2,tagStart)), &
            & strobj = tmpStr)
          CALL parseTagAttributes( &
            & chars=trim(tmpStr%chars()), &
            & tAttributes = obj%tAttributes, &
            & attrNames = obj%attrNames, &
            & attrValues = obj%attrValues, ierr = ierr)
          IF(ierr .NE. 0) THEN
            CALL e%raiseError(modName//'::'//myName// " - "// &
              & 'Some error has occured while parsing tag attributes')
          ENDIF
          !Determine the number of children
          CALL getChildTagInfo( tagStart=tagStart, tagEnd=tagEnd, &
            & iTag=iTag, tChild=tChild, childTags=childTags, ierr=ierr)
          IF(tChild > 0) THEN
            !Process the children if any
            ALLOCATE(obj%children(tChild))
            DO iChild=1,tChild
              !Find the tag begin and end for the child
              CALL obj%children(ichild)%Initiate( cachedFile=cachedFile, &
                & iTag=iTag, lines=lines, tagStart=childTags(1,iChild), &
                & tagEnd=childTags(2,iChild) )
              SELECT TYPE(xmle => obj)
              TYPE IS( XMLTag_ ); obj%children(ichild)%parent => xmle
              END SELECT
            END DO
          ELSE
            !Store Content
            CALL ConvertCharArrayToStr( &
              & chars=cachedFile(iTag(2,tagStart)+1:iTag(1,tagEnd)-1), &
              & strobj=obj%content )
            obj%content=TRIM(obj%content%chars())
          END IF
        ELSE
          CALL e%raiseError(modName//'::'//myName// " - "// &
            & 'Some error has occured')
        END IF
      ELSE
        CALL e%raiseError(modName//'::'//myName// " - "// &
          & 'Some error has occured')
      END IF
    ELSE
      CALL e%raiseError(modName//'::'//myName// " - "// &
        & 'Some error has occured')
    END IF
  ELSE
    CALL e%raiseError(modName//'::'//myName// " - "// &
      & 'Some error has occured')
  END IF
END PROCEDURE xmlTag_Initiate

!----------------------------------------------------------------------------
!                                                         Deallocate
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_Deallocate
  INTEGER( I4B ) :: i
  !>
  obj%name=''
  obj%content=''
  IF(obj%tAttributes > 0) THEN
    DO i=obj%tAttributes,1,-1
      obj%attrNames(i)=''
      obj%attrValues(i)=''
    ENDDO
    IF(ALLOCATED(obj%attrNames)) DEALLOCATE(obj%attrNames)
    IF(ALLOCATED(obj%attrValues)) DEALLOCATE(obj%attrValues)
  ENDIF
  !>
  IF(ASSOCIATED(obj%children)) THEN
    DO i=SIZE(obj%children),1,-1
      CALL obj%children(i)%Deallocate()
    ENDDO
    DEALLOCATE(obj%children)
  ENDIF
  NULLIFY(obj%parent)
  obj%tAttributes=0
END PROCEDURE xmlTag_Deallocate

!----------------------------------------------------------------------------
!                                                                 Final
!----------------------------------------------------------------------------

MODULE PROCEDURE xmlTag_Final
  CALL obj%Deallocate()
END PROCEDURE xmlTag_Final

!----------------------------------------------------------------------------
!
!----------------------------------------------------------------------------

END SUBMODULE ConstructorMethods
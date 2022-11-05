
!----------------------------------------------------------------------------
!                                                          MeshFacet_Display
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacet_Display
  !!
  CALL Display( TRIM(msg), unitno=unitno )
  CALL Display( "# elementType=BOUNDARY_ELEMENT", unitno=unitno)
  !!
  IF( ALLOCATED( obj%masterCellNumber ) ) THEN
    CALL Display( obj%masterCellNumber, msg="# masterCellNumber=", &
      & unitno=unitno)
  ELSE
    CALL Display( "# masterCellNumber NOT ALLOCATED", unitno=unitno)
  END IF
  !!
  IF( ALLOCATED( obj%masterlocalFacetID ) ) THEN
    CALL Display( obj%masterlocalFacetID, msg="# masterlocalFacetID=", &
      & unitno=unitno)
  ELSE
    CALL Display( "# masterlocalFacetID NOT ALLOCATED", unitno=unitno)
  END IF
  !!
END PROCEDURE MeshFacet_Display


!----------------------------------------------------------------------------
!                                                        SetSlaveCellNumber
!----------------------------------------------------------------------------

MODULE PROCEDURE meshFacet_SetSlaveCellNumber
  obj%slaveCellNumber( facetElement ) = slaveCellNumber
END PROCEDURE meshFacet_SetSlaveCellNumber

!----------------------------------------------------------------------------
!                                                       SetSlaveLocalFacetID
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacet_SetSlaveLocalFacetID
  obj%slaveLocalFacetID( facetElement ) = slaveLocalFacetID
END PROCEDURE MeshFacet_SetSlaveLocalFacetID

!----------------------------------------------------------------------------
!                                                       SetSlaveData
!----------------------------------------------------------------------------

MODULE PROCEDURE MeshFacet_SetSlaveData
  obj%slaveCellNumber( facetElement ) = slaveCellNumber
  obj%slaveLocalFacetID( facetElement ) = slaveLocalFacetID
END PROCEDURE MeshFacet_SetSlaveData

!----------------------------------------------------------------------------
!                                                              meshFacet_Set
!----------------------------------------------------------------------------

MODULE PROCEDURE meshFacet_Set
  obj%masterCellNumber( facetElement ) = domainFacetData%masterCellNumber
  obj%masterLocalFacetID( facetElement ) = domainFacetData%masterLocalFacetID
END PROCEDURE meshFacet_Set


!----------------------------------------------------------------------------
!                                              SetSlaveCellNumber@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Mesh Facet Data

INTERFACE
  MODULE PURE SUBROUTINE meshFacet_SetSlaveCellNumber(obj, facetElement, &
    & slaveCellNumber)
    CLASS( MeshFacetData_ ), INTENT(INOUT) :: obj
    INTEGER( I4B ), INTENT( IN ) :: facetElement
    INTEGER( I4B ), INTENT( IN ) :: slaveCellNumber
  END SUBROUTINE meshFacet_SetSlaveCellNumber
END INTERFACE


!----------------------------------------------------------------------------
!                                            SetSlaveLocalFacetID@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Mesh Facet Data

INTERFACE
  MODULE PURE SUBROUTINE MeshFacet_SetSlaveLocalFacetID(obj, facetElement, &
    & slaveLocalFacetID)
    CLASS( MeshFacetData_ ), INTENT(INOUT) :: obj
    INTEGER( I4B ), INTENT( IN ) :: facetElement
    INTEGER( I4B ), INTENT( IN ) :: slaveLocalFacetID
  END SUBROUTINE MeshFacet_SetSlaveLocalFacetID
END INTERFACE

!----------------------------------------------------------------------------
!                                              SetSlaveData@setMethods
!----------------------------------------------------------------------------

!> authors: Vikas Sharma, Ph. D.
! date: 18 May 2022
! summary: Mesh Facet Data

INTERFACE
  MODULE PURE SUBROUTINE MeshFacet_SetSlaveData(obj, facetElement, &
    & slaveCellNumber, slaveLocalFacetID)
    CLASS( MeshFacetData_ ), INTENT(INOUT) :: obj
    INTEGER( I4B ), INTENT( IN ) :: facetElement
    INTEGER( I4B ), INTENT( IN ) :: slaveCellNumber
    INTEGER( I4B ), INTENT( IN ) :: slaveLocalFacetID
  END SUBROUTINE MeshFacet_SetSlaveData
END INTERFACE

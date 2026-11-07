!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: Testing GetFacetConnectivity_ in broken spaces
!
! In a broken space a trace on a facet is evaluated from the complete block
! of the owning cell, because the mesh vertices and the faces own no DOFs.
! GetFacetConnectivity_ must return this block for every local facet.

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: order = 2, nloc = 6, tfacets = 3
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
INTEGER(I4B) :: found(20), want(20), nfound, nwant, iel, iface
LOGICAL(LGT) :: isSize, isDOF

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                  isBrokenSpace=math%yes, order=order, dom=dom)

isSize = math%yes
isDOF = math%yes

DO iel = 1, obj%GetTotalCells()
  CALL obj%GetCellDOF(globalCell=iel, islocal=math%yes, ans=want, &
                      tsize=nwant)
  DO iface = 1, tfacets
    CALL obj%GetFacetConnectivity_(ans=found, tsize=nfound, &
                                   globalElement=iel, &
                                   localFaceNumber=iface, islocal=math%yes)
    isSize = isSize .AND. (nfound .EQ. nloc) .AND. (nwant .EQ. nloc)
    IF (nfound .EQ. nwant) THEN
      isDOF = isDOF .AND. ALL(found(1:nfound) .EQ. want(1:nwant))
    ELSE
      isDOF = math%no
    END IF
  END DO
END DO

CALL OK(isSize, "facet: size of the cell block")
CALL OK(isDOF, "facet: DOFs of the cell block")

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main

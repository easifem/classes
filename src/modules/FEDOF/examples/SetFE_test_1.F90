!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: Testing SetFE for continuous and broken hierarchical spaces
!
! SetFE configures the finite element of the topology of an element. After
! SetFE the size of the local basis of the finite element must be equal to
! the number of DOFs of the element, for every element. The cells of the
! broken space have different orders, so that the finite element must take
! the order of each cell in turn.

PROGRAM main
USE AbstractFE_Class, ONLY: AbstractFE_
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
TYPE(FEDOF_) :: obj
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
CLASS(AbstractFE_), POINTER :: feptr => NULL()

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

! Test 1: continuous space of uniform order
BLOCK
  INTEGER(I4B), PARAMETER :: order = 3, nloc = 10
  INTEGER(I4B) :: iel
  LOGICAL(LGT) :: isok

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    isBrokenSpace=math%no, order=order, dom=dom)

  isok = math%yes
  DO iel = 1, obj%GetTotalCells()
    CALL obj%SetFE(globalElement=iel, islocal=math%yes)
    feptr => obj%GetFEPointer(globalElement=iel, islocal=math%yes)
    isok = isok .AND. (feptr%opt%GetTotalDOF() .EQ. nloc)
    isok = isok .AND. (obj%GetTotalDOF(globalElement=iel, &
                                       islocal=math%yes) .EQ. nloc)
  END DO
  CALL OK(isok, "H1: local basis size of every element")
END BLOCK

! Test 2: broken space, the cells have orders 1, 2 and 3
BLOCK
  INTEGER(I4B), ALLOCATABLE :: orders(:)
  INTEGER(I4B) :: iel, tcells, nloc
  LOGICAL(LGT) :: isok

  CALL obj%DEALLOCATE()

  tcells = dom%GetTotalElements()
  ALLOCATE (orders(tcells))
  DO iel = 1, tcells
    orders(iel) = 1 + MOD(iel, 3)
  END DO

  CALL obj%Initiate(baseContinuity="H1", baseInterpolation="Hierarchical", &
                    isBrokenSpace=math%yes, order=orders, islocal=math%yes, &
                    dom=dom)

  isok = math%yes
  DO iel = 1, tcells
    nloc = (orders(iel) + 1) * (orders(iel) + 2) / 2
    CALL obj%SetFE(globalElement=iel, islocal=math%yes)
    feptr => obj%GetFEPointer(globalElement=iel, islocal=math%yes)
    isok = isok .AND. (feptr%opt%GetTotalDOF() .EQ. nloc)
    isok = isok .AND. (obj%GetTotalDOF(globalElement=iel, &
                                       islocal=math%yes) .EQ. nloc)
  END DO
  CALL OK(isok, "broken: local basis size of every cell")

  DEALLOCATE (orders)
END BLOCK

feptr => NULL()
CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main

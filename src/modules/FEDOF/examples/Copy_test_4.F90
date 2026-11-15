!> author: Vikas Sharma, Ph. D.
! date: 2026-11-07
! summary: Testing the ownership of the finite elements in Copy
!
! A copy shares the finite elements of its source and does not own them.
! Here the target of the copy was initiated before, so it owns finite
! elements of its own. After the copy, Deallocate of the target must not
! free the finite elements of the source: the source stays usable, and its
! own Deallocate must not free them a second time.

PROGRAM main
USE AbstractFE_Class, ONLY: AbstractFE_
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE FEDOF_Class, ONLY: FEDOF_
USE FEDomain_Class, ONLY: FEDomain_
USE GlobalData, ONLY: I4B
USE HDF5File_Class, ONLY: HDF5File_
USE Test_Method, ONLY: OK
IMPLICIT NONE

CHARACTER(*), PARAMETER :: filename = "./meshdata/small_tri3_mesh.h5"
INTEGER(I4B), PARAMETER :: order = 2, nloc = 6
TYPE(FEDOF_) :: obj, obj2
TYPE(FEDomain_) :: dom
TYPE(HDF5File_) :: meshfile
CLASS(AbstractFE_), POINTER :: feptr => NULL()

CALL e%SetQuietMode(math%yes)
CALL meshfile%Initiate(filename, mode="READ")
CALL meshfile%OPEN()
CALL dom%Initiate(meshfile, '')

CALL obj%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                  order=order, dom=dom)
CALL obj2%Initiate(baseContinuity="H1", baseInterpolation="HIER", &
                   order=1, dom=dom)

! Test 1: the copy replaces the data of the target
obj2 = obj
CALL OK(obj2%GetTotalDOF() .EQ. obj%GetTotalDOF(), "copy: GetTotalDOF")

! Test 2: the source survives Deallocate of the copy
CALL obj2%DEALLOCATE()
CALL OK(.NOT. obj2%IsInitiated(), "copy: deallocated")
CALL OK(obj%IsInitiated(), "source: still initiated")

CALL obj%SetFE(globalElement=1, islocal=math%yes)
feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
CALL OK(feptr%opt%GetTotalDOF() .EQ. nloc, "source: FE still usable")
feptr => NULL()

! Test 3: a target which was deallocated does not own what it copies
CALL obj2%Copy(obj)
CALL obj2%DEALLOCATE()
CALL obj%SetFE(globalElement=1, islocal=math%yes)
feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
CALL OK(feptr%opt%GetTotalDOF() .EQ. nloc, "source: FE usable after reuse")
feptr => NULL()

! Test 4: an owner which copies from its own copy keeps its finite elements
obj2 = obj
obj = obj2
CALL obj2%DEALLOCATE()
CALL obj%SetFE(globalElement=1, islocal=math%yes)
feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
CALL OK(feptr%opt%GetTotalDOF() .EQ. nloc, "owner: FE usable after copy back")
feptr => NULL()

! Test 5: a copy onto itself keeps the data and the finite elements
CALL obj%Copy(obj)
CALL OK(obj%IsInitiated(), "self copy: still initiated")
CALL obj%SetFE(globalElement=1, islocal=math%yes)
feptr => obj%GetFEPointer(globalElement=1, islocal=math%yes)
CALL OK(feptr%opt%GetTotalDOF() .EQ. nloc, "self copy: FE usable")
feptr => NULL()

CALL obj%DEALLOCATE()
CALL dom%DEALLOCATE()
CALL meshfile%DEALLOCATE()
END PROGRAM main

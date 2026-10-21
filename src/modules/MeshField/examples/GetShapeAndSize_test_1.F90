!> author: Vikas Sharma, Ph. D.
! date: 2026-10-21
! summary: Shape of data inside an element
!
! AbstractMeshFieldGetShapeAndSize returns the shape s(1:tsize) of the
! data stored in one element for a given rank and varType. This is the
! value of s which should be given to Initiate. No mesh is needed.

PROGRAM main
USE AbstractMeshField_Class, ONLY: AbstractMeshFieldGetShapeAndSize
USE BaseType, ONLY: fevaropt => TypeFEVariableOpt
USE GlobalData, ONLY: I4B
USE GlobalData, ONLY: LGT
USE Test_Method, ONLY: OK

IMPLICIT NONE

INTEGER(I4B), PARAMETER :: nns = 3, nnt = 2, spaceCompo = 2, &
                           dim1 = 2, dim2 = 3
INTEGER(I4B) :: s(4), tsize
LOGICAL(LGT) :: isok

! scalar
CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%scalar, varType=fevaropt%constant, s=s, tsize=tsize)
CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. 1), "scalar, constant")

CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%scalar, varType=fevaropt%space, s=s, tsize=tsize, &
  nns=nns)
CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. nns), "scalar, space")

CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%scalar, varType=fevaropt%spaceTime, s=s, tsize=tsize, &
  nns=nns, nnt=nnt)
isok = (tsize .EQ. 2) .AND. ALL(s(1:2) .EQ. [nns, nnt])
CALL OK(isok, "scalar, spaceTime")

! vector
CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%vector, varType=fevaropt%constant, s=s, tsize=tsize, &
  spaceCompo=spaceCompo)
CALL OK((tsize .EQ. 1) .AND. (s(1) .EQ. spaceCompo), "vector, constant")

CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%vector, varType=fevaropt%space, s=s, tsize=tsize, &
  spaceCompo=spaceCompo, nns=nns)
isok = (tsize .EQ. 2) .AND. ALL(s(1:2) .EQ. [spaceCompo, nns])
CALL OK(isok, "vector, space")

CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%vector, varType=fevaropt%spaceTime, s=s, tsize=tsize, &
  spaceCompo=spaceCompo, nns=nns, nnt=nnt)
isok = (tsize .EQ. 3) .AND. ALL(s(1:3) .EQ. [spaceCompo, nns, nnt])
CALL OK(isok, "vector, spaceTime")

! matrix
CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%matrix, varType=fevaropt%constant, s=s, tsize=tsize, &
  dim1=dim1, dim2=dim2)
isok = (tsize .EQ. 2) .AND. ALL(s(1:2) .EQ. [dim1, dim2])
CALL OK(isok, "matrix, constant")

CALL AbstractMeshFieldGetShapeAndSize( &
  rank=fevaropt%matrix, varType=fevaropt%spaceTime, s=s, tsize=tsize, &
  dim1=dim1, dim2=dim2, nns=nns, nnt=nnt)
isok = (tsize .EQ. 4) .AND. ALL(s(1:4) .EQ. [dim1, dim2, nns, nnt])
CALL OK(isok, "matrix, spaceTime")

END PROGRAM main

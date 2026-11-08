!> author: Vikas Sharma, Ph. D.
! date: 2026-11-06
! summary: Testing Initiate, Deallocate and query methods of MeshSelection_

PROGRAM main
USE BaseType, ONLY: math => TypeMathOpt
USE ExceptionHandler_Class, ONLY: e
USE GlobalData, ONLY: LGT
USE MeshSelection_Class, ONLY: MeshSelection_
USE MeshSelection_Class, ONLY: TypeMeshSelection
USE Test_Method, ONLY: OK
IMPLICIT NONE

TYPE(MeshSelection_) :: obj

CALL e%SetQuietMode(math%yes)

! Test 1: a new instance is not initiated
BLOCK
  CALL OK(.NOT. obj%IsInitiated(), "new instance is not initiated")
END BLOCK

! Test 2: Initiate without options switches every selection off
BLOCK
  CALL obj%Initiate()
  CALL OK(obj%IsInitiated(), "Initiate: IsInitiated")
  CALL OK(.NOT. obj%IsSelectionByMeshID(), "Initiate: default meshID")
  CALL OK(.NOT. obj%IsSelectionByElemNum(), "Initiate: default elemNum")
  CALL OK(.NOT. obj%IsSelectionByNodeNum(), "Initiate: default nodeNum")
  CALL OK(.NOT. obj%IsSelectionByBox(), "Initiate: default box")
END BLOCK

! Test 3: each option sets only its own selection
BLOCK
  CALL obj%Initiate(isSelectionByMeshID=math%yes)
  CALL OK(obj%IsSelectionByMeshID(), "Initiate: option meshID")
  CALL OK(.NOT. obj%IsSelectionByBox(), "Initiate: box stays off")

  CALL obj%Initiate(isSelectionByElemNum=math%yes)
  CALL OK(obj%IsSelectionByElemNum(), "Initiate: option elemNum")
  CALL OK(.NOT. obj%IsSelectionByMeshID(), "Initiate: resets meshID")

  CALL obj%Initiate(isSelectionByNodeNum=math%yes)
  CALL OK(obj%IsSelectionByNodeNum(), "Initiate: option nodeNum")

  CALL obj%Initiate(isSelectionByBox=math%yes)
  CALL OK(obj%IsSelectionByBox(), "Initiate: option box")
END BLOCK

! Test 4: GetParam returns the same values as the query functions
BLOCK
  LOGICAL(LGT) :: isInit, byMesh, byElem, byNode, byBox

  CALL obj%Initiate(isSelectionByMeshID=math%yes, &
                    isSelectionByNodeNum=math%yes)
  CALL obj%GetParam(isInitiated=isInit, isSelectionByMeshID=byMesh, &
                    isSelectionByElemNum=byElem, &
                    isSelectionByNodeNum=byNode, isSelectionByBox=byBox)
  CALL OK(isInit .AND. byMesh .AND. byNode, "GetParam: true values")
  CALL OK(.NOT. (byElem .OR. byBox), "GetParam: false values")
END BLOCK

! Test 6: Initiate removes the data of an earlier selection
BLOCK
  CALL obj%Initiate(isSelectionByMeshID=math%yes)
  CALL obj%Add(dim=2, meshID=[1, 2])
  CALL obj%Initiate(isSelectionByMeshID=math%yes)
  CALL OK(.NOT. obj%IsMeshIDAllocated(dim=2), "Initiate: clears meshID")
END BLOCK

! Test 7: Deallocate resets the instance
BLOCK
  CALL obj%Add(dim=2, meshID=[1, 2])
  CALL obj%DEALLOCATE()
  CALL OK(.NOT. obj%IsInitiated(), "Deallocate: IsInitiated")
  CALL OK(.NOT. obj%IsSelectionByMeshID(), "Deallocate: meshID flag")
  CALL OK(.NOT. obj%IsMeshIDAllocated(dim=2), "Deallocate: meshID data")
END BLOCK

CALL obj%DEALLOCATE()
END PROGRAM main

---
date: 2025-10-13
summary:  Set the face orientation

---
 
# obj_SetFaceOrientation
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_SetFaceOrientation(obj, faceOrient, tFace, errCheck)
CLASS(BasisOpt_), INTENT(INOUT) :: obj
!! abstract finite element
INTEGER(I4B), OPTIONAL, INTENT(IN) :: faceOrient(:, :)
!! face orient, necessary for Hierarchical interpolation
!! Size(faceOrient, 1) should be 3
!! Size(faceOrient, 2) should be at least tFace
INTEGER(I4B), OPTIONAL, INTENT(IN) :: tFace
!! size of cellOrder, necessary for Hierarchical interpolation
LOGICAL(LGT), OPTIONAL, INTENT(IN) :: errCheck
!! user can ignore this option
!! for dev: this option checks the errors in debug mode
END SUBROUTINE obj_SetFaceOrientation
END INTERFACE
```
 

---
date: 2025-10-30
summary: Get the number of spatial dimensions

---
 
# obj_GetTotalFace
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE FUNCTION obj_GetTotalFace(obj) RESULT(ans)
CLASS(BasisOpt_), INTENT(IN) :: obj

INTEGER(I4B) :: ans

END FUNCTION obj_GetTotalFace
END INTERFACE
```
 

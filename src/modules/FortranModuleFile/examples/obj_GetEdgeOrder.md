---
date: 2025-10-17
summary: Get edge order

---
 
# obj_GetEdgeOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_GetEdgeOrder(obj, ans, tsize)
CLASS(BasisOpt_), INTENT(IN) :: obj

INTEGER(I4B), INTENT(INOUT) :: ans(:)

INTEGER(I4B), INTENT(OUT) :: tsize

END SUBROUTINE obj_GetEdgeOrder
END INTERFACE
```
 

---
date: 2025-10-17
summary: Get cell order

---
 
# obj_GetCellOrder
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_GetCellOrder(obj, ans, tsize)
CLASS(BasisOpt_), INTENT(IN) :: obj

INTEGER(I4B), INTENT(INOUT) :: ans(:)

INTEGER(I4B), INTENT(OUT) :: tsize

END SUBROUTINE obj_GetCellOrder
END INTERFACE
```
 

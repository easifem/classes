---
date: 2025-07-09
summary:  Copy basis options from one object to another

---
 
# obj_Copy
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_Copy(obj, obj2)
CLASS(BasisOpt_), INTENT(INOUT) :: obj

CLASS(BasisOpt_), INTENT(IN) :: obj2

END SUBROUTINE obj_Copy
END INTERFACE
```
 

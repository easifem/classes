---
date: 2025-07-09
summary:  Display the contents of basis options

---
 
# obj_Display
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE SUBROUTINE obj_Display(obj, msg, unitno, notFull)
CLASS(BasisOpt_), INTENT(IN) :: obj

CHARACTER(*), INTENT(IN) :: msg

INTEGER(I4B), OPTIONAL, INTENT(IN) :: unitno

LOGICAL(LGT), OPTIONAL, INTENT(IN) :: notFull

END SUBROUTINE obj_Display
END INTERFACE
```
 

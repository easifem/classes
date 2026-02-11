---
date: 2025-07-14
summary:  Returns the topology type

---
 
# obj_GetTopologyType
 
 
## Interface
 
 
```fortran
INTERFACE
MODULE FUNCTION obj_GetTopologyType(obj) RESULT(ans)
CLASS(BasisOpt_), INTENT(IN) :: obj

INTEGER(I4B) :: ans

END FUNCTION obj_GetTopologyType
END INTERFACE
```
 

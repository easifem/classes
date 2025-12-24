
INTEGER(I4B) :: ii, dim, nsd, tsize
LOGICAL(LGT) :: isok
CLASS(AbstractMesh_), POINTER :: meshptr

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[START] ')
#endif DEBUG_VER

nsd = obj%GetNSD()

DO dim = 1, nsd
  tsize = obj%GetTotalEntities(dim=dim)

  DO ii = 1, tsize
    meshptr => obj%GetMeshPointer(dim=dim, entitynum=ii)
    isok = ASSOCIATED(meshptr)
    IF (.NOT. isok) CYCLE

    CALL meshptr%__METHOD_NAME__()
  END DO

END DO

NULLIFY (meshptr)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
  & '[END] ')
#endif DEBUG_VER

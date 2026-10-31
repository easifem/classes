
!----------------------------------------------------------------------------
!                                                             SetOrientation
!----------------------------------------------------------------------------

MODULE PROCEDURE obj_SetOrientation
#ifdef DEBUG_VER
CHARACTER(*), PARAMETER :: myName = "obj_SetOrientation()"
#endif
INTEGER(I4B) :: cellOrient0(3), faceOrient0(3, TypeRefelemOpt%maxFaces), &
                edgeOrient0(TypeRefelemOpt%maxEdges)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[START] ')
#endif

cellOrient0 = math%one_i
faceOrient0 = math%one_i
edgeOrient0 = math%one_i

CALL obj%opt%SetCellOrientation(cellOrient=cellOrient0, &
                                tCell=SIZE(cellOrient0), &
                                errCheck=math%no)

CALL obj%opt%SetFaceOrientation(faceOrient=faceOrient0, &
                                tFace=SIZE(faceOrient0, 2), &
                                errCheck=math%no)

CALL obj%opt%SetEdgeOrientation(edgeOrient=edgeOrient0, &
                                tEdge=SIZE(edgeOrient0), &
                                errCheck=math%no)

#ifdef DEBUG_VER
CALL e%RaiseInformation(modName//'::'//myName//' - '// &
                        '[END] ')
#endif
END PROCEDURE obj_SetOrientation

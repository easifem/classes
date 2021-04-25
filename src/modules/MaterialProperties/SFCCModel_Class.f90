!> authors: Dr. Vikas Sharma
!
!  This module consists of volumetric heat capacity model for porous media
!
MODULE SFCCModel_Class
USE GlobalData
IMPLICIT NONE
PRIVATE

#include "./PMDefine.inc"

!----------------------------------------------------------------------------
!                                                         PMVolHeatCapModel_
!----------------------------------------------------------------------------

TYPE, ABSTRACT :: SFCCModel_
  PROCEDURE( ExpSFCC_get_val ), POINTER, PASS( obj ) :: getValue => NULL()
  PROCEDURE( ExpSFCC_get_slope ), POINTER, PASS( obj ) :: getSlope => NULL()
  PROCEDURE( ExpSFCC_PhaseInfo ), POINTER, PASS( obj ) :: PhaseInfo => NULL()
END TYPE SFCCModel_

PUBLIC :: SFCCModel_

!----------------------------------------------------------------------------
!                                                          SFCCModelPointer_
!----------------------------------------------------------------------------
TYPE :: SFCCModelPointer_
  CLASS( SFCCModel_ ), POINTER :: Ptr=>NULL()
END TYPE SFCCModelPointer_

PUBLIC :: SFCCModelPointer_

!----------------------------------------------------------------------------
!                                                                   ExpSFCC_
!----------------------------------------------------------------------------

TYPE, EXTENDS( SFCCModel_ ) :: ExpSFCC_
  REAL( DFP ) :: Theta_r = 0.0_DFP !! residual
  REAL( DFP ) :: Theta_w = 0.0_DFP !! TotalWater content
  REAL( DFP ) :: Temp_l = 0.0_DFP !! Freezing temp liquid side
  REAL( DFP ) :: Temp_s = 0.0_DFP !! Freezing temp solid dide
  REAL( DFP ) :: Coeff = 1.0_DFP !! exponent
END TYPE ExpSFCC_

PUBLIC :: ExpSFCC_

TYPE( ExpSFCC_ ), PUBLIC, PARAMETER :: TypeExpSFCC=ExpSFCC_()

!----------------------------------------------------------------------------
!                                                                 UserSFCC_
!----------------------------------------------------------------------------

TYPE, EXTENDS( SFCCModel_ ) :: UserSFCC_
END TYPE UserSFCC_

PUBLIC :: UserSFCC_

TYPE( UserSFCC_ ), PUBLIC, PARAMETER :: TypeUserSFCC = UserSFCC_()

!----------------------------------------------------------------------------
!                                                              UserSFCC@User
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION UserSFCC_Pointer(  ) RESULT( ans )
  CLASS( UserSFCC_ ), POINTER :: ans
END FUNCTION UserSFCC_Pointer
END INTERFACE

PUBLIC :: UserSFCC_Pointer

!----------------------------------------------------------------------------
!                                                        Constructor@ExpSFCC
!----------------------------------------------------------------------------

INTERFACE
MODULE FUNCTION ExpSFCC_Pointer( Theta_r, Theta_w, Temp_l, Temp_s, &
  &  Coeff ) RESULT( ans )
  CLASS( ExpSFCC_ ), POINTER :: ans
  REAL( DFP ), INTENT( IN ) :: Theta_r
  REAL( DFP ), INTENT( IN ) :: Theta_w
  REAL( DFP ), INTENT( IN ) :: Temp_l
  REAL( DFP ), INTENT( IN ) :: Temp_s
  REAL( DFP ), OPTIONAL, INTENT( IN ) :: Coeff
END FUNCTION ExpSFCC_Pointer
END INTERFACE

PUBLIC :: ExpSFCC_Pointer

!----------------------------------------------------------------------------
!                                                           getValue@expSFCC
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ExpSFCC_get_val( obj, Temp ) RESULT( ans )
  CLASS( SFCCModel_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ) :: ans
END FUNCTION ExpSFCC_get_val
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getSlope@expSFCC
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ExpSFCC_get_slope( obj, Temp ) RESULT( ans )
  CLASS( SFCCModel_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Temp
  REAL( DFP ) :: ans
END FUNCTION ExpSFCC_get_slope
END INTERFACE

!----------------------------------------------------------------------------
!                                                    PhaseInfo@expSFCC
!----------------------------------------------------------------------------

INTERFACE
MODULE PURE FUNCTION ExpSFCC_PhaseInfo( obj, Temp ) RESULT( ans )
  CLASS( SFCCModel_ ), INTENT( IN ) :: obj
  REAL( DFP ), INTENT( IN ) :: Temp
  CHARACTER( LEN = 1 ) :: ans
END FUNCTION ExpSFCC_PhaseInfo
END INTERFACE

!----------------------------------------------------------------------------
!                                                           getSlope@ExpSFCC
!----------------------------------------------------------------------------

END MODULE SFCCModel_Class
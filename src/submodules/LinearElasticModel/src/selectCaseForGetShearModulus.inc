acase = 'FFFF' ! K, E, Lambda, Nu

isok = PRESENT(K); IF (isok) acase(1:1) = 'T'
isok = PRESENT(E); IF (isok) acase(2:2) = 'T'
isok = PRESENT(lambda); IF (isok) acase(3:3) = 'T'
isok = PRESENT(nu); IF (isok) acase(4:4) = 'T'

SELECT CASE (acase)
CASE ('TTFF')
  ! isK isE
  ans = 3.0_DFP * K * E / (9.0_DFP * K - E)

CASE ('TFTF')
  ! isK isLambda
  ans = 1.5_DFP * (K - lambda)

CASE ('TFFT')
  ! isK isNu
  ans = 1.5_DFP * K * (1.0_DFP - 2.0_DFP * nu) / (1.0_DFP + nu)

CASE ('FTTF')
  ! isE isLambda
  r = E**2 + 9.0_DFP * lambda**2 + 2.0_DFP * E * lambda
  r = SQRT(r)
  ans = 0.25_DFP * (E - 3.0_DFP * lambda + r)

CASE ('FTFT')
  ! isE isNu
  ans = 0.5_DFP * E / (1.0_DFP + nu)

CASE ('FFTT')
  ! isLambda isNu
  ans = lambda * (1.0_DFP - 2.0_DFP * nu) / 2.0_DFP / nu

#ifdef DEBUG_VER
CASE DEFAULT
  CALL AssertError1(.FALSE., myName, &
                    'No case found for acase='//acase)
#endif
END SELECT


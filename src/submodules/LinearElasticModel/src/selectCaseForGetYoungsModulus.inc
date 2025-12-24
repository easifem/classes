acase = 'FFFFF' ! K, Lambda, G, Mu, Nu
isok = PRESENT(K); IF (isok) acase(1:1) = 'T'
isok = PRESENT(lambda); IF (isok) acase(2:2) = 'T'
isok = PRESENT(G); IF (isok) acase(3:3) = 'T'
isok = PRESENT(mu); IF (isok) acase(4:4) = 'T'
isok = PRESENT(nu); IF (isok) acase(5:5) = 'T'

SELECT CASE (acase)
CASE ('TTFFF')
  ! isK isLambda
  ans = 9.0_DFP * K * (K - lambda) / (3.0_DFP * K - lambda)

CASE ('TFTFF')
  ! isK isG
  ans = 9.0_DFP * K * G / (3.0_DFP * K + G)

CASE ('TFFTF')
  ! isK isMu
  ans = 9.0_DFP * K * mu / (3.0_DFP * K + mu)

CASE ('TFFFT')
  ! isK isNu
  ans = 3.0_DFP * K * (1.0_DFP - 2.0_DFP * nu)

CASE ('FTTFF')
  ! isLambda isG
  ans = G * (3.0_DFP * lambda + 2.0_DFP * G) / (lambda + G)

CASE ('FTFTF')
  ! isLambda isMu
  ans = mu * (3.0_DFP * lambda + 2.0_DFP * mu) / (lambda + mu)

CASE ('FTFFT')
  ! isLambda isNu
  ans = lambda * (1.0_DFP + nu) * (1.0_DFP - 2.0_DFP * nu) / nu

CASE ('FFTFT')
  ! isG isNu
  ans = 2.0_DFP * G * (1.0_DFP + nu)

CASE ('FFFTT')
  ! isMu isNu
  ans = 2.0_DFP * mu * (1.0_DFP + nu)

#ifdef DEBUG_VER
CASE DEFAULT
CALL AssertError1(.FALSE., myName, &
                  'No case found for acase='//acase)
#endif
END SELECT

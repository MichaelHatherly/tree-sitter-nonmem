$PROB Tricky identifiers - keyword-like names
$INP ID TIME DV AMT EVID
$DATA data.csv IGNORE=@

$SUB ADVAN2 TRANS2
$PK
; Identifiers that look like keywords
IF1 = 1
DO1 = 2
THEN1 = 3
ELSE1 = 4
ENDIF1 = 5
ENDDO1 = 6

; Look like parameter refs
THETA1 = THETA(1)
ETA1 = ETA(1)
ERR1 = 0
EPS1 = 0

; Look like functions
EXP1 = 1
LOG1 = 2
SQRT1 = 3
SIN1 = 4
COS1 = 5
ABS1 = 6

; Look like options
METHOD1 = 1
MAXEVAL1 = 9999
PRINT1 = 5

; Actual use in context
TVCL = THETA1
TVV = THETA(2)
CL = TVCL * EXP(ETA1)
V = TVV * EXP(ETA(2))

; IF with IF-like identifier
IF (IF1.EQ.1) THEN
  K = CL / V
ELSE
  K = TVCL / TVV
ENDIF

; DOG identifier (starts with DO)
DOG = 1
DOSE = AMT
DOMAIN = 1
DOUBLE = 2.0

; CALL-like
CALL1 = 1
CALLFL1 = -1

S1 = V

$ERR
IPRED = F
; More tricky names
IPRED1 = IPRED
Y1 = IPRED
Y = IPRED * (1 + ERR(1))

$THETA (0, 5) ; THETA1
       (0, 50)

$OMEGA 0.1 0.1
$SIGMA 0.04

$EST METHOD=1 MAXEVAL=9999
$COV
$TAB ID TIME DV IPRED Y FILE=sdtab

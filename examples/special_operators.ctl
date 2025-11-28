$PROB Special operators - all Fortran and C-style
$INP ID TIME DV AMT FLAG SEX WT AGE
$DATA data.csv IGNORE=@

$SUB ADVAN2 TRANS2
$PK
TVCL = THETA(1)
TVV = THETA(2)

; Fortran operators
IF (FLAG.EQ.1.AND.SEX.EQ.0) THEN
  CL = TVCL * 0.8
ELSEIF (FLAG.NE.1.OR.AGE.GT.65) THEN
  CL = TVCL * 1.2
ELSE
  CL = TVCL
ENDIF

; More Fortran comparisons
IF (WT.GE.70.AND.WT.LE.100) THEN
  V = TVV * (WT/70)
ELSE
  V = TVV
ENDIF

; C-style operators
IF (FLAG==2.AND.SEX/=1) THEN
  F1 = 0.9
ELSEIF (FLAG>0.OR.AGE<18) THEN
  F1 = 0.8
ELSE
  F1 = 1
ENDIF

; Mixed in expressions
TEST1 = (FLAG.EQ.1) + (SEX==0)
TEST2 = (WT>=50) * (AGE<=65)

; Exponentiation
PWR = WT**0.75
LOGCL = LOG(CL)
EXPCL = EXP(-K*TIME)

; Negation
NEG = -THETA(3)
DIFF = CL - (-V)

K = CL/V
S1 = V

$ERR
IPRED = F
; All arithmetic ops
Y = IPRED + ERR(1) - 0.01 * ERR(2) / 2 ** 0.5

$THETA (0, 5) (0, 50) 0.75
$OMEGA 0.1 0.1
$SIGMA 0.04 0.01

$EST METHOD=1 MAXEVAL=9999
$COV
$TAB ID TIME DV IPRED FILE=sdtab

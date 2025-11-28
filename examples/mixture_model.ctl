$PROBLEM Mixture model with two subpopulations

$INPUT ID TIME AMT DV MDV

$DATA ../data/mixture_data.csv IGNORE=@

$SUBROUTINES ADVAN2 TRANS2

$PK
MU_1 = THETA(1)
MU_2 = THETA(2)
MU_3 = THETA(3)
MU_4 = THETA(4)

CL1 = EXP(MU_1 + ETA(1))
CL2 = EXP(MU_2 + ETA(1))
V = EXP(MU_3 + ETA(2))
KA = EXP(MU_4 + ETA(3))

IF (MIXNUM.EQ.1) THEN
  CL = CL1
ELSE
  CL = CL2
ENDIF

S2 = V

$MIX
NSPOP = 2
P(1) = THETA(5)
P(2) = 1 - THETA(5)

$ERROR
IPRED = F
W = SQRT(THETA(6)**2 + (THETA(7)*IPRED)**2)
Y = IPRED + W * ERR(1)

$THETA
(0, 1)          ; log CL1
(0, 2)          ; log CL2
(0, 3)          ; log V
(0, 0)          ; log KA
(0.01, 0.5, 0.99) ; P1
(0, 0.1)        ; ADD
(0, 0.1)        ; PROP

$OMEGA
0.09            ; IIV CL
0.09            ; IIV V
0.09            ; IIV KA

$SIGMA
1 FIX

$ESTIMATION METHOD=1 INTER MAXEVAL=9999 PRINT=5 NOABORT

$TABLE ID TIME DV IPRED MIXNUM
       NOPRINT ONEHEADER FILE=sdtab4

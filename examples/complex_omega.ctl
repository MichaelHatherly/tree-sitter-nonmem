$PROB Complex OMEGA structures - BLOCK, SAME, correlations
$INPUT ID TIME DV AMT EVID
$DATA data.csv IGNORE=@

$SUBROUTINES ADVAN4 TRANS4
$PK
; Population parameters
TVCL = THETA(1)
TVV1 = THETA(2)
TVQ = THETA(3)
TVV2 = THETA(4)
TVKA = THETA(5)

; IIV with full correlation matrix
CL = TVCL * EXP(ETA(1))
V1 = TVV1 * EXP(ETA(2))
Q = TVQ * EXP(ETA(3))
V2 = TVV2 * EXP(ETA(4))
KA = TVKA * EXP(ETA(5))

; IOV - occasion variability
IF (OCC.EQ.1) THEN
  IOVCL = ETA(6)
  IOVV1 = ETA(7)
ELSE
  IOVCL = ETA(8)
  IOVV1 = ETA(9)
ENDIF

CL = CL * EXP(IOVCL)
V1 = V1 * EXP(IOVV1)

S1 = V1

$ERROR
IPRED = F
W = SQRT(THETA(6)**2 + (THETA(7)*IPRED)**2)
Y = IPRED + W*ERR(1)

$THETA
(0, 5)    ; CL
(0, 50)   ; V1
(0, 2)    ; Q
(0, 100)  ; V2
(0, 1)    ; KA
(0, 0.1)  ; ADD
(0, 0.2)  ; PROP

; Full 5x5 correlation block for IIV
$OMEGA BLOCK(5)
0.09              ; PPV CL
0.01 0.09         ; PPV V1
0.01 0.01 0.04    ; PPV Q
0.01 0.01 0.01 0.04   ; PPV V2
0.005 0.005 0.005 0.005 0.16  ; PPV KA

; IOV block with SAME for occasions
$OMEGA BLOCK(2)
0.04              ; IOV CL occ 1
0.01 0.04         ; IOV V1 occ 1

$OMEGA BLOCK(2) SAME  ; IOV occ 2

$SIGMA 1 FIX

$EST METHOD=1 INTER MAXEVAL=9999 PRINT=5 NOABORT
$COV PRINT=E UNCONDITIONAL

$TABLE ID TIME IPRED CWRES CL V1 Q V2 KA
       ETA1 ETA2 ETA3 ETA4 ETA5
       FILE=sdtab NOPRINT ONEHEADER

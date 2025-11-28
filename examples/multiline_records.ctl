$PROBLEM Multi-line records with continuation

$INPUT ID
       TIME
       AMT
       DV
       MDV

$DATA ../data/study.csv
      IGNORE=@
      IGNORE=(DV.EQ.-99)

$SUBROUTINES ADVAN4
             TRANS4

$PK
; Assignment split across lines
TVCL = THETA(1) * &
       (WT/70)**THETA(3)

TVV1 = THETA(2) * &
       (WT/70)**THETA(4)

; Complex expression continuation
CL = TVCL * EXP(ETA(1)) * &
     (1 + THETA(5) * (SEX - 1))

V1 = TVV1 * &
     EXP(ETA(2))

Q = THETA(6)
V2 = THETA(7)
K = CL/V1
K12 = Q/V1
K21 = Q/V2
S1 = V1

$ERROR
IPRED = F
W = SQRT(THETA(8)**2 + &
         (THETA(9)*IPRED)**2)
Y = IPRED + W*ERR(1)

$THETA (0,   5)   ; CL
       (0,  50)   ; V1
       0.75 FIX   ; WT-CL
       1 FIX      ; WT-V1
       0          ; SEX-CL
       (0,   2)   ; Q
       (0, 100)   ; V2
       (0, 0.1)   ; ADD
       (0, 0.3)   ; PROP

$OMEGA 0.1
       0.1

$SIGMA 1 FIX

$ESTIMATION METHOD=1
            INTER
            MAXEVAL=9999
            PRINT=5
            NOABORT

$COVARIANCE PRINT=E
            UNCONDITIONAL

$TABLE ID TIME
       IPRED CWRES
       CL V1 Q V2 ETA1 ETA2
       FILE=sdtab
       NOPRINT
       ONEHEADER

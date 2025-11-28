$PROB Edge cases - whitespace, continuations, mixed case
$INP ID TIME AMT DV MDV ; inline comment after variables
$DATA ../data.csv IGNORE=@

$SUB ADVAN2 TRANS2
$PK
; No spaces around equals
TVCL=THETA(1)
TVV=THETA(2)*1.5
; Extra whitespace
CL    =    TVCL    *    EXP(ETA(1))
V  =  TVV  *  EXP(ETA(2))
; Mixed case keywords
If (AMT.GT.0) Then
  F1 = tHeTa(3)
endIf
; Line continuation
K = CL / &
    V
S1=V

$ERR
IPRED=F
; Compound expression no spaces
Y=IPRED*(1+ERR(1))+ERR(2)

; Abbreviated with mixed case
$tHeT 0.5 ; CL
(0,1) ; V
0.8 FIX ; F1
$OME 0.1 0.2
$SIG 0.04 0.01

; Full name caps
$ESTIMATION METHOD=1 MAXEVAL=9999 PRINT=5 NOABORT
$COV PRINT=E

$TAB ID TIME IPRED DV FILE=sdtab NOPRINT ONEHEADER

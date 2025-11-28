$PROB Compact syntax - minimal whitespace
$INP ID TIME DV AMT EVID
$DATA data.csv IGNORE=#

$SUB ADVAN1 TRANS2
$PK
CL=THETA(1)*EXP(ETA(1))
V=THETA(2)*EXP(ETA(2))
S1=V

$ERR
Y=F*(1+ERR(1))

; Bounds without spaces
$THETA (0,0.5,10) (0.01,5,100)
; Single line omega block
$OMEGA BLOCK(2) 0.1 0.01 0.1
$SIGMA 0.04

$EST METH=1 MAX=9999 NOAB PRINT=5
$COV
$TAB ID TIME DV IPRED FILE=tab1

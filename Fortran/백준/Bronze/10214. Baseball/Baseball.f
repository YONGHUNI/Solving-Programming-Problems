READ*,I
DO L=1,I
M=0
O=0
DO N=1,9
READ*,J,K
M=M+J
O=O+K
ENDDO
IF(M>O)PRINT'(A)','Yonsei'
IF(M<O)PRINT'(A)','Korea'
IF(M==O)PRINT'(A)','Draw'
ENDDO
END
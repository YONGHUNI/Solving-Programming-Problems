READ*,I,J,K
DO WHILE(I/=0.AND.J/=0.AND.K/=0.)
IF(MAX(I,J,K)==K) THEN
IF(I*I+J*J==K*K) THEN
PRINT'(A)','right'
ELSE
PRINT'(A)','wrong'
ENDIF
ELSE IF(MAX(I,J,K)==I) THEN
IF(K*K+J*J==I*I) THEN
PRINT'(A)','right'
ELSE
PRINT'(A)','wrong'
ENDIF
ELSE IF(MAX(I,J,K)==J) THEN
IF(I*I+K*K==J*J) THEN
PRINT'(A)','right'
ELSE
PRINT'(A)','wrong'
ENDIF
ENDIF
READ*,I,J,K
ENDDO
END
integer::a(1000)=1001
read*,n,(a(j),j=1,n)
j=0
k=0
do i=1,n        
j=j+minval(a)
a(minloc(a))=1001
k=k+j
enddo
print"(i0)",k
end
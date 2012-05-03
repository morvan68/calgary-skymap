pro test1

st0 = systime(1)
for indx=0,99999L do test = dot0d(dindgen(3,99)+1,dindgen(3,99)+99)
t0 = systime(1)-st0
print,t0
st1 = systime(1)
for indx=0,99999L do test = dot1(dindgen(3,99)+1,dindgen(3,99)+99)
t1 = systime(1)-st1
print,test
print,t1
st2 = systime(1)
for indx=0,99999L do test = dot1d(dindgen(3,99)+1,dindgen(3,99)+99)
test=total(test)
t2 = systime(1)-st2
print,test
print,t2

end
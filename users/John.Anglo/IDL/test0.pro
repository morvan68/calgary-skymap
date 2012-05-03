pro test0

st0 = systime(1)
for indx=0,99999L do test = dot0([1,2,3],[4,5,6])
t0 = systime(1)-st0
print,t0
st1 = systime(1)
for indx=0,99999L do test = dot1([1,2,3],[4,5,6])
t1 = systime(1)-st1
print,t1
print,(t0-t1)/t1
end
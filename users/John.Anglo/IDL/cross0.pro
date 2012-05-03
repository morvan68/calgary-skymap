function cross0,vec1,vec2

test=intarr(3,1)

for indx=0,2 do test[indx,0]=vec1[(indx+1) mod 3]*vec2[(indx+2) mod 3]-vec1[(indx+2) mod 3]*vec2[(indx+1) mod 3]
return, test
end

pro testc

st0 = systime(1)
for indx=0,99999L do test = cross0([1,2,3],[4,5,6])
t0 = systime(1)-st0
print,t0
st1 = systime(1)
for indx=0,99999L do test = crossp([1,2,3],[4,5,6])
t1 = systime(1)-st1
print,t1
print,(t0-t1)/t1

end
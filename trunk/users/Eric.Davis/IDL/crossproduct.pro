function cross,a,b,c,d,e,f,g,h,i
result = a*((e*i)-(f*h)) - b*((d*i)-(f*g)) + c*((d*h)-(e*g))
print, result
end

st= systime(1)
array = indgen (3, 3)
print, array
test=cross([array(0, 0)],[array(1, 0)],[array(2, 0)],[array(0, 1)],[array(1, 1)],[array(2, 1)],[array(0, 2)],[array(1, 2)],[array(2,2)])
print, systime(1)-st
end
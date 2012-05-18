function crossprod,a,b,c,d,e,f
x = b*f-c*e
y = -(a*f-c*d)
z = a*e-b*d
print,"x hat", x 
print,"y hat", y
print,"z hat", z
end

st= systime(1)
test= crossprod([-6475544540],[154545454522],[343],[54],[55785678],[776])
print, systime(1)-st
end
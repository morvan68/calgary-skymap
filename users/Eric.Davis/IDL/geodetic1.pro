function spherical,x,y,z
  r= double((x^2+y^2+z^2)^(0.5))
  phi = double(asin(z/r))
  lamda= double(atan((y/x)))
 return, [r,phi,lamda]
end

st= systime(1)
;defining constants
st = systime(1)
x= 1000.00
y= 1000.00
z= 1000.00

;constants given in manual
a= 6378.137
b= 6356.752
e= 8.181919e-2

coord = spherical([x],[y],[z])

p= (x^2+y^2)^0.5

phi= coord(1)
lamda= coord(2)

I=0

while (1) do begin
    I = I+1
    N= double(a^2/sqrt((a^2)*(cos(phi))^2+(b^2)*(sin(phi))^2))
    h= double(p/(cos(phi))-N)
    phi1 = double(atan((z/p)*(1-(e^2)*(N/(N+h)))^(-1)))
    if phi1 eq phi then break else (phi = atan((z/p)*(1-(e^2)*(N/(N+h)))^(-1)))
endwhile

print, 'phi =', phi, '    h =', h, '    lamda = ',coord(2)




xf = double((N+h)*cos(phi)*cos(lamda))
yf = double((N+h)*cos(phi)*sin(lamda))
zf = double((((b^2)/(a^2))*N+h)*sin(phi))

print, x, y, z
print, xf, yf, zf
print, 'Calculation Time:', systime(1)-st, '    Iterations:', I
end
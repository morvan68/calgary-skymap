;Enter some cartesian coordiantes brah!

x= 1000.00
y= 1000.00
z= 1000.00

st= systime(1)

;some constants from the manual

a= 6378.137
b= 6356.752
e= 8.181919e-2

;Time to establish some variables

p= double((x^2+y^2)/a^2)
q= double(((1-e^2)/a^2)*z^2)
r= double((p+q-e^4)/6)
s= double((e^4)*((p*q)/(4*r^3)))
t= double((1+s+sqrt(s*(2+s)))^(0.33333333))
u= double(r*(1+t+1/t))
v= double(sqrt(u^2+(e^4)*q))
w= double((e^2)*((u+v-q)/(2*v)))
k= double(sqrt(u+v+w^2)-w)
D= double(((k)*sqrt(x^2+y^2))/(k+e^2))

;now for functions:

lamda = double(2*atan((y)/(x+sqrt(x^2+y^2))))
phi = double(2*atan((z)/(D+sqrt(D^2+z^2))))
h = double(((k+e^2-1)/k)*sqrt(d^2+z^2))

print, 'lamda =', lamda
print, 'phi   =', phi
print, 'h     =', h
print, 'calculation time:', systime(1)-st

end 
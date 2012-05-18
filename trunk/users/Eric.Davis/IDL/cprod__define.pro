function cprod::init,a
    if a eq !null then a=[randomn(seed, /long),randomn(seed, /long),randomn(seed, /long),randomn(seed, /long),randomn(seed, /long),randomn(seed, /long)]    
        b=double(a(0))
        c=double(a(1))
        d=double(a(2))
        e=double(a(3))
        f=double(a(4))
        g=double(a(5))
     ; For the first vector   
        r1= (b^2+c^2+d^2)^(0.5)
        phi1 = atan(c,b)
        theta1= acos(d/r1)
        
     ; For the 2nd vector
        r2= (e^2+f^2+g^2)^(0.5)
        phi2= atan(f,e)
        theta2= acos(g/r2)
        
    print, 'Vector 1:', '(',b,',',c,',',d,')'
    print, 'Vector 2:', '(',d,',',f,',',g,')'    
    print, 'Vector 1 in spherical coordinates:', '(',r1,',',phi1,',',theta1,')'
    print, 'Vector 2 in spherical coordinates:', '(',r2,',',phi2,',',theta2,')'    
        x= c*g-d*f
        y= -(b*g-d*e)
        z= b*f-c*e
    print, 'Cross product:', x,' x hat', y,' y hat', z,' z hat'
    print, 'Dot product:', (b*e+c*f+d*g)
return, 1
end
    

    
pro cprod__define
    struct={cprod,b:fltarr(6)}
    return
end
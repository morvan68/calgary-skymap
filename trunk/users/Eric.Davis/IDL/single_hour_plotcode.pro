FUNCTION NORSTAR_MSP_READFILE,filename,COUNT=n_records
  result= NORSTAR_MSP_DAT_READFILE(filename,COUNT=n_records)
RETURN,result
END

 datadir= 'c:/data/norstar/msp/2011/12/23/'
 filelist= FILE_SEARCH(datadir,'*_FSMI_IV[23].dat',COUNT=nfiles)

 n=0          ;# Choose which filter you want to look at
 time=(NORSTAR_MSP_READFILE(filelist[i])).seconds
 data=(NORSTAR_MSP_READFILE(filelist[i])).counts(n)
 
 
 ;# ((size(filelist))(1)-1)
 
data= [data,(NORSTAR_MSP_READFILE(filelist[6])).counts(n)]
 

 ;# GETTING IT ALL INTO ONE ARRAY
 siz=(size(data))(1)
 n=siz/564
 m=564*n
 y=data[0:m-1]
 yarray=reform(y,564,n)

j=0
yalt=yarray(j,*)

while j lt 500 do begin
  j=j+1
  yalt=[yalt,yarray(j,*)]
endwhile

 tvscl,yalt
 print,time(0)
END
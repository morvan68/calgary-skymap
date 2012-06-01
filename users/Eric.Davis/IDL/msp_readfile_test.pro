FUNCTION NORSTAR_MSP_READFILE,filename,COUNT=n_records
  result= NORSTAR_MSP_DAT_READFILE(filename,COUNT=n_records)
RETURN,result
END

 datadir= 'c:/data/norstar/msp/2011/11/06/'
 filelist= FILE_SEARCH(datadir,'*_FSMI_iv[23].dat',COUNT=nfiles)
 data0= NORSTAR_MSP_READFILE(filelist[0])
 ;data1= NORSTAR_MSP_READFILE(filelist[1])
 ;data2= NORSTAR_MSP_READFILE(filelist[2])
 ;data3= NORSTAR_MSP_READFILE(filelist[3])
 ;data4= NORSTAR_MSP_READFILE(filelist[4])
 ;data5= NORSTAR_MSP_READFILE(filelist[5])
 ;data6= NORSTAR_MSP_READFILE(filelist[6])
 ;data7= NORSTAR_MSP_READFILE(filelist[7])
 ;data8= NORSTAR_MSP_READFILE(filelist[8])
 ;data9= NORSTAR_MSP_READFILE(filelist[9])
 ;data10= NORSTAR_MSP_READFILE(filelist[10])
 ;data11= NORSTAR_MSP_READFILE(filelist[11])
 ;data12= NORSTAR_MSP_READFILE(filelist[12])
 ;data13= NORSTAR_MSP_READFILE(filelist[13])
 ;data14= NORSTAR_MSP_READFILE(filelist[14])
 ;# GETTING IT ALL INTO ONE ARRAY
 data=[data0.counts(0),data1.counts(0),data2.counts(0),data3.counts(0),data4.counts(0),data5.counts(0),data6.counts(0),data7.counts(0),data8.counts(0),data9.counts(0),data10.counts(0),data11.counts(0),data12.counts(0),data13.counts(0),data14.counts(0)]
 siz=(size(data))(1)
 n=siz/564
 m=564*n
 y=data[0:m-1]
 yarray=reform(y,564,n)
 tvscl,yarray
END
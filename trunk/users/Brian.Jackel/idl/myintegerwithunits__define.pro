; Subversion $id$

; This is a comment block that some tools will automatically parse.
;+
; This IDL code is a bare-bones example of how to do object inheritance.
;
; Examples:
;
;   test= myIntegerWithUnits(6,'metres')
;   print,test.get()
;-


function myIntegerWithUnits::init,value,units
  self.set,value,units
  return,1  ;# success
end


; No need to define a "cleanup" method, as parent class does all we need.


; A better way might allow setting value and units separately
pro myIntegerWithUnits::set,value,units
  IF (value NE !NULL) then self.value=value
  IF (units NE !NULL) then self.units=units
  return
end


; Returning value and units could be done using a procedure, or a list, or hash, or...
; For simplicity just combine into a string.
function myIntegerWithUnits::get
  return,STRING(self.value,self.units,FORMAT='(I,X,A)')
end


pro myIntegerWithUnits__define

  void= {myIntegerWithUnits      $
          ,inherits myInteger    $
          ,units:''              }
          
  return
end
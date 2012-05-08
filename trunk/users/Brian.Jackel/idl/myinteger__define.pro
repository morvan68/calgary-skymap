; Subversion $id$

; This is a comment block that some tools will automatically parse.
;+
; This IDL code is a bare-bones example of how to create an object.
;
; Examples:
; 
;   test= myInteger(3)
;   print,test.get()    ;# should be 3
;   test.set,9
;   x= test.get()       ;# should be 9
;   obj_destroy,test    ;# call cleanup method
;-


; IDL automatically defines a function with the object class name that just creates the object.  
; Only need to define if we want to add extra functionality.  Included here for completeness.
function myInteger,value
  return,obj_new('myInteger',value)
end


; The "init" method should only be called by "obj_new".
; It does any set-up required before the object can be used eg. reading data files.
; Nothing is required for this example, so just return 1 for success.
function myInteger::init,value
  IF (value NE !NULL) then self.set,value      ;# could just do "self.value=value"
  return,1  ;# success
end


; The "cleanup" method should only be called by "obj_destroy".
; It does any tasks required before the object can be used eg. closing open files.
; Nothing is required for this example, so just return.
pro myInteger::cleanup
  return
end


; By convention, the object state is usually changed using a "set" method.
; The could be either a function or a procedure, your decision.
; For this example we just assign an input value to the object without any checking.
pro myInteger::set,value
  self.value= value          ;# !!NOTE!! really should check input...
  return
end


; By convention, the object state is usually accessed using a "get" method.
; The could be either a function or a procedure, your decision.
; For this example we just return the object value
function myInteger::get
  return,self.value
end


; Define a structure containing all internal information for the object class.
; Place this at the end of the file so that all preceeding methods are compiled.
pro myInteger__define
  void= {myInteger, value:0}
  return 
end
(* lab4/fac0.p *)

var n, f;

proc fac();
begin
  if n = 0 then
    return f;
  else
    f := f*n; n := n-1; 
    return fac()
  end
end;

begin
  n := 10; f := 1;
  print fac(); newline
end.

(*<<
 3628800
>>*)

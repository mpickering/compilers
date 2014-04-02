(* lab4/fac.p *)

proc fac(i);
begin
  if i = 0 then
    return 1
  else
    return i * fac(i-1)
  end
end;

begin
  print fac(10); newline
end.

(*<<
 3628800
>>*)

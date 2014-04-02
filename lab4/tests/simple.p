
proc f(x, y);
  var a;
begin
  a := x-y;
  return a * a
end;

begin
  print f(5,2);
  newline
end.

(*<<
 9
>>*)


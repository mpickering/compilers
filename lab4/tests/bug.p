proc noret();
begin
end;

begin
  print noret()
end.

(*<<
Runtime error: function failed to return a result in module Main
In procedure noret_1
   called from MAIN
>>*)

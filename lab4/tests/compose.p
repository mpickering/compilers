(* lab4/compose.p *)

var p;

proc compose(f, g);
  proc fg(x);
  begin
    return f(g(x))
  end;
begin
  return fg
end;

proc dummy(f, g);
  var a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
begin
  return compose(f, g)
end;

proc add2(x); begin return x+2 end;
proc square(x); begin return x * x end;

begin
  p := dummy(square, add2);
  print p(2); newline
end.

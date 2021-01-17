{ true }                                      x := 2;
{ x = 2 }                                     y := 2;
{ 0 <= x and 0 <= y }                         while true do
{ 0 <= x and 0 <= y }                           if * then
{ 0 <= x and 0 <= y }                             z := Unif (-2,1);
{ 0 <= x and 0 <= y and -2 <= z and z <= 1 }      x := x + z
                                                else
{ 0 <= x and 0 <= y }                             z := Unif (-2,1);
{ 0 <= x and 0 <= y and -2 <= z and z <= 1 }      y := y + z
                                                fi;
{ -2 <= x and -2 <= y }                         refute (x <= 0);
{ 0 <= x and -2 <= y }                          refute (y <= 0)
                                              od

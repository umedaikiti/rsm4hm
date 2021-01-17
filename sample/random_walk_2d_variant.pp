{ true }                           x := 3;
{ x = 3 }                          y := 2;
{ x >= y }                         while true do
{ x >= y }                           if * then
{ x >= y }                             if prob(0.7) then
{ x >= y }                               z := Unif (-2,1);
{ x >= y and -2 <= z and z <= 1 }        x := x + z
                                       else
{ x >= y }                               z := Unif (-2,1);
{ x >= y and -2 <= z and z <= 1 }        y := y + z
                                       fi
                                     else
{ x >= y }                             if prob(0.7) then
{ x >= y }                               z := Unif (-1,2);
{ x >= y and -1 <= z and z <= 2 }        y := y + z
                                       else
{ x >= y }                               z := Unif (-1,2);
{ x >= y and -1 <= z and z <= 2 }        x := x + z
                                       fi
                                     fi;
{ x >= y + 2 }                       refute (x <= y)
                                   od

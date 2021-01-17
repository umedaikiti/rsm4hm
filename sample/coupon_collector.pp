$ 0 <= c0 and c0 <= 1 and 0 <= c1 and c1 <= 1

c0 := 0;
c1 := 0;

while true do
	if prob(0.5) then
		c0 := 1
	else
		c1 := 1
	fi;
	refute (c0 + c1 > 1)
od

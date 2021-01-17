$ 0 <= c0 and c0 <= 1 and 0 <= c1 and c1 <= 1 and 0 <= c2 and c2 <= 1 and 0 <= c3 and c3 <= 1

c0 := 0;
c1 := 0;
c2 := 0;
c3 := 0;

while true do
	if prob(0.5) then
		if prob(0.5) then
			c0 := 1
		else
			c1 := 1
		fi
	else
		if prob(0.5) then
			c2 := 1
		else
			c3 := 1
		fi
	fi;
	refute (c0 + c1 + c2 + c3 > 3)
od

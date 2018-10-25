	subroutine fortpr(x,i)

	integer x(2)

	type *,x(1),x(2),i

	x(1) = -1234567
	x(2) = 99999

	return

	end

	function fortfn(x,i)

	integer x(2)

	fortfn = 456.0

	return

	end

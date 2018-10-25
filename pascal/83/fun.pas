program fun (input, output);
var ch: char;
begin
    repeat
	write ('Yes or No --> ');
	readln (ch);
    until ch in ['y', 'n', 'Y', 'N'];
end.

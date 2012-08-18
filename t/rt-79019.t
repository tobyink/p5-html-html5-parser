use Test::More tests => 1;
use HTML::HTML5::Parser;

my $dom = HTML::HTML5::Parser::->load_html(IO => \*DATA);

is(
	$dom->documentElement->lookupNamespaceURI('fb'),
	'http://ogp.me/ns/fb#',
);

__DATA__
<html>
<html xmlns:fb="http://ogp.me/ns/fb#">
</html>

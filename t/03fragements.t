use Test::More tests => 5;
use HTML::HTML5::Parser;

my $parser = HTML::HTML5::Parser->new;
my $input  = "<b>Hello</b></td></tr> <i>World</i>";
my $NS     = 'xmlns="http://www.w3.org/1999/xhtml"';

can_ok $parser => 'parse_balanced_chunk';

is(
	$parser->parse_balanced_chunk($input, {within=>'div'})->toString,
	"<b $NS>Hello</b> <i $NS>World</i>",
	'within div',
	);

is(
	$parser->parse_balanced_chunk($input, {within=>'td'})->toString,
	"<i $NS>World</i><b $NS>Hello</b> ",
	'within td',
	);

is(
	$parser->parse_balanced_chunk($input, {force_within=>'td'})->toString,
	"<b $NS>Hello</b>",
	'force within td',
	);

my $list = $parser->parse_balanced_chunk($input, {mark_outliers=>1, within=>'td', as=>'list'});
ok(
	$list->get_node(1)->hasAttribute('data-perl-html-html5-parser-outlier'),
	'mark outliers',
	);


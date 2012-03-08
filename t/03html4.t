use Test::More tests => 2;
use HTML::HTML5::Parser;

my $parser = HTML::HTML5::Parser->new;

my $html = <<HTML;
<title>foo</title>
<object></object>
<p>foo</p>
HTML

my $dom_4 = $parser->parse_string('<!doctype html system "html4.dtd">'.$html);
my $dom_5 = $parser->parse_string('<!doctype html>'.$html);

my ($object_4) = $dom_4->getElementsByTagName('object');
my ($object_5) = $dom_5->getElementsByTagName('object');

is($object_4->parentNode->tagName, 'head', 'HTML 4 allows <object> in <head>.');
is($object_5->parentNode->tagName, 'body', 'HTML 5 disallows <object> in <head>.');

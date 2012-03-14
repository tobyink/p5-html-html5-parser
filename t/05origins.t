use Test::More tests => 16;
use HTML::HTML5::Parser;

my $dom = HTML::HTML5::Parser->load_html(string => <<'HTML');
<!doctype html>
<html>
  <title>Test 5: Origins</title>
  <p>
    <b>This</b> <i>is</i>
    <a href="http://example.com/">a</a>
    <tt>test!</tt>
  </p>
</html>
HTML

can_ok 'HTML::HTML5::Parser' => 'source_line'
	or BAIL_OUT('No "source_line" method!!');

my @root = HTML::HTML5::Parser->source_line($dom->documentElement);
is($root[0], 2, 'root element has correct line number');
is($root[1], 1, 'root element has correct col number');

my @head = HTML::HTML5::Parser->source_line($dom->getElementsByTagName('head')->get_node(1));
ok(defined $head[0], 'head element has a line number');
ok(defined $head[1], 'head element has a col number');

my @para = HTML::HTML5::Parser->source_line($dom->getElementsByTagName('p')->get_node(1));
is($para[0], 4, 'p element has correct line number');
is($para[1], 3, 'p element has correct col number');

my $para = HTML::HTML5::Parser->source_line($dom->getElementsByTagName('p')->get_node(1));
is($para, 4, 'p element has correct line number (scalar context)');

my @b = HTML::HTML5::Parser->source_line($dom->getElementsByTagName('b')->get_node(1));
is($b[0], 5, 'b element has correct line number');
is($b[1], 5, 'b element has correct col number');

my @i = HTML::HTML5::Parser->source_line($dom->getElementsByTagName('i')->get_node(1));
is($i[0], 5, 'i element has correct line number');
is($i[1], 17, 'i element has correct col number');

my @a = HTML::HTML5::Parser->source_line($dom->getElementsByTagName('a')->get_node(1));
is($a[0], 6, 'a element has correct line number');
is($a[1], 5, 'a element has correct col number');

my @href = HTML::HTML5::Parser->source_line($dom->getElementsByTagName('a')->get_node(1)->getAttributeNode('href'));
is($href[0], 6, 'href attribute has correct line number');
is($href[1], 8, 'href attribute has correct col number');

use Test::More tests => 2;
use HTML::HTML5::Parser;
use Encode qw(decode_utf8);
use Devel::Peek;

subtest 'U+2193 DOWNWARDS ARROW' => sub {
	my $filename	= 't/data/rt-96399-1.html';
	my $parser = HTML::HTML5::Parser->new;
	my $doc	= $parser->parse_file($filename);
	is($parser->charset($doc), 'utf-8', 'recognized encoding as utf-8');
	like(decode_utf8($doc->toString()), qr/\x{2193}/, 'encoding properly round-trips U+2193 DOWNWARDS ARROW');
};

subtest 'U+00E9 LATIN SMALL LETTER E WITH ACUTE' => sub {
	my $filename	= 't/data/rt-96399-2.html';
	my $parser = HTML::HTML5::Parser->new;
	my $doc	= $parser->parse_file($filename);
	is($parser->charset($doc), 'utf-8', 'recognized encoding as utf-8');
	like(decode_utf8($doc->toString()), qr/\x{00E9}/, 'encoding properly round-trips U+00E9 DOWNWARDS ARROW');
};


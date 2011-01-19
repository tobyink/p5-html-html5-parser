#!/usr/bin/perl

use lib "lib";
use Data::Dumper;
use HTML::HTML5::Parser;
use HTML::HTML5::Writer;
use XML::LibXML::Debugging;

my $p = HTML::HTML5::Parser->new;
my $h = join '', <>;

if ($ENV{HTML_OUTPUT} =~ /hash/i)
{
	print Dumper($p->parse_string($h)->toDebuggingHash);
}
elsif ($ENV{HTML_OUTPUT} =~ /clark/i)
{
	print $p->parse_string($h)->toClarkML;
}
elsif ($ENV{HTML_OUTPUT} =~ /html/i)
{
	print HTML::HTML5::Writer->new->document($p->parse_string($h));
}
else
{
	print $p->parse_string($h)->toString;
}

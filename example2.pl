#!/usr/bin/perl

use lib "lib";
use Data::Dumper;
use HTML::HTML5::Parser;
use HTML::HTML5::Writer;
use Scalar::Util qw[refaddr];
use XML::LibXML::Debugging;

my $p = HTML::HTML5::Parser->new;
my $h = join '', <>;

my $dom = $p->parse_string($h);

@heads = $dom->getElementsByTagName('head');
print ${$heads[0]}."\n";

@heads = $dom->getElementsByTagName('head');
print ${$heads[0]}."\n";

@heads = $dom->getElementsByTagName('head');
print ${$heads[0]}."\n";

print $p->compat_mode($dom) . "\n";
print $p->dtd_public_id($dom) . "\n";
print $p->dtd_system_id($dom) . "\n";

print (join ';', $p->source_line($heads[0])) . "\n";

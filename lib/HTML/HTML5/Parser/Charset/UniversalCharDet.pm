package HTML::HTML5::Parser::Charset::UniversalCharDet;

use strict;
use HTML::Encoding qw(encoding_from_first_chars encoding_from_html_document);

our $VERSION='0.110';
our $DEBUG;

sub _detect {
	my $d = encoding_from_html_document($_[0], 'xhtml'=>0);
	return {encoding=>$d} if $d;
	$d = encoding_from_first_chars($_[0]);
	return {encoding=>$d} if $d;
	return {};
}

sub detect_byte_string ($$) {
  my $de;
  eval {
    $de = _detect ($_[1]);
    1;
  } or do {
    warn $@ unless $DEBUG;
    die $@ if $DEBUG;
  };
  if (defined $de and defined $de->{encoding}) {
    return lc $de->{encoding};
  } else {
    return undef;
  }
} # detect_byte_string

#Copyright 2007-2011 Wakaba <w@suika.fam.cx>
#Copyright 2009-2012 Toby Inkster <tobyink@cpan.org>
#
#This library is free software; you can redistribute it
#and/or modify it under the same terms as Perl itself.

1;

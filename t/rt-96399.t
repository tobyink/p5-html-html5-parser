#!/usr/bin/env perl

use strict;
use Test::More;
use_ok('HTML::HTML5::Parser');

use utf8;                            # for the characters in the script.
use open ':encoding(UTF-8)';         # for the file arguments.
binmode STDIN, ':encoding(UTF-8)';   # for stdin.
binmode STDOUT, ':encoding(UTF-8)';  # for stdout.

@ARGV == 1 or die "Usage: $0 <file.html>\n";

my $parser = HTML::HTML5::Parser->new;
my $doc = $parser->parse_file($ARGV[0]);
print "Charset: '", $parser->charset($doc), "'\n";
print $doc->toString();


my $dom = HTML::HTML5::Parser::->load_html(IO => \*DATA);

is(
	$dom->documentElement->lookupNamespaceURI('fb'),
	'http://ogp.me/ns/fb#',
);

=head1 PURPOSE

Check that certain UTF8 characters aren't encoded or decoded too much.

=head1 SEE ALSO

L<https://bugs.debian.org/750946> and
L<https://rt.cpan.org/Public/Bug/Display.html?id=96399>.

=head1 AUTHOR

Vincent Lefevre E<lt>vincent@vinc17.netE<gt>,
Kjetil Kjernsmo E<lt>kjetil@kjernsmo.netE<gt>.

=head1 COPYRIGHT AND LICENCE

Copyright (C) 2017 by the authors.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

done_testing();

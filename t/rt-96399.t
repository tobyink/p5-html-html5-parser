#!/usr/bin/env perl

use strict;
use Test::More;
use_ok('HTML::HTML5::Parser');
use FindBin qw($Bin);

use utf8;                            # for the characters in the script.
use open ':encoding(UTF-8)';         # for the file arguments.
binmode STDIN, ':encoding(UTF-8)';   # for stdin.
binmode STDOUT, ':encoding(UTF-8)';  # for stdout.

my %chars = ('arrow.html'  => qr/\N{U+2193}/,
				 'eacute.html' => qr/\N{U+00E9}/,
				);

my @files = glob($Bin . "/data/*.html");

foreach my $file (@files) {
	tests($file);
}

sub tests {
	my $file = shift;
	my $parser = HTML::HTML5::Parser->new;
	my $doc = $parser->parse_file($file);
	my ($key) = $file =~ m|/([^/]+?)$|;
	is($parser->charset($doc), 'utf-8', "Correct charset in $key example");
	like($doc->toString, $chars{$key}, 'Unicode char found in string for ' . $key);
	like($doc->toString, qr/title/, 'Word title found in string for' . $key);
}


=head1 PURPOSE

Check that certain UTF8 characters aren't encoded or decoded too much.

=head1 SEE ALSO

L<https://bugs.debian.org/750946> and
L<https://rt.cpan.org/Public/Bug/Display.html?id=96399>.

=head1 AUTHOR

Vincent Lefevre E<lt>vincent@vinc17.netE<gt>,
Kjetil Kjernsmo E<lt>kjetilk@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

Copyright (C) 2017 by the authors.

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself.

=cut

done_testing();

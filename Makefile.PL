use strict;
use warnings;

use inc::Module::Install;

my $dist = 'HTML-HTML5-Parser';
my $fn   = "lib/$dist.pm"; $fn =~ s#-#/#g;

name                $dist;
perl_version_from   $fn;
version_from        $fn;
abstract_from       $fn;
readme_from         $fn;
author              'Toby Inkster <tobyink@cpan.org>';
license             'perl';

requires            'Error'              => 0;
requires            'HTML::Encoding'     => '0.55';
requires            'LWP::UserAgent'     => 0;
test_requires       'Test::More'         => '0.61';
requires            'XML::LibXML'        => '1.60';

resources(
	'homepage'   => "http://search.cpan.org/dist/$dist/",
	'repository' => "http://goddamn.co.uk/viewvc/perlmods/$dist/",
	'bugtracker' => "http://rt.cpan.org/Dist/Display.html?Queue=$dist",
	);

write_doap_changes;
write_doap_changes_xml;

install_script 'html2xhtml';

include 'Test::Signature';
auto_install;
WriteAll(
	'meta' => 1,
	'sign' => 1,
	);


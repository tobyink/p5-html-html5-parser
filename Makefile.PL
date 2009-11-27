use strict;
use warnings;

use inc::Module::Install;

license             'perl';

name                'HTML-HTML5-Parser';
author              'Toby Inkster <tobyink@cpan.org>';
perl_version_from   'lib/HTML/HTML5/Parser.pm';
version_from        'lib/HTML/HTML5/Parser.pm';
abstract_from       'lib/HTML/HTML5/Parser.pm';

test_requires       'Test::More' => '0.61';

requires            'LWP::UserAgent' => 0;
requires            'XML::LibXML'    => 0;
requires            'Error'          => 0;

auto_install;

sign;

WriteAll;

use Pod::Text;
my $parser = Pod::Text->new (sentence => 0, width => 78);
$parser->parse_from_file('lib/HTML/HTML5/Parser.pm', 'README');

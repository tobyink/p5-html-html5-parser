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
requires            'HTML::Encoding' => '0.55';

auto_set_homepage;
auto_set_bugtracker;

install_script 'html2xhtml';

include 'Test::Signature';
auto_install;
WriteAll(
	'meta' => 1,
	'sign' => 1,
	);


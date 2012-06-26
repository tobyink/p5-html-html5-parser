use Test::More;
use HTML::HTML5::Parser::UA;

eval { require LWP::UserAgent; 1; }
	or plan skip_all => "Could not use LWP::UserAgent: $@";

$HTML::HTML5::Parser::UA::NO_LWP = '';

do '07ua.t' if -s '07ua.t';
do 't/07ua.t' if -s 't/07ua.t';


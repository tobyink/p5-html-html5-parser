use Test::More;
use Test::Pod::Coverage;

my @modules = qw(HTML::HTML5::Parser HTML::HTML5::Parser::Error);
pod_coverage_ok($_, "$_ is covered")
	foreach @modules;
done_testing(scalar @modules);


use Test::More tests => 3;
BEGIN { use_ok('HTML::HTML5::Parser') };

my $parser = new_ok 'HTML::HTML5::Parser';
can_ok $parser, qw/
	parse_file parse_html_file
	parse_fh parse_html_fh
	parse_string parse_html_string
	parse_balanced_chunk
	load_xml load_html
	error_handler
	errors
	compat_mode
	dtd_public_id
	dtd_system_id
	source_line
	/;
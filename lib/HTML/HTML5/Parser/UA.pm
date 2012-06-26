package HTML::HTML5::Parser::UA;

use 5.008001;
use strict;

use Encode qw(decode);
use HTTP::Tiny;
use URI::file;

our $NO_LWP = '0';

sub get
{
	my ($class, $uri, $ua) = @_;

	if (ref $ua and $ua->isa('HTTP::Tiny') and $uri =~ /^https?:/i)
		{ goto \&_get_tiny }
	if (ref $ua and $ua->isa('LWP::UserAgent'))
		{ goto \&_get_lwp }
	if (UNIVERSAL::can('LWP::UserAgent', 'can') and not $NO_LWP)
		{ goto \&_get_lwp }
	if ($uri =~ /^file:/i)
		{ goto \&_get_fs }
	
	goto \&_get_tiny;
}

sub _get_lwp
{
	require LWP::UserAgent;
	
	my ($class, $uri, $ua) = @_;

	$ua ||= LWP::UserAgent->new(
		agent => sprintf(
			"%s/%s ",
			'HTML::HTML5::Parser',
			HTML::HTML5::Parser->VERSION,
		),
		default_headers => HTTP::Headers->new(
			'Accept' => join q(, ) => qw(
				text/html
				application/xhtml+xml;q=0.9
				application/xml;q=0.1
				text/xml;q=0.1
			)
		),
		parse_head => 0,
	);
	
	my $response = $ua->get($uri);
	
	my $h = $response->headers;
	my %header_hash =
		map { lc($_) => $h->header($_); }
		$h->header_field_names;
	
	return +{
		success  => $response->is_success,
		status   => $response->code,
		reason   => $response->message,
		headers  => \%header_hash,
		content  => $response->content,
		decoded_content => $response->decoded_content,
	};
}

sub _get_tiny
{
	my ($class, $uri, $ua) = @_;
	
	$ua ||= HTTP::Tiny->new(
		agent => sprintf("%s/%s", 'HTML::HTML5::Parser', HTML::HTML5::Parser->VERSION),
		default_headers => +{
			'Accept' => join(q(, ) => qw(
				text/html
				application/xhtml+xml;q=0.9
				application/xml;q=0.1
				text/xml;q=0.1
			)),
		},
	);
	
	my $response = $ua->get($uri);
	
	if ($response->{headers}{'content-type'} =~ /charset=(\S+)/)
	{
		(my $encoding = $1) =~ s/["']//g;
		$response->{decoded_content} = eval {
			decode($encoding, $response->{content})
		};
	}
	
	$response->{decoded_content} //= $response->{content};
	return $response;
}

sub _get_fs
{
	my $class = shift;
	my ($uri) = map { ref() ? $_ : URI->new($_) } @_;
	my $file  = $uri->file;

	my ($status, $reason, $content, $content_type) = do {
		if (not -e $file)
			{ (404 => 'Not Found', 'File not found.', 'text/plain') }
		elsif (not -r $file)
			{ (403 => 'Forbidden', 'File not readable by effective guid.', 'text/plain') }
		else
			{ (200 => 'OK') }
	};
	
	$content //= do {
		if (open my $fh, '<', $file)
			{ local $/ = <$fh> }
		else
			{ $status = 418; $reason = "I'm a teapot"; $content_type = 'text/plain'; $! }
	};
	
	$content_type //= 'text/xml' if $file =~ /\.xml$/i;
	$content_type //= 'application/xhtml+xml' if $file =~ /\.xht(ml)?$/i;
	$content_type //= 'text/html' if $file =~ /\.html?$/i;
	$content_type //= 'application/octet-stream';
	
	return +{
		success  => ($status == 200),
		status   => $status,
		reason   => $reason,
		headers  => +{
			'content-type'   => $content_type,
			'content-length' => length($content),
		},
		content  => $content,
		decoded_content => $content,
	};
}


1;
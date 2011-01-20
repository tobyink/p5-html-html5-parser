package HTML::HTML5::Parser;

=head1 NAME

HTML::HTML5::Parser - parse HTML reliably

=head1 SYNOPSIS

  use HTML::HTML5::Parser;
  
  my $parser = HTML::HTML5::Parser->new;
  my $doc    = $parser->parse_string(<<'EOT');
  <!doctype html>
  <title>Foo</title>
  <p><b><i>Foo</b> bar</i>.
  <p>Baz</br>Quux.
  EOT
  
  my $fdoc   = $parser->parse_file( $html_file_name );
  my $fhdoc  = $parser->parse_fh( $html_file_handle );

=cut

use 5.008001;
use strict;
use warnings;

our $AUTOLOAD;
our $VERSION = '0.102';

use Carp;
use HTML::HTML5::Parser::TagSoupParser;
use LWP::UserAgent;
use URI::file;
use XML::LibXML;

=head1 DESCRIPTION

This library is substantially the same as the non-CPAN module Whatpm::HTML.
Changes include:

=over 8

=item * Provides an XML::LibXML-like DOM interface. If you usually use XML::LibXML's DOM parser, this should be a drop-in solution for tag soup HTML.

=item * Constructs an XML::LibXML::Document as the result of parsing.

=item * Via bundling and modifications, removed external dependencies on non-CPAN packages.

=back

=head2 Constructor

=over 8

=item C<new>

  $parser = HTML::HTML5::Parser->new;

The constructor does not do anything interesting.

=back

=cut

sub new
{
	my $class = shift;
	my $self  = bless {
		'errors' => [],
		}, $class;
	
	return $self;
}

=head2 XML::LibXML-Compatible Methods

=over 8

=item C<parse_file>, C<parse_html_file>

  $doc = $parser->parse_file( $html_file_name [,\%opts] );
  
This function parses an HTML document from a file or network;
C<$html_file_name> can be either a filename or an URL.

Options include 'encoding' to indicate file encoding (e.g.
'utf-8') and 'user_agent' which should be a blessed C<LWP::UserAgent>
object to be used when retrieving URLs.

If requesting a URL and the response Content-Type header indicates
an XML-based media type (such as XHTML), XML::LibXML::Parser
will be used automatically (instead of the tag soup parser). The XML
parser can be told to use a DTD catalogue by setting the option
'xml_catalogue' to the filename of the catalogue.

HTML (tag soup) parsing can be forced using the option 'force_html', even
when an XML media type is returned. If an options hashref was passed,
parse_file will set $options->{'parser_used'} to the name of the class used
to parse the URL, to allow the calling code to double-check which parser
was used afterwards.

If an options hashref was passed, parse_file will set $options->{'response'}
to the HTTP::Response object obtained by retrieving the URI.

=cut

sub parse_file
{
	my $self   = shift;
	my $file   = shift;
	my $opts   = shift || {};
	
	unless (UNIVERSAL::isa($file, 'URI'))
	{
		if ($file =~ /^[a-z0-9_\.-]+:\S+$/i)
		{
			$file = URI->new($file);
		}
		else
		{
			$file = URI::file->new($file);
		}
	}
	
	my $ua;
	if ($opts->{'user_agent'})
	{
		$ua = $opts->{'user_agent'};
	}
	else
	{
		$ua = LWP::UserAgent->new;
		$ua->agent("HTML::HTML5::Parser/".$VERSION." ");
		$ua->default_header('Accept' => 'text/html, '
			.'application/xhtml+xml;q=0.9, '
			.'application/xml;q=0.1, '
			.'text/xml;q-0.1');
		$ua->parse_head(0);
	}
	
	my $response = $ua->get($file);
	croak "HTTP response code was not 200 OK. (Set \$opts{ignore_http_response_code} to ignore this error.)"
		unless ($response->code == 200 || $opts->{'ignore_http_response_code'});
	
	my $content = $response->decoded_content;
	my $c_type  = $response->headers->content_type;
	my $charset = $response->headers->content_type_charset;
	
	$opts->{'response'} = $response;
	
	if ($c_type =~ /xml/i && !$opts->{'force_html'})
	{
		$opts->{'parser_used'} = 'XML::LibXML::Parser';
		my $xml_parser = XML::LibXML->new;
		$xml_parser->validation(0);
		$xml_parser->recover(2);
		$xml_parser->base_uri($response->base);
		$xml_parser->load_catalog($opts->{'xml_catalogue'})
			if -r $opts->{'xml_catalogue'};
		return $xml_parser->parse_string($content);
	}
	
	if (!defined $opts->{'encoding'})
	{
		$opts->{'encoding'} = $charset;
	}
	
	return $self->parse_string($content, $opts);
}
*parse_html_file = \&parse_file;

=item C<parse_fh>, C<parse_html_fh>

  $doc = $parser->parse_fh( $io_fh [,\%opts] );
  
C<parse_fh()> parses a IOREF or a subclass of C<IO::Handle>.

Options include 'encoding' to indicate file encoding (e.g.
'utf-8').

=cut

sub parse_fh
{
	my $self   = shift;
	my $handle = shift;
	my $opts   = shift || {};
	
	my $string = '';
	while (<$handle>)
	{
		$string .= $_;
	}
	
	return $self->parse_string($string, $opts);
}
*parse_html_fh = \&parse_fh;

=item C<parse_string>, C<parse_html_string>

  $doc = $parser->parse_string( $html_string [,\%opts] );

This function is similar to C<parse_fh()>, but it parses an HTML
document that is available as a single string in memory.

Options include 'encoding' to indicate file encoding (e.g.
'utf-8').

=cut

sub parse_string
{
	my $self = shift;
	my $text = shift;
	my $opts = shift || {};
	
	$opts->{'parser_used'} = 'HTML::HTML5::Parser';
	my $dom = XML::LibXML::Document->createDocument;
	
	if (defined $opts->{'encoding'} || 1)
	{
		HTML::HTML5::Parser::TagSoupParser->parse_byte_string($opts->{'encoding'}, $text, $dom, sub{
			my $err = \@_;
			push @{$self->{'errors'}}, $err;
			});
	}
	else
	{
		HTML::HTML5::Parser::TagSoupParser->parse_char_string($text, $dom, sub{
			my $err = \@_;
			push @{$self->{'errors'}}, $err;
			});
	}
	
	return $dom;
}
*parse_html_string = \&parse_string;

=back

The push parser and SAX-based parser are not supported. Trying
to change an option (such as recover_silently) will make
HTML::HTML5::Parser carp a warning. (But you can inspect the
options.)

=cut

sub AUTOLOAD
{
	my $self = shift;
	my $func = $AUTOLOAD;
	$func =~ s/.*://;
	
	# LibXML Push Parser.
	if ($func =~ /^( parse_chunk | start_push | push | finish_push )$/xi)
	{
		croak "Push parser ($func) not implemented by HTML::HTML5::Parser.";
	}
	
	# Misc LibXML functions with no compatible interface provided.
	if ($func =~ /^( parse_balanced_chunk | parse_xml_chunk |
		process_?xincludes | get_last_error )$/xi)
	{
		croak "$func not implemented by HTML::HTML5::Parser.";
	}
	
	# Fixed options which are true.
	if ($func =~ /^( recover | recover_silently | expand_entities |
		keep_blanks | no_network )$/xi)
	{
		my $set = shift;
		if ((!$set) && defined $set)
		{
			carp "Option $func cannot be switched off.";
		}
		return 1;
	}

	# Fixed options which are false.
	if ($func =~ /^( validation | pedantic_parser | line_numbers 
		load_ext_dtd | complete_attributes | expand_xinclude |
		load_catalog | base_uri | gdome_dom | clean_namespaces )$/xi)
	{
		my $set = shift;
		if (($set) && defined $set)
		{
			carp "Option $func cannot be switched on.";
		}
		return 0;
	}

}

=head2 Additional Methods

The module provides a few additional methods to obtain additional,
non-DOM data from DOM nodes.

=over 8

=item C<compat_mode>

  $mode = $parser->compat_mode( $doc );
  
Returns 'quirks', 'limited quirks' or undef (standards mode).

=cut

sub compat_mode
{
	my $self = shift;
	my $node = shift;
	
	return HTML::HTML5::Parser::TagSoupParser::DATA($node)->{'manakai_compat_mode'};
}

=item C<dtd_public_id>

  $pubid = $parser->dtd_public_id( $doc );
  
For an XML::LibXML::Document which has been returned by
HTML::HTML5::Parser, using this method will tell you the
Public Identifier of the DTD used (if any).

=cut

sub dtd_public_id
{
	my $self = shift;
	my $node = shift;
	
	return HTML::HTML5::Parser::TagSoupParser::DATA($node)->{'DTD_PUBLIC_ID'};
}

=item C<dtd_system_id>

  $sysid = $parser->dtd_system_id( $doc );
  
For an XML::LibXML::Document which has been returned by
HTML::HTML5::Parser, using this method will tell you the
System Identifier of the DTD used (if any).

=cut

sub dtd_system_id
{
	my $self = shift;
	my $node = shift;
	
	return HTML::HTML5::Parser::TagSoupParser::DATA($node)->{'DTD_SYSTEM_ID'};
}

=item C<source_line>

  ($line, $col) = $parser->source_line( $node );
  $line = $parser->source_line( $node );
  
In scalar context, C<source_line> returns the line number of the
source code that started a particular node (element, attribute or
comment).

In list context, returns a line/column pair. (Tab characters count as
one column, not eight.)

=cut

sub source_line
{
	my $self = shift;
	my $node = shift;
	
	my $line = HTML::HTML5::Parser::TagSoupParser::DATA($node)->{'manakai_source_line'};
	
	if (wantarray)
	{
		my $col = HTML::HTML5::Parser::TagSoupParser::DATA($node)->{'manakai_source_column'};
		return ($line, $col);
	}
	else
	{
		return HTML::HTML5::Parser::TagSoupParser::DATA($node)->{'manakai_source_line'};
	}
}


=back

=cut

1;
__END__

=head1 SEE ALSO

L<http://suika.fam.cx/www/markup/html/whatpm/Whatpm/HTML.html>

=head1 AUTHOR

Toby Inkster, E<lt>tobyink@cpan.orgE<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2007-2010 by Wakaba

Copyright (C) 2009-2010 by Toby Inkster

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.1 or,
at your option, any later version of Perl 5 you may have available.


=cut

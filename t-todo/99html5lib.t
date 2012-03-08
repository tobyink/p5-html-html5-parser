# Tests from html5lib's testdata/tree-construction.

use Test::More;
use HTML::HTML5::Parser;

BEGIN {
	eval { require Mo; 1 }
		or plan skip_all => 'Need Mo OO!'
}

{
	package XML::LibXML::Document;
	sub pythonDebug
	{
		my $self = shift;
		my $return;
		
		my $element = HTML::HTML5::Parser->dtd_element($self);
		my $public  = HTML::HTML5::Parser->dtd_public_id($self);
		my $system  = HTML::HTML5::Parser->dtd_system_id($self);
		
		if (defined $element)
		{
			$return = sprintf(
				"| <!DOCTYPE %s%s%s>\n",
				$element,
				(($public||$system) ? " \"$public\"" : ""),
				(($public||$system) ? " \"$system\"" : ""),
				);
		}
		
		$return .= $_->pythonDebug(q{| }) foreach $self->childNodes;
		return $return;
	}
}

{
	package XML::LibXML::DocumentFragment;
	sub pythonDebug
	{
		my ($self, $indent) = @_;
		
		$self->normalize;
		
		my $return;
		foreach ($self->childNodes)
		{
			$return .= $_->pythonDebug($indent . q{| });
		}		
		return $return;
	}
}

{
	package XML::LibXML::Element;
	sub pythonDebug
	{
		my ($self, $indent) = @_;
		
		$self->normalize;
		
		my $nsbit  = '';
		$nsbit = 'svg ' if $self->namespaceURI =~ /svg/i;
		$nsbit = 'math ' if $self->namespaceURI =~ /math/i;
		my $return = sprintf("%s<%s%s>\n", $indent, $nsbit, $self->localname);
		
		my @attribs = 
			sort { $a->localname cmp $b->localname }
			grep { not $_->isa('XML::LibXML::Namespace') }
			$self->attributes;
		foreach (@attribs)
		{
			$return .= $_->pythonDebug($indent . q{  });
		}
		
		if ($self->localname eq 'noscript')
		{
			my $innerHTML = join q{}, map { $_->toString } $self->childNodes;
			$return .= $indent . q{  "} . $innerHTML . "\"\n";
		}
		else
		{
			foreach ($self->childNodes)
			{
				$return .= $_->pythonDebug($indent . q{  });
			}
		}
		
		return $return;
	}
}

{
	package XML::LibXML::Text;
	sub pythonDebug
	{
		my ($self, $indent) = @_;
		return sprintf("%s\"%s\"\n", $indent, $self->data);
	}
}

{
	package XML::LibXML::Comment;
	sub pythonDebug
	{
		my ($self, $indent) = @_;
		return sprintf("%s<!-- %s -->\n", $indent, $self->data);
	}
}

{
	package XML::LibXML::Attr;
	sub pythonDebug
	{
		my ($self, $indent) = @_;
		return sprintf("%s%s %s=\"%s\"\n", $indent, split(/:/, $self->nodeName), $self->value)
			if $self->namespaceURI && $self->nodeName=~/:/;
		return sprintf("%s%s=\"%s\"\n", $indent, $self->localname, $self->value);
	}
}

{
	package Local::HTML5Lib::Test;
	
	use Mo;
	
	has test_file         => (isa => 'Local::HTML5Lib::TestFile');
	has test_number       => (isa => 'Num');
	has data              => (isa => 'Str');
	has errors            => (isa => 'Str');
	has document          => (isa => 'Str');	
	has document_fragment => (isa => 'Str');
	
	sub test_id
	{
		my $self = shift;
		sprintf('%s:%s', $self->test_file->filename, $self->test_number);
	}
	
	sub dom
	{
		my ($self) = @_;
		
		if ($self->document_fragment)
		{
			return HTML::HTML5::Parser->new->parse_balanced_chunk(
				$self->data,
				{within => $self->document_fragment},
				);
		}
		
		return eval {
			HTML::HTML5::Parser->new->parse_string($self->data);
		} || do {
			my $e   = $@;
			my $xml = XML::LibXML::Document->new('1.0', 'utf-8');
			$xml->setDocumentElement( $xml->createElementNS('http://www.w3.org/1999/xhtml', 'html') );
			$xml->documentElement->appendText("ERROR: $e");
			$xml;
		}
	}
	
	sub run
	{
		my ($self) = @_;
		my $expected = $self->document."\n";
		my $got      = $self->dom->pythonDebug;
		
		local $Test::Builder::Level = $Test::Builder::Level + 1;
		
		SKIP: {
			my $excuse = $::SKIP->{ $self->test_id };
			Test::More::skip($excuse, 1) if defined $excuse;
			
			if ($got eq $expected)
			{
				Test::More::pass("DATA: ".$self->data);
				return 1;
			}
			else
			{
				Test::More::fail("DATA: ".$self->data);
				Test::More::diag("ID: ".$self->test_id);
				Test::More::diag("GOT:\n$got");
				Test::More::diag("EXPECTED:\n$expected");
				return 0;
			}
		}
	}
}
	
{
	package Local::HTML5Lib::TestFile;
	
	use Mo;
	
	has filename   => (isa => 'Str');
	has tests      => (isa => 'ArrayRef');
	has last_score => (isa => 'Num');
	
	sub read_file
	{
		my ($class, $filename) = @_;
		
		my $self = $class->new(
			filename  => $filename,
			);
			
		my @tests;
		
		open my $fh, '<', $filename;
		push @tests, (my $current_test = { test_file=>$self });
		my $current_key;
		my @lines = <$fh>; # sometimes we need to peek at the next line;
		while (defined ($_ = shift @lines))
		{
			no warnings;
			
			if (!/\S/ and (!defined $lines[0] or $lines[0]=~ /^\#data/))
			{
				$current_test->{test_number} = @tests;
				chomp $current_test->{$current_key} if defined $current_key;
				$current_test = { test_file=>$self };
				$current_key  = undef;
				push @tests, $current_test;
				next;
			}
			
			if (/^\#(.+)/)
			{
				chomp $current_test->{$current_key} if defined $current_key;
				($current_key = $1) =~ s/-/_/g;
				next;
			}
			
			$current_test->{$current_key} .= $_;
		}

		chomp $current_test->{$current_key};
		
		$self->tests([ map { Local::HTML5Lib::Test->new(%$_) } @tests]);
		return $self;
	}

	sub run
	{
		local $Test::Builder::Level = $Test::Builder::Level + 1;

		my $self = shift;
		$self->{last_score} = 0;
		Test::More::subtest(
			sprintf("Test file: %s", $self->filename),
			sub {	$self->{last_score} += ($_->run ? 1 : 0) for @{ $self->tests } },
			);
	}
}

package main;

our $SKIP = {
	't-todo/tree-construction/tests26.dat:10'
		=> 'requires HTML parser to construct a DOM tree which is illegal in libxml (bad attribute name)',
	't-todo/tree-construction/webkit01.dat:14'
		=> 'requires HTML parser to construct a DOM tree which is illegal in libxml (bad element name)',
	't-todo/tree-construction/webkit01.dat:42'
		=> 'requires HTML parser to construct a DOM tree which is illegal in libxml (bad attribute name)',
	't-todo/tree-construction/webkit02.dat:4'
		=> 'I basically just disagree with this test.',
	};

my $count;
my @fails;
my @passes;

unless (@ARGV)
{
	@ARGV = <t-todo/tree-construction/*.dat>;
}

while (my $f = shift)
{
	my $F = Local::HTML5Lib::TestFile->read_file($f);
	if ($F->run)
	{
		push @passes, $F;
	}
	else
	{
		push @fails, $F;
	}
	
	++$count;
}

if (@fails)
{
	diag "FAILED:";
	diag sprintf("  %s [%d/%d]", $_->filename, $_->last_score, scalar(@{$_->tests}))
		for @fails;
}

if (@passes)
{
	diag "PASSED:";
	diag sprintf("  %s [%d/%d]", $_->filename, $_->last_score, scalar(@{$_->tests}))
		for @passes;
}

done_testing($count);

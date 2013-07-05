#!perl

## skip Test::Tabs
use strict;
use warnings FATAL => 'all';

use Test::More
    # tests => 1;
    skip_all => 'Enable this test to hunt for memory leaks in the parser.';

use Scalar::Util qw(blessed refaddr);

SKIP: {
    # Thanks to MST for this method - DORIAN
    eval {
        use Devel::Gladiator qw(walk_arena arena_ref_counts arena_table);
        use Devel::FindRef;
    };
    skip 'Install Devel::Gladiator and Devel::FindRef for these tests.', 1
        if $@;

    my $data = do { local $/; <DATA> };

    my %seen;
    my $yes = 1;

    #diag(arena_table);

    require HTML::HTML5::Parser;

    {
        my $p = HTML::HTML5::Parser->new(no_cache => 1);
        #diag(arena_table);
        my $doc = $p->parse_string($data);
        undef $p;
    }

    for my $sv (@{walk_arena()}) {
        # I have to admit this is clever.
        $seen{refaddr $sv} = \$yes;
    }

    #diag(arena_table);
    my $a = arena_ref_counts;

    {
        my $p = HTML::HTML5::Parser->new(no_cache => 1);
        #diag(arena_table);
        my $doc = $p->parse_string($data);
        undef $p;
    }

    my $b = arena_ref_counts;

    # DOUBLE XXX this whole strategy for leak testing is busted
    # because we can't account for things Test::More does behind the
    # scenes BUT it will still find genuine leaks.

    # XXX BEFORE WE COMPARE THESE NUMBERS WE HAVE TO ACCOUNT FOR $b
    #$b->{HASH} -= 1;
    # Note, we use the keys from $a here in case any other reference
    # types sprang up.
    #$b->{SCALAR} -= scalar keys %$a;
    # XXX This still might fail because I'm not sure if
    # arena_ref_counts will count $b before it is assigned, or what.

    my $ok = is_deeply($b, $a, 'Reference counts should match across runs');

    unless ($ok) {
        # clear these before proceeding
        undef $a;
        undef $b;
        undef $ok;
        #diag(arena_table);
        #diag(scalar keys %seen);
        for my $sv (@{walk_arena()}) {
            my $ra = refaddr $sv;
            #next if ($ra == refaddr $a or $ra == refaddr $b
            #             or $ra == refaddr \$ok);

            # This is the only way I could think of to skip over the actual
            # contents of %seen. Actually kind of clever.
            next if ref $sv eq 'REF' and refaddr $$sv == refaddr \$yes;
            next if $seen{$ra};

            #next unless blessed $sv;
            #next unless $sv =~ /HTML/;
            #next if $sv =~ /Charset::Info/;
            diag(Devel::FindRef::track($sv));
        }
    }
}

__DATA__
<!DOCTYPE html>
<html>
  <head>
    <title>hi</title>
  </head>
  <body>
    <p>yo</p>
  </body>
</html>

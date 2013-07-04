#!perl

use strict;
use warnings FATAL => 'all';

use Test::More;

use Devel::Gladiator qw(walk_arena arena_ref_counts arena_table);
use Devel::FindRef;
use Scalar::Util qw(blessed refaddr);

my $data = do { local $/; <DATA> };

my %seen;
my $yes = 1;

#diag(arena_table);

require HTML::HTML5::Parser;

    {
        my $p = HTML::HTML5::Parser->new;
#diag(arena_table);
        my $doc = $p->parse_string($data);
        undef $p;
    }

for my $sv (@{walk_arena()}) {
    $seen{refaddr $sv} = \$yes;
}

diag(arena_table);

    {
        my $p = HTML::HTML5::Parser->new;
#diag(arena_table);
        my $doc = $p->parse_string($data);
        undef $p;
    }

diag(arena_table);
diag(scalar keys %seen);
for my $sv (@{walk_arena()}) {
    #next unless blessed $sv;
    #next unless $sv =~ /HTML/;
    next if $sv =~ /Charset::Info/;
    next if refaddr $sv == refaddr \$yes;
    next if $seen{refaddr $sv};
#    diag(Devel::FindRef::track($sv));
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

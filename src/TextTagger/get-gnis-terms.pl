#!/usr/bin/perl -a -n -F/\|/

use lib "./Perl";
use TextTagger::Normalize qw(normalize);

next if ($. == 1);
my ($id, $name, $class) = @F[0,1,2];
next if ($id =~ /^\s*$/ or $name =~ /^\s*$/ or $class =~ /^\s*$/);
$id =~ s/[^\w\.-]/_/g;
$class =~ s/[^\w\.-]/_/g;
my $norm = normalize($name);
push @{$norm2unnorm2entries{$norm}{$name}}, [$id, $class];

END {
  for my $norm (sort keys %norm2unnorm2entries) {
    print $norm;
    my $unnorm2entries = $norm2unnorm2entries{$norm};
    my $first = 1;
    for my $unnorm (sort keys %$unnorm2entries) {
      if ($first) {
	$first = 0;
      } else {
	print "\t,";
      }
      print "\t$unnorm";
      for my $entry (@{$unnorm2entries->{$unnorm}}) {
	my ($id, $class) = @$entry;
	print qq/\t(place :id GNIS::_$id :class $class)/;
      }
    }
    print "\n";
  }
}

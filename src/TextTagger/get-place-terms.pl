#!/usr/bin/perl -a -n -F/\t/

use lib "./Perl";
use TextTagger::Normalize qw(normalize);

BEGIN {
  die "USAGE: ./get-place-terms.pl input.csv skip-first-line id-package id-column-index (lex-column-index dsi-args)+"
    unless (@ARGV >= 6 and @ARGV % 2 == 0);
  while (@ARGV > 4) {
    $dsiArgs	= pop @ARGV;
    $lexCol	= pop @ARGV;
    push @cols, [$lexCol, $dsiArgs];
  }
  $idCol	= pop @ARGV;
  $pkg		= pop @ARGV;
  $skipFirstLine= pop @ARGV;
}

next if ($skipFirstLine and $. == 1);
my $id = $F[$idCol];
chomp $id;
next if ($id =~ /^\s*$/);
$id =~ s/[^\w\.-]/_/g;
$id = "_$id" if ($id =~ /^[\d-]/);
$id = $pkg . '::' . $id;
my @prevLexes = ();
for my $col (@cols) {
  my ($lexCol, $dsiArgs) = @$col;
  my $lex = $F[$lexCol];
  chomp $lex;
  next if ($lex =~ /^(\s*|\d+|<null>|not covered by field work)$/i);
  $lex =~ s/"/\\"/g;
  $lex =~ s/(?<!\\)"//g;
  next if (grep { $_ eq $lex } @prevLexes);
  push @prevLexes, $lex;
  my $norm = normalize($lex);
  my $source = $ARGV;
  $source =~ s/.*\///;
  my $dsi = qq/(place :id $id $dsiArgs :source "$source/;
  push @{$norm2unnorm2dsi2rows{$norm}{$lex}{$dsi}}, $.;
}

END {
  for my $norm (sort keys %norm2unnorm2dsi2rows) {
    print $norm;
    my $unnorm2dsi2rows = $norm2unnorm2dsi2rows{$norm};
    my $first = 1;
    for my $unnorm (sort keys %$unnorm2dsi2rows) {
      if ($first) {
	$first = 0;
      } else {
	print "\t,";
      }
      print "\t$unnorm";
      my $dsi2rows = $unnorm2dsi2rows->{$unnorm};
      for my $dsi (sort keys %$dsi2rows) {
	my @rows = @{$dsi2rows->{$dsi}};
	my $rows;
	if (@rows == 1) {
	  $rows = "row $rows[0]";
	} elsif ($rows[-1] - $rows[0] == $#rows) { # contiguous range
	  $rows = "rows $rows[0]-$rows[-1]";
	} else {
	  $rows = "rows " . join(',', @rows);
	}
	print qq/\t$dsi $rows")/;
      }
    }
    print "\n";
  }
}

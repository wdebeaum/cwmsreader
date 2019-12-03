#!/usr/bin/perl -a -n -F/\t/

# get-gns-terms.pl - convert a .txt file from NGA GNS to a TextTagger terms list
# 2019-10-09
# William de Beaumont

use lib "./Perl";
use TextTagger::Normalize qw(normalize);

next if ($. == 1);
# Unique Feature Identifier, Feature Class, feature DeSiGnation code, Name Type
my ($ufi, $fc, $dsg, $nt, @names) = @F[1,9,10,17];
$ufi =~ s/[^\w-\.]/_/g;
$ufi = '_' . $ufi if ($ufi =~ /^[\d-]/);
my $source = $ARGV;
$source =~ s/.*\///;
$source .= " row $.";
my $dsi = qq/(place :id GNS::$ufi :status $nt :class $fc :code $dsg :source "$source")/;

# name forms:
# (skipping sort names since they're a bit redundant and lose capitalization
# info)
#my ($short_form, $full_name_ro, $full_name_nd_ro, $full_name_rg, $full_name_nd_rg) =
my @names = @F[19,22,23,25,26];
for my $name (@names) {
  print normalize($name) . "\t$name\t$dsi\n" if ($name ne '');
}

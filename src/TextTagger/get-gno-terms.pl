#!/usr/bin/perl -a -n -F/\t/

# get-gno-terms.pl - convert a .txt file from geonames.org to TextTagger terms list
# 2019-10-15
# William de Beaumont

use lib "./Perl";
use TextTagger::Normalize qw(normalize);

my ($geonameid, $name, $asciiname, $alternatenames, $feature_class, $feature_code) = @F[0,1,2,3,6,7];
my $id = $geonameid;
$id =~ s/[^\w\.-]/_/g;
$id = 'P' . $id if ($id =~ /^[\d-]/);
my @alts = split(/,/, $alternatenames);
my $source = $ARGV;
$source =~ s/.*\///;
$source .= " row $.";

# NOTE: "sub output {" doesn't capture the above vars properly
$output = sub {
  my ($unnorm, $status) = @_;
  my $norm = normalize($unnorm);
  my $pkg = (($source =~ /^gadm_/) ? 'GADM::' : 'GNO::');
  print qq/$norm\t$unnorm\t(place :id $pkg$id :status $status :class $feature_class :code $feature_code :source "$source")\n/;
};

&$output($name, 'name');
&$output($asciiname, 'ascii') unless ($asciiname eq $name);
for (@alts) {
  &$output($_, 'alternate') unless ($_ eq $name or $_ eq $asciiname);
}

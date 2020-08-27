#!/usr/bin/perl

package TextTagger::PlaceNames;
require Exporter;
@ISA = qw(Exporter);
@EXPORT_OK = qw(init_place_names tag_place_names fini_place_names);

use IPC::Open2;
use KQML::KQML;
use TextTagger::Util qw(word_is_in_trips_lexicon remove_duplicates structurally_equal);
use TextTagger::Normalize qw(is_bad_match characterize_match $dash_re);
use TextTagger::Tags2Trips qw(trips2tagNative);

use strict vars;

my $debug = 0;

my @stoplist = (# see also PersonalNames.pm
  # closed-class temporal names
  # week days
  qw(Sunday Monday Tuesday Wednesday Thursday Friday Saturday),
  qw(Sun Mon Tue Tues Wed Thu Thurs Fri Sat),
  # months
  qw(January February March April May June July August September October November December),
  qw(Jan Feb Mar Apr Jun Jul Aug Sept Oct Nov Dec),
  # seasons
  qw(Spring Summer Fall Autumn Winter),
  # the words "Day" and "Month", capitalized, as part of observances
  qw(Day Month),
  # cardinal direction names
  (map { ($_, lc($_)) } # get both capitalized and lowercase versions, since they're not all in the TRIPS lexicon
   map { ($_, $_ . "ern") } # get both the noun and the -ern adj formed from it
    qw(North Northeast North-East
       East Southeast South-East
       South Southwest South-West
       West Northwest North-West
    ), 'North East', 'South East', 'South West', 'North West'),
  # certain common non-names that can get capitalized at BOS:
  # (NOTE: many, but not all of these are also covered by the rule against
  # short (<= 3 letter) words already in TRIPS at BOS; they're listed here
  # anyway for completeness.)
  # determiners etc.
  qw(A An The That This Those These
     No None
     Few Little Fewer Less Fewest Least
     Some Any
     Many Much More Most
     All Every Each),
  # personal pronouns
  qw(I Me My Mine Myself
     You Your Yours Yourself
     He Him His Himself
     She Her Hers Herself
     It Its Itself
     They Them Their Theirs Themselves
     We Us Our Ours Ourselves
  ),
  # question words
  qw(Who What When Where Why How Which Whose),
  # aux verbs
  qw(Be Is Are Am Was Were Been Being
     Do Does Did Done Doing
     Has Have Had Having
     May Might
     Can Could
     Will Would
     Shall Should
  ),
  # a few more short words that should've been caught by the rule mentioned
  # above, but weren't, because BOS detection isn't perfect, and sometimes
  # sentences are all-caps, and sometimes people randomly capitalize words in
  # the middle of sentences (!)
  qw(As For Not),
  (map { ($_, lc($_)) } # get both capitalized and lowercase versions
    # single words you might think are in the TRIPS lexicon, but aren't
    qw(Agency Asylum
       Battle
       Camp Cereal Commons Core Cyclone
       Dam Dire Donation
       Equality Equity
       Federation Foremost Forest
       Goodwill Gravity Grenade
       Institute
       Legal Lieu Locust
       Maize Mega Meta Minister Mission
       Officer Oral
       Paradox Peace Peoples Pest Poverty
       Quarantine
       Reader Refuge Republic Ripe
       Same Sector Settlement Shields Singer Stark Swan
       Unto
    ),
    # multiword phrases
    'Emergency Medical Team', 'High Level', 'Home Base', 'Recreation Center',
    'The Horn', 'World Bank', 'World Vision',
    # names that are more likely to be personal names than place names
    # TODO? use PersonalNames tagger's output to detect this case? not enabled in cwms, though
    qw(Amanda Steven),
  ),
  # a few words that are in the trips lexicon, but are too long for that to
  # prevent them from being tagged if they're capitalized
  qw(Children
     Enterprise
     International
     Location
     Marriott
     Opportunity Overview
     Protection
     Register
     Simmering Standard
     Transfer
  )
);
# add all-caps versions of everything on the stoplist
push @stoplist, map { uc($_) } @stoplist;

my %countries_dsi_type2ont_type = (qw(
  capital CITY
  country COUNTRY
  demonym NATIONALITY
  region GEOGRAPHIC-REGION
  subregion GEOGRAPHIC-REGION
));

my ($terms_in, $terms_out, $terms_pid);

sub init_place_names {
  my $self = shift;
  $terms_pid = open2($terms_in, $terms_out,
                     $ENV{TRIPS_BASE} . '/bin/terms2',
                     $ENV{TRIPS_BASE} . '/etc/TextTagger/place-names.tsv');
  binmode $terms_in, ':utf8';
  binmode $terms_out, ':utf8';
}

sub fini_place_names {
  close($terms_in);
  close($terms_out);
  waitpid $terms_pid, 0;
}

my @source_score_levels = ( # low to high score
  # USA
  ['NationalFile.txt'],
  # rest of world (not USA, ETH, SSD)
  [qw(admin1CodesASCII.txt admin2Codes.txt cities50000.txt countries.json)],
  # (if we had resources specific to all of Africa, they'd go here)
  # South Sudan
  [qw(od.txt ssd_ica_mainsettlements_geonode_feb2016.csv ssd_ica_predlhz_geonode_feb2016.csv ssd_populatedplaces_tabulardata.csv SS.txt)],
  # Ethiopia
  # FIXME? should gadm_woredas.txt be scored lower than ET.txt (GNO)? or do they never overlap and disagree?
  [qw(eth_bnd_adm2_wfpco.csv Ethiopia_bnd_adm2_woreda.csv eth_pop_adm3.csv eth_populatedplaces_tabulardata.csv et.txt ET.txt gadm_woredas.txt)]
);
my %source2score = ();
for my $i (0..$#source_score_levels) {
  for my $source (@{$source_score_levels[$i]}) {
    $source2score{$source} = $i;
  }
}

my @status_score_levels = (		# low to high score
  [qw(d p ds undef)],			# unverified names/no status
  [qw(alternate alt alt1 alt2 v vs)],	# alternative names
  [qw(ascii ref c va common)],		# conventional names/encoding variants
  [qw(name n ns official)]		# official/approved names
);
my %status2score = ();
for my $i (0..$#status_score_levels) {
  for my $status (@{$status_score_levels[$i]}) {
    $status2score{$status} = $i;
  }
}

sub score_match {
  my $m = shift;
  print STDERR "PlaceNames::score_match(" . Data::Dumper->Dump([$m], ['*m']) . ")\n" if ($debug);
  my $variant_score = TextTagger::Normalize::score_match($m);
  die "missing :source in:\n" . Data::Dumper->Dump([$m],['*match'])
    unless (exists($m->{source}));
  my $source = $m->{source}; # (exists($m->{source}) ? $m->{source} : 'undef');
  $source =~ s/ .*$//; # remove " row N" from end
  my $source_score = $source2score{$source};
  my $status = (exists($m->{status}) ? $m->{status} : 'undef');
  my $status_score = $status2score{$status};
  print STDERR Data::Dumper->Dump([$status, $status_score, $variant_score], [qw(source source_score status status_score variant_score)]) if ($debug);
  my $final_score = ((((0
    ) * scalar(@source_score_levels) + $source_score
    ) * scalar(@status_score_levels) + $status_score
    ) * 2 + $variant_score
    ) / (scalar(@source_score_levels) * scalar(@status_score_levels) * 2 - 1);
  print STDERR "PlaceNames::score_match returning $final_score\n" if ($debug);
  return $final_score;
}

sub tag_without_matches {
  my $tag = shift;
  return +{
    %$tag,
    'domain-specific-info' => +{
      %{$tag->{'domain-specific-info'}},
      matches => undef
    }
  };
}

# kind of like sense_tags_are_combinable in CombineTags.pm, but specific to
# PlaceNames.pm and the problem of tags that differ only in their matches
sub tags_combinable {
  return (
    # first, a shortcut for the common case of different spans
    $_[0]{start} == $_[1]{start} && $_[0]{end} == $_[1]{end} &&
    # for the same span, compare full tags without matches
    structurally_equal(map { tag_without_matches($_) } @_)
  );
}

sub tag_place_names {
  my ($self, $str, @input_tags) = @_;
  $str =~ s/\n/ /g;
  print $terms_out "$str\n";
  print STDERR "$str\n"
    if ($debug);
  my %sentence_starts = ();
  for my $tag (@input_tags) {
    if ($tag->{type} eq 'sentence') {
      my $start = $tag->{start};
      my $text = $tag->{text};
      if ($text =~ /^\P{L}+/) {
	# sentence starts with non-letters (e.g. bullet point); skip those
	$text = $';
	$start += $+[0];
      }
      for(;;) {
	$sentence_starts{$start} = 1;
	if ($text =~ /^\p{Lu}+\P{L}+/) {
	  # sentence starts with all-caps words, count each as part of the
	  # sentence start so we don't mistakenly think that they're
	  # capitalized because they're names
	  $text = $';
	  $start += $+[0];
	} else { # no more all-caps
	  last;
	}
      }
    }
  }
  my @terms = ();
  my %concept2terms = ();
  while ((my $term = <$terms_in>)) {
    print STDERR $term
      if ($debug);
    chomp $term;
    last if ($term eq '');
    my ($normalized_matched_variant, $start, $end, @rest) = split(/\t/, $term, -1);
    die "Bogus output from term tagger (PlaceNames): '$term'"
      unless ($start =~ /^\d+$/ and $end =~ /^\d+$/ and @rest);
    # remake lex from original input, since the matched variant could be
    # different
    my $lex = substr($str, $start, $end - $start);
    my $bos = $sentence_starts{$start};
    my $len = $end - $start;
    my $short = ($len <= 3);
    my $shortish = ($len <= 7);
    # skip stoplisted words
    next if (grep { $lex eq $_ } @stoplist);
    # skip bad matches
    next if (is_bad_match($str, $lex, $start, $end));
    # skip matches starting or ending with a dash, e.g. "- The"
    next if ($lex =~ /^$dash_re|$dash_re$/);
    # skip certain words already in the TRIPS lexicon
    next if ( $lex !~ /\s/ && # single word
	      ( ($lex =~ /^\p{Ll}/) || # uncapitalized
	        ($shortish && $bos) # short-ish, at beginning of sentence
	      ) &&
	      word_is_in_trips_lexicon($self, $lex)
	    );
    # skip matches of two words, the first being case-insensitive "the", and
    # the second on the stoplist or already in the TRIPS lexicon, e.g. "the
    # river" (an alternate name in GNO for New Brunswick, NJ!)
    next if (($lex =~ /^the (\S+)$/i) and
             ((grep { $1 eq $_ } @stoplist) or
	      word_is_in_trips_lexicon($self, $1)));
    my %tag_common = (
      type => 'named-entity',
      lex => $lex,
      start => $start,
      end => $end
    );
    while (@rest) {
      my $matched_variant = shift(@rest); # unnormalized
      # require short names to match exactly (e.g. don't match "is" to "IS" =
      # "Iceland")
      if ($short and $lex ne $matched_variant) {
	# eat @rest until next comma or EOL
	while (@rest) {
	  last if (shift(@rest) eq ',');
	}
	next;
      }
      my $match = characterize_match($lex, $matched_variant, $bos);
      while (@rest) {
	my $dsi_str = shift(@rest);
	last if ($dsi_str eq ',');
	my $dsi_kqml = KQML::KQMLReadFromString($dsi_str);
	my $dsi = trips2tagNative($dsi_kqml);
	print STDERR Data::Dumper->Dump([$dsi], ['*dsi']) if ($debug);
	# move status and source from dsi into match when present, and score
	my $match_with_status = +{ %$match };
	if (exists($dsi->{status})) {
	  $match_with_status->{status} = $dsi->{status};
	  delete $dsi->{status};
	}
	if (exists($dsi->{source})) {
	  $match_with_status->{source} = $dsi->{source};
	  delete $dsi->{source};
	# also fill in :source where we can
	} elsif ($dsi->{type} eq 'place' and $dsi->{id} =~ /^GNIS::/) {
	  $match_with_status->{source} = 'NationalFile.txt';
	} elsif ($dsi->{type} ne 'place') {
	  $match_with_status->{source} = 'countries.json';
	}
	$match_with_status->{score} = score_match($match_with_status);
	# add domain and matches fields to dsi
	$dsi->{domain} = 'cwms';
	$dsi->{matches} = [$match_with_status];
	if ($dsi->{type} eq 'place') { # need mapping, store for later
	  $dsi->{id} =~ /^(\w+)::/
	    or die "missing package on place name id: $dsi->{id}";
	  my $id_pkg = $1;
	  # be stricter about capitalization for GNIS tags (there are a lot of
	  # US placenames that overlap with common English words and phrases)
	  next if ($id_pkg eq 'GNIS' and
	           (exists($match_with_status->{'no-caps-initial-cap'}) or 
		    ($lex !~ /\s|$dash_re/ and 
	             exists($match_with_status->{'sentence-cap-initial-cap'})
		     )));
	  my $concept_pkg = ($id_pkg eq 'GNIS' ? 'GNIS' : 'GNO');
	  my $concept = $concept_pkg . '::' . uc($dsi->{code} || $dsi->{class});
	  $dsi->{mappings} = [];
	  push @{$concept2terms{$concept}}, +{
	    %tag_common,
	    'penn-pos' => ['NNP'], # no plurals here
	    'lftype' => [],
	    'domain-specific-info' => $dsi
	  };
	} else { # special DSI types from countries.json, just add them
	  my $ont_type = $countries_dsi_type2ont_type{$dsi->{type}};
	  push @terms, +{
	    %tag_common,
	    'penn-pos' => ['NNP'], # no plurals here
	    'lftype' => [$ont_type],
	    'domain-specific-info' => $dsi
	  };
	  # add an extra JJ ONT::nationality-val tag for demonyms
	  if ($dsi->{type} eq 'demonym') {
	    push @terms, +{
	      %tag_common,
	      'penn-pos' => ['JJ'],
	      'lftype' => ['NATIONALITY-VAL'],
	      'domain-specific-info' => $dsi
	    };
	  }
	}
      }
    }
    print STDERR "Got term '$lex'.\n"
      if ($debug);
  }
  print STDERR Data::Dumper->Dump([\%concept2terms],['*concept2terms'])
    if ($debug);
  if (%concept2terms) { # if we have concepts requring mappings
    my @concepts = sort keys %concept2terms;
    my $concepts = join(' ', @concepts);
    # get the mappings from DSL
    my $reply_content =
      KQML::KQMLKeywordify($self->send_and_wait(
        "(request :content (get-trips-ont-mappings :concept-ids ($concepts)))"
      ));
    # put the mappings and the mapped-to lftypes in the right places in the tags
    for (@{$reply_content->{':mappings'}}) {
      my $m = KQML::KQMLKeywordify($_);
      my $from = $m->{':from'}[1];
      my $to = $m->{':to'};
      $to =~ s/^ONT:://;
      for my $tag (@{$concept2terms{$from}}) {
	push @{$tag->{'domain-specific-info'}{mappings}}, $m;
	push @{$tag->{lftype}}, $to;
      }
    }
    # remove duplicate lftypes and add successfully mapped tags to @terms
    for my $concept (@concepts) {
      for my $tag (@{$concept2terms{$concept}}) {
	if (@{$tag->{lftype}}) {
	  $tag->{lftype} = remove_duplicates($tag->{lftype});
	  push @terms, $tag;
	}
      }
    }
  }
  # combine tags for different matches with same other info
  my @combined_terms = ();
  for my $new_tag (@terms) {
    my $add_new = 1;
    for my $old_tag (@combined_terms) {
      if (tags_combinable($old_tag, $new_tag)) {
	push @{$old_tag->{'domain-specific-info'}{matches}},
	     @{$new_tag->{'domain-specific-info'}{matches}};
	$add_new = 0;
	last;
      }
    }
    push @combined_terms, $new_tag if ($add_new);
  }
  # sort matches, and propagate max match score to DSI and tag
  for my $tag (@combined_terms) {
    if (exists($tag->{'domain-specific-info'}) and
        exists($tag->{'domain-specific-info'}{matches})) {
      # get maximum score of any match
      my $max_score = 0;
      for my $match (@{$tag->{'domain-specific-info'}{matches}}) {
	$max_score = $match->{score} if ($max_score < $match->{score});
      }
      # put it in the DSI and the tag
      $tag->{'domain-specific-info'}{score} = $max_score;
      $tag->{score} = $max_score;
      # sort matches by descending score
      use sort 'stable';
      @{$tag->{'domain-specific-info'}{matches}} =
        sort { $b->{score} <=> $a->{score} }
	@{$tag->{'domain-specific-info'}{matches}};
    }
  }
  return [@combined_terms];
}

push @TextTagger::taggers, {
  name => "place_names",
  init_function => \&init_place_names,
  tag_function => \&tag_place_names,
  fini_function => \&fini_place_names,
  output_types => ['named-entity', 'sense'],
  optional_input_types => ['sentence'],
  input_text => 1
};

1;

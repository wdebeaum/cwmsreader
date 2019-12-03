# Which data sources do we use to build the lookup table for the PlaceNames
# tagger, place-names.tsv?
# default: all of them
PLACE_NAME_SOURCES ?= GNIS countries GADM GNO HDX GNS WFP
$(info PLACE_NAME_SOURCES=$(PLACE_NAME_SOURCES))

# Which *-terms.tsv files do we use to build place-names.tsv?
PLACE_NAME_TSVS=
ifeq (GNIS,$(findstring GNIS,$(PLACE_NAME_SOURCES)))
PLACE_NAME_TSVS += gnis-terms.tsv
endif
ifeq (countries,$(findstring countries,$(PLACE_NAME_SOURCES)))
PLACE_NAME_TSVS += countries.tsv
endif
ifeq (GADM,$(findstring GADM,$(PLACE_NAME_SOURCES)))
PLACE_NAME_TSVS += gadm-terms.tsv
endif
ifeq (GNO,$(findstring GNO,$(PLACE_NAME_SOURCES)))
PLACE_NAME_TSVS += \
	gno-ET-terms.tsv \
	gno-SS-terms.tsv \
	gno-adm1-terms.tsv \
	gno-adm2-terms.tsv \
	gno-cities50000-terms.tsv
endif
ifeq (HDX,$(findstring HDX,$(PLACE_NAME_SOURCES)))
PLACE_NAME_TSVS += \
	hdx-eth-adm3-terms.tsv \
	hdx-eth-ppl-terms.tsv \
	hdx-ssd-ppl-terms.tsv
endif
ifeq (GNS,$(findstring GNS,$(PLACE_NAME_SOURCES)))
PLACE_NAME_TSVS += \
	gns-eth-terms.tsv \
	gns-ssd-terms.tsv
endif
ifeq (WFP,$(findstring WFP,$(PLACE_NAME_SOURCES)))
PLACE_NAME_TSVS += \
	wfp-eth-adm2-terms.tsv \
	wfp-eth-adm3-terms.tsv \
	wfp-ssd-ppl-terms.tsv \
	wfp-ssd-adm2-terms.tsv
endif
$(info PLACE_NAME_TSVS=$(PLACE_NAME_TSVS))

#
# top-level rules
#

all:: place-names.tsv

install:: place-names.tsv
	$(INSTALL_DATA) $< $(etcdir)/$(MODULE)

place-names.tsv: merge-terms-files.pl $(PLACE_NAME_TSVS)
	$(PERL) ./$+ >$@

clean::
	rm -f $(PLACE_NAME_TSVS) place-names.tsv

#
# rules for each data source and related downloads
#

#
# United States Geological Survey (USGS)
# Geographic Names Information System (GNIS)
#

gnis-terms.tsv: $(TEXTTAGGER_geonames) get-gnis-terms.pl
	unzip -p $< | $(PERL) ./get-gnis-terms.pl >$@

#
# Humanitarian Data Exchange (HDX)
#

# Ethiopia

hdx-eth-adm3-terms.tsv: get-place-terms.pl downloads/GeoNames/HumanitarianDataExchange-GeoNames/eth_pop_adm3.csv
	$(PERL) ./$+ 1 HDX 7 6 ":status name :class A :code ADM3" >$@

# TODO? use popPlaceClass* columns?
hdx-eth-ppl-terms.tsv: get-place-terms.pl downloads/GeoNames/HumanitarianDataExchange-GeoNames/eth_populatedplaces_tabulardata.csv
	$(PERL) ./$+ 1 HDX 3 \
	  2 ":status name :class P :code PPL" \
	  4 ":status ref :class P :code PPL" \
	  5 ":status alt1 :class P :code PPL" \
	  6 ":status alt2 :class P :code PPL" \
	  >$@

# South Sudan (same as hdx-eth-ppl-terms.tsv, but missing first two columns :-P)

hdx-ssd-ppl-terms.tsv: get-place-terms.pl downloads/GeoNames/HumanitarianDataExchange-GeoNames/ssd_populatedplaces_tabulardata.csv
	$(PERL) ./$+ 1 HDX 1 \
	  0 ":status name :class P :code PPL" \
	  2 ":status ref :class P :code PPL" \
	  3 ":status alt1 :class P :code PPL" \
	  4 ":status alt2 :class P :code PPL" \
	  >$@

#
# National Geospatial-intelligence Agency (NGA) GEONet Names Server (GNS)
#

gns-eth-terms.tsv: get-gns-terms.pl downloads/GeoNames/NGAGeoNames/et-Ethiopia/et.txt
	$(PERL) ./$+ |(LC_ALL=C sort) >$@

gns-ssd-terms.tsv: get-gns-terms.pl downloads/GeoNames/NGAGeoNames/od-SouthSudan/od.txt
	$(PERL) ./$+ |(LC_ALL=C sort) >$@

#
# World Food Programme (WFP) GeoNode
#

# Ethiopia

wfp-eth-adm2-terms.tsv: get-place-terms.pl downloads/GeoNames/WFPGeonodes/eth_bnd_adm2_wfpco.csv
	$(PERL) ./$+ 1 WFP 3 5 ":status name :class A :code ADM2" >$@

wfp-eth-adm3-terms.tsv: get-place-terms.pl downloads/GeoNames/WFPGeonodes/Ethiopia_bnd_adm2_woreda.csv
	$(PERL) ./$+ 1 WFP 4 9 ":status name :class A :code ADM3" >$@

# South Sudan

wfp-ssd-ppl-terms.tsv: get-place-terms.pl downloads/GeoNames/WFPGeonodes/ssd_ica_mainsettlements_geonode_feb2016.csv
	$(PERL) ./$+ 1 WFP 0 2 ":status name :class P :code PPL" >$@

wfp-ssd-adm2-terms.tsv: get-place-terms.pl downloads/GeoNames/WFPGeonodes/ssd_ica_predlhz_geonode_feb2016.csv
	$(PERL) ./$+ 1 WFP 0 4 ":status name :class A :code ADM2" >$@

#
# GeoNames archive, containing HDX, GNS, WFP data
#

.PRECIOUS:: downloads/GeoNames/%

# so we can depend on individual files from the archive and have them in $<
downloads/GeoNames/%: downloads/GeoNames/MADE
	touch $@

downloads/GeoNames/MADE: downloads/GeoNames.20191005.tar.bz2
	cd downloads && tar -jxf $(notdir $<)
	# fix quoted newlines in HDX files (this is a little sloppy since it
	# also removes embedded " characters, but those only occur in lat/lon
	# fields we're not using)
	for f in downloads/GeoNames/HumanitarianDataExchange-GeoNames/*.csv ; \
	do $(PERL) -p -i -e "BEGIN { \$$/ = '\"'; } chomp; s/\\n//g if (\$$. % 2 == 0);" $$f ; \
	done
	touch $@

downloads/GeoNames.20191005.tar.bz2:
	$(call download,http://trips.ihmc.us/worldmod/GeoNames.20191005.tar.bz2)	
#
# GeoNames.org (GNO)
#

gno-adm1-terms.tsv: get-place-terms.pl downloads/geonames.org/admin1CodesASCII.txt
	$(PERL) ./$+ 0 GNO 3 \
	  1 ":status name :class A :code ADM1" \
	  2 ":status ascii :class A :code ADM1" \
	  >$@

gno-adm2-terms.tsv: get-place-terms.pl downloads/geonames.org/admin2Codes.txt
	$(PERL) ./$+ 0 GNO 3 \
	  1 ":status name :class A :code ADM2" \
	  2 ":status ascii :class A :code ADM2" \
	  >$@

gno-%-terms.tsv: get-gno-terms.pl downloads/geonames.org/%.txt
	$(PERL) ./$+ |(LC_ALL=C sort) >$@

# we don't need all these cities, just get those with pop.>=50k instead of 15k
downloads/geonames.org/cities50000.txt: downloads/geonames.org/cities15000.txt
	$(PERL) -a -n -F'/\t/' -e 'print join("\t", @F) if ($$F[14] >= 50000);' <$< >$@

.PRECIOUS:: downloads/geonames.org/%.txt downloads/geonames.org/%.zip

downloads/geonames.org/%.txt: downloads/geonames.org/%.zip
	cd downloads/geonames.org/ && unzip $*.zip $*.txt
	touch $@

downloads/geonames.org/%.zip: downloads/geonames.org/MADE
	$(call download,http://download.geonames.org/export/dump/$*.zip)
	mv downloads/$*.zip downloads/geonames.org/$*.zip

# these two aren't zipped
downloads/geonames.org/admin1CodesASCII.txt downloads/geonames.org/admin2Codes.txt: downloads/geonames.org/MADE
	$(call download_compressed,http://download.geonames.org/export/dump/$(notdir $@))
	mv downloads/$(notdir $@) $@

downloads/geonames.org/MADE:
	mkdir -p $(dir $@)
	touch $@

#
# EIDOS/CLULab/Arizona/GADM
#

gadm-terms.tsv: get-gno-terms.pl downloads/gadm_woredas.txt
	$(PERL) ./$+ |(LC_ALL=C sort) >$@

downloads/gadm_woredas.txt:
	$(call download_compressed,http://clulab.cs.arizona.edu/models/gadm_woredas.txt)

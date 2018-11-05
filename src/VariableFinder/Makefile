MODULE=VariableFinder
# For pure Python:
MAIN=vars_server.py
SRCS= \
	variables.py \
	vars_server.py
REQUIREMENTS=requirements.txt

CONFIGDIR=../config
include $(CONFIGDIR)/python/prog.mk

EIDOS_ONTS=fao_variable topoflow un wdi
RESOURCES_PATH=$(etcdir)/$(MODULE)/resources
RESOURCES= \
	counter-fitted-vectors-gensim.txt \
	stopwords.txt \
	dsatVars.xlsx \
	$(EIDOS_ONTS:%=%_ontology.yml) \
	codes/crop_codes.csv \
	codes/management_codes.csv \
	codes/metadata_codes.csv \
	codes/other_codes.csv \
	codes/country_codes.csv

$(RESOURCES_PATH)/%: resources/%
	$(MKINSTALLDIRS) $(dir $@)
	$(INSTALL_DATA) $< $@

# special case for this one large resource: download it directly to install loc
$(RESOURCES_PATH)/counter-fitted-vectors-gensim.txt: download_g.py $(VENV_SH)
	$(MKINSTALLDIRS) $(RESOURCES_PATH)
	. $(VENV_SH) ; \
	python download_g.py 0B26sZWRA4lQmNXBTd1BXb05lYjA $@

resources/%_ontology.yml:
	cd resources ; curl -L -O "https://raw.githubusercontent.com/clulab/eidos/master/src/main/resources/org/clulab/wm/eidos/english/ontologies/$*_ontology.yml"

install:: $(RESOURCES:%=$(RESOURCES_PATH)/%)
	$(MKINSTALLDIRS) $(RESOURCES_PATH)/codes
	. $(VENV_SH) ; \
	python -m nltk.downloader wordnet

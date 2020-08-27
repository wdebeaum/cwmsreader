(in-package :dsl)

(defresource (GNIS USGS Geographic-Names-Information-System United-States-Geological-Survey))
(defresource (GNO GeoNames.org))
(defresource (GADM))
(defresource (GNS NGA GEONet-Names-Server))
(defresource (HDX HDE Humanitarian-Data-Exchange))
(defresource (WFP WFP-GeoNode World-Food-Programme-GeoNode))

;; for covid19 in domain-terms.tsv
(defresource (NCIT NCI-Thesaurus))

(require-resource-version :ont)
(require-dsl-file #!TRIPS"src;TextTagger;cwms-mappings.lisp")

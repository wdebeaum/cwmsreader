(in-package :lxm)

#|
An example of defining a new sense (ONT::PERSON) for a word ("character").  The word does not have to exist currently in the TRIPS lexicon, or anywhere else. 

:pos indicates the part-of-speech of the word sense. Some common categories are
   W::n (noun)
   W::name  (proper name)
   W::v (verb)
   W::adj (adjective)
   W::adv (adverb)

LF-PARENT indicates the ontology type.  This type must exist in the TRIPS ontology.  

TEMPL indicates the syntactic template of the word sense.  It denotes how this word sense can be used in a sentence.  The most common ones are
   countable nouns: COUNT-PRED-TEMPL (e.g., three chairs)
   mass nouns: MASS-PRED-TEMPL
   names:    NAME-TEMPL
   transitive verbs: AGENT-AFFECTED-XP-TEMPL (e.g., I ate the cookie)
   intransitive verbs: AGENT-TEMPL (e.g., I walked)

The templates are in templates.lisp and noun-templates.lisp in .../src/LexiconManager/Data/templates/

An easy (or easier) way to identify an appropriate template is to look up the existing lexical entry for a word that behaves similarly to the target word. The templates are shown in the left panel of the online ontology browser.

Note that there can be multiple entries for a word sense, each with a different template, indicating different usages.

|#

#|
(define-words :pos W::n  
:words (
 (w::character
   (SENSES
    ((LF-PARENT ONT::PERSON)
     (TEMPL COUNT-PRED-TEMPL)
     )
    )
   )
))

|#

;; El Nino & La Nina
(define-words :pos W::name  
  :words (
	  ((w::El w::Nino)
	   (SENSES
	    ((LF-PARENT ONT::natural-phenomenon)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  ((w::El w::Niño)
	   (SENSES
	    ((LF-PARENT ONT::natural-phenomenon)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  ((w::La w::Nina)
	   (SENSES
	    ((LF-PARENT ONT::natural-phenomenon)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  ((w::La w::Niña)
	   (SENSES
	    ((LF-PARENT ONT::natural-phenomenon)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  ))

;; important abbreviations
;; _lgalescu
(define-words :pos W::N
  :words (
	  (w::IDP ;; internally displaced person
	   (SENSES
	    ((LF-PARENT ONT::PERSON)
	     (TEMPL COUNT-PRED-TEMPL)
	     )
	    )
	   )
	  ))
(define-words :pos W::name  
  :words (
	  (w::GAM ;; Global Acute Malnutrition
	   (SENSES
	    ((LF-PARENT ONT::medical-disorders-and-conditions)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::SAM ;; Severe Acute Malnutrition
	   (SENSES
	    ((LF-PARENT ONT::medical-disorders-and-conditions)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::MAM ;; Moderately Acute Malnutrition
	   (SENSES
	    ((LF-PARENT ONT::medical-disorders-and-conditions)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  ))

;; season names in SS & ET
;; _lgalescu
(define-words :pos W::name  
  :words (
	  (w::belg ;; short rainy season from March to May (in highland and mid-land areas)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::sugum ;; rainy season from March to May (Ethiopia)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::diraac ;; rainy season from March to May (Ethiopia)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::deyr ;; short rainy season from October to December (in Somali Region)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::hagaya ;; short rainy season from October to December (in Oromia Region)
	   ;; also: hegaya? hagayita?
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::gu ;; main rainy season from March to June (in Somali region)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::genna ;; main rainy season from March to June (in Oromia region)
	   ;; also: ganna?
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::ganna ;; another name for w::genna
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::kiremt ;; long and heavy rainy season Jun-Sep (summer; in Ethiopia)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::karan ;; rainy season from June to September (Ethiopia)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::karma ;; rainy season from June to September (Ethiopia)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  (w::meher ;; Sep-Nov rainy season (in South Omo zone)
	   (SENSES
	    ((LF-PARENT ONT::season)
	     (TEMPL name-templ)
	     )
	    )
	   )
	  ))

;; administrative divisions
;; _lgalescu
(define-words :pos W::N
  :words (
	  (w::woreda ;; third-level administrative divisions of Ethiopia
	   (SENSES
	     ((LF-PARENT ONT::DISTRICT)
	      (TEMPL COUNT-PRED-TEMPL)
	      )
	     )
	   )
	  (w::kebele ;; Lowest (fourth-level) Administrative Level (= ward)
	   (SENSES
	     ((LF-PARENT ONT::DISTRICT)
	      (TEMPL COUNT-PRED-TEMPL)
	      )
	     )
	   )
	  ))

(define-words :pos W::name
  :words (
	  (w::DSSAT 
	   (SENSES
	     ((LF-PARENT ONT::computer-program)
	      (TEMPL nname-templ) ; so we can say DSSAT 4.6
	      )
	     )
	   )
	  ))


;; COVID-19
;; _lgalescu
(define-words :pos W::name
  :words (
	  (W::COVID-PUNC-MINUS-19
	   (SENSES
	     ((LF-PARENT ONT::DISEASE)
	      (TEMPL name-templ)
	      )
	     )
	   )
	  ))

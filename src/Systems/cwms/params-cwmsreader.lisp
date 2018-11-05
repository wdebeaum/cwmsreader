;;;;
;;;; params-cwmsreader.lisp - lisp params for reader version of CWMS web parser
;;;;
;;;; To set up the cwmsreader web parser:
;;;; - add the following to your checkout:
;;;;   src/WebParser/
;;;;   src/DrumGUI/
;;;; - rebuild the system
;;;; - mkdir $TRIPS_BASE/etc/Data
;;;; - mkdir $TRIPS_BASE/src/Systems/cwmsreader
;;;; - cd $TRIPS_BASE/src/WebParser/ && make -f Makefile-component install
;;;; - pass the -reader option to trips-cwms (and not -nolisp)
;;;; - go to the URL with "cwmsreader" at the end instead of "cwms"
;;;;

;;PARSER, TEXTTAGGER and WORDFINDER settings

;;(parser::setmaxchartsize 5000)
(setf parser::*include-parse-tree-in-messages* '(w::lex)) ; for WebParser

(setq *use-texttagger* T)
;;(setf (parser::number-parses-to-find parser::*chart*) 2)
;;(setf (parser::number-parses-desired parser::*chart*) 1)
(setq parser::*filter-and-preparse-input* nil)
;;(parser::setmaxchartsize 3000)
;;(setf (parser::flexible-semantic-matching parser::*chart*) t)

;;;; Parser options
(setq parser::*parser-init-settings*
      '((parser::*parser-display-enabled* nil)
	(parser::*in-system* :cwms)
	;; average number of letters in a word (not critical)
	(parser::*word-length* 8)
	;; boost factor based on length of input covered
	(parser::*score-length-multiplier* .2)
	;; not clear this is helpful
	(parser::*score-corner-multiplier* 0)
	;; indicate we should use POS information
	(parser::*use-tags-as-filter* t)
	;; penalty multiplier for lex entries that do not match POS tags
	(parser::*bad-tag-multiplier* .9)
	;; penalty for referential-sem lex items
	(parser::*referential-sem-penalty* .96)
	;; constituents that we expect in the skeleton
	(parser::*skeleton-constit-cats* '(W::NP W::CP W::VP W::ADVBL W::PP W::S))
	;; boost constituents that match the skeleton (from stat. parser)
	(parser::*skeleton-boost-factor* .2)   ;; new boosting scheme - 20% of difference to 1.0
	;; penalty for crossing skeleton constituent boundaries
	((setf (parser::barrier-penalty parser::*chart*) .99))
	;;
	(parser::*use-senses-as-filter* t)
	;;
	(parser::*bad-sense-multiplier* .96)
	;;
	(parser::*no-positions-in-lf* nil)
	;;
	;(parser::*beam-pruning-on* nil)   ; no pruning
	(parser::*beam-pruning-on* t)   ; no pruning
	;;
	;(parser::*pruning-frequency* 500)
	(parser::*pruning-frequency* 1000)
	;;
	(parser::*beam-width* 50)
	;; max number of constituents built before stopping
	;((parser::setmaxnumberentries 3000))
	((parser::setmaxnumberentries 6000))
	;;
	;((parser::setmaxchartsize 3000))
	((parser::setmaxchartsize 8000))
	;;
	;;(parser::*kr-type-info-desired* '(:WNsense :DRUM :wordnet))
	;;
	((setf (parser::flexible-semantic-matching parser::*chart*) t))
	;; boost content words that have domain specific info attached
	(parser::*domain-boosting-factor* .2)
	;; have the parser remove tagged constituents that are subparts of other terms with domain info
	(parser::*filter-texttagger-input* t)
	;; number of interpretations to obtain before stopping
	((setf (parser::number-parses-to-find parser::*chart*) 4))
	;; number of interpretations returned
	((setf (parser::number-parses-desired parser::*chart*) 1))
	;; disfavor speech acts not common in text

	;; required for WebParser
	(parser::*include-parse-tree-in-messages* '(w::lex))

	((parser::customize-cost-table 
	  '((ont::SA_TELL 1)
	    (ont::SA_identify 1.3)
	    (ont::SA_CONFIRM 1.3)
	    (ont::SA_QUERY 2)
	    (ont::SA_pred-fragment 2)
	    (ont::SA_request 2.5)
	    (ont::SA_YN-QUESTION 2.5)
	    (ont::SA_WH-QUESTION 2.5)
	    (w::ISOLATED-CP 1.2)
	    (w::CP 1.5)  
	    (w::VP 2)
	    (w::punc .5))))
	))
(parser::initialize-settings) 

;; dialogue manager, eg: textIM, simpleIM, extractIM...
;(setq im::*current-dialog-manager* #'im::SequentialLFTransformIM)   ;;#'im::simpleIM)
(setq im::*current-dialog-manager* #'im::extractsequenceIM)
;(setq im::*cps-control* t)

;(setq im::*substitute-types-in-pros* t)
(setq im::*substitute-types-in-pros* nil)
(setq im::*compute-force-from-tmas* t)
;(setq im::*max-allowed-utts-in-turn* 2) ;; we're being a little generous to try to pick up more referring expressions
(setq im::*max-allowed-utts-in-turn* 30) ;; we're being hard nosed as get started
(setq im::*external-name-resolution* nil)  ;; will eventually be set to T when we do reference resolution in context
(setq im::*show-lf-graphs* nil)
(setq im::*lf-output-by-utterance* t)
(setq im::*max-cover-with-strict-disjoint-extractions* nil)
(setq im::*eliminate-subevents* nil)
(setq im::*allow-optional-lfs* t) ;; set to t for optional term matching
(setq im::*output-format* 'im::lf-term)
;(setq im::*output-format* 'im::LF)
(setq im::*external-phrase-tagger* 'im::variable-finder)
;(setq im::*symbol-map* nil)

(setq *print-pretty* t)

;;;; LxM options
;; use WordFinder?
(setq lxm::*use-wordfinder* t)
;; we are trying to really depend on the Stanford parser (for now!)
(setq lxm::*use-tagged-senses-only* t)
;; don't use wordnet if we have domain-specific info from TextTagger
(setq lxm::*no-wf-senses-for-words-tagged-with-ont-types* t)
;; don't use wordnet if we have TRIPS entries  
(setq  lxm::*use-trips-and-wf-senses* nil) 

;;;; LOGGING options
(setq logging::*logging-enabled* nil)
(setq logging2::*logging-enabled* nil)

;;  DM settings
(setq dagent::*silent-failures* nil)  ;; don't ignore utterance failure
(setq dagent::*using-alarms* nil)   ;; no alarms
(setq dagent::*disabled-wizard* t)  ;; no wizard
(setq dagent::*use-quick-generator* t)
(dagent::trace-on 1)

; just the default user
(setq dagent::*users* (list (cons "desktop" (dagent::make-user :name "desktop" :channel-id 'dagent::desktop))))

;; and here's the state definitions for the dialogue manager

(setq im::*extraction-sequence* '((im::preprocessRules) (im::cwmsRules) (im::drum) (im::cwms_ev_add) (im::drumMod) (im::cwmsCC) (im::postprocessRules)))
(setq im::*substitute-terms-in-extraction* t)
(setq im::*roles-to-emit* nil)

(setq dagent::*external-CSM* t)

(setq *texttagger-split-mode*
;  :split-clauses
  :split-sentences
  )

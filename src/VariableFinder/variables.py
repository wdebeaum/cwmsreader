# -*- coding: utf-8 -*-
import gensim
import pandas as pd
import re
from nltk.corpus import wordnet as wn
import Levenshtein
import numpy as np
from yaml import safe_load

class WMVariable:

    def __init__(self, resources_path):
        print 'Loading VariableFinder resources...'
        wv_path = resources_path + '/counter-fitted-vectors-gensim.txt'
        stop_words_path = resources_path + '/stopwords.txt'
        self.stop_words = [line[:len(line) - 1] for line in open(stop_words_path)]
        self.word_vectors =  gensim.models.KeyedVectors.load_word2vec_format(wv_path, binary=False)
        self.load_vars(resources_path)
        self.split_codes_vars_defs()
        print 'Done loading VariableFinder resources.'

    def clean_text(self, r):
        """Return a plain ascii version of the string r."""
        return r.decode('unicode_escape').encode('ascii', 'ignore')

    def load_vars(self, resources_path):
        """Load and index variable and code tables."""
        country_codes_path = resources_path + '/codes/country_codes.csv'
        country_codes = pd.read_csv(country_codes_path)[
            ['name', 'alpha-2', 'alpha-3', 'iso_3166-2', 'region', 'sub-region', 'region-code', 'sub-region-code']]

        crop_codes_path = resources_path + '/codes/crop_codes.csv'
        crop_codes = pd.read_csv(crop_codes_path)[
            ['Code_display', 'Crop_code', 'DSSAT_code', 'APSIM_code', 'Common_name']]

        management_codes_path = resources_path + '/codes/management_codes.csv'
        management_codes = pd.read_csv(management_codes_path)[
            ['Code_Display', 'Code', 'Description']]

        metadata_codes_path = resources_path + '/codes/metadata_codes.csv'
        metadata_codes = pd.read_csv(metadata_codes_path)[
            ['Code_display', 'Code', 'Description']]

        other_codes_path = resources_path + '/codes/other_codes.csv'
        other_codes = pd.read_csv(other_codes_path)[
            ['Variable', 'Code_display', 'Code', 'Description', 'DSSAT code']]

        self.all_codes = []
        for index, row in country_codes.iterrows():
            mp = {}
            # print row
            # print pd.lib.infer_dtype(row['name'])
            # print row['name']
            if str(row['name']).lower() != '':
                mp['name'] = self.clean_text(row['name'])


            if str(row['alpha-2']).lower() != '':
                mp['alpha-2'] = str(row['alpha-2'])

            if str(row['alpha-3']).lower() != '':
                mp['alpha-3'] = str(row['alpha-3'])

            if str(row['iso_3166-2']).lower() != '':
                mp['iso_3166-2'] = str(row['iso_3166-2'])

            if str(row['region']).lower() != '':
                mp['region'] = str(row['region'])

            if str(row['sub-region']).lower() != '':
                mp['sub-region'] = str(row['sub-region'])

            if str(row['region-code']).lower() != '':
                mp['region-code'] = str(row['region-code'])

            if str(row['sub-region-code']).lower() != '':
                mp['sub-region-code'] = str(row['sub-region-code'])

            self.all_codes.append({
                'variable': {'TRIPS': ['LOCATION', 'LOC_AFFECTED']},
                'code': mp,
                'defs': [self.clean_text(row['name'])]
            })

        for index, row in crop_codes.iterrows():
            # ky = {'variable': {'ICASA': row['Code_display']},
            #       'code': {'ICPCR': row['Crop_code'], 'DSSAT': row['DSSAT_code'], 'APSIM': row['APSIM_code']}}
            mp = {}
            # print row.to_dict()
            if str(row['APSIM_code']).lower() != 'nan':
                mp['APSIM'] = str(row['APSIM_code'])


            if str(row['DSSAT_code']).lower() != 'nan':
                mp['DSSAT'] = str(row['DSSAT_code'])

            if str(row['Crop_code']).lower() != 'nan':
                if str(row['Crop_code']) == 'WHT': # not actually ICASA
                    mp['TRIPS'] = str(row['Crop_code'])
                else:
                    mp['ICASA'] = str(row['Crop_code'])

            self.all_codes.append({
                'variable': {'ICASA': row['Code_display']},
                'code': mp,
                'defs': row['Common_name'].split('/')
            })

        # TODO? management, metadata, and other_codes

        #variables
        vars_path = resources_path + '/dsatVars.xlsx'
        variables = pd.read_excel(vars_path)

        self.variables_clusters = []
        for index, row in variables[['ICASA name', 'TRIPS name', 'DSSAT name', 'Definition']].iterrows():
            mp = {}
            if str(row['ICASA name']).lower() != 'nan' and str(row['ICASA name']).lower() != 'na':
                mp['ICASA'] = str(row['ICASA name'])

            if str(row['DSSAT name']).lower() != 'nan' and str(row['DSSAT name']).lower() != 'na':
                mp['DSSAT'] = str(row['DSSAT name'])

            if str(row['TRIPS name']).lower() != 'nan' and str(row['TRIPS name']).lower() != 'na':
                mp['TRIPS'] = str(row['TRIPS name'])
            mp['defs'] = [str(row['Definition'])]
            self.variables_clusters.append(mp)
        
        # EIDOS "ontologies"
        for ont_name in ['fao_variable','topoflow','un','wdi']:
            ont_path = resources_path + '/' + ont_name + '_ontology.yml'
            with open(ont_path) as f:
                yml = safe_load(f)
                for ont_key, ont in yml[0].iteritems():
                    self.append_eidos_var_clusters(ont_key, [], ont)

    def append_eidos_var_clusters(self, ont_key, id_prefix, node):
        """
        Recursively traverse a node of a loaded EIDOS YAML file, adding
        elements to self.variables_clusters for each OntologyNode leaf.
        """
        if isinstance(node, dict):
            if 'OntologyNode' in node:
                leaf_id = '/'.join(id_prefix + [node['name']])
                leaf_descs = None
                if 'descriptions' in node:
                    leaf_descs = node['descriptions']
                elif 'examples' in node:
                    leaf_descs = node['examples']
                else:
                    raise TypeError, "expected OntologyNode to have descriptions or examples, but found neither in " + str(node)
                mp = {}
                mp[ont_key] = leaf_id
                mp['defs'] = leaf_descs
                self.variables_clusters.append(mp)
            else:
                for k, v in node.iteritems():
                    self.append_eidos_var_clusters(ont_key, id_prefix + [k], v)
        elif isinstance(node, list):
            for v in node:
                self.append_eidos_var_clusters(ont_key, id_prefix, v)
        else:
            raise TypeError, "expected dict or list, but got " + str(node)

    def find_replacement(self, word):
        """
        Find a replacement for word among its WordNet hypernyms (including
        itself) such that the replacement is represented in self.word_vectors.
        """
        if word in self.word_vectors:
            return word

        all_syns = wn.synsets(word)
        syns = all_syns[:]
        for s in all_syns:
            for h in s.hypernyms():
                syns.append(h)

        found = False
        res = ''
        for syn in syns:
            if found:
                break
            for l in syn.lemma_names():
                if l in self.word_vectors:
                    res = l
                    found = True
                    break
        if found:
            return res

        return None

    def tokenize_sentence(self, sent):
        """
        Return a list of words represented in self.word_vectors from the given
        sentence.
        """
        delimiters = "/", ",", "\n", ";", " "
        regexPattern = '|'.join(map(re.escape, delimiters))
        split = re.split(regexPattern, sent.lower().replace('(', ' ').replace(')', ' '))
        split_no_stops = filter(lambda x: x not in self.stop_words, split)
        res = []
        for w in split_no_stops:
            # print w
            rep = self.find_replacement(w)
            if rep is not None:
                res.append(rep)
        return res

    def sent_similarity(self, sent1, sent2):
        """
        Compute similarity of two sentences based on word vector similarity.
        Sentences with no (non-stop) words are infinitely dissimilar to any
        sentence (including other empty sentences).
        """
        sim = 0
        if len(sent1) == 0 or len(sent2) == 0:
            sim = -float('inf')
        else:
            sim = self.word_vectors.n_similarity(sent1, sent2)
        # print sent1, sent2, sim
        return sim

    def get_sims(self, target_sentence_split, database):
        """
        Compute similarity of target_sentence_split to each definition in
        database, based on word vector similarity, and take
        the average for each cluster. Return a list of database entries with
        average similarity scores added in the score field.
        """
        results = []
        for entry in database:
            defs_split = entry['defs_split']
            if len(defs_split) > 0:
                r = entry.copy()
                r['score'] = np.average([self.sent_similarity(d, target_sentence_split) for d in defs_split])
                results.append(r)
        return results

    def match_literal(self, target, database, thresh):
        """
        Compute edit-distance similarity ratios between target and each of the
        defs in the database, and return a list of database entries for which
        the ratio is above thresh, with the ratio added in the score field.
        """
        res = []
        for entry in database:
            for d in entry['defs']:
                r = Levenshtein.ratio(target.lower(), str(d).lower())
                if r >= thresh:
                    re = entry.copy()
                    re['score'] = float(r)
                    res.append(re)
        return res

    def match_defs(self, target, database, exact_match_thresh, top_n):
        """
        Return the top top_n matches for target among the definitions in
        database, using either scoring method (edit distance or word vectors).
        Edit distance scores must be above exact_match_thresh.
        """
        target_tokenized = self.tokenize_sentence(target)
        lit_results = self.match_literal(target, database, exact_match_thresh)
        dist_results = self.get_sims(target_tokenized, database)
        all_results = dist_results + lit_results
        sorted_results = sorted(all_results, key=lambda entry: float(entry['score']), reverse=True)
        return sorted_results[:top_n]

    def split_codes_vars_defs(self):
        """Tokenize code/var definitions."""
        for c in self.variables_clusters:
            c['defs_split'] = \
                filter(lambda l: len(l) > 0,
                       [self.tokenize_sentence(d) for d in c['defs']])
        for c in self.all_codes:
            c['defs_split'] = \
                filter(lambda l: len(l) > 0,
                       [self.tokenize_sentence(d) for d in c['defs']])

    def find_variables(self, target, exact_match_thresh=0.95, top_n=1):
        """Return the top top_n matches for target among variable defs."""
        return self.match_defs(target, self.variables_clusters, exact_match_thresh, top_n)

    def find_codes(self, target, exact_match_thresh=0.95, top_n=1):
        """Return the top top_n matches for target among code defs."""
        return self.match_defs(target, self.all_codes, exact_match_thresh, top_n)

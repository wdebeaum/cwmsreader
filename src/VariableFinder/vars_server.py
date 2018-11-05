#!/usr/bin/python2.7

import sys, os, re
from TripsModule import TripsModule
from KQML.kqml_performative import KQMLPerformative
from KQML.kqml_list import KQMLList
from variables import WMVariable

class VariableFinder(TripsModule):

    def __init__(self, argv):
        TripsModule.__init__(self, argv)

    def init(self):
        self.name = "VariableFinder"
        TripsModule.init(self)
        self.send(KQMLPerformative.from_string("(subscribe :content (request &key :content (find-var . *)))"))
        self.send(KQMLPerformative.from_string("(subscribe :content (request &key :content (find-code . *)))"))
        self.trips_base = os.environ['TRIPS_BASE']
        resources_path = self.trips_base + '/etc/VariableFinder/resources'
        self.var_tagger = WMVariable(resources_path=resources_path)
        self.ready()

    def try_parse_float(self, value):
        """Return the float value of value, or None if it's not parseable."""
        try:
            variable = float(value)
        except ValueError:
            variable = None
        return variable

    def try_parse_int(self, value):
        """Return the int value of value, or None if it's not parseable."""
        try:
            variable = int(value)
        except ValueError:
            variable = None
        return variable

    def receive_tell(self, msg, content):
        if not isinstance(content, KQMLList):
            self.error_reply(msg, "expected :content to be a list")
            return
        verb = content[0].to_string().lower()
        if verb == "i-am-here":
            if self.iam_here_flag:
                rep_msg = KQMLPerformative.from_string(self.definition_message)
                self.iam_here_flag = False
                self.reply(msg, rep_msg)
        return

    def parse_args(self, content):
        """
        Parse arguments for a find-* request and return them as multiple
        values: phrase, top-n, l-thresh.
        """
        top_n = content.get(':top-n') # number of results to return
        literal_t = content.get(':l-thresh') # Levenshtein score threshold
        phrase = content.get(':phrase') # phrase to match against
        top = None
        thresh = None
        if phrase is not None and isinstance(phrase, KQMLList):
            phrase = phrase.to_string()[1: len(phrase.to_string()) - 1]
        if top_n is not None:
            top = self.try_parse_int(top_n.to_string())
        if literal_t is not None:
            thresh = self.try_parse_float(literal_t.to_string())
        return phrase, top, thresh

    def kqml_normalize(self, s):
        """
        Remove characters that shouldn't be in a KQML/Lisp symbol. See also
        $symbol_component_re in KQML.pm.
        """
        return re.sub(r"[,'`\"#\(\):\|\\]", '', re.sub(r"\s", '_', s))

    def create_result(self, result, is_code, msg):
        """
        Reply to msg with results from result; is_code indicates this was a
        find-code request; otherwise it's a find-var request.
        """
        reply_content = KQMLList('ANSWER')
        for ky_mp in result:
            score = ky_mp['score']
            variable = KQMLList()
            match = KQMLPerformative('match')
            if is_code:
                for k, v in ky_mp['variable'].iteritems():
                    kqml_v = v
                    if isinstance(v, str):
                        kqml_v = self.kqml_normalize(v)
                    elif isinstance(v, list):
                        kqml_v = KQMLList([self.kqml_normalize(e) for e in v])
                    variable.add(KQMLList([self.kqml_normalize(k), kqml_v]))
                code = KQMLList()
                for k, v in ky_mp['code'].iteritems():
                    code.add(KQMLList([self.kqml_normalize(k), self.kqml_normalize(v)]))
                match.set(':code', code)
            else:
                for k in ['DSSAT', 'ICASA', 'TRIPS', 'FAO', 'TOPOFLOW', 'UN', 'WDI']:
                    if k not in ky_mp:
                        continue
                    v = ky_mp[k]
                    variable.add(KQMLList([self.kqml_normalize(k), self.kqml_normalize(v)]))
            match.set(':variable', variable)
            match.set(':score', score)
            reply_content.add(match)
        reply_msg = KQMLPerformative("reply")
        reply_msg.set(":content", reply_content)
        self.reply(msg, reply_msg)

    def receive_request(self, msg, content):
        if not isinstance(content, KQMLList):
            self.error_reply(msg, "expected :content to be a list")
            return
        verb = content[0].to_string().lower()
        if verb == "restart":
            self.iam_here_flag = True
            self.send(KQMLPerformative.from_string("(request :content (ARE-YOU-THERE :who CWMSAGENT)))"))
            return
        if verb == "find-var":
            phrase, top, thresh = self.parse_args(content)
            if phrase is None:
                self.error_reply(msg, 'There should be a :phrase slot')
                return
            if top is None:
                top = 1
            if thresh is None:
                thresh = 0.95
            results = self.var_tagger.find_variables(phrase, thresh, top)
            self.create_result(results, is_code=False, msg=msg)

        elif verb == "find-code":
            phrase, top, thresh = self.parse_args(content)
            if phrase is None:
                self.error_reply(msg, 'There should be a :phrase slot')
                return
            if top is None:
                top = 1
            if thresh is None:
                thresh = 0.95
            results = self.var_tagger.find_codes(phrase, thresh, top)
            self.create_result(results, is_code=True, msg=msg)

        else:
            self.error_reply(msg, "unknown request verb " + verb)

        return

if __name__ == "__main__":
    VariableFinder(sys.argv[1:]).run()

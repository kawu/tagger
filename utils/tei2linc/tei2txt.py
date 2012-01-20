import sys
import os
import shutil
import argparse
import tarfile
import re
from collections import defaultdict

class Lexem:

    def __init__(self, ctag=None, base=None, msds=None):
        self.ctag = ctag
        self.base = base
        self.msds = msds if msds is not None else set()

    def add(self, msd):
        self.msds.add(msd)


class Disamb:

    def __init__(self, ctag, base, msd):
        self.ctag = ctag
        self.base = base if base.strip() else None
        self.msd = msd

    def label(self):
        return (self.ctag + ":" + self.msd).strip(":")


class Segment:

    def __init__(self, orth=None, nps=False, disamb=None, lexs=None):
        self.orth = orth
        self.nps = nps
        self.disamb = disamb
        self.lexs = lexs if lexs is not None else []

    def append(self, lex):
        self.lexs.append(lex)

    def label(self):
        return self.disamb.label()

    def labels(self):
        return set([
            (lex.ctag + ":" + msd).strip(":")
            for lex in self.lexs
            for msd in lex.msds
            ])


SYMBOL = re.compile('<symbol .*? ?value="(.*?)"')
STRING = re.compile('<string>(.*?)</string>')
BINARY = re.compile('<binary .*? ?value="(.*?)"')

CTAGS = set(["adja", "adjp", "adjc", "conj", "comp", "interp", "pred",
    "xxx", "adv", "imps", "inf", "pant", "pcon", "qub", "prep",
    "siebie", "subst", "depr", "ger", "ppron12", "ppron3", "num",
    "numcol", "adj", "pact", "ppas", "winien", "praet", "bedzie",
    "fin", "impt", "aglt", "ign", "brev", "burk", "interj"])

def parse_disamb(disamb):
    k = 0
    disamb = list(reversed(disamb.split(":")))
    for x in disamb:
        if x.strip() in CTAGS:
            break
        k += 1
    ctag = disamb[k].strip()
    base = ":".join(reversed(disamb[k+1:]))
    msd = ":".join(reversed([x.strip() for x in disamb[:k]]))
    # return ":".join(reversed(result)).strip(": ")
    return Disamb(ctag, base, msd)

def value(line, regex):
    match = regex.search(line)
    return match.group(1) if match else None

def make_args_parser():
    parser = argparse.ArgumentParser(
            description="Convert TEI corpus to raw text.")
    parser.add_argument("tei",
            help="TEI corpus compressed as a tar archive file")
    return parser

def parse_args(parser):
    args = parser.parse_args()
    return args

def morph_files_in_tar(tar):
    for member in tar:
        if "ann_morphosyntax.xml" in member.name:
            yield member

def _parse_morph(f):
    felem = re.compile('<f name="(.*?)"')
    seg = []
    sent = []
    for line in f:
        line = line.strip()
        if "</s>" in line:
            yield sent
            sent = []
        if "</seg>" in line:
            sent.append(seg)
            seg = []
        match = felem.search(line)
        if match != None:
            inside = match.group(1)
        if line.startswith("<string"):
            seg.append((inside, value(line, STRING)))
        elif line.startswith("<symbol"):
            seg.append((inside, value(line, SYMBOL)))
        elif line.startswith("<binary"):
            seg.append((inside, value(line, BINARY)))

def parse_morph(f):
    for sent_info in _parse_morph(f):
        sent = []
        for k, seg_info in enumerate(sent_info):
            for (tp, val) in seg_info:
                if tp == "orth":
                    seg = Segment(orth=val)
                elif tp == "nps" and val == "true":
                    seg.nps = True
                elif tp == "base":
                    lex = Lexem(base=val)
                    seg.append(lex)
                elif tp == "ctag":
                    lex.ctag = val
                elif tp == "msd":
                    lex.add(val.strip(": "))
                    # interp = (ctag + ":" + val).strip(": ")
                    # interps.append(interp)
                elif tp == "interpretation":
                    seg.disamb = parse_disamb(val)
            # interps = list(set(interps))
            # sent.append((orth, disamb, interps))
            assert seg.label() in seg.labels()
            # print [msd for lex in seg.lexs for msd in lex.msds]
            sent.append(seg)
        yield sent

def print_sent(sent):
    s = [("" if seg.nps else " ") + seg.orth for seg in sent]
    print ''.join(s).strip()

if __name__ == "__main__":
    parser = make_args_parser()
    args = parse_args(parser)

    tar = tarfile.open(args.tei)
    for morph in morph_files_in_tar(tar):
        for sent in parse_morph(tar.extractfile(morph)):
            print_sent(sent)
    tar.close()

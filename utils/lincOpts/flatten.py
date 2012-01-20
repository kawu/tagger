import sys
import re

if len(sys.argv) != 2:
    print "usage: %s INPUT" % sys.argv[0]
    sys.exit()

class Word:
    def __init__(self, orth, nps, disamb, interps):
        self.orth = orth
        self.nps = nps
        self.disamb = disamb
        self.interps = sorted(interps)

WORD_LINE = re.compile("\s*(Word|Orth|-|\*)")
ORTH = re.compile("\s*Orth\s*(\".*\")")

def line_type(line, ltype):
    if line.strip().startswith(ltype):
        return True
    return False

def parse_word(word_str):
    orth = None
    nps = False
    disamb = None
    interps = set()
    for line in word_str:
        if line_type(line, "Orth"):
            orth = ORTH.match(line).group(1)
        elif line_type(line, "Word"):
            if "Nps" in line:
                nps = True
        elif line_type(line, "*") or line_type(line, "-"):
            interp = line.split()[1].strip()
            if line_type(line, "*"):
                disamb = interp
            interps.add(interp)
    return Word(orth, nps, disamb, interps)

def lines(inp):
    for line in inp:
        yield line.strip()

def group_by(lines, pref):
    group = None
    for line in lines:
        if line.strip().startswith(pref):
            if group:
                yield group
            group = []
        if group is not None:
            group.append(line)
    if group:
        yield group

def parse(path):
    with open(path) as inp:
        for sent_part in group_by(lines(inp), "Sent"):
            yield [ parse_word(word) for word
                    in group_by(sent_part, "Word") ]

for sent in parse(sys.argv[1]):
    print "Sent"
    for word in sent:
        print " " + word.orth + (" Nps" if word.nps else "")
        for interp in word.interps:
            if interp == word.disamb:
                print "  *", interp
            else:
                print "  -", interp

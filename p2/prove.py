# -*- coding: utf-8 -*-

"""
This script is part of the paper available at
https://github.com/csirmaz/AlgebraicSyncPaper/tree/master/p2

The statements are tested on a minimal model of a filesystem that includes information
about two nodes, their immediate surroundings that determine the tree-property
the filesystem needs to satisfy, and the relationship between the nodes.

Copyright (c) 2018 Elod Pal Csirmaz

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
"""

from itertools import chain
import sys

# Whether directories have equal contents, i.e. there is only a single
# directory-type value (object).
ONE_DIRECTORY_VALUE = True

DEBUG = False

# Types
EMPTY = 'Empty'
FILE = 'File'
DIR = 'Dir'

# Relationships
SAME = 'Same'
DIRECT = 'Direct'
DISTANT = 'Distant'
SEPARATE = 'Separate'

# Paths
PATH1 = 'Path1' # If they are comparable, this is the ancestor
PATH2 = 'Path2' # If they are comparable, this is the descendant

class Node:
    
    def __init__(self, type=EMPTY, value='Unknown', has_children=False, is_parent_dir=False):
        self.type = type
        self.value = value
        
        # Do not query the following flags. These are initial values only that are
        # used subject to the actual values of the nodes. Query the filesystem instead.
        self._has_children = has_children
        self._is_parent_dir = is_parent_dir

    def clone(self):
        return self.__class__(self.type, self.value, self._has_children, self._is_parent_dir)

    def is_content_same(self, other):
        if self.type != other.type: return False
        if self.type == EMPTY: return True
        if self.type == DIR and ONE_DIRECTORY_VALUE: return True
        return (self.value == other.value)


def NodeFactory(value='Unknown'):
    for type in (EMPTY, FILE, DIR):
        for has_children in (False, True):
            for is_parent_dir in (False, True):
                yield Node(type, value, has_children, is_parent_dir)


class Filesystem:

    def __init__(self, node1, node2, relationship, broken=False):
        self.node1 = node1 # If they are comparable, this is the ancestor
        self.node2 = node2 # If they are comparable, this is the descendant
        self.relationship = relationship
        self.broken = broken

    def clone(self):
        return self.__class__(self.node1.clone(), self.node2.clone(), self.relationship, self.broken)
        
    def node_info(self, path):
        (node, npath) = self.get_node_path(path)
        c = self.has_children(npath)
        return ("d-" if self.is_parent_dir(npath) else "") + node.type + "(" + node.value + ")" + ("-o" if c == 1 else ("-oo" if c == 2 else ""))

    def info(self):
        if self.broken:
            return 'Broken';
        if self.relationship == SAME:
            return self.node_info(PATH1) + " <SAME>"
        return self.node_info(PATH1) + " >" + self.relationship + "> " + self.node_info(PATH2)

    def get_node_path(self, path):
        if self.relationship == SAME or path == PATH1: return (self.node1, PATH1)
        return (self.node2, PATH2)
    
    def has_children(self, path):
        """ Returns if the node at the path has no non-empty children (0) or as some non-empty childrent (1 or 2).
            Returns 2 if the ancestor node (path1) has non-empty children in addition to the one related to the descendant
            (path2)."""
        if self.relationship == SAME:
            return self.node1._has_children + 0
        
        if self.relationship == SEPARATE:
            if path == PATH1:
                return self.node1._has_children + 0
            else:
                return self.node2._has_children + 0

        if self.relationship == DIRECT:
            if path == PATH1: # the parent
                # The parent's _has_children flag is used in addition to the child (node2)
                # to note if the parent has more non-empty children
                return ((self.node2.type != EMPTY) + self.node1._has_children)
            else: # the child
                return self.node2._has_children + 0
            
        if self.relationship == DISTANT:
            if path == PATH1: # the ancestor
                # The ancestor's _has_children flag is used in addition to its child that is the ancestor of node2
                # to note if the parent has more non-empty children
                return ((self.node2.type != EMPTY) + self.node1._has_children)
            else: # the descendant
                return self.node2._has_children + 0

    def is_parent_dir(self, path):
        """ Returns whether there is a directory at the parent of the node at path (1) or not (0).
            Returns False if the underlying _is_parent_dir flag has a non-permitted value. """
        if self.relationship == SAME:
            return self.node1._is_parent_dir + 0
        
        if self.relationship == SEPARATE:
            if path == PATH1:
                return self.node1._is_parent_dir + 0
            else:
                return self.node2._is_parent_dir + 0
            
        if self.relationship == DIRECT:
            if path == PATH1: # the parent
                return self.node1._is_parent_dir
            else: # the child
                return (self.node1.type == DIR) + 0
            
        if self.relationship == DISTANT:
            if path == PATH1: # the ancestor
                return self.node1._is_parent_dir + 0
            else: # the descendant
                # The _is_parent_dir flag is used to note whether the descendant (path2) has a directory
                # at its parent, which may still be false even if there is a directory at the ancestor (path1).
                # However, if the ancestor does not have a directory, _is_parent_dir should not be set at all
                # as otherwise changing the ancestor to a directory would suddenly make the descendant have
                # a directory parent, too. To note this, we return False.
                if self.node1.type != DIR and self.node2._is_parent_dir: return False
                return (self.node1.type == DIR and self.node2._is_parent_dir) + 0

    def is_same(self, other):
        if self.broken and other.broken:
            return True

        if not (self.relationship == other.relationship and self.broken == other.broken):
            return False

        for path in (PATH1, PATH2):
            (self_node, self_npath) = self.get_node_path(path)
            (other_node, other_npath) = other.get_node_path(path)

            if not self_node.is_content_same(other_node): return False
            if self.has_children(self_npath) != other.has_children(other_npath): return False
            if self.is_parent_dir(self_npath) != other.is_parent_dir(other_npath): return False

        return True

    def is_extended_by(self, other):
        """ Returns True or False """
        if self.broken: return True
        return self.is_same(other)

    def has_tree_property(self):
        """ Returns True or False """
        for path in (PATH1, PATH2):
            (node, npath) = self.get_node_path(path)
            # Non-empty nodes must have directories as parents
            if node.type != EMPTY:
                if not self.is_parent_dir(npath): return False
            # Nodes with childrent must be directories
            if self.has_children(npath):
                if node.type != DIR: return False
            # Cannot have a potential directory parent (see above).
            # This way we filter these filesystems out in FilesystemFactory.
            if self.is_parent_dir(npath) is False: return False
        return True

    def apply_command(self, command):
        """ Clones the filesystem and applies command to it. Returns the new filesystem. """
        fs = self.clone()
        (node, npath) = fs.get_node_path(command.path)
        if DEBUG: print("Apply command " + command.info() + " to " + npath + "\n  Filesystem: " + fs.info())
        if node.type != command.intype:
            fs.broken = True
            if DEBUG: print("  FS broken due to intype mismatch")
            return fs
        node.type = command.outtype
        node.value = command.outvalue
        if not fs.has_tree_property():
            fs.broken = True
            if DEBUG: print("  FS broken due to tree property violation")
            return fs
        if DEBUG: print("  Result: " + fs.info())
        return fs
    
    def apply_commandpair(self, commandpair):
        """ Clones and applies a pair of commands """
        return self.apply_command(commandpair.first).apply_command(commandpair.last)


def FilesystemFactory(relationship):
    for node1 in NodeFactory('Old1'):
        for node2 in NodeFactory('Old2'):
            fs = Filesystem(node1, node2, relationship)
            if fs.has_tree_property():
                yield fs


class Command:
    
    def __init__(self, intype, outtype, outvalue, path):
        self.intype = intype
        self.outtype = outtype
        self.outvalue = outvalue
        self.path = path

    def info(self):
        return "<" + ",".join((self.intype,self.outtype,self.outvalue,self.path)) + ">"

    def is_empty_empty(self):
        return (self.intype == EMPTY and self.outtype == EMPTY)

    def is_dir_dir(self):
        return (self.intype == DIR and self.outtype == DIR)

    def is_assertion(self):
        if self.is_empty_empty(): return True
        if self.is_dir_dir() and ONE_DIRECTORY_VALUE: return True
        return False


def CommandFactory(path, outvalue):
    for intype in (EMPTY, FILE, DIR):
        for outtype in (EMPTY, FILE, DIR):
            yield Command(intype, outtype, outvalue, path)


class CommandPair:
    
    def __init__(self, first, last):
        self.first = first
        self.last = last
        
    def info(self):
        return self.first.info() + self.last.info()
    
    def get_reverse(self):
        return self.__class__(self.last, self.first)
    
    def can_get_singlecommand(self):
        """ Returns whether the output type of the first command
        matches the input type of the second. Only meaningful
        if the relationship between the paths is SAME."""
        return (self.first.outtype == self.last.intype)
    
    def get_singlecommand(self):
        """ Returns a single command that could replace the two
        commands. Only meaningful if the relationship between
        the paths is SAME. """
        return Command(
            self.first.intype,
            self.last.outtype,
            self.last.outvalue,
            self.first.path
        )
    
    def get_commands_on_ancestor_descendant(self):
        """ Returns the command on the ancestor and the command on
        the descendant node in this order. """
        if self.first.path == PATH1:
            return (self.first, self.last)
        else:
            return (self.last, self.first)


def CommandPairFactory():
    for (firstpath, secondpath) in ((PATH1, PATH2), (PATH2, PATH1)):
        for firstcommand in CommandFactory(firstpath, 'New1'):
            for secondcommand in CommandFactory(secondpath, 'New2'):
                yield CommandPair(firstcommand, secondcommand)


def pr(s):
    """Print to STDOUT (without newline) and flush"""
    sys.stdout.write(s)
    sys.stdout.flush()


count_tests = 0
count_ok = 0
count_fail = 0
count_errors = 0
testname_current = None

def begintest(testname, label):
    """Print the rule name being tested"""
    global count_tests, testname_current, count_errors
    count_tests += 1
    if not testname_current is None:
        pr("ERROR: {} did not conclude\n".format(testname_current))
        count_errors += 1
    testname_current = testname
    pr(label + ": ")

def fail(testname):
    global count_fail, testname_current, count_errors
    count_fail += 1
    if testname_current != testname:
        pr("ERROR while failing test {} vs {}\n".format(testname, testname_current))
        count_errors += 1
    testname_current = None    
    pr("FAIL\n")
    
def ok(testname):
    global count_ok, testname_current, count_errors
    count_ok += 1
    if testname_current != testname:
        pr("ERROR while OKing test {} vs {}\n".format(testname, testname_current))
        count_errors += 1
    testname_current = None    
    pr("Ok\n")

def conclude():
    global count_tests, count_fail, count_ok
    if count_ok + count_fail != count_tests:
        pr("ERROR: fail() and ok() calls do not match the number of tests\n")
    if count_errors > 0:
        pr("ERROR: some errors occurred\n")
    if count_fail > 0:
        pr("ERROR: some tests failed\n")
    pr("Done.\n")


begintest('R1', 'Rule 1')
# Commands on incomparable nodes commute
for sq in CommandPairFactory():
    sq_rev = sq.get_reverse()
    for fs in FilesystemFactory(SEPARATE):
        if DEBUG: print("---")
        fs_res = fs.apply_commandpair(sq)
        fs_rev_res = fs.apply_commandpair(sq_rev)
        if not fs_res.is_same(fs_rev_res):
            if DEBUG: print("Filesystem: " + fs.info())
            if DEBUG: print("Sequence: " + sq.info())
            if DEBUG: print("Result: " + fs_res.info())
            if DEBUG: print("Reverse: " + sq_rev.info())
            if DEBUG: print("Result: " + fs_rev_res.info())
            break # fail
    else:
        continue # trick to achieve break(2)
    fail('R1')
    break
else:
    ok('R1')


begintest('R2', 'Rule 2')
# Commands on incomparble nodes do not break all filesystems
for sq in CommandPairFactory():
    for fs in FilesystemFactory(SEPARATE):
        fs_res = fs.apply_commandpair(sq)
        if not fs_res.broken:
            break # OK
    else:
        continue # trick to achieve break(2)
    ok('R2')
    break
else:
    fail('R2')


begintest('R3', 'Rule 3')
# Commands on the same node break every filesystem if their types are incompatible
for sq in CommandPairFactory():
    if sq.can_get_singlecommand():
        continue # skip
    for fs in FilesystemFactory(SAME):
        if DEBUG: print("---")
        fs_res = fs.apply_commandpair(sq)
        if not fs_res.broken:
            if DEBUG: print("Filesystem: " + fs.info())
            if DEBUG: print("Sequence: " + sq.info())
            if DEBUG: print("Result: " + fs_res.info())
            break # fail
    else:
        continue # trick to achieve break(2)
    fail('R3')
    break
else:
    ok('R3')


begintest('R4', 'Rule 4')
# Commands on the same node simplify into an empty sequence
for sq in CommandPairFactory():
    if not sq.can_get_singlecommand():
        continue # skip

    singlecommand = sq.get_singlecommand()
    
    if not(singlecommand.is_assertion()):
        continue # skip

    for fs in FilesystemFactory(SAME):
        if DEBUG: print("---")
        fs_res = fs.apply_commandpair(sq)
        if not fs_res.is_extended_by(fs):
            if DEBUG: print("Filesystem: " + fs.info())
            if DEBUG: print("Sequence: " + command_sq_info(sq))
            if DEBUG: print("Result: " + fs_res.info())
            if DEBUG: print("SingleCommand: " + singlecommand.info())
            break # fail
    else:
        continue # trick to achieve break(2)
    fail('R4')
    break
else:
    ok('R4')


begintest('R5', 'Rule 5')
# Commands on the same node simplify into one command
for sq in CommandPairFactory():
    if not sq.can_get_singlecommand():
        continue # skip
    
    singlecommand = sq.get_singlecommand()
    
    if singlecommand.is_assertion():
        continue # skip

    for fs in FilesystemFactory(SAME):
        if DEBUG: print("---")
        fs_res = fs.apply_commandpair(sq)
        fs_single = fs.apply_command(singlecommand)
        if not fs_res.is_same(fs_single):
            if DEBUG: print("Filesystem: " + fs.info())
            if DEBUG: print("Sequence: " + sq.info())
            if DEBUG: print("Result: " + fs_res.info())
            if DEBUG: print("SingleCommand: " + singlecommand.info())
            if DEBUG: print("Result: " + fs_single.info())
            break # fail
    else:
        continue # trick to achieve break(2)
    fail('R5')
    break
else:
    ok('R5')


begintest('R6', 'Rule 6')
# Commands on distant relatives break all filesystems
for sq in CommandPairFactory():
    (command_on_ancestor, command_on_descendant) = sq.get_commands_on_ancestor_descendant()
    if command_on_ancestor.is_dir_dir() or command_on_descendant.is_empty_empty(): continue # skip

    for fs in FilesystemFactory(DISTANT):
        if DEBUG: print("---")
        fs_res = fs.apply_commandpair(sq)
        if not fs_res.broken:
            if DEBUG: print("Filesystem: " + fs.info())
            if DEBUG: print("Sequence: " + sq.info())
            if DEBUG: print("Result: " + fs_res.info())
            break # fail
    else:
        continue # trick to achieve break(2)
    fail('R6')
    break
else:
    ok('R6')





conclude()

exit(0)

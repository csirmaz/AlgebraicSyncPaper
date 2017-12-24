# -*- coding: utf-8 -*-

from itertools import chain
import sys

"""
This script is part of the paper available at
https://github.com/csirmaz/AlgebraicSyncPaper/tree/master/p2

It is used to test pairs of commands on filesystems and determine
(1) if they break all filesystems, 
(2) or they are equivalent to (or extended by) the empty function
(3) or they are equivalent to (or extended by) a single command
(4) or they are equivalent to (or extended by) the same commands applied in reverse order.

The commands are tested on a minimal model of a filesystem that includes information
about two nodes, their immediate surroundings that determine the tree-property
the filesystem needs to satisfy, and the relationship between the nodes.

Copyright (c) 2017 Elod Pal Csirmaz

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

"""
The script considers the following possible relationships between the nodes (paths)
to which the commands are applied:
"""

# Constants for Filesystem.rel and CommandPair.rel:
DISTANT_PARENT     = 'DistantParent'     # path p2 is an ancestor of path p1 but not its parent
DIRECT_PARENT      = 'DirectParent'      # p2 is the parent of p1 and p1 is not the only child
DIRECT_PARENT_ONLY = 'DirectParentOnly'  # p2 is the parent of p1 and p1 is the only child
DISTANT_CHILD      = 'DistantChild'      # p2 is the descendant of p1 but not its child
DIRECT_CHILD       = 'DirectChild'       # p2 is the child of p2 and p2 is not the only child
DIRECT_CHILD_ONLY  = 'DirectChildOnly'   # p2 is the child of p1 and p2 is the only child
SAME               = 'Same'              # two paths are the same (used for command pairs)
SEPARATE           = 'Separate'          # all other cases

"""
For each relationship, the script outputs 4 matrices according to the four properties
to investigate. In each matrix, the rows represent the first command, and columns
represent the second command. The notation for the commands is derived from the paper,
and the first character notes the input type, while the second character notes the
output type:
b - Empty
F - File
D - Directory

Each cell in the matrix has one of the following values:
BR - the command pair breaks all filesystems (for (1) and (3))
== - the pair is equivalent to the empty sequence, single command or reversed pair
[[ - the pair is extended by the empty sequence, single command or reversed pair
.. - none of the above is true    

Finally, the script prints rules for substituting command pairs with single commands.
"""

# CONFIGURATION STARTS

# Whether directories have equal contents, i.e. there is only a single
# directory-type value (object).
ONE_DIRECTORY_VALUE = True

# Whether a command pairs should be tested not only using the relationship 
# between the nodes (paths) they specify,
# but a DISTANT relationship should also be tested on DIRECT and DIRECT_ONLY,
# and a rule using a DIRECT relationship should also be tested on DIRECT_ONLY.
RULE_RELATIONSHIPS_INCLUSIVE = False

# Increase the debug level to have results of internal calculations printed
DEBUG = 0

# Style of info() methods: 'normal or 'debug'
STYLE = 'normal' # TODO

# CONFIGURATION ENDS

# Constants for Content.type:
DIR   = 'Dir'
FILE  = 'File'
EMPTY = 'Empty'

# Display strings
DISPLAY = {
    'ContentValue': {
        EMPTY: '⊖', # 'b'
        FILE:  'F',
        DIR:   'D'
    },
    'Node_has_parent': '⁌', # 'o--',
    'Node_has_child': '⁍', #  '--o',
    'Node_has_no_parent': '.',
    'Node_has_no_child': '.',
    'FileSystemRel': { # arrow is always pointing to right as we swap nodes if needed
        SEPARATE:           '≀≀', # '=x=',
        DISTANT_CHILD:      '>=>',
        DIRECT_CHILD:       '=<>',
        DIRECT_CHILD_ONLY:  '⇾', # '=>>',
        SAME:               '≅' # '-=-'
    },
    'CommandPairRel': {
        SEPARATE:           '≀≀', # '-x-'
        DISTANT_CHILD:      '>->',
        DIRECT_CHILD:       '-<>',
        DIRECT_CHILD_ONLY:  '->>',
        DISTANT_PARENT:     '<-<',
        DIRECT_PARENT:      '<>-',
        DIRECT_PARENT_ONLY: '<<-',
        SAME:               '≅', # '---'
    },
    'FS_broken':    '⊥', # '[Broken]'
    'Res_broken':   '⊥  ', # 'BR ',
    'Res_equiv':    '≡  ', # '== ',
    'Res_equiv_s':  '≡',   # '==',
    'Res_extend':   '⊑  ', # '[[ ',
    'Res_extend_s': '⊑',   # '[[',
    'Res_nothing':  '.  ', # '.. '
}


class Content:
    """Represents some value (object; element of set V) at a given node (path) in the filesystem
    
    Private properties:
        type (enum): DIR or FILE or EMPTY
        invalue (str): an arbitrary string representing contents and metadata

    """

    def __init__(self, type=EMPTY, invalue='Unknown'):
        """Constructor
        
        Args:
            type (Optional[enum]): see above
            value (Optional[str]): see above
        
        """
        self.type = type
        self.invalue = invalue
 
    def clone(self):
        """Returns a deep clone of the object."""
        return self.__class__(self.type, self.invalue)

    def info(self, addvalue=True):
        """Returns human-readable information about the object.
        
        Args:
            addvalue (bool): whether to add a description of the invalue
        
        """
        r = DISPLAY['ContentValue'][self.type]
        if STYLE == 'debug' or (addvalue and not self.isEmpty() and not(ONE_DIRECTORY_VALUE and self.isDir())):
            r += '(' + self.invalue + ')'
        return r
    
    def label(self):
        """Returns a short string describing some parts of the object."""
        return DISPLAY['ContentValue'][self.type]
        
    def isSame(self, content):
        """Returns whether the object is the same as another content object."""
        if self.type != content.type: return False
        if self.isEmpty(): return True
        if ONE_DIRECTORY_VALUE and self.isDir(): return True
        return (self.invalue == content.invalue)
        
    def getType(self):
        return self.type
    
    def getValue(self):
        if self.isEmpty(): return 'EmptyValue'
        return self.invalue
        
    def isDir(self):
        """Returns whether the type of the object is 'diretory'."""
        return (self.type == DIR)

    def isFile(self):
        """Returns whether the type of the object is 'file'."""
        return (self.type == FILE)

    def isEmpty(self):
        """Returns whether the type of the object is 'empty'."""
        return (self.type == EMPTY)


def ContentFactory(invalue='Unknown'):
    """Generates all possible contents.
    
    Args:
        invalue (Optional[str]): the invalue of the content objects (where relevant)
        
    Yields:
        All possible content objects
        
    """
    yield Content(EMPTY)
    yield Content(FILE, invalue)
    yield Content(DIR, invalue)


class Node:
    """Represents a node (path) in the filesystem and information about its environment

    Private properties:    
        has_parent (bool): whether the node has a parent (i.e. there is a directory at the parent node)
        content (Content): a content object
        has_child (bool): whether the node has a child (i.e. one of the child nodes is not empty)
        broken (None|str): None if the filesystem is not broken at this node;
            the reason for being broken otherwise

    """

    def __init__(self, has_parent=False, content=None, has_child=False, broken=False):
        """Constructor.

        Args:
            has_parent (Optional[bool]):
            content (Optional[Content]):
            has_child (Optional[bool]):
            broken (Optional[None|str]):

        """
        self.has_parent = has_parent
        self.content = Content() if content is None else content
        self.has_child = has_child
        self.broken = 'Constructor' if broken else None
        self.checkTreeProperty()
        
    def clone(self):
        """Returns a deep clone of the object."""
        return self.__class__(self.has_parent, self.content.clone(), self.has_child, self.broken)
        
    def info(self):
        """Returns a human-readable string describing the object."""
        r = []
        if self.isBroken():
            return "(" + DISPLAY['FS_broken'] + ":" + self.broken + ")"
        r.append(DISPLAY['Node_has_parent' if self.has_parent else 'Node_has_no_parent'])
        r.append(self.content.info())
        r.append(DISPLAY['Node_has_child' if self.has_child else 'Node_has_no_child'])
        return "(" + "".join(r) + ")"
        
    def isSame(self, node):
        """Returns whether the object is the same as another node object."""
        if self.isBroken() and node.isBroken(): return True
        if self.isBroken() or node.isBroken(): return False
        return (self.has_parent == node.has_parent and self.content.isSame(node.content) and self.has_child == node.has_child)

    def isBroken(self):
        """Returns whether the node is marked as broken."""
        return not self.broken is None
        
    def setBroken(self, reason='unknown'):
        """Mark the node as broken.

        Args:
            reason (Optional[str]):

        Returns:
            self

        """
        if self.isBroken(): return self
        self.broken = reason
        return self

    def getContent(self):
        return self.content
        
    def setContent(self, content):
        self.content = content
        self.checkTreeProperty()
        return self

    def getHasChild(self):
        return self.has_child
    
    def getHasParent(self):
        return self.has_parent

    def setHasChild(self, v):
        self.has_child = v
        self.checkTreeProperty()
        return self
        
    def setHasParent(self, v):
        self.has_parent = v
        self.checkTreeProperty()
        return self
        
    def checkTreeProperty(self):
        """Mark the node as broken if there is a contradiction between its environment and its contents.

        Returns:
            self

        """
        
        if self.isBroken(): return self

        # The node has content but the parent is not a directory
        if not self.content.isEmpty() and not self.has_parent:
            self.broken = 'Tree prop nonempty has no parent'

        # The node has child(ren) but is not a directoy
        if self.has_child and not self.content.isDir():
            self.broken = 'Tree prop non-dir has child'

        return self
        
    def assertDescendant(self):
        """Mark the node as broken if it does not have children.

        Returns:
            self

        """
        if self.isBroken(): return self

        # print("assert child on " + self.info())
        if not self.has_child or not self.content.isDir():
            self.broken = 'Assert has child failed'
        return self
        
    def assertNoDescendants(self):
        """Mark the node as broken if it has a child.

        Returns:
            self

        """
        if self.isBroken(): return self

        # print("assert no child on " + self.info())
        if self.has_child:
            self.broken = 'Assert no child failed'
        return self
        
    def assertParent(self):
        """Mark the node as broken if it has no parent.

        Returns:
            self

        """
        if self.isBroken(): return self

        # print("assert parent on " + self.info())
        if not self.has_parent:
            self.broken = 'Assert has parent failed'
        return self
        
    def assertNoParent(self):
        """ Mark the node as broken if it has a parent.

        Returns:
            self

        """
        if self.isBroken(): return self

        # print("assert no parent on " + self.info())
        if self.has_parent:
            self.broken = 'Assert no parent failed'
        return self


def NodeFactory(invalue='Unknown'):
    """Generates all possible nodes.

    Args:
        invalue (Optional[str]): the invalue of the content of the node

    Yields:
        All possible node objects

    """
    yield Node(broken=True)
    for has_parent in [False, True]:
        for content in ContentFactory(invalue):
            for has_child in [False, True]:
                node = Node(has_parent, content, has_child)
                if not node.isBroken():
                    yield node


def isChildRel(rel):
    """Returns if according to the relation p2 is a descendant"""
    return (rel in [DISTANT_CHILD, DIRECT_CHILD, DIRECT_CHILD_ONLY])

def isDirectRel(rel):
    """Returns if the relation is a direct relation"""
    return (rel in [DIRECT_CHILD, DIRECT_CHILD_ONLY, DIRECT_PARENT, DIRECT_PARENT_ONLY])
    
def isOnlyRel(rel):
    """Returns if the relation is an "only" relation"""
    return (rel in [DIRECT_CHILD_ONLY, DIRECT_PARENT_ONLY])

def getReverseRel(rel):
    """Returns the reverse relation"""
    rev = {
        DISTANT_PARENT: DISTANT_CHILD,
        DIRECT_PARENT: DIRECT_CHILD,
        DIRECT_PARENT_ONLY: DIRECT_CHILD_ONLY,
        DISTANT_CHILD: DISTANT_PARENT,
        DIRECT_CHILD: DIRECT_PARENT,
        DIRECT_CHILD_ONLY: DIRECT_PARENT_ONLY,
        SAME: SAME,
        SEPARATE: SEPARATE
    }
    return rev[rel]

def getFilesystemRelationship(rel):
    fs_rel = [SEPARATE if rel == SAME else rel]
    
    if RULE_RELATIONSHIPS_INCLUSIVE:
        if rel == DISTANT_CHILD:
            fs_rel.append(DIRECT_CHILD)
            fs_rel.append(DIRECT_CHILD_ONLY)
        if rel == DIRECT_CHILD:
            fs_rel.append(DIRECT_CHILD_ONLY)
        if rel == DISTANT_PARENT:
            fs_rel.append(DIRECT_PARENT)
            fs_rel.append(DIRECT_PARENT_ONLY)
        if rel == DIRECT_PARENT:
            fs_rel.append(DIRECT_PARENT_ONLY)
    return fs_rel


class Filesystem:
    """Models two paths in a filesystem to simulate the effects of a pair of commands.

    Private properties:    
        p1 (Node): a node object; the filesystem at path p1
        p2 (Node): a node object; the filesystem at path p2
        rel (enum): the relationship between p1 and p2:
            DISTANT_PARENT, DIRECT_PARENT, DIRECT_PARENT_ONLY, DISTANT_CHILD, DIRECT_CHILD, DIRECT_CHILD_ONLY, SEPARATE

    """
    
    def __init__(self, p1, p2, rel):
        """ Constructor.

        Args:
            p1 (Node):
            p2 (Node):
            rel (enum):

        """
        self.p1 = p1
        self.p2 = p2
        self.rel = rel
        self.checkTreeProperty()
        
    def info(self):
        """Returns a human-readable string describing the object."""
        brokenprefix = ""
        if self.isBroken():
            brokenprefix = DISPLAY['FS_broken'] + "  Details: " 
        if not isChildRel(self.rel):
            return brokenprefix + self.p2.info() + DISPLAY['FileSystemRel'][getReverseRel(self.rel)] + self.p1.info()
        else:
            return brokenprefix + self.p1.info() + DISPLAY['FileSystemRel'][self.rel] + self.p2.info()
    
    def clone(self):
        """Returns a deep clone of the object."""
        return self.__class__(self.p1.clone(), self.p2.clone(), self.rel)
        
    def isSame(self, fs):
        """Returns whether the object is the same as another filesystem object."""
        if self.isBroken() and fs.isBroken(): return True
        if self.isBroken() or fs.isBroken(): return False
        return (self.p1.isSame(fs.p1) and self.p2.isSame(fs.p2) and self.rel == fs.rel)
        
    def isExtendedBy(self, fs):
        """Returns whether self is the same as another filesystem object
            when self is not broken.
        """
        if self.isBroken(): return True
        if fs.isBroken(): return False
        return (self.p1.isSame(fs.p1) and self.p2.isSame(fs.p2) and self.rel == fs.rel)
    
    def isBroken(self):
        """Returns whether the filesystem is broken."""
        return (self.p1.isBroken() or self.p2.isBroken())
        
    def checkTreeProperty(self):
        """ Ensure that the environments stored in the nodes
            reflect their contents and their relationship.
            Mark the filesystem as broken if there is a contradiction.
            This is called from the constructor.
            checkTreeProperty on the nodes has already been called.
        """
        
        # If the nodes are separate, all properties are independent,
        # and there is nothing to check
        if self.rel == SEPARATE:
            return self

        # SAME is not possible here, so we have comparable but different paths.

        if isChildRel(self.rel):
            parent = self.p1
            child = self.p2
        else:
            parent = self.p2
            child = self.p1

        # The parent of the parent: we have no extra constraints.
        # The descendant of the parent:
        # (a) If direct and only relationship, and if child is empty, parent has no descendants.
        # (b) If child is not empty, parent has descendants.
        # The parent of the child:
        # (c) If direct, and the parent is not empty, child has a parent.
        # (d) If parent is empty, child has no parent.
        # The descendant of the child: we have no extra constraints.
        #
        # These are mirrored by updates in applyCommand.
        
        # (a)
        if isOnlyRel(self.rel) and child.getContent().isEmpty():
            parent.assertNoDescendants()
            
        # (b)
        if not child.getContent().isEmpty():
            parent.assertDescendant()
            
        # (c)
        if isDirectRel(self.rel) and not parent.getContent().isEmpty():
            child.assertParent()
            
        # (d)
        if parent.getContent().isEmpty():
            child.assertNoParent()

            
    def applyCommand(self, command):
        """Apply a command to the filesystem.

        Note:
            The path to apply the command at is stored in the command.
            This function implements actions based on what the filesystem
            knows: the relationship between the nodes. It updates the
            environments stored in the nodes accordingly.

        Args:
            command (Command): a command object

        Returns:
            self            

        """

        dbg(3, "Applying " + command.info())

        command_path = command.getPath()
        new_content = command.getOutput()
        
        # We update the environments stored in the other node if needed.
        # The node objects mark themselves as broken after any change
        # if that violates the tree property. This is safe, however,
        # as we apply only a single change to each path.
        if self.rel != SEPARATE:
            if isChildRel(self.rel):
                childpath = PATH2
                child = self.p2
                parentpath = PATH1
                parent = self.p1
            else:
                childpath = PATH1
                child = self.p1
                parentpath = PATH2
                parent = self.p2                

            # The parent of the parent: we have no extra constraints.
            # The descendant of the parent:
            # (a) If direct and only relationship, and if child is empty, parent has no descendants.
            # (b) If child is not empty, parent has descendants.
            # The parent of the child:
            # (c) If direct, and the parent is not empty, child has a parent.
            # (d) If parent is empty, child has no parent.
            # The descendant of the child: we have no extra constraints.
            #
            # These are mirrored by the checks in checkTreeProperty

            if command_path == childpath:
                
                # (a) If the only child gets deleted, the parent loses all children
                if isOnlyRel(self.rel) and new_content.isEmpty():
                    parent.setHasChild(False)

                # (b) If the child gets content, the parent gets a child
                if isDirectRel(self.rel) and not new_content.isEmpty():
                    parent.setHasChild(True)
                    
            if command_path == parentpath:
                                
                # (c) If the parent becomes a directory, the child gets a parent
                if isDirectRel(self.rel) and not new_content.isEmpty():
                    child.setHasParent(True)

                # (d) If the parent becomes empty, the child loses parent
                if isDirectRel(self.rel) and new_content.isEmpty():
                    child.setHasParent(False)
                    
  
        # Here the command is always applied to a different path than the one we changed above
        command.applyToNode(self.p1 if command_path == PATH1 else self.p2)
            
        return self
        
    def applySequence(self, sequence):
        """Apply a sequence of commands to the filesystem."""
        sequence.map(lambda x: self.applyCommand(x))
        return self


def FilesystemFactory(rel_list):
    """Generates all possible filesystems with the given relationship between p1 and p2.

    Args:
        rel_list (list): List of
            DISTANT_PARENT, DIRECT_PARENT, DIRECT_PARENT_ONLY, DISTANT_CHILD, DIRECT_CHILD, DIRECT_CHILD_ONLY, SEPARATE

    Yields:
        possible filesystem objects

    """
    for rel in rel_list:
        yield Filesystem(Node(broken=True), Node(broken=True), rel)
        for p1_source in NodeFactory('Old1'):
            for p2 in NodeFactory('Old2'):
                fs = Filesystem(p1_source.clone(), p2, rel)  # the constructor may break the p1 node, so we need to clone
                if not fs.isBroken():
                    yield fs


# Constants for Command.path:
PATH1 = 'P1'
PATH2 = 'P2'


class Command:
    """Represents a command.

    Private properties:
        path (enum): PATH1 or PATH2
        inp (Content): a content object. Its invalue is disregarded; only the type is used
            to note the type of content the command expects in the filesystem
            before it runs
        outp (Content): a content object the command stores at the path
    """
    
    def __init__(self, path, inp, outp):
        """Constructor.

        Args:
            path (enum):
            inp (Content):
            outp (Content):

        """
        self.path = path
        self.inp = inp
        self.outp = outp
        
    def info(self):
        """Returns a human-readable string describing the object."""
        return "<" + self.inp.info(False) + "," + self.outp.info(True) + "," + self.path + ">"
        
    def label(self):
        """Returns a short string describing some parts of the object."""
        return self.inp.label() + self.outp.label()

    def isSame(self, command):
        """Returns whether self is the same as another command object."""
        if self.path != command.path: return False
        if not self.inp.isSame(command.inp): return False
        if not self.outp.isSame(command.outp): return False
        return True
    
    def isDirToDir(self):
        """Returns if the command is a Dir->Dir."""
        return self.getInput().isDir() and self.getOutput().isDir()
    
    def getPath(self):        
        return self.path
    
    def getInput(self):
        return self.inp
        
    def getOutput(self):
        return self.outp

    def applyToNode(self, node):
        """Apply the command to a node.

        Args:
            node (Node): a node object

        Returns:
            self

        """
        if node.getContent().getType() != self.inp.getType():
            node.setBroken('Command in type mismatch')
            return self
        node.setContent(self.outp)
        return self


def CommandFactory(path, invalue):
    """Generates all possible commands that uses the given path and value in its output content (if applicable).

    Args:
        path (enum): the path
        invalue (str): an arbitrary string representing the invalue (all possible types are generated)

    Yields:
        all possible command objects

    """
    for c1 in ContentFactory('N/A'):
        for c2 in ContentFactory(invalue):
            yield Command(path, c1, c2)


class Sequence:
    """Represents a sequence of commands.
    
    Private properties:
        commands (list): a list of command objects

    """
    
    def __init__(self, commands):
        self.commands = commands
        
    def info(self):
        """Returns a human-readable string describing the object"""
        return "; ".join(map(lambda x: x.info(), self.commands))
        
    def clone(self):
        """Returns a shallow clone of the sequence. Commands are not mutable."""
        return self.__class__(self.commands[:])
        
    def getReverse(self):
        """Returns a new object with the sequence reversed."""
        tmp = self.clone()
        tmp.commands.reverse()
        return tmp
        
    def map(self, func):
        """Applies func to all commands in the sequence."""
        list(map(func, self.commands))


class CommandPair(Sequence):
    """Represents a pair of commands, including the relationship between their paths.
    
    Private properties:
        commands (list): see parent class
        rel (enum): the relationship between the paths of the two commands.
            DISTANT_PARENT, DIRECT_PARENT, DIRECT_PARENT_ONLY, DISTANT_CHILD, DIRECT_CHILD, DIRECT_CHILD_ONLY, SEPARATE, SAME

    """
    # There is no need to consider any other relationship, e.g. when one path is a parent,
    # but not a direct parent of the other one, as unless there is a direct relationship,
    # the command on one path is not going to change the environment of the other.

    def __init__(self, command1, command2, rel):
        """Constructor.
        
        Args:
            command1 (Command): the first command in the pair
            command2 (Command): the second command in the pair
            rel (enum): the relationship between the paths of the two commands
        
        """
        self.commands = [command1, command2]
        self.rel = rel
        
    def info(self):
        """Returns a human-readable string describing the object"""
        return self.commands[0].info() + ' ' + DISPLAY['CommandPairRel'][self.rel] + ' ' + self.commands[1].info()
        
    def label(self):
        """Returns a short string describing some of the object"""
        return self.commands[0].label() + DISPLAY['CommandPairRel'][self.rel] + self.commands[1].label();
    
    def clone(self):
        """Returns a shallow clone of the object. Commands are not mutable."""
        return self.__class__(self.commands[0], self.commands[1], self.rel)
        
    def getRelationship(self):
        return self.rel
    
    def getReverse(self):
        """Returns a new object with the pair reversed."""
        tmp = Sequence.getReverse(self)
        tmp.rel = getReverseRel(self.rel)
        return tmp
    
    def getFirst(self):
        """Returns the first command in the pair."""
        return self.commands[0]
    
    def getLast(self):
        """Returns the last command in the pair."""
        return self.commands[1]
    

def CommandPairFactory(rel, printtable=False):
    """Generates all possible command pairs. The file content values used will always be different."""
    
    # Print a header line:
    if DEBUG == 0 and printtable:
        pr('    ')
        for c2 in CommandFactory(PATH2, 'New2'):
           pr(c2.label() + ' ')
        pr('\n')
    
    for c1 in CommandFactory(PATH1, 'New1'):
        if printtable: pr_ex(c1.label() + ': ') # Table row header
        for c2 in CommandFactory(PATH1 if (rel == SAME) else PATH2, 'New2'):
            yield CommandPair(c1, c2, rel)
        if printtable: pr_ex('\n') # Table row ends


def pr(s):
    """Print to STDOUT (without newline) and flush"""
    sys.stdout.write(s)
    sys.stdout.flush()

def pr_ex(s):
    """Print to STDOUT only if not in debug mode"""
    if DEBUG == 0:
        pr(s)

def dbg(level, msg):
    """Print debug message"""
    if level <= DEBUG:
        pr("  " * level + msg + "\n")

def ruletitle(s):
    """Print the rule name being tested"""
    pr(s + ": ")

def fail():
    pr("FAIL\n")
    # TODO Abort
    
def ok():
    pr("Ok\n")



ruletitle('Rule 1')
# Commands on incomparable nodes commute
for sq in CommandPairFactory(SEPARATE):
    sq_rev = sq.getReverse()
    for fs in FilesystemFactory(SEPARATE):
        fs_res = fs.clone()
        fs_res.applySequence(sq)
        fs_rev_res = fs.clone()
        fs_rev_res.applySequence(sq_rev)
        if not fs_res.isSame(fs_rev_res):
            fail()
ok()


ruletitle('Rule 2')
# Commands on incomparble nodes do not break all filesystems
for sq in CommandPairFactory(SEPARATE):
    for fs in FilesystemFactory(SEPARATE):
        fs.applySequence(sq)
        if not fs.isBroken():
            break
    else:
        continue # trick to achieve break(2)
    ok()
    break
else:
    fail()


ruletitle('Rule 3')
# Commands on the same node break every filesystem if their types are incompatible
for sq in CommandPairFactory(SAME):
    if sq.getFirst().getOutput().getType() == sq.getLast().getInput().getType():
        continue
    for fs in FilesystemFactory(SEPARATE):
        fs.applySequence(sq)
        if not fs.isBroken():
            break
    else:
        continue # trick to achieve break(2)
    fail()
    break
else:
    ok()


ruletitle('Rule 4')
# Commands on the same node simplify into an empty sequence
for sq in CommandPairFactory(SAME):
    if sq.getFirst().getOutput().getType() != sq.getLast().getInput().getType():
        continue
    if not(
        (sq.getFirst().getInput().getType() == EMPTY and sq.getLast().getOutput().getType() == EMPTY)
        or
        (sq.getFirst().getInput().getType() == DIR and sq.getLast().getOutput().getType() == DIR)
    ):
        continue


exit(0)





        

SingleCommandRules = ""

def pr_s(s):
    """Print to STDOUT and save to single command rules"""
    global SingleCommandRules
    SingleCommandRules += s
    pr(s)


def checkBreaksAll(sq):
    """Returns if the command pair sq breaks all filesystems"""
    for fs in FilesystemFactory(fs_rel):
        dbg(2, "Before: " + fs.info())
        fs.applySequence(sq)
        dbg(2, "After: " + fs.info())
        if not fs.isBroken():
            return False
    return True


print("Please see the documentation in the script");

for rel in [SEPARATE, SAME, DISTANT_CHILD, DIRECT_CHILD, DIRECT_CHILD_ONLY, DISTANT_PARENT, DIRECT_PARENT, DIRECT_PARENT_ONLY]:
    pr_s('\n===== Relationship between nodes: ' + rel + ' =====\n')
    fs_rel = getFilesystemRelationship(rel)

    pr('\nDo they break all filesystems?\n')
    for sq in CommandPairFactory(rel):

        # Does the pair break all filesystems?
        dbg(1, "Does " + sq.info() + " break all filesystems?")
        if checkBreaksAll(sq):
            dbg(1, "Yes")
            pr_ex(DISPLAY['Res_broken'])
        else:
            dbg(1, "No")
            pr_ex(DISPLAY['Res_nothing'])

    pr('\nRelationship to an empty sequence:\n')
    for sq in CommandPairFactory(rel):

        # Is the pair the same as no command at all?
        nothingEq = True  # Whether sq is equivalent to no commands on all filesystems
        nothingExt = True # Whether no commands is an extenstion of sq
        dbg(1, "Relationship to empty sequence of " + sq.info())
        for fs in FilesystemFactory(fs_rel):
            fs_res = fs.clone()
            dbg(2, "Before: " + fs_res.info())
            fs_res.applySequence(sq)
            dbg(2, "After: " + fs_res.info())
            if not fs_res.isSame(fs):
                dbg(3, "Not same")
                nothingEq = False
            if not fs_res.isExtendedBy(fs):
                dbg(3, "Not extending")
                nothingExt = False
                break
            
        if nothingEq: 
            dbg(1, "Equals empty sequence")
            pr_ex(DISPLAY['Res_equiv'])
        elif nothingExt: 
            dbg(1, "Extended by empty sequence")
            pr_ex(DISPLAY['Res_extend'])
        else:
            dbg(1, "No result")
            pr_ex(DISPLAY['Res_nothing'])
        
    pr('\nRelationship to a single command:\n')
    for sq in CommandPairFactory(rel):

        # Equal to the empty (break) function
        if checkBreaksAll(sq):
            dbg(1, "Equal to the break command")
            pr_ex(DISPLAY['Res_broken'])
            continue

        # Try to find a single command with the same effect
        # We try to find a command based on both commands in the pair.
        # NB this can lead to finding the same command multiple times
        singleIsEq = False
        singleIsExt = False
        dbg(1, "Can " + sq.info() + " be simplified?")
        for command in chain(
                CommandFactory(sq.getFirst().getPath(), sq.getFirst().getOutput().getValue()), 
                CommandFactory(sq.getLast().getPath(), sq.getLast().getOutput().getValue())
        ):
            dbg(2, "Try if " + command.info() + " works")
            simplifiesEq = True  # Whether command is equivalent to sq on all filesystems
            simplifiesExt = True # Whether command extends sq
            for fs in FilesystemFactory(fs_rel):
                dbg(3, "Before: " + fs.info())
                # Apply the original sequence
                fs_res = fs.clone()
                fs_res.applySequence(sq)
                dbg(3, "After original: " + fs_res.info())
                # Apply the single command
                fs_single = fs.clone()
                fs_single.applyCommand(command)
                dbg(3, "After single c: " + fs_single.info())
                if not fs_res.isSame(fs_single):
                    simplifiesEq = False
                if not fs_res.isExtendedBy(fs_single):
                    simplifiesExt = False
                    break
            if simplifiesEq:
                SingleCommandRules += sq.info() + " " + DISPLAY['Res_equiv_s'] + " " + command.info() + "\n"
                singleIsEq = True
                dbg(2, "It is equal")
            elif simplifiesExt:
                SingleCommandRules += sq.info() + " " + DISPLAY['Res_extend_s'] + " " + command.info() + "\n"
                singleIsExt = True
                dbg(2, "It extends")

        if singleIsEq:
            pr_ex(DISPLAY['Res_equiv'])
            dbg(1, "Yes via equivalence")
        elif singleIsExt:
            pr_ex(DISPLAY['Res_extend'])
            dbg(1, "Yes via extending")
        else:
            pr_ex(DISPLAY['Res_nothing'])
            dbg(1, "No")
        
    pr('\nRelationship to the reverse sequence:\n')
    for sq in CommandPairFactory(rel):

        # Reverse sequence
        sq_rev = sq.getReverse()
        dbg(1, "How does " + sq.info() + " relate to " + sq_rev.info() + " ?")
        
        reverseEq = True  # Whether the reversed pair is equivalent to sq on all filesystems
        reverseExt = True # Whether the reversed pair extends sq
        for fs in FilesystemFactory(fs_rel):
            dbg(2, "Before: " + fs.info())
            # Apply the original sequence
            fs_res = fs.clone()
            fs_res.applySequence(sq)
            dbg(2, "After original: " + fs_res.info())
            # Apply the reverse sequence
            fs_rev_res = fs.clone()
            fs_rev_res.applySequence(sq_rev)
            dbg(2, "After reverse: " + fs_rev_res.info())
            if not fs_res.isSame(fs_rev_res):
                reverseEq = False
            if not fs_res.isExtendedBy(fs_rev_res):
                reverseExt = False

        if reverseEq:
            pr_ex(DISPLAY['Res_equiv'])
            dbg(1, "Equivalent")
        elif reverseExt: 
            pr_ex(DISPLAY['Res_extend'])
            dbg(1, "Extends")
        else:
            pr_ex(DISPLAY['Res_nothing'])
            dbg(1, "No relationship")

print("\n\n===== Substitutions for single commands =====\n\n" + SingleCommandRules)

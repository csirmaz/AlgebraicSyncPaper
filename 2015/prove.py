from itertools import chain

# Whether directories have equal contents, i.e. there is only a single
# directory-type value in the filesystem.
ONE_DIRECTORY_VALUE = False

STYLE = 'tex' # 'normal or 'debug' or 'tex'

# Constants for Content.type:
DIR   = ' Dir'
FILE  = 'File'
EMPTY = 'Empty'


class Content:
    """Represents some content (element of set V) at a given path in the filesystem
    
    Private properties:
        type (enum): DIR or FILE or EMPTY
        value (str): an arbitrary string representing e.g. file contents and metadata
        
    """

    def __init__(self, type=EMPTY, value='Unknown'):
        """Constructor
        
        Args:
            type (Optional[enum]):
            value (Optional[str]):
        
        """
        self.type = type
        self.value = value
        
    def clone(self):
        """Returns a deep clone of the object."""
        return self.__class__(self.type, self.value)

    def info(self, addvalue=True, path=None):
        """Returns human-readable information about the object.
        
        Args:
            addvalue (bool): whether to add a description of the value
            path (None or string): used with TeX output; added if addvalue to become the first argument of a command
        
        """
        if STYLE == 'tex':
            if self.isEmpty(): rc = 'b'
            elif self.isDir(): rc = 'd'
            elif self.isFile(): rc = 'f'
            r = rc
            if addvalue: r += '(' + path
        else:
            r = self.type
        if STYLE == 'debug' or (addvalue and not self.isEmpty() and not(ONE_DIRECTORY_VALUE and self.isDir())):
            if STYLE == 'tex':
                r += ', ' + rc.upper() + '_' + self.value[-1:]
            else:
                r += '(' + self.value + ')'
        if STYLE == 'tex' and addvalue: r += ')'
        return r
    
    def label(self):
        """Returns a short string describing some parts of the object."""
        return self.info(False)
        
    def isSame(self, content):
        """Returns whether the object is the same as another content object."""
        if self.type != content.type: return False
        if self.isEmpty(): return True
        if ONE_DIRECTORY_VALUE and self.isDir(): return True
        return (self.value == content.value)
        
    def getType(self):
        return self.type
    
    def getValue(self):
        if self.isEmpty(): return 'Unknown'
        return self.value
        
    def isDir(self):
        """Returns whether the type of the object is 'diretory'."""
        return (self.type == DIR)

    def isFile(self):
        """Returns whether the type of the object is 'file'."""
        return (self.type == FILE)

    def isEmpty(self):
        """Returns whether the type of the object is 'empty'."""
        return (self.type == EMPTY)


def ContentFactory(value='Unknown'):
    """Generates all possible contents.
    
    Args:
        value (Optional[str]): the value of the content objects
        
    Yields:
        All possible content objects
        
    """
    yield Content(EMPTY)
    yield Content(FILE, value)
    yield Content(DIR, value)


class Node:
    """Represents a path in the filesystem and information about its environment

    Private properties:    
        has_parent (bool): whether the node has a parent (i.e. there is a directory at the parent path)
        content (Content): a content object
        has_child (bool): whether the node has a child (i.e. one of the child paths is not empty)
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
            if debug:
                r.append("Broken(" + self.broken + ") ")
            else:
                return "(Broken)"
        if self.has_parent: r.append("o--")
        r.append(self.content.info())
        if self.has_child: r.append("--o")
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
            broken (Optional[str]):

        Returns:
            self

        """
        self.broken = reason
        return self

    def getContent(self):
        return self.content
        
    def setContent(self, content):
        self.content = content
        self.checkTreeProperty()
        return self
        
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
        if not self.content.isEmpty() and not self.has_parent:
            self.broken = 'tree-nonempty-noparent'
        if self.has_child and not self.content.isDir():
            self.broken = 'tree-notdir-haschild'
        return self
        
    def assertDescendant(self):
        """Mark the node as broken if it does not have children.

        Returns:
            self

        """
        # print "assert child on " + self.info()
        if not self.has_child or not self.content.isDir():
            self.broken = 'assert-child'
        return self
        
    def assertNoDescendants(self):
        """Mark the node as broken if it has a child.

        Returns:
            self

        """
        # print "assert no child on " + self.info()
        if self.has_child:
            self.broken = 'assert-no-child'
        return self
        
    def assertParent(self):
        """Mark the node as broken if it has no parent.

        Returns:
            self

        """
        # print "assert parent on " + self.info()
        if not self.has_parent:
            self.broken = 'assert-parent'
        return self
        
    def assertNoParent(self):
        """ Mark the node as broken if it has a parent.

        Returns:
            self

        """
        # print "assert no parent on " + self.info()
        if self.has_parent:
            self.broken = 'assert-no-parent'
        return self


def NodeFactory(contents='Unknown'):
    """Generates all possible nodes.

    Args:
        contents (Optional[str]): the content of the value of the node

    Yields:
        All possible node objects

    """
    yield Node(broken=True)
    for has_parent in [False, True]:
        for content in ContentFactory(contents):
            for has_child in [False, True]:
                node = Node(has_parent, content, has_child)
                if not node.isBroken():
                    yield node


# Constants for Filesystem.rel and CommandPair.rel:
DIRECT_PARENT      = 'DirectParent'      # p2 is the parent of p1
DIRECT_PARENT_ONLY = 'DirectParentOnly'  # p2 is the parent of p1 and p1 is the only child
DIRECT_CHILD       = 'DirectChild'       # p2 is the child of p2
DIRECT_CHILD_ONLY  = 'DirectChildOnly'   # p2 is the child of p1 and p2 is the only child
SEPARATE           = 'Separate'          # all other cases
SAME               = 'Same'              # two paths are the same (used for command pairs)

class Filesystem:
    """Models two paths in a filesystem to simulate the effects of a pair of commands.

    Private properties:    
        p1 (Node): a node object; the filesystem at path p1
        p2 (Node): a node object; the filesystem at path p2
        rel (enum): the relationship between p1 and p2:
            DIRECT_PARENT or DIRECT_PARENT_ONLY or DIRECT_CHILD or DIRECT_CHILD_ONLY or SEPARATE

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
        if self.isBroken() and not STYLE == 'debug':
            return "[Broken]"
        if self.rel == SEPARATE:
            return self.p1.info() + " ==x== " + self.p2.info()
        if self.rel == DIRECT_CHILD:
            return self.p1.info() + " ===<> " + self.p2.info()
        if self.rel == DIRECT_CHILD_ONLY:
            return self.p1.info() + " ===>> " + self.p2.info()
        if self.rel == DIRECT_PARENT:
            return self.p2.info() + " ~~~<> " + self.p1.info()
        if self.rel == DIRECT_PARENT_ONLY:
            return self.p2.info() + " ~~~>> " + self.p1.info()
    
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
        """Mark the filesystem as broken if there is a contradiction
            between the environments noted in the nodes an their relationship.
        """
        if self.rel == SEPARATE:
            return self

        if self.rel in [DIRECT_CHILD, DIRECT_CHILD_ONLY]:
            parent = self.p1
            child = self.p2
        else:
            parent = self.p2
            child = self.p1
            
        only = self.rel in [DIRECT_CHILD_ONLY, DIRECT_PARENT_ONLY]
    
        if not child.getContent().isEmpty():
            # print "child not empty"
            parent.assertDescendant()

        if not parent.getContent().isEmpty():
            # print "parent not empty"
            child.assertParent()
        else:
            # print "parent empty"
            child.assertNoParent()

        if only and child.getContent().isEmpty():
            # print "only child and child empty"
            parent.assertNoDescendants()
            
            
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

        command_path = command.getPath()
        new_content = command.getEnd()

        # We update the environments stored in the other node if needed.
        # The node objects mark themselves as broken after any change
        # if that violates the tree property. This is safe, however,
        # as we apply only a single change to each path.
        if self.rel != SEPARATE:
            if self.rel in [DIRECT_CHILD, DIRECT_CHILD_ONLY]:
                childpath = PATH2
                child = self.p2
                parentpath = PATH1
                parent = self.p1
            else:
                childpath = PATH1
                child = self.p1
                parentpath = PATH2
                parent = self.p2                

            if command_path == childpath:
                
                # If the child will have content, the parent will have a child
                if not new_content.isEmpty():
                    parent.setHasChild(True)
                
                # If the only child is deleted, the parent will have no child
                if self.rel in [DIRECT_CHILD_ONLY, DIRECT_PARENT_ONLY] and new_content.isEmpty():
                    parent.setHasChild(False)
                    
            if command_path == parentpath:
            
                # If the parent is deleted, the child will have no parent
                if new_content.isEmpty():
                    child.setHasParent(False)
                
                # If the parent is created, the child will have a parent
                else:
                    child.setHasParent(True)
  
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
        rel_list (list): List of DIRECT_PARENT or DIRECT_PARENT_ONLY or DIRECT_CHILD or DIRECT_CHILD_ONLY or SEPARATE

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
        start (Content): a content object. Its value is disregarded; only the type is used
            to note the type of content the command expects in the filesystem
            before it runs
        end (Content): a content object the command turns the path into
    """
    
    def __init__(self, path, start, end):
        """Constructor.

        Args:
            path (enum):
            start (Content):
            end (Content):

        """
        self.path = path
        self.start = start
        self.end = end
        
    def info(self, isParent=False, asSequence=False):
        """Returns a human-readable string describing the object."""
        if STYLE == 'tex':
            path = '\\pp' if isParent else 'p'
            path += '_1' if self.path == PATH1 else '_2'
            r = '\\c' + self.start.info(False) + self.end.info(True, path)
            if asSequence: return '[' + r + ']'
            return r
        else:
            return "{" + self.path + ":" + self.start.info(False) + ">" + self.end.info(True) + "}"
        
    def label(self):
        """Returns a short string describing some parts of the object."""
        return self.start.label() + self.end.label()

    def isSame(self, command):
        """Returns whether self is the same as another command object."""
        if self.path != command.path: return False
        if not self.start.isSame(command.start): return False
        if not self.end.isSame(command.end): return False
        return True
    
    def isDirToDir(self):
        """Returns if the command is a Dir->Dir."""
        return self.getStart().isDir() and self.getEnd().isDir()
    
    def getPath(self):        
        return self.path
    
    def getStart(self):
        return self.start
        
    def getEnd(self):
        return self.end

    def applyToNode(self, node):
        """Apply the command to a node.

        Args:
            node (Node): a node object

        Returns:
            self

        """
        if node.getContent().getType() != self.start.getType():
            node.setBroken('command-start')
            return self
        node.setContent(self.end)
        return self


def CommandFactory(path, value):
    """Generates all possible commands that uses the given path and value in its end-content (if applicable).

    Args:
        path (enum): the path
        value (str): an arbitrary string representing the value (e.g. file contents and metadata)

    Yields:
        all possible command objects

    """
    for c1 in ContentFactory('N/A'):
        for c2 in ContentFactory(value):
            if not c1.isSame(c2): # Skip noop (assertion) commands
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
        return map(func, self.commands)


# Cosntants for axiom groups
A_BREAKING = 'Br'
A_SIMPLIFY = 'Sp'
A_COMMUTE = 'Cm'
A_NORULE = 'No'
A_UNKNOWN = '??'


class CommandPair(Sequence):
    """Represents a pair of commands, including the relationship between their paths.
    
    Private properties:
        commands (list): see parent class
        rel (enum): the relationship between the paths of the two commands.
            DIRECT_PARENT or DIRECT_CHILD or SEPARATE or SAME

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
        if STYLE == 'tex':
            return '[' + self.commands[0].info(self.rel == DIRECT_CHILD) + '; ' + self.commands[1].info(self.rel == DIRECT_PARENT) + ']'
        else:
            Dsep = {SEPARATE: 'xx', DIRECT_CHILD: '->', DIRECT_PARENT: '<-', SAME: '--'}
            return self.commands[0].info() + ' ' + Dsep[self.rel] + ' ' + self.commands[1].info()
        
    def label(self):
        """Returns a short string describing some of the object"""
        Dsep = {SEPARATE: 'x', DIRECT_CHILD: '>', DIRECT_PARENT: '<', SAME: '='}        
        return self.commands[0].label() + Dsep[self.rel] + self.commands[1].label();
    
    def info_label(self):
        """Returns the label and info."""
        return self.label() + " " + self.predictAxiomGroup() + "  " + self.info()
        
    def clone(self):
        """Returns a shallow clone of the object. Commands are not mutable."""
        return self.__class__(self.commands[0], self.commands[1], self.rel)
        
    def getRelationship(self):
        return self.rel

    def getReverse(self):
        """Returns a new object with the pair reversed."""
        tmp = Sequence.getReverse(self)
        if self.rel == DIRECT_PARENT:
            tmp.rel = DIRECT_CHILD
        elif self.rel == DIRECT_CHILD:
            tmp.rel = DIRECT_PARENT
        return tmp
    
    def getFirst(self):
        """Returns the first command in the pair."""
        return self.commands[0]
    
    def getLast(self):
        """Returns the last command in the pair."""
        return self.commands[1]
    
    def predictAxiomGroup(self):
        """Returns the predicted rule type."""
        if self.rel == SEPARATE:
            return A_COMMUTE
        if self.rel == SAME and self.getFirst().getEnd().getType() != self.getLast().getStart().getType():
            return A_BREAKING
        if self.rel == SAME:
            return A_SIMPLIFY
        
        if not ONE_DIRECTORY_VALUE:
            if self.rel == DIRECT_CHILD and self.getFirst().isDirToDir():
                return A_COMMUTE
            if self.rel == DIRECT_PARENT and self.getLast().isDirToDir():
                return A_COMMUTE
        
        return A_UNKNOWN


def CommandPairFactory():
    """Generates all possible command pairs. The file content values used will always be different."""
    for rel in [SEPARATE, DIRECT_CHILD, DIRECT_PARENT]:
        for c1 in CommandFactory(PATH1, 'New1'):
            for c2 in CommandFactory(PATH2, 'New2'):
                yield CommandPair(c1, c2, rel)
    for c1 in CommandFactory(PATH1, 'New1'):
        for c2 in CommandFactory(PATH1, 'New2'):
            yield CommandPair(c1, c2, SAME);

# We test command pairs and aim to answer the following questions:
# - Will the pair break all filesystems?
RulesBreaking = []
# - If not, can the pair be substituted by no commands at all?
# - If not, can the pair be substituted by a single command?
RulesSimplified = []
# - If not, can the pair be reversed?
RulesCommute = []
# We are also interested in substitutions that extend the domain of the sequence.

RulesNoRule = []

if STYLE == 'tex':
    Pequ = ' \\equiv '
    Pext = ' \\eqext '
    Pbreak = '\\break'
    Pnocomm = '[]'
    Pnorule = ''
else:
    Pequ = ' == '
    Pext = ' =[ '
    Pbreak = 'break'
    Pnocomm = '(no commands)'
    Pnorule = ' (no rule)'
    

# print "Default: XY xx ZW == ZW xx XY"
# print "Default otherwise: XY ?? ZW == break"

for sq in CommandPairFactory():
    
    # print "  " + sq.info()

    # We investigate filesystem models in which the two paths
    # have the relationship encoded in the command pair.
    # If the two commands in the pair use the same path,
    # we get the other path in the filesystem model out of the way
    # by using a SEPARATE relationship.
    fs_rel = [sq.getRelationship()]
    if fs_rel[0] == SAME:
        fs_rel[0] = SEPARATE
    elif fs_rel[0] == DIRECT_CHILD:
        fs_rel.append(DIRECT_CHILD_ONLY)
    elif fs_rel[0] == DIRECT_PARENT:
        fs_rel.append(DIRECT_PARENT_ONLY)
        
    # Does the pair break all filesystems?
    for fs in FilesystemFactory(fs_rel):
        fs.applySequence(sq)
        if not fs.isBroken():
            break # Skips "else" below
    else: # If none is not broken
        # if sq.getRelationship() == SEPARATE:
        RulesBreaking.append(sq.info_label() + Pequ + Pbreak)
        continue
        
    # Is the pair the same as no command at all?
    nothingEq = True  # Whether sq is equivalent to no commands on all filesystems
    nothingExt = True # Whether no commands is an extenstion of sq
    for fs in FilesystemFactory(fs_rel):
        fs_res = fs.clone()
        fs_res.applySequence(sq)
        if not fs_res.isSame(fs): nothingEq = False
        if not fs_res.isExtendedBy(fs): nothingExt = False
    if nothingEq:
        RulesSimplified.append(sq.info_label() + Pequ + Pnocomm)
        continue
    if nothingExt:
        RulesSimplified.append(sq.info_label() + Pext + Pnocomm)
        continue
    

    # Try to find a single command with the same effect
    # We try to find a command based on both commands in the pair.
    # However, this can lead to finding the same command twice;
    # a simple deduplication attempt is coded below.
    simplifiedByEq = None
    simplifiedByExt = None
    for command in chain(CommandFactory(sq.getFirst().getPath(), sq.getFirst().getEnd().getValue()), CommandFactory(sq.getLast().getPath(), sq.getLast().getEnd().getValue())):
        # print " " + command.info()
        simplifiesEq = True  # Whether command is equivalent to sq on all filesystems
        simplifiesExt = True # Whether command extends sq
        for fs in FilesystemFactory(fs_rel):
            # print "  " + fs.info()
            # Apply the original sequence
            fs_res = fs.clone()
            fs_res.applySequence(sq)
            # print "    " + fs_res.info()
            # Apply the single command
            fs_single = fs.clone()
            fs_single.applyCommand(command)
            # print "    " + fs_single.info()
            if not fs_res.isSame(fs_single): simplifiesEq = False
            if not fs_res.isExtendedBy(fs_single): simplifiesExt = False
        if simplifiesEq:
            if simplifiedByEq is None or not simplifiedByEq.isSame(command):
                simplifiedByEq = command
                RulesSimplified.append(sq.info_label() + Pequ + command.info(asSequence=True))
        elif simplifiesExt:
            if simplifiedByExt is None or not simplifiedByExt.isSame(command):
                simplifiedByExt = command
                RulesSimplified.append(sq.info_label() + Pext + command.info(asSequence=True))
    
    if not(simplifiedByEq is None) or not(simplifiedByExt is None) : continue

    # Reverse sequence
    sq_rev = sq.getReverse()
    # print sq_rev.info()
    
    reverseEq = True  # Whether the reversed pair is equivalent to sq on all filesystems
    reverseExt = True # Whether the reversed pair extends sq
    for fs in FilesystemFactory(fs_rel):
        # Apply the original sequence
        fs_res = fs.clone()
        fs_res.applySequence(sq)
        # Apply the reverse sequence
        fs_rev_res = fs.clone()
        fs_rev_res.applySequence(sq_rev)
        if not fs_res.isSame(fs_rev_res): reverseEq = False
        if not fs_res.isExtendedBy(fs_rev_res): reverseExt = False
    if reverseEq:
        # if sq.getRelationship() != SEPARATE:
        RulesCommute.append(sq.info_label() + Pequ + sq_rev.info())
        continue
    if reverseExt:
        RulesCommute.append(sq.info_label() + Pext + sq_rev.info())
        continue
    
    RulesNoRule.append(sq.info_label() + Pnorule)

print "\nBreaking rules"
print "\n".join(RulesBreaking)
print "\nSimplification rules"
print "\n".join(RulesSimplified)
print "\nCommuting rules"
print "\n".join(RulesCommute)
print "\nNo rules"
print "\n".join(RulesNoRule)

